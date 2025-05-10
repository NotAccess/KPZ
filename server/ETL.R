library(httr)
library(dplyr)
library(purrr)
library(duckdb)
library(DBI)
library(lubridate)

GITHUB_TOKEN <- Sys.getenv('GITHUB_TOKEN')
github_api_get <- function(url) {
  # обработка запросов к GitHub API по URL 
  if (nzchar(GITHUB_TOKEN)) {
    response <- GET(url, add_headers(Authorization = paste("token", GITHUB_TOKEN)))
  } else {
    response <- GET(url)
  }

  if (status_code(response) == 204) {
    message("Репозиторий не содержит данных (204 No Content).")
    return(NULL)
  }

  if (status_code(response) == 401) {
    stop(paste("Ошибка авторизации (проверьте токен):", status_code(response)))
  }

  if (status_code(response) == 403) {
    stop("Лимит запросов исчерпан. Пожалуйста, обновите GitHub токен.")
  }
  
  if (status_code(response) == 404) {
    message("Пользователь GitHub с данным именем не найден.")
    return(NULL)
  }

  if (status_code(response) == 409) {
    message("Репозиторий пустой или конфликт (409 Conflict).")
    return(NULL)
  }

  if (status_code(response) >= 500 && status_code(response) < 600) {
    message("Ошибка сервера.")
    return(NULL)
  }
  
  if (status_code(response) != 200) {
    stop(paste("Ошибка при запросе к GitHub API:", status_code(response)))
  } 

  return(response)
}

get_user_repos <- function(username, setProgress) {
  repos <- list()
  url <- paste0("https://api.github.com/users/", username, "/repos?per_page=100")

  total_pages <- 1
  current_page <- 1
  start_time <- Sys.time()
  setProgress(message = "🌐 Поиск репозиториев:", value = 0)

  repeat {
    response <- github_api_get(url)
    if (is.null(response)) break

    current_repos <- content(response, "parsed")
    repos <- c(repos, current_repos)

    if (current_page == 1) {
      link_header <- headers(response)$link
      if (!is.null(link_header) && grepl('rel="last"', link_header)) {
        total_pages <- regexpr('<https://[^>]+>; rel="last"', link_header) %>%
          regmatches(link_header, .) %>%
          gsub('<|>; rel="last"', '', .) %>%
          gsub(".*page=(\\d+).*", "\\1", .) %>%
          as.numeric()
      }
    }

    setProgress(detail = sprintf("%d", length(repos)))

    link_header <- headers(response)$link
    if (is.null(link_header) || !grepl('rel="next"', link_header)) break
    
    url <- regexpr('<https://[^>]+>; rel="next"', link_header) %>%
      regmatches(link_header, .) %>%
      gsub('<|>; rel="next"', '', .)
    current_page <- current_page + 1
  }

  total_repos <- length(repos)
  if (total_repos == 0) return(NULL)
  
  start_process <- Sys.time()
  setProgress(message = "⚙️ Обработка репозиториев:", value = 0)
  repo_data <- map(seq_along(repos), function(i) {
    repo <- repos[[i]]
    
    remaining <- (Sys.time() - start_time) %>% 
      as.numeric(units = "secs") %>% 
      {. * (total_repos - i) / i}
    
    setProgress(
      value = i / total_repos,
      detail = sprintf(
        "%d/%d (%s)",
        i, total_repos,
        format(.POSIXct(remaining, tz = "GMT"), "%H:%M:%S")
      )
    )

    contributors_response <- github_api_get(paste0("https://api.github.com/repos/", repo$full_name, "/contributors"))
    contributors_count <- if (!is.null(contributors_response)) length(content(contributors_response, "parsed")) else 0

    list(
      username = username,
      avatar = current_repos$avatar_url,
      name = repo$name,
      full_name = repo$full_name,
      description = if (!is.null(repo$description)) repo$description else "Нет описания",
      language = if (!is.null(repo$language)) repo$language else "Не указан",
      stars = repo$stargazers_count,
      forks = repo$forks_count,
      created_at = as.POSIXct(repo$created_at, format = "%Y-%m-%dT%H:%M:%SZ"),
      updated_at = as.POSIXct(repo$updated_at, format = "%Y-%m-%dT%H:%M:%SZ"),
      url = repo$html_url,
      open_issues = repo$open_issues_count,
      contributors = contributors_count,
      license = if (!is.null(repo$license)) repo$license$name else "Нет лицензии",
      size = repo$size
    )
  })

  return(repo_data)
}

DUCK_DB <- Sys.getenv('DUCK_DB')
COMMITS_TABLE <- Sys.getenv('COMMITS_TABLE')

get_user_commits_df <- function(repos, setProgress = NULL,
                                batch_size = 200, log_file='logs.log') {
  start_time_log <- Sys.time()
  log_message <- function(event_name, message, file_path) {
    elapsed <- round(as.numeric(difftime(Sys.time(), start_time_log, units = "secs")), 2)
    log_entry <- sprintf("[%05.2fs] %-16s %s\n", elapsed, paste0(event_name, ":"), message)
    cat(log_entry, file = log_file, append = TRUE)
  }
  log_message("\n--------FUNCTION_START", "Commit processing initiated---\n")
  # Initialize DuckDB connection
  con <- dbConnect(duckdb(), paste0(DUCK_DB, ".db"))
  on.exit(dbDisconnect(con), add = TRUE)
  
  log_message("DB_CONNECTED", "Connected to DuckDB database")
  
  # Create table and index
  dbExecute(con, sprintf("CREATE TABLE IF NOT EXISTS %s (
      id VARCHAR,
      patch VARCHAR,
      repo VARCHAR,
      author VARCHAR,
      date VARCHAR,
      filename VARCHAR,
      status VARCHAR,
      additions INTEGER,
      deletions INTEGER,
      changes INTEGER,
      message VARCHAR,
      branch VARCHAR
    )", COMMITS_TABLE))
  dbExecute(con, sprintf("CREATE INDEX IF NOT EXISTS idx_%s_id ON %s (id)", COMMITS_TABLE, COMMITS_TABLE))
  
  # Collect all commit SHAs first for accurate progress tracking
  all_commits <- list()
  if (!is.null(repos)) {
    log_message("PAGES_PROCESS_START", "Starting repository processing")
    if (!is.null(setProgress)) {
      setProgress(message = "🌐 Поиск коммитов:", value = 0)
    }
    for (repo in repos) {
      
      repo_name <- repo$full_name
      log_message("REPO_PROCESS_START",paste0( "Processing repository: ", repo_name))
      
      branches_response <- github_api_get(paste0("https://api.github.com/repos/", repo_name, "/branches?per_page=100"))
      if (is.null(branches_response)) next
      branches <- content(branches_response, "parsed")
      log_message("BRANCHES", paste0("Found ", length(branches), " branches in ", repo_name))
      
      for (branch in branches) {
        branch_name <- branch$name
        url <- paste0("https://api.github.com/repos/", repo_name, "/commits?per_page=100&sha=", branch_name)
        log_message("COMMITS_PROCESS_START", paste0("Fetching commits for branch: ", branch_name, " (", repo_name, ")"))
        
        repeat {
          response <- github_api_get(url)
          if (is.null(response)) break
          commits <- content(response, "parsed")
          if (length(commits) == 0) break
          
          # Collect commit references
          new_commits <- lapply(commits, function(c) list(
            sha = c$sha,
            repo = repo_name,
            branch = branch_name
          ))
          all_commits <- c(all_commits, new_commits)
          log_message("SHAS_INFO", paste0("Collected ", length(new_commits), " commit SHAs from ", 
                             branch_name, " (", repo_name, ")"))
          
          # Update collection progress
          if (!is.null(setProgress)) {
            setProgress(detail = sprintf("%d", length(all_commits)))
          }
          
          # Pagination
          link_header <- headers(response)$link
          if (is.null(link_header) || !grepl('rel="next"', link_header)) break
          url <- regmatches(link_header, regexpr('<https://[^>]+>; rel="next"', link_header)) |> 
            gsub('<|>; rel="next"', '', x = _)
        }
      }
    }
  }
  
  total_commits <- length(all_commits)
  if (total_commits == 0) return(NULL)
  
  # Get existing SHAs from database
  log_message("SHA'S_IN_DB","Checking existing commits in database")
  existing_shas <- dbGetQuery(con, sprintf("SELECT DISTINCT id FROM %s", COMMITS_TABLE))$id
  
  # Initialize batch processing variables
  commits_list <- list()
  new_commits_batch <- list()
  batch_counter <- 0
  
  # Batch writing helper function
  write_batch <- function() {
    if (length(new_commits_batch) > 0) {
      combined_batch <- do.call(rbind, new_commits_batch)
      log_message("DB_WRITE_START", sprintf("Writing batch of %d records", nrow(combined_batch)))
      dbWriteTable(con, COMMITS_TABLE, combined_batch, append = TRUE)
      commits_list <<- c(commits_list, list(combined_batch))
      new_commits_batch <<- list()
      batch_counter <<- 0
      log_message("DB_WRITE_ENDS", sprintf("Writing batch of %d records", nrow(combined_batch)))
    }
  }
  
  # Start processing phase
  start_time <- Sys.time()
  if (!is.null(setProgress)) {
    setProgress(message = "⚙️ Обработка коммитов:", value = 0)
  }
  
  for (i in seq_along(all_commits)) {
    commit <- all_commits[[i]]
    commit_sha <- commit$sha
    repo_name <- commit$repo
    branch_name <- commit$branch
    
    if (commit_sha %in% existing_shas) {
      log_message("EXISTING_COMMIT", sprintf("Commit %s already in database", substr(commit_sha, 1, 7)))
      # Retrieve existing data from DB
      existing_data <- dbGetQuery(con, sprintf("SELECT * FROM %s WHERE id = '%s'", 
                                               COMMITS_TABLE, commit_sha))
      commits_list <- c(commits_list, list(existing_data))
    } else {
      
      log_message("NEW_COMMIT_START", sprintf("Processing new commit: %s", substr(commit_sha, 1, 7)))
      # Process new commit
      
      commit_details <- github_api_get(paste0("https://api.github.com/repos/", repo_name, "/commits/", commit_sha))
      if (is.null(commit_details)) next
      commit_data <- content(commit_details, "parsed")
      
      if (!is.null(commit_data$files) && length(commit_data$files) > 0) {
        file_data <- lapply(commit_data$files, function(file) {
          commit_date <- ymd_hms(commit_data$commit$author$date)
          commit_date_local <- with_tz(commit_date, tzone = Sys.timezone())
          
          data.frame(
            id = commit_sha,
            patch = if (is.null(file$patch)) "NULL" else file$patch,
            repo = repo_name,
            author = commit_data$commit$author$name,
            date = format(commit_date_local, "%Y.%m.%d %H:%M:%S"),
            filename = file$filename,
            status = file$status,
            additions = if (is.null(file$additions)) 0 else file$additions,
            deletions = if (is.null(file$deletions)) 0 else file$deletions,
            changes = if (is.null(file$changes)) 0 else file$changes,
            message = commit_data$commit$message,
            branch = branch_name,
            stringsAsFactors = FALSE
          )
        })
        
        new_commit_df <- do.call(rbind, file_data)
        if (!is.null(new_commit_df) && nrow(new_commit_df) > 0) {
          new_commits_batch <- c(new_commits_batch, list(new_commit_df))
          batch_counter <- batch_counter + 1
          existing_shas <- c(existing_shas, commit_sha)
          
          if (batch_counter >= batch_size) {
            write_batch()
          }
        }
      }
    }
    
    # Update processing progress
    if (!is.null(setProgress)) {
      elapsed <- as.numeric(Sys.time() - start_time)
      remaining <- elapsed * (total_commits - i) / i
      setProgress(
        value = i / total_commits,
        detail = sprintf("%d/%d (%s)",
                         i, total_commits,
                         format(.POSIXct(remaining, tz = "GMT"), "%H:%M:%S"))
      )
    }
  }
  
  # Write final batch
  write_batch()
  log_message("LAST_DATA_WAS_WRITTEN", paste0("Writing final batch of ", batch_counter, " commits"))
  
  # Combine and return results
  commits_df <- if (length(commits_list) > 0) do.call(rbind, commits_list) else NULL
  final_count <- if (!is.null(commits_df)) nrow(commits_df) else 0
  log_message("NY, WOT I WSE", paste0("Function completed. Total commits processed: ", final_count))
  return(commits_df)
}

get_user_profile <- function(username) {
  response <- github_api_get(paste0("https://api.github.com/users/", username))
  if (is.null(response)) return(NULL)

  profile <- content(response, "parsed")

  list(
    name = profile$login,
    bio = profile$bio %||% "Биография не указана",
    avatar_url = profile$avatar_url,
    created_at = profile$created_at,
    updated_at = profile$updated_at,
    company = profile$company,
    location = profile$location,
    followers = profile$followers,
    following = profile$following,
    public_repos = profile$public_repos,
    html_url = profile$html_url,
    blog = profile$blog
  )
}

prepare_activity_data <- function(repos) {
  # на основе репозиториев, визуализирует данные о forks и issues 
  if (is.null(repos)) {
    return(NULL)
  }

  activity_data <- map_dfr(repos, function(repo) {
    data.frame(
      date = as.Date(repo$created_at),
      count = repo$open_issues,
      type = "Issues"
    )
  })

  activity_data <- bind_rows(activity_data, map_dfr(repos, function(repo) {
    data.frame(
      date = as.Date(repo$updated_at),
      count = repo$forks,
      type = "Forks"
    )
  }))

  return(activity_data)
}

prepare_language_data <- function(repos) {
  # на основе репозиториев, визуализирует данные о использовании языков
  if (is.null(repos)) {
    return(NULL)
  }

  language_data <- map_dfr(repos, function(repo) {
    if (!is.null(repo$language)) {
      data.frame(
        language = repo$language,
        count = 1
      )
    }
  })

  language_data <- language_data %>%
    group_by(language) %>%
    summarise(count = sum(count), .groups = "drop")

  return(language_data)
}

prepare_commit_heatmap_data <- function(commits) {
  # строит тепловую карту по дате коммита
  if (is.null(commits)) {
    return(NULL)
  }

  commits <- commits %>%
    distinct(id, .keep_all = TRUE) %>%
    mutate(date = as.POSIXct(date, format = "%Y.%m.%d %H:%M", tz = "UTC")) %>%
    filter(!is.na(date))

  commits <- commits %>%
    mutate(
      day = factor(weekdays(date, abbreviate = FALSE), levels = c("понедельник", "вторник", "среда", "четверг", "пятница", "суббота", "воскресенье")),
      hour = as.integer(format(date, "%H"))
    )

  heatmap_data <- commits %>%
    group_by(hour, day) %>%
    summarise(count = n(), .groups = "drop")

  return(heatmap_data)
}
