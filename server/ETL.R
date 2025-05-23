library(httr2)
library(dplyr)
library(purrr)
library(duckdb)
library(DBI)
library(lubridate)

GITHUB_TOKEN <- Sys.getenv('GITHUB_TOKEN')

github_api_get <- function(url) {
  req <- request(url) %>%
    req_user_agent("ShinyApp") %>%
    req_error(is_error = \(resp) FALSE)
  
  if (nzchar(GITHUB_TOKEN)) {
    req <- req %>% 
      req_headers(Authorization = paste("token", GITHUB_TOKEN))
  }
  
  response <- req %>% req_perform()
  status <- resp_status(response)
  
  # Обработка статусов
  if (status == 204) {
    flog.warn("[INFO] Репозиторий пуст (204 No Content)")
    return(NULL)
  }

  if (status == 401) {
    flog.error("[ERROR] Ошибка авторизации (проверьте токен): %s",status)
    stop(paste("Ошибка авторизации (проверьте токен):", status))
  }

  if (status == 403) {
    flog.error("[ERROR] Лимит запросов исчерпан): %s",status)
    stop("Лимит запросов исчерпан. Пожалуйста, обновите GitHub токен.")
  }

  if (status == 404) {
    flog.warn("[WARN] Пользователь GitHub с данным именем не найден: %s",status)
    return(NULL)
  }

  if (status == 409) {
    flog.warn("[WARN] Репозиторий пустой или конфликт (409 Conflict): %s",status)
    return(NULL)
  }

  if (status >= 500 && status < 600) {
    flog.warn("[WARN] Ошибка сервера: %s",status)
    return(NULL)
  }

  if (status != 200) {
    flog.error("Ошибка при запросе к GitHub API: %s", status)
    stop(paste("Ошибка при запросе к GitHub API:", status))
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
    
    current_repos <- response %>% resp_body_json()
    repos <- c(repos, current_repos)

    if (current_page == 1) {
      link_header <- resp_headers(response)$link
      if (!is.null(link_header) && grepl('rel="last"', link_header)) {
        total_pages <- regexpr('<https://[^>]+>; rel="last"', link_header) %>%
          regmatches(link_header, .) %>%
          gsub('<|>; rel="last"', '', .) %>%
          gsub(".*page=(\\d+).*", "\\1", .) %>%
          as.numeric()
      }
    }

    setProgress(detail = sprintf("%d", length(repos)))

    link_header <- resp_headers(response)$link
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
    contributors_count <- if (!is.null(contributors_response)) length(resp_body_json(contributors_response)) else 0

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
      is_fork = repo$fork,  # Поле проверки форки ли это
      license = if (!is.null(repo$license)) repo$license$name else "Нет лицензии",
      size = repo$size
    )
  })

  return(repo_data)
}

get_user_commits_df <- function(repos, setProgress = NULL, batch_size = 200, log_file='logs.log') {

  flog.info("\n-------- FUNCTION START: Commit processing initiated --------")
  #Get variables
  DUCK_DB <- Sys.getenv('DUCK_DB')
  COMMITS_TABLE <- Sys.getenv('COMMITS_TABLE')
  flog.trace("[VARS] Varribles of db loaded DB=%s, table=%s", DUCK_DB, COMMITS_TABLE)
  
  # Initialize DuckDB connection
  con <- dbConnect(duckdb(), paste0(DUCK_DB, ".db"))
  on.exit(dbDisconnect(con), add = TRUE)
  
  flog.info("[DB_CONNECTED] Connected to DuckDB database")
  
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
    flog.info("[PAGES_PROCESS] Starting repository processing")
    
    setProgress(message = "🌐 Поиск коммитов:", value = 0)
    for (repo in repos) {
      
      repo_name <- repo$full_name
      flog.info("[REPO_START] Processing repository: %s", repo_name)
      
      branches_response <- github_api_get(paste0("https://api.github.com/repos/", repo_name, "/branches?per_page=100"))
      if (is.null(branches_response)) next
      branches <- resp_body_json(branches_response)
      flog.debug("[BRANCHES] Found %d branches in %s", length(branches), repo_name)
      
      for (branch in branches) {
        branch_name <- branch$name
        url <- paste0("https://api.github.com/repos/", repo_name, "/commits?per_page=100&sha=", branch_name)
        flog.info("[BRANCH_START] Processing branch: %s (%s)", branch_name, repo_name)
        
        repeat {
          response <- github_api_get(url)
          if (is.null(response)) {
            flog.error("[API_ERROR] Failed to get commits for %s/%s", repo_name, branch_name)
            break
            }
          commits <- resp_body_json(response)
          if (length(commits) == 0) {
            flog.debug("[NO_COMMITS] No commits in branch %s", branch_name)
            break
          }
          
          # Collect commit references
          new_commits <- lapply(commits, function(c) list(
            sha = c$sha,
            repo = repo_name,
            branch = branch_name
          ))
          all_commits <- c(all_commits, new_commits)
          flog.trace("[SHAS_ADDED] Added %d SHAs from %s", length(new_commits), branch_name)
          
          # Update collection progress
          
          setProgress(detail = sprintf("%d", length(all_commits)))
          
          
          # Pagination
          link_header <- resp_headers(response)$link
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
  
  existing_shas <- dbGetQuery(con, sprintf("SELECT DISTINCT id FROM %s", COMMITS_TABLE))$id
  
  # Initialize batch processing variables
  commits_list <- list()
  new_commits_batch <- list()
  batch_counter <- 0
  
  # Batch writing helper function
  write_batch <- function() {
    if (length(new_commits_batch) > 0) {
      combined_batch <- do.call(rbind, new_commits_batch)
      flog.debug("[DB_WRITE_START] Writing batch of records %s", nrow(combined_batch))
      dbWriteTable(con, COMMITS_TABLE, combined_batch, append = TRUE)
      commits_list <<- c(commits_list, list(combined_batch))
      new_commits_batch <<- list()
      batch_counter <<- 0
      flog.debug("DB_WRITE_ENDS")
    }
  }
  
  # Start processing phase
  start_time <- Sys.time()

  setProgress(message = "⚙️ Обработка коммитов:", value = 0)
  
  
  for (i in seq_along(all_commits)) {
    commit <- all_commits[[i]]
    commit_sha <- commit$sha
    repo_name <- commit$repo
    branch_name <- commit$branch
    
    if (commit_sha %in% existing_shas) {
      flog.trace("[EXISTING_COMMIT] Commit %s already in database", substr(commit_sha, 1, 7))
      # Retrieve existing data from DB
      existing_data <- dbGetQuery(con, sprintf("SELECT * FROM %s WHERE id = '%s'", 
                                               COMMITS_TABLE, commit_sha))
      commits_list <- c(commits_list, list(existing_data))
    } else {
      
      flog.trace("[NEW_COMMIT_START] Processing new commit: %s", substr(commit_sha, 1, 7))
      # Process new commit
      
      commit_details <- github_api_get(paste0("https://api.github.com/repos/", repo_name, "/commits/", commit_sha))
      if (!is.null(commit_details)) {
        commit_data <- commit_details %>% resp_body_json()
      }
      
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
    
    elapsed <- as.numeric(Sys.time() - start_time)
    remaining <- elapsed * (total_commits - i) / i
    setProgress(
      value = i / total_commits,
      detail = sprintf("%d/%d (%s)",
                       i, total_commits,
                       format(.POSIXct(remaining, tz = "GMT"), "%H:%M:%S"))
    )
    
  }
  
  # Write final batch
  write_batch()
  flog.debug("[LAST_DATA_WAS_WRITTEN], Writing final batch of %s", batch_counter)
  # Combine and return results
  commits_df <- if (length(commits_list) > 0) do.call(rbind, commits_list) else NULL
  final_count <- if (!is.null(commits_df)) nrow(commits_df) else 0
  
  flog.debug("[NY, WOT I WSE], Function completed. Total commits processe %s", final_count)
  return(commits_df)
}

get_user_profile <- function(username) {
  response <- github_api_get(paste0("https://api.github.com/users/", username))
  if (is.null(response)) return(NULL)

  profile <- resp_body_json(response)

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
  # Визуализирует данные о forks и issues, на основе репозиториев
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
  # Визуализирует данные о использовании языков, на основе репозиториев
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
  # Устанавливаем локаль на английскую для корректного определения дней недели
  Sys.setlocale("LC_TIME", "C")
  
  commits %>%
    mutate(
      date = as.POSIXct(date, format = "%Y.%m.%d %H:%M:%S"),
      hour = as.integer(format(date, "%H")),
      day_number = as.POSIXlt(date)$wday, # 0-6 (воскресенье = 0)
      day = factor(
        case_when(
          day_number == 0 ~ "Вс",
          day_number == 1 ~ "Пн",
          day_number == 2 ~ "Вт",
          day_number == 3 ~ "Ср",
          day_number == 4 ~ "Чт",
          day_number == 5 ~ "Пт",
          day_number == 6 ~ "Сб"
        ),
        levels = c("Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс"),
        ordered = TRUE
      )
    ) %>%
    count(day, hour, name = "count") %>%
    complete(day, hour = 0:23, fill = list(count = 0)) %>%
    # Возвращаем локаль обратно
    { Sys.setlocale("LC_TIME", ""); . }
}

prepare_commit_stats <- function(commits) {
  if (is.null(commits)) return(NULL)
  
  commits %>%
    mutate(
      # Явное преобразование строки в дату
      date = as.POSIXct(date, format = "%Y.%m.%d %H:%M:%S"),
      weekday = lubridate::wday(date, week_start = 1, label = TRUE, abbr = FALSE, locale = "ru_RU.UTF-8"),
      hour = lubridate::hour(date)
    ) %>%
    group_by(weekday, hour) %>%
    summarise(total = n(), .groups = 'drop') %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    transmute(
      top_day = stringr::str_to_title(as.character(weekday)), # Русские названия с заглавной
      top_hour = sprintf("%02d:00-%02d:00", hour, hour+1)
    )
}
