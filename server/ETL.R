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

get_user_repos <- function(username, setProgress = NULL) {
  repos <- list()
  url <- paste0("https://api.github.com/users/", username, "/repos?per_page=100")

  total_pages <- 1
  current_page <- 1

  repeat {
    response <- github_api_get(url)
    if (is.null(response)) {
      break
    }

    current_repos <- content(response, "parsed")
    repos <- c(repos, current_repos)

    if (current_page == 1) {
      link_header <- headers(response)$link
      if (!is.null(link_header)) {
        if (grepl('rel="last"', link_header)) {
          last_url <- regmatches(link_header, regexpr('<https://[^>]+>; rel="last"', link_header))
          last_url <- gsub('<|>; rel="last"', '', last_url)
          total_pages <- as.numeric(gsub(".*page=(\\d+).*", "\\1", last_url))
        }
      }
    }

    if (!is.null(setProgress)) {
      setProgress(value = current_page / (total_pages + 1), detail = paste(current_page, "/", total_pages))
    }

    link_header <- headers(response)$link
    if (is.null(link_header) || !grepl('rel="next"', link_header)) {
      break
    }

    next_url <- regmatches(link_header, regexpr('<https://[^>]+>; rel="next"', link_header))
    next_url <- gsub('<|>; rel="next"', '', next_url)
    url <- next_url
    current_page <- current_page + 1
  }

  if (length(repos) == 0) {
    return(NULL)
  }

  if (!is.null(setProgress)) {
    setProgress(value = total_pages / (total_pages + 1), detail = "*обработка*")
  }

  repo_data <- imap(repos, function(repo, index) {
    if (!is.null(setProgress)) {
      repo_progress <- (total_pages + (index / length(repos))) / (total_pages + 1)
      setProgress(value = repo_progress,detail = paste("*обработка*", "(", index, "/", length(repos), ")"))
    }

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
get_user_commits_df <- function(repos, setProgress = NULL) {
  # Initialize DuckDB connection
  con <- dbConnect(duckdb(), paste0(DUCK_DB, ".db"))
  on.exit(dbDisconnect(con), add = TRUE) # Ensure connection closes on exit
  
  dbExecute(con, paste0("CREATE TABLE IF NOT EXISTS ", COMMITS_TABLE,
  " (
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
    )
  ")
  )
  
  # Initialize data frame for commits
  commits_df <- data.frame(
    id = character(),
    patch = character(),
    repo = character(),
    author = character(),
    date = character(),
    filename = character(),
    status = character(),
    additions = numeric(),
    deletions = numeric(),
    changes = numeric(),
    message = character(),
    branch = character(),
    stringsAsFactors = FALSE
  )
  
  if (is.null(repos)) {
    return(NULL)
  }
  
  ind <- 0
  count_commits <- 0
  for (repo in repos) {
    repo_name <- repo$full_name
    
    branches_url <- paste0("https://api.github.com/repos/", repo_name, "/branches?per_page=100")
    branches_response <- github_api_get(branches_url)
    if (is.null(branches_response)) {
      next
    }
    branches <- content(branches_response, "parsed")
    
    for (branch in branches) {
      branch_name <- branch$name
      url <- paste0("https://api.github.com/repos/", repo_name, "/commits?per_page=100&sha=", branch_name)
      
      repeat {
        response <- github_api_get(url)
        if (is.null(response)) {
          break
        }
        
        commits <- content(response, "parsed")
        count_commits <- count_commits + length(commits)
        if (length(commits) == 0) {
          break
        }
        
        for (commit in commits) {
          ind <- ind + 1
          commit_sha <- commit$sha
          
          query <- sprintf(paste0("SELECT * FROM ", COMMITS_TABLE, " WHERE id = '%s'"), commit_sha)
          existing_commit <- dbGetQuery(con, query)
          
          if (nrow(existing_commit) > 0) {
            commits_df <- rbind(commits_df, existing_commit)
          } else {
            # Fetch commit details from GitHub API
            commit_details <- github_api_get(paste0("https://api.github.com/repos/", repo_name, "/commits/", commit_sha))
            commit_data <- content(commit_details, "parsed")
            
            if (!is.null(commit_data$files)) {
              for (file in commit_data$files) {
                commit_date <- ymd_hms(commit_data$commit$author$date)  # парсинг ISO 8601 (автоматически распознает UTC, если есть 'Z')
                commit_date_local <- with_tz(commit_date, tzone = Sys.timezone())
                
                new_row <- data.frame(
                  id = commit_data$sha,
                  patch = file$patch %||% "NULL",
                  repo = repo_name,
                  author = commit_data$commit$author$name,
                  date = format(commit_date_local, "%Y.%m.%d %H:%M:%S"),
                  filename = file$filename,
                  status = file$status,
                  additions = file$additions %||% 0,
                  deletions = file$deletions %||% 0,
                  changes = file$changes %||% 0,
                  message = commit_data$commit$message,
                  branch = branch_name,
                  stringsAsFactors = FALSE
                )
                
                # Append to data frame
                commits_df <- rbind(commits_df, new_row)
                
                # Insert into DuckDB
                dbWriteTable(con, COMMITS_TABLE, new_row, append = TRUE)
              }
            }
          }
          
          if (!is.null(setProgress)) {
            setProgress(value = ind / count_commits, detail = paste(ind, "/", count_commits))
          }
        }
        link_header <- headers(response)$link
        if (is.null(link_header) || !grepl('rel="next"', link_header)) {
          break
        }
        
        next_url <- regmatches(link_header, regexpr('<https://[^>]+>; rel="next"', link_header))
        next_url <- gsub('<|>; rel="next"', '', next_url)
        url <- next_url
      }
    }
  }
  
  if (nrow(commits_df) > 0) {
    return(commits_df)
  } else {
    return(NULL)
  }
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
