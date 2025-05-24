library(dplyr)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(plotly)
library(tools)
library(httr2)
library(yaml)

perform_pca <- function(commits, scale = TRUE) {
  flog.debug("[PCA] PCA starts")
  if (is.null(commits)) {
    flog.debug("[PCA] commits for PCA = Null")
    stop("Ошибка: commits равен NULL")
  }

  META_COLUMNS <- c("id", "author")
  DUMMY_COLUMNS <- c("repo", "author_copy", "status", "file_extension", "branch",
                     "year", "month", "day", "hour", "minute", "second", "time_of_day",
                     "weekday", "quarter", "file_type")

  # Загрузка актуальной версии languages.yml с GitHub
  load_languages_data <- function() {
    url <- "https://raw.githubusercontent.com/github-linguist/linguist/main/lib/linguist/languages.yml"
    response <- request(url) %>%  req_perform()
    yaml_text <- response %>% resp_body_string(encoding = "UTF-8")
    languages <- yaml.load(yaml_text)

    # Создаем таблицы соответствий
    ext_to_type <- list()
    filename_to_type <- list()

    for (lang in names(languages)) {
      type <- languages[[lang]]$type %||% "unknown"

      # Обрабатываем расширения
      if (!is.null(languages[[lang]]$extensions)) {
        exts <- tolower(sub("^\\.", "", languages[[lang]]$extensions))
        for (ext in exts) {
          ext_to_type[[ext]] <- type
        }
      }

      # Обрабатываем filenames
      if (!is.null(languages[[lang]]$filenames)) {
        fnames <- tolower(languages[[lang]]$filenames)
        for (fname in fnames) {
          filename_to_type[[fname]] <- type
        }
      }
    }

    list(extensions = ext_to_type, filenames = filename_to_type)
  }

  lang_data <- load_languages_data()

  # Предобработка данных
  preprocess_data <- function(data) {
    flog.trace("[PCA] PCA preprocess starts")
    # Функция классификации
    classify_file_type <- function(filename) {
      # Нормализация имени файла
      base_name <- tolower(basename(filename))
      ext <- tolower(tools::file_ext(filename))

      # Проверка по filenames
      if (base_name %in% names(lang_data$filenames)) {
        return(lang_data$filenames[[base_name]])
      }

      # Проверка по расширению
      if (nzchar(ext) && ext %in% names(lang_data$extensions)) {
        return(lang_data$extensions[[ext]])
      }

      return("other")
    }

    data %>%
      mutate(
        date_struct = as.POSIXct(date, format = "%Y.%m.%d %H:%M:%S"),
        year = year(date_struct),
        month = month(date_struct, label = TRUE),
        day = day(date_struct),
        hour = hour(date_struct),
        minute = minute(date_struct),
        second = second(date_struct),
        time_of_day = case_when(
          hour >= 6 & hour < 12 ~ "утро",
          hour >= 12 & hour < 18 ~ "день",
          hour >= 18 & hour < 24 ~ "вечер",
          TRUE ~ "ночь"
        ),
        weekday = wday(date_struct, label = TRUE, abbr = FALSE),
        quarter = quarter(date_struct),

        file_extension = file_ext(filename),
        author_copy = author,
        len_message = nchar(message),
        len_patch = nchar(patch),
        ratio = ifelse(deletions == 0, additions, additions/deletions),
        file_type = sapply(filename, classify_file_type)
      ) %>%
      # Добавляем время с последнего изменения файла
      group_by(repo, filename) %>%
      arrange(date_struct) %>%
      mutate(
        time_since_last_file = as.numeric(difftime(date_struct, lag(date_struct), units = "secs"))
      ) %>%
      ungroup() %>%
      mutate(
        time_since_last_file = ifelse(is.na(time_since_last_file), -1, time_since_last_file)
      ) %>%
      # Вычисление времени между коммитами
      {
        # Создаем уникальные коммиты
        commits_unique <- select(., id, repo, author, date_struct) %>%
          distinct(id, .keep_all = TRUE) %>%
          arrange(date_struct)

        # Время с последнего коммита в репозитории
        repo_times <- commits_unique %>%
          group_by(repo) %>%
          arrange(date_struct) %>%
          mutate(time_since_last_repo = as.numeric(difftime(date_struct, lag(date_struct), units = "secs"))) %>%
          ungroup() %>%
          select(id, time_since_last_repo) %>%
          mutate(time_since_last_repo = ifelse(is.na(time_since_last_repo), -1, time_since_last_repo))

        # Время с последнего коммита автора
        author_times <- commits_unique %>%
          group_by(author) %>%
          arrange(date_struct) %>%
          mutate(time_since_last_author = as.numeric(difftime(date_struct, lag(date_struct), units = "secs"))) %>%
          ungroup() %>%
          select(id, time_since_last_author) %>%
          mutate(time_since_last_author = ifelse(is.na(time_since_last_author), -1, time_since_last_author))

        # Совмещаем с исходными данными
        left_join(., repo_times, by = "id") %>%
          left_join(author_times, by = "id") %>%
          mutate(
            time_since_last_repo = coalesce(time_since_last_repo, -1),
            time_since_last_author = coalesce(time_since_last_author, -1)
          )
      } %>%
      dummy_cols(
        select_columns = DUMMY_COLUMNS,
        remove_selected_columns = TRUE,
        ignore_na = TRUE
      )
  }


  # Агрегация данных
  aggregate_data <- function(data) {
    flog.trace("[PCA] PCA aggreagation starts")
    dummy_features <- paste0(DUMMY_COLUMNS, "_")

    data %>%
      group_by(across(all_of(META_COLUMNS))) %>%
      summarise(
        files_changed = n(),
        additions = sum(additions, na.rm = TRUE),
        deletions = sum(deletions, na.rm = TRUE),
        changes = sum(changes, na.rm = TRUE),
        ratio = mean(ratio, na.rm = TRUE),
        time_since_last_repo = first(time_since_last_repo),
        time_since_last_author = first(time_since_last_author),
        time_since_last_file = coalesce(mean(time_since_last_file, na.rm = TRUE), -1),
        len_message = first(len_message),
        len_patch = first(len_patch),
        patches = paste0("patch (", filename, "): ```", patch, "```;", collapse = "\n"),
        across(starts_with(dummy_features), sum, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Основной пайплайн обработки
  processed_data <- commits %>%
    preprocess_data() %>%
    aggregate_data()

  # Выделение числовых данных
  numeric_data <- processed_data %>%
    select(-all_of(META_COLUMNS)) %>%
    select(where(is.numeric)) %>%
    select(where(~ sd(.x, na.rm = TRUE) > 0)) # Выбираем столбцы с ненулевой дисперсией

  if (ncol(numeric_data) < 2) {
    stop("Ошибка: недостаточно числовых столбцов для PCA")
  }

  flog.trace("[PCA] PCA compute begins")
  # Вычисление PCA
  pca_result <- prcomp(numeric_data, center = TRUE, scale. = scale)

  # Формирование итоговых данных
  pca_data <- bind_cols(
    processed_data[META_COLUMNS],
    processed_data["patches"],
    as_tibble(pca_result$x),
    distance = sqrt(rowSums(pca_result$x^2))
  )

  return(pca_data)
}

detect_outliers <- function(pca_data, threshold = 2) {
  # Определение аномалий (threshold - порог выброса)
  z_scores <- scale(pca_data$distance)

  outliers <- pca_data %>%
    mutate(z_score = as.numeric(z_scores)) %>%
    filter(abs(z_scores) >= threshold) %>%
    select(id, author, distance, z_score, patches)

  return(outliers)
}
