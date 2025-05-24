library(dplyr)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(plotly)
library(tools)
library(httr2)
library(yaml)

# Основная функция для выполнения PCA анализа
perform_pca <- function(commits, scale = TRUE) {
  # Логируем начало выполнения PCA
  flog.debug("[PCA] PCA starts")

  # Проверяем, что данные о коммитах не равны NULL
  if (is.null(commits)) {
    flog.debug("[PCA] commits for PCA = Null")
    stop("Ошибка: commits равен NULL")
  }

  # Мета-колонки
  META_COLUMNS <- c("id", "author")

  # Колонки, которые нужно преобразовать в dummy
  DUMMY_COLUMNS <- c("repo", "author_copy", "status", "file_extension", "branch",
                     "year", "month", "day", "hour", "minute", "second", "time_of_day",
                     "weekday", "quarter", "file_type")

  # Функция для загрузки актуальной версии languages.yml с GitHub
  load_languages_data <- function() {
    url <- "https://raw.githubusercontent.com/github-linguist/linguist/main/lib/linguist/languages.yml"
    response <- request(url) %>%  req_perform()
    yaml_text <- response %>% resp_body_string(encoding = "UTF-8")
    languages <- yaml.load(yaml_text)

    # Создаем таблицы соответствий
    ext_to_type <- list() # Соответствие расширений типу файла
    filename_to_type <- list() # Соответствие имен файлов типу файла

    # Обрабатываем каждый язык в YAML
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

  # Загружаем данные о языках
  lang_data <- load_languages_data()

  # Функция предварительной обработки данных
  preprocess_data <- function(data) {
    flog.trace("[PCA] PCA preprocess starts")

    # Функция классификации типа файла
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

      return("other")  # Если не нашли соответствий
    }

    # Основной пайплайн предобработки
    data %>%
      mutate(
        # Преобразуем дату в структурированный формат
        date_struct = as.POSIXct(date, format = "%Y.%m.%d %H:%M:%S"),

        # Извлекаем компоненты даты
        year = year(date_struct),
        month = month(date_struct, label = TRUE),
        day = day(date_struct),
        hour = hour(date_struct),
        minute = minute(date_struct),
        second = second(date_struct),

        # Определяем время суток
        time_of_day = case_when(
          hour >= 6 & hour < 12 ~ "утро",
          hour >= 12 & hour < 18 ~ "день",
          hour >= 18 & hour < 24 ~ "вечер",
          TRUE ~ "ночь"
        ),
        weekday = wday(date_struct, label = TRUE, abbr = FALSE), # День недели
        quarter = quarter(date_struct), # Квартал

        # Информация о файле
        file_extension = file_ext(filename),
        author_copy = author, # Копия автора для dummy-кодирования

        # Характеристики коммита
        len_message = nchar(message),
        len_patch = nchar(patch),

        # Соотношение добавлений/удалений
        ratio = ifelse(deletions == 0, additions, additions/deletions),
        # Классифицируем тип файла
        file_type = sapply(filename, classify_file_type)
      ) %>%
      # Добавляем время с последнего изменения файла
      group_by(repo, filename) %>%
      arrange(date_struct) %>%
      mutate(time_since_last_file = as.numeric(difftime(date_struct,
                                                        lag(date_struct),
                                                        units = "secs"))) %>%
      ungroup() %>%
      mutate(time_since_last_file = ifelse(is.na(time_since_last_file),
                                           -1,
                                           time_since_last_file)
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
          mutate(time_since_last_repo = as.numeric(difftime(date_struct,
                                                            lag(date_struct),
                                                            units = "secs"))) %>%
          ungroup() %>%
          select(id, time_since_last_repo) %>%
          mutate(time_since_last_repo = ifelse(is.na(time_since_last_repo),
                                               -1,
                                               time_since_last_repo))

        # Время с последнего коммита автора
        author_times <- commits_unique %>%
          group_by(author) %>%
          arrange(date_struct) %>%
          mutate(time_since_last_author = as.numeric(difftime(date_struct,
                                                              lag(date_struct),
                                                              units = "secs"))) %>%
          ungroup() %>%
          select(id, time_since_last_author) %>%
          mutate(time_since_last_author = ifelse(is.na(time_since_last_author),
                                                 -1,
                                                 time_since_last_author))

        # Совмещаем с исходными данными
        left_join(.,
                  repo_times,
                  by = "id") %>%
        left_join(author_times, by = "id") %>%
        mutate(time_since_last_repo = coalesce(time_since_last_repo, -1),
               time_since_last_author = coalesce(time_since_last_author, -1))
      } %>%
      # Преобразуем категориальные переменные в dummy-переменные
      dummy_cols(select_columns = DUMMY_COLUMNS,
                 remove_selected_columns = TRUE,
                 ignore_na = TRUE
      )
  }


  # Функция агрегации данных по коммитам
  aggregate_data <- function(data) {
    flog.trace("[PCA] PCA aggreagation starts")
    dummy_features <- paste0(DUMMY_COLUMNS, "_")

    data %>%
    group_by(across(all_of(META_COLUMNS))) %>%
    summarise(
        # Агрегируем числовые характеристики
        files_changed = n(), # Количество измененных файлов
        additions = sum(additions, na.rm = TRUE), # Сумма добавлений
        deletions = sum(deletions, na.rm = TRUE), # Сумма удалений
        changes = sum(changes, na.rm = TRUE), # Сумма изменений
        ratio = mean(ratio, na.rm = TRUE), # Среднее соотношение
        time_since_last_repo = first(time_since_last_repo),
        time_since_last_author = first(time_since_last_author),
        time_since_last_file = coalesce(mean(time_since_last_file,
                                             na.rm = TRUE),
                                        -1),
        len_message = first(len_message), # Длина сообщения
        len_patch = first(len_patch), # Длина патча
        patches = paste0("patch (", filename, "): ```", patch, "```;", collapse = "\n"), # Объединяем патчи
        # Суммируем dummy-переменные
        across(starts_with(dummy_features), sum, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Основной пайплайн обработки данных
  processed_data <- commits %>%
  preprocess_data() %>% # Предварительная обработка
  aggregate_data() # Агрегация

  # Выделение числовых данных для PCA
  numeric_data <- processed_data %>%
  select(-all_of(META_COLUMNS)) %>% # Удаляем мета-колонки
  select(where(is.numeric)) %>% # Выбираем только числовые колонки
  select(where(~ sd(.x, na.rm = TRUE) > 0)) # Выбираем столбцы с ненулевой дисперсией

  # Проверка, что осталось достаточно колонок
  if (ncol(numeric_data) < 2) {
    stop("Ошибка: недостаточно числовых столбцов для PCA")
  }

  flog.trace("[PCA] PCA compute begins")
  # Вычисление PCA (метод главных компонент)
  pca_result <- prcomp(numeric_data, center = TRUE, scale. = scale)

  # Формирование итоговых данных
  pca_data <- bind_cols(
    processed_data[META_COLUMNS], # Мета-информация
    processed_data["patches"], # Патчи
    as_tibble(pca_result$x), # Компоненты PCA
    distance = sqrt(rowSums(pca_result$x^2)) # Евклидово расстояние до центра
  )

  return(pca_data)
}

# Функция для обнаружения выбросов (аномалий)
detect_outliers <- function(pca_data, threshold = 2) {
  # Вычисляем Z-оценки для расстояний (threshold - порог выброса)
  z_scores <- scale(pca_data$distance)

  # Выбираем записи, где Z-оценка превышает порог
  outliers <- pca_data %>%
  mutate(z_score = as.numeric(z_scores)) %>%
  filter(abs(z_scores) >= threshold) %>% # Фильтруем по порогу
  select(id, author, distance, z_score, patches) # Выбираем нужные колонки

  return(outliers)
}
