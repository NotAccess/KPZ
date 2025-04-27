library(dplyr)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(plotly)

perform_pca <- function(commits, scale = TRUE) {
  # Проверка входных данных
  if (is.null(commits)) {
    stop("Ошибка: commits равен NULL")
  }
  
  DUMMY_COLUMNS <- c("repo", "status", "file_extension", "branch")
  META_COLUMNS <- c("id", "author")
  
  # Предобработка данных
  preprocess_data <- function(data) {
    data %>%
      mutate(file_extension = tools::file_ext(filename)) %>%
      fastDummies::dummy_cols(
        select_columns = DUMMY_COLUMNS,
        remove_selected_columns = TRUE,
        ignore_na = TRUE
      )
  }
  
  # Агрегация данных
  aggregate_data <- function(data) {
    dummy_features <- paste0(DUMMY_COLUMNS, "_")
    
    data %>%
      group_by(across(all_of(META_COLUMNS))) %>%
      summarise(
        additions = sum(additions, na.rm = TRUE),
        deletions = sum(deletions, na.rm = TRUE),
        changes = sum(changes, na.rm = TRUE),
        files_changed = n(),
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
  
  # Вычисление PCA
  pca_result <- prcomp(numeric_data, center = TRUE, scale. = scale)
  
  # Формирование итоговых данных
  pca_data <- bind_cols(
    processed_data[META_COLUMNS],
    as_tibble(pca_result$x),
    distance = sqrt(rowSums(pca_result$x^2))
  )
  
  return(pca_data)
}

detect_outliers <- function(pca_data, threshold = 2) {
  # пределение аномалий (threshold - порог выброса)
  z_scores <- scale(pca_data$distance)
  
  outliers <- pca_data %>%
    mutate(z_score = as.numeric(z_scores)) %>%
    filter(z_scores > threshold) %>%
    select(id, author, distance, z_score)
  
  return(outliers)
}
