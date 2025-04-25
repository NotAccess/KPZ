library(dplyr)
library(tidyr)
library(fastDummies)
library(ggplot2)
library(plotly)

preprocess_commits <- function(commits) {
  # Очистка данных
  if (is.null(commits)) {
    stop("Ошибка: commits равен NULL")
  }
  
  commits$file_extension <- tools::file_ext(commits$filename)
  return(dummy_cols(commits, select_columns = c("repo", "status", "file_extension"), remove_selected_columns = TRUE))
}

aggregate_commits <- function(commits_dummies) {
  # агрегация (group by) данных
  if (is.null(commits_dummies)) {
    stop("Ошибка: commits_dummies равен NULL")
  }
  
  aggregated_commits <- commits_dummies %>%
    group_by(id, author) %>%
    summarise(
      additions = sum(additions, na.rm = TRUE),
      deletions = sum(deletions, na.rm = TRUE),
      changes = sum(changes, na.rm = TRUE),
      files_changed = n(),
      across(starts_with("repo_") | starts_with("status_") | starts_with("file_extension_"), sum),
      .groups = "drop"
    )
  
  return(aggregated_commits)
}

perform_pca <- function(commits, scale = TRUE, normalize = "minmax") {
  # Проверка входных данных
  if (is.null(commits)) {
    stop("Ошибка: commits равен NULL")
  }
  
  # Предобработка и агрегация данных
  aggregated_commits <- commits %>%
    preprocess_commits() %>%
    aggregate_commits()
  
  # Выбор числовых столбцов
  numeric_data <- aggregated_commits %>% select(where(is.numeric))
  
  if (ncol(numeric_data) < 2) {
    stop("Ошибка: недостаточно числовых столбцов для PCA")
  }
  
  # Нормализация числовых данных
  if (normalize == "minmax") {
    # Минимакс-нормализация (приведение к [0, 1])
    numeric_data <- numeric_data %>%
      mutate(across(.cols = everything(), 
                    .fns = ~ (. - min(., na.rm = TRUE)) / 
                      (max(., na.rm = TRUE) - min(., na.rm = TRUE))))
  } else if (normalize == "zscore") {
    # Z-нормализация (стандартизация)
    numeric_data <- numeric_data %>%
      mutate(across(.cols = everything(), 
                    .fns = ~ scale(., center = TRUE, scale = TRUE)))
  } else {
    stop("Ошибка: неизвестный метод нормализации. Используйте 'minmax' или 'zscore'.")
  }
  
  # Выполнение PCA
  pca <- prcomp(numeric_data, center = TRUE, scale. = scale)
  
  # Подготовка данных PCA
  pca_data <- as_tibble(pca$x) %>%
    mutate(
      author = aggregated_commits$author,
      id = aggregated_commits$id,
      distance = sqrt(PC1^2 + PC2^2)
    )
  
  return(pca_data)
}

detect_outliers <- function(pca_data, threshold = 2) {
  # пределение аномалий (threshold - порог выброса)
  z_scores <- scale(pca_data$distance)
  
  outliers <- pca_data %>%
    filter(z_scores > threshold) %>%
    select(id, author, distance)
  
  return(outliers)
}

plot_pca <- function(pca_data) {
  # отображение результатов
  if (is.null(pca_data)) {
    stop("Ошибка: pca_data равен NULL")
  }
  
  plot_ly(
    data = pca_data,
    x = ~PC1,
    y = ~PC2,
    color = ~author,
    text = ~paste("ID:", id, "<br>PC1:", round(PC1, 2), "<br>PC2:", round(PC2, 2)),
    hoverinfo = "text",
    type = "scatter",
    mode = "markers"
  ) %>%
    layout(title = "Анализ коммитов методом главных компонент (PCA)")
}
