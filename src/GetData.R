library(gert)
library(httr)
library(rvest)
library(dplyr)
library(jsonlite)
library(lubridate)

get_github_repositories <- function(username) {
  # Указание URL страницы
  url <- paste0("https://github.com/", username, "?tab=repositories")

  # Чтение HTML-кода страницы
  webpage <- read_html(url)

  # Извлечение названий репозиториев
  repos_list <- webpage %>%
    html_nodes("a[itemprop='name codeRepository']") %>%  # Указание CSS-селектора для извлечения ссылок на репозитории
    html_text()  # Получение текста из найденных узлов

  # Печать списка репозиториев
  cleaned_list <- gsub("[\n ]", "", repos_list)
  return(cleaned_list)
}
clone_and_log_repository <- function(userName, repoName) {
  # Формирование ссылки репозитория
  repoPath <- paste0("https://github.com/", userName, "/", repoName)
  # Клонирование репозитория
  clonned_repo <- tempfile(pattern="if1dz24ha60ma60s-")
  dir.create(clonned_repo)
  git_clone(repoPath, clonned_repo)

  if ((length(list.dirs(clonned_repo, recursive = FALSE)) + length(list.files(clonned_repo, recursive = FALSE))) != 1) {
    # Получение информации о всех коммитах
    log <- git_log(ref = "HEAD", max = 100, after = NULL, repo = clonned_repo)

    # Извлечение изменений файлов каждого коммита
    temp_tibble <- tibble(file_changes = character(),
                          #line_changes = character(),
                          time_from_commit = duration(units = "days"))
    for (i in 1:(length(log$message))) {
      # Список изменяемых файлов
      gitD <- git_diff(repo = clonned_repo, ref = log$commit[[i]])[, -4] %>%
        summarise(output = paste(status, old, new, sep = " ", collapse = "*")) %>%
        pull(output)

      # Список изменяемых в файлах
      #gitP <- git_diff_patch(repo = clonned_repo, ref = log$commit[[i]]) #пофиксить, не работает
      #git_diff_patch(repo = clonned_repo, ref = log$commit[[i]])[1] |> #для проверки
        #write.table(file = paste0(log$commit[[i]], ".tsv"), sep = "\t", row.names = FALSE, quote = TRUE)

      # Добавление строки в tibble
      temp_tibble <- add_row(temp_tibble,
                             file_changes = gitD,
                             #line_changes = gitP,
                             time_from_commit = Sys.time() - as.POSIXct(log$time[i]))
    }

    # Запись в фрейм
    print(repoName) # Для тестирования
    result <- tibble(
      commit_ID = log$commit,                                       # ИД коммита
      author = log$author,                                          # Автор коммита
      date = log$time,                                              # Дата коммита
      #changed_lines = temp_tibble$line_changes,                     # Список изменённых и/или добавленных и/или удалённых строк
      changed_files = temp_tibble$file_changes,                     # Список изменённых и/или добавленных и/или удалённых файлов
      count_of_changed_files = log$files,                           # Количество изменённых файлов
      comment = log$message,                                        # Комментарий коммита
      time_from_last_commit = temp_tibble$time_from_commit          # Время прошедшее с коммита
    )

    return(result)
  } else {
    return(repoName) # Если директория пустая
  }
}

# Вариант хранения данных, на будущее
write_list_to_json <- function(listOfData) {
  existingJSONFile <- "output.json"
  # Читаем существующий JSON
  existingData <- fromJSON(existingJSONFile)

  # Проходимся по двумерному списку
  for (i in seq_along(listOfData)) {
    existingData <- append(existingData, list(listOfData[[i]]))
  }

  # Записываем обновлённые данные обратно в JSON файл
  write_json(existingData, existingJSONFile)
}
