source("src/GetData.R")

# Пример вызова функции
username <- "NotAccess"
repos <- list("KPZ", "Iatp", "TMP", "thdataimport") # Нужна функция для парсинга имён репозиториев
for (repo in repos) {
  logs <- clone_and_log_repository(username, repo)
  print(logs) # Вывод для теста
}
