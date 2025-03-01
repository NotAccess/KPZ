source("src/GetData.R")

# Пример вызова функции
username <- "NotAccess"
repos <- get_github_repositories(username)
for (repo in repos) {
  logs <- clone_and_log_repository(username, repo)
  print(logs) # Вывод для теста
}
