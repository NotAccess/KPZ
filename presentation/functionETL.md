# Описание функций ETL

## github_api_get

```R
  github_api_get <- function(url) {
  ...
  return(response)
}
```
Функция предназначена для получения информации о репозиториях пользователя GitHub через GitHub API по URL.

Входные данные: 
- url - URL-адрес
Выходные данные:
- response - ответ от GitHub API

## get_user_repos

Функция предназначена для получения списка репозиториев пользователя.

Входные данные: 

- url - URL-адрес
- setProgress - progressbar

Выходные данные:

- repo_data - список репозиториев пользователя

## get_user_commits_df



Фкнкция предназначена для получения таблицы с характеристиками коммитов

Входные данные: 

- url - URL-адрес
- setProgress - progressbar

на основе полученных репозиториев, выдаёт таблицу с характеристиками коммитов
  # на вход список репозиториев и progressbar

## prepare_language_data

## prepare_commit_heatmap_data


