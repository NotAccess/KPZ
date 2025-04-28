library(shiny)
library(shinyjs)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(stringr)
library(jsonlite)
library(markdown)

source("server/ETL.R")
source("server/ML.R")

YANDEX_FOLDER_ID <- Sys.getenv('YANDEX_FOLDER_ID')
YANDEX_API_KEY <- Sys.getenv('YANDEX_API_KEY')

filter_repos <- function(repos, filters) {
  keep(repos, ~ {
    repo <- .
    all(
      (filters$language == "Все" || repo$language == filters$language),
      between(repo$stars, filters$stars[1], filters$stars[2]),
      between(as.Date(repo$created_at), as.Date(filters$created_date_range[1]), as.Date(filters$created_date_range[2])),
      between(as.Date(repo$updated_at), as.Date(filters$updated_date_range[1]), as.Date(filters$updated_date_range[2])),
      between(repo$open_issues, filters$issues[1], filters$issues[2]),
      between(repo$contributors, filters$contributors[1], filters$contributors[2]),
      between(repo$size/1024, filters$size[1], filters$size[2]),
      (filters$license == "Все" || repo$license == filters$license)
    )
  })
}

server <- function(input, output, session) {
  hide("filters")

  data <- reactiveValues(
    repos = NULL,
    user_profile = NULL,
    commits = NULL,
    activity_data = NULL,
    language_data = NULL,
    commit_heatmap_data = NULL
  )

  observeEvent(input$toggle_filters, {
    toggle("filters")
    if (input$toggle_filters %% 2 == 1) {
      updateActionButton(session, "toggle_filters", label = "Скрыть фильтры", icon = icon("eye-slash"))
    } else {
      updateActionButton(session, "toggle_filters", label = "Показать фильтры", icon = icon("eye"))
    }
  })

  filters <- reactive({
    list(
      language = input$language_filter,
      stars = input$stars_filter,
      created_date_range = as.POSIXct(input$created_date_range),
      updated_date_range = as.POSIXct(input$updated_date_range),
      issues = input$issues_filter,
      contributors = input$contributors_filter,
      size = input$size_filter,
      license = input$license_filter
    )
  })

  observeEvent(input$submit_button, {
    user_text <- input$user_input

    data$user_profile <- NULL
    data$commits <- NULL
    data$activity_data <- NULL
    data$language_data <- NULL
    data$commit_heatmap_data <- NULL

    withProgress(message = "Загрузка репозиториев...", {
      data$repos <- get_user_repos(user_text, setProgress) %>%
        filter_repos(filters())
      
      if (!is.null(data$repos)) {
        data$user_profile <- get_user_profile(user_text)
        data$activity_data <- prepare_activity_data(data$repos)
        data$language_data <- prepare_language_data(data$repos)
      } else {
        showNotification("Ошибка при загрузке репозиториев", type = "error")
      }
    })
  })

  observe({
    if (!is.null(data$repos)) {
      withProgress(message = "Загрузка коммитов...", {
        commits <- get_user_commits_df(data$repos, setProgress)
        if (!is.null(commits)) {
          data$commits <- commits %>% arrange(desc(date))
          data$commit_heatmap_data <- prepare_commit_heatmap_data(data$commits)
        } else {
          showNotification("Коммиты не найдены", type = "warning")
        }
      })
    }
  })

  output$user_report <- renderUI({
    profile <- data$user_profile
    if (!is.null(profile)) {
      tags$div(
        class = "user-report",
        style = "padding: 20px;",

        tags$div(
          class = "profile-header",
          style = "display: flex; align-items: center; margin-bottom: 30px;",

          tags$img(
            src = profile$avatar_url,
            style = "width: 150px; height: 150px; border-radius: 50%; margin-right: 30px;"
          ),

          tags$div(
            tags$h1(profile$name, style = "margin: 0 0 10px 0;"),
            tags$p(profile$bio, style = "font-size: 16px; color: #666;"),
            tags$div(
              style = "display: flex; gap: 15px; margin-top: 10px;",
              tags$span(icon("users"), "Подписчиков: ", profile$followers),
              tags$span(icon("user-plus"), "Подписок: ", profile$following),
              tags$span(icon("database"), "Репозиториев: ", profile$public_repos)
            )
          )
        ),

        tags$div(
          class = "stats-grid",
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px;",

          # Левая колонка
          tags$div(
            class = "stats-column",
            style = "background: #f8f9fa; padding: 20px; border-radius: 10px;",

            tags$h3(icon("chart-line"), "Активность", style = "margin-top: 0;"),
            tags$p(icon("calendar"), "Создан: ", format(as.Date(profile$created_at), "%d.%m.%Y")),
            tags$p(icon("sync"), "Последняя активность: ", format(as.Date(profile$updated_at), "%d.%m.%Y")),
            tags$p(icon("building"), "Компания: ", profile$company %||% "Не указана"),
            tags$p(icon("map-marker"), "Локация: ", profile$location %||% "Не указана")
          ),

          # Правая колонка
          tags$div(
            class = "stats-column",
            style = "background: #f8f9fa; padding: 20px; border-radius: 10px;",

            tags$h3(icon("trophy"), "Достижения", style = "margin-top: 0;"),
            tags$p(icon("star"), "Среднее звёзд на репозиторий: ", round(mean(sapply(data$repos, function(r) r$stars)), 1)),
            tags$p(icon("code-branch"), "Среднее форков на репозиторий: ", round(mean(sapply(data$repos, function(r) r$forks)), 1)),
            tags$p(icon("exclamation-triangle"), "Репозиториев с лицензией: ", sum(sapply(data$repos, function(r) r$license != "Нет лицензии")))
          )
        ),

        tags$div(
          style = "margin-top: 30px;",
          tags$h3(icon("link"), "Ссылки"),
          tags$a(
            href = profile$html_url,
            target = "_blank",
            class = "btn btn-primary",
            style = "margin-right: 10px;",
            icon("github"), "Профиль GitHub"
          ),
          downloadButton(
            "download_report",
            label = "Экспорт PDF",
            class = "btn btn-danger",
            style = "color: white;"
          )
        )
      )
    } else { }
  })

  output$repo_info <- renderUI({
    repos <- data$repos
    if (!is.null(repos)) {
      lapply(repos, function(repo) {
        tags$div(
          class = "repo-card",
          style = "border: 1px solid #ddd; border-radius: 8px; padding: 16px; margin-bottom: 16px; background: #f9f9f9;",

          # Заголовок с иконкой
          tags$div(
            style = "display: flex; align-items: center; margin-bottom: 12px;",
            tags$i(class = "fas fa-book", style = "font-size: 24px; margin-right: 8px; color: #0366d6;"),
            tags$h3(repo$name, style = "margin: 0; font-size: 24px; color: #0366d6;")
          ),

          # Описание
          if (!is.null(repo$description) && repo$description != "") {
            tags$p(
              style = "font-size: 14px; color: #586069; margin-bottom: 12px;",
              tags$i(class = "fas fa-align-left", style = "margin-right: 8px;"),
              repo$description
            )
          },

          # Основные метрики
          tags$div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px; margin-bottom: 12px;",

            # Язык программирования
            tags$div(
              style = "background: #fff; padding: 8px; border-radius: 4px;",
              tags$p(
                style = "margin: 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-code", style = "margin-right: 8px;"),
                "Язык: ", tags$b(repo$language)
              )
            ),

            # Звёзды
            tags$div(
              style = "background: #fff; padding: 8px; border-radius: 4px;",
              tags$p(
                style = "margin: 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-star", style = "margin-right: 8px; color: #ffd33d;"),
                "Звёзды: ", tags$b(repo$stars)
              )
            ),

            # Форки
            tags$div(
              style = "background: #fff; padding: 8px; border-radius: 4px;",
              tags$p(
                style = "margin: 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-code-branch", style = "margin-right: 8px; color: #28a745;"),
                "Форки: ", tags$b(repo$forks)
              )
            ),

            # Участники
            tags$div(
              style = "background: #fff; padding: 8px; border-radius: 4px;",
              tags$p(
                style = "margin: 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-users", style = "margin-right: 8px; color: #6f42c1;"),
                "Участники: ", tags$b(repo$contributors)
              )
            )
          ),

          # Прогресс-бары для числовых показателей
          tags$div(
            style = "margin-bottom: 12px;",

            # Issues
            tags$div(
              style = "margin-bottom: 8px;",
              tags$p(
                style = "margin: 0 0 4px 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-exclamation-circle", style = "margin-right: 8px; color: #d73a49;"),
                "Открытые issues:"
              ),
              tags$div(
                style = paste0(
                  "width: 100%; height: 8px; background: #e1e4e8; border-radius: 4px;",
                  "position: relative; overflow: hidden;"
                ),
                tags$div(
                  style = paste0(
                    "width: ", (repo$open_issues / max(1, repo$open_issues + 10)) * 100, "%; ",
                    "height: 100%; background: #d73a49; border-radius: 4px;"
                  )
                )
              )
            ),

            # Размер репозитория
            tags$div(
              style = "margin-bottom: 8px;",
              tags$p(
                style = "margin: 0 0 4px 0; font-size: 14px; color: #586069;",
                tags$i(class = "fas fa-weight", style = "margin-right: 8px; color: #6a737d;"),
                "Размер: ", round(repo$size / 1024, 2), " MB"
              ),
              tags$div(
                style = paste0(
                  "width: 100%; height: 8px; background: #e1e4e8; border-radius: 4px;",
                  "position: relative; overflow: hidden;"
                ),
                tags$div(
                  style = paste0(
                    "width: ", (repo$size / max(1, repo$size + 10240)) * 100, "%; ",
                    "height: 100%; background: #6a737d; border-radius: 4px;"
                  )
                )
              )
            )
          ),

          # Даты и ссылка
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 12px;",

            # Даты
            tags$div(
              style = "font-size: 12px; color: #586069;",
              tags$p(
                style = "margin: 0;",
                tags$i(class = "fas fa-calendar-plus", style = "margin-right: 4px;"),
                "Создан: ", format(repo$created_at, "%d.%m.%Y")
              ),
              tags$p(
                style = "margin: 0;",
                tags$i(class = "fas fa-calendar-check", style = "margin-right: 4px;"),
                "Обновлён: ", format(repo$updated_at, "%d.%m.%Y")
              )
            ),

            # Ссылка
            tags$a(
              href = repo$url,
              target = "_blank",
              class = "btn btn-primary",
              style = "background: #0366d6; color: #fff; padding: 6px 12px; border-radius: 4px; text-decoration: none;",
              tags$i(class = "fas fa-external-link-alt", style = "margin-right: 4px;"),
              "Открыть репозиторий"
            )
          )
        )
      })
    } else { }
  })

  output$commits_table <- renderDataTable({
    commits <- data$commits
    if (!is.null(commits)) {
      datatable(commits, options = list(pageLength = 10))
    } else {
      NULL
    }
  })

  output$activity_plot <- renderPlotly({
    if (!is.null(data$activity_data)) {
      ggplotly(
        ggplot(data$activity_data, aes(x = date, y = count, color = type)) +
          geom_line() +
          labs(title = "Активность", x = "Дата", y = "Количество событий")
      )
    }
  })

  output$language_plot <- renderPlotly({
    if (!is.null(data$language_data)) {
      ggplotly(
        ggplot(data$language_data, aes(x = language, y = count, fill = language)) +
          geom_bar(stat = "identity") +
          labs(title = "Популярность языков программирования", x = "Язык", y = "Количество репозиториев")
      )
    }
  })

  output$commit_heatmap <- renderPlotly({
    if (!is.null(data$commit_heatmap_data)) {
      ggplotly(
        ggplot(data$commit_heatmap_data, aes(x = hour, y = day, fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "white", high = "red") +
          labs(title = "Активность", x = "Час", y = "День недели")
      )
    }
  })

  output$pca_plot <- renderPlotly({
    if (!is.null(data$commits)) {
      pca_data <- perform_pca(data$commits)

      plot_ly(
        data = pca_data,
        x = ~PC1,
        y = ~PC2,
        color = ~author,
        text = ~paste("ID:", id, "<br>PC1:", round(PC1, 2), "<br>PC2:", round(PC2, 2)),
        hoverinfo = "text",
        type = "scatter",
        mode = "markers"
      ) %>% layout()
    }
  })
  
  output$outlier_cards <- renderUI({
    req(data$commits)
    outliers <- detect_outliers(perform_pca(data$commits))
    
    if (!is.null(outliers) && nrow(outliers) > 0) {
      # Группируем коммиты по (ID, author)
      outlier_commits <- merge(outliers, data$commits, by = "id") %>% 
        group_by(id, author.x) %>%
        summarise(
          date = first(date),
          patch = first(patches),
          message = first(message),
          branch = first(branch),
          repo = first(repo),
          files_changed = n_distinct(filename),
          additions = sum(additions, na.rm = TRUE),
          deletions = sum(deletions, na.rm = TRUE),
          distance = first(distance),
          z_score = first(z_score),
          .groups = "drop"
        ) %>%
        arrange(desc(z_score))
      
      lapply(1:nrow(outlier_commits), function(i) {
        commit <- outlier_commits[i,]
        commit_url <- paste0("https://github.com/", commit$repo, "/commit/", commit$id)
        
        # Определение цветов для z-score
        z_color <- case_when(
          commit$z_score >= 3 ~ list(
            bg = "#FFEBEE",
            border = "#FF5252",
            text = "#D32F2F",
            label = "🔥 Критично"
          ),
          commit$z_score >= 2 ~ list(
            bg = "#FFF3E0",
            border = "#FF9100",
            text = "#EF6C00",
            label = "⚠️ Высокий"
          ),
          TRUE ~ list(
            bg = "#E8F5E9",
            border = "#43A047",
            text = "#2E7D32",
            label = "✅ Норма"
          )
        )
        
        tags$div(
          class = "commit-card",
          style = paste(
            "border: 1px solid #e1e4e8;",
            "border-radius: 12px;",
            "padding: 16px;",
            "margin-bottom: 16px;",
            "background: linear-gradient(to right, #fff 95%, #fdd 100%);",
            "box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
            "position: relative;"
          ),
          
          # Лента аномалии
          tags$div(
            style = paste(
              "position: absolute;",
              "top: 0;",
              "right: 0;",
              "background: #d73a49;",
              "color: white;",
              "padding: 4px 12px;",
              "border-radius: 0 12px 0 12px;",
              "font-size: 0.8em;"
            ),
            icon("exclamation-triangle"), " Аномалия"
          ),
          
          # Основной контент
          tags$div(
            # Заголовок
            tags$div(
              style = "margin-bottom: 12px; border-bottom: 1px solid #eee; padding-bottom: 8px;",
              tags$a(
                href = commit_url,
                target = "_blank",
                style = "text-decoration: none; color: inherit;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 8px;",
                  tags$span(
                    style = paste(
                      "font-family: monospace;",
                      "font-weight: bold;",
                      "color: #0366d6;",
                      "cursor: pointer;",
                      "text-decoration: underline;"
                    ),
                    paste0("ID: ", substr(commit$id, 1, 7))
                  ),
                  tags$span(
                    style = "font-size: 0.9em; color: #586069;",
                    icon("user-circle"), commit$author.x
                  )
                )
              ),
              tags$div(
                style = "display: flex; gap: 12px; margin-top: 6px;",
                tags$span(
                  style = "display: flex; align-items: center; gap: 4px;",
                  icon("code-branch"), 
                  tags$span(style = "color: #6f42c1;", commit$branch)
                ),
                tags$span(
                  style = "display: flex; align-items: center; gap: 4px;",
                  icon("box"), 
                  tags$span(style = "color: #28a745;", commit$repo)
                )
              )
            ),
            
            # Метрики
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap: 8px; margin-bottom: 12px;",
              
              # Блок даты
              tags$div(
                class = "metric-card",
                style = "display: flex; align-items: center; gap: 8px;",
                icon("calendar", style = "color: #6a737d; font-size: 1.2em;"),
                tags$div(
                  tags$div(style = "font-size: 0.8em; color: #586069;", "Дата"),
                  tags$div(style = "font-weight: 500;", format(as.POSIXct(commit$date, format = "%Y.%m.%d %H:%M:%S"), "%d.%m.%Y %H:%M:%S"))
                )
              ),
              
              # Блок файлов
              tags$div(
                class = "metric-card",
                style = "display: flex; align-items: center; gap: 8px;",
                icon("file-code", style = "color: #6a737d; font-size: 1.2em;"),
                tags$div(
                  tags$div(style = "font-size: 0.8em; color: #586069;", "Файлов"),
                  tags$div(style = "font-weight: 500; color: #0366d6;", commit$files_changed)
                )
              ),
              
              # Блок изменений
              tags$div(
                class = "metric-card",
                style = "display: flex; align-items: center; gap: 8px;",
                icon("edit", style = "color: #6a737d; font-size: 1.2em;"),
                tags$div(
                  tags$div(style = "font-size: 0.8em; color: #586069;", "Изменения"),
                  tags$div(
                    style = "display: flex; gap: 6px;",
                    tags$span(style = "color: #28a745;", paste0("+", commit$additions)),
                    tags$span(style = "color: #d73a49;", paste0("-", commit$deletions))
                  )
                )
              )
            ),
            
            # Сообщение коммита
            tags$div(
              style = "background: #f6f8fa; padding: 12px; border-radius: 6px; margin-bottom: 12px;",
              tags$div(
                style = "display: flex; gap: 8px; color: #586069;",
                icon("comment-dots"),
                tags$em(commit$message)
              )
            ),
            
            # Метрики МГК
            tags$div(
              style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 8px; margin-bottom: 12px;",
              
              # Блок z-score
              tags$div(
                style = paste(
                  "padding: 8px;",
                  "background:", z_color$bg, ";",
                  "border-radius: 6px;",
                  "text-align: center;",
                  "border: 2px solid", z_color$border, ";"
                ),
                tags$div(
                  style = paste("font-size: 0.8em; color:", z_color$text, "; font-weight: 600;"),
                  "Уровень аномалии"
                ),
                tags$div(
                  style = paste("font-weight: bold; color:", z_color$text, "; font-size: 1.1em;"),
                  round(commit$z_score, 2),
                  tags$span(style = "margin-left: 5px;", z_color$label)
                )
              ),
              
              # Блок расстояния МГК
              tags$div(
                style = paste(
                  "padding: 8px;",
                  "background: #ffebee;",
                  "border-radius: 6px;",
                  "text-align: center;",
                  "border: 1px solid #ffcdd2;"
                ),
                tags$div(style = "font-size: 0.8em; color: #d32f2f;", "Расстояние МГК"),
                tags$div(style = "font-weight: bold; color: #b71c1c;", round(commit$distance, 2))
              )
            ),
            
            # Блок отчёта
            tags$div(
              style = paste(
                "background: #f8f9fa;",
                "border-left: 3px solid #0366d6;",
                "padding: 12px;",
                "border-radius: 6px 0 0 6px;",
                "margin-top: 12px;"
              ),
              tags$div(
                style = "display: flex; gap: 8px; align-items: flex-start;",
                icon("code", style = "color: #0366d6; margin-top: 3px;"),
                tags$div(
                  style = "flex-grow: 1; min-width: 0;",
                  HTML(
                    commit$patch %>%
                      response_otchet() %>%
                      format_report() %>%
                      str_replace(
                        pattern = "<pre><code>", 
                        replacement = "<pre style='background: #eef4fb; padding: 12px; border-radius: 4px;'><code>"
                      )
                  )
                )
              ),
              tags$style(HTML(
                "
    .report-content pre {
      background: #eef4fb;
      border: 1px solid #d0d7de;
      border-radius: 6px;
      padding: 12px;
      margin: 8px 0;
      overflow-x: auto;
      font-family: 'Fira Code', monospace;
      font-size: 0.85em;
    }
    
    .report-content code {
      background: #eef4fb;
      padding: 2px 4px;
      border-radius: 4px;
      font-family: 'Fira Code', monospace;
    }
    
    .report-content pre code {
      background: transparent;
      padding: 0;
      border-radius: 0;
    }
    "
              ))
            )
          )
        )
      })
    } else {
      tags$div(
        style = "text-align: center; color: #586069; padding: 20px;",
        icon("check-circle"), " Аномальных коммитов не обнаружено"
      )
    }
  })
  
  format_report <- function(text) {
    text <- gsub("```r\n", "```\n", text, fixed = TRUE)
    text <- gsub("\n", "  \n", text)
    text <- paste0("**Отчёт:**  \n", text)
    markdownToHTML(
      text = text,
      fragment.only = TRUE,
      options = c("escape", "fragment_only")
    ) %>% 
      str_replace_all("&lt;", "<") %>% # Исправляем HTML-сущности
      str_replace_all("&gt;", ">")
  }
  
  response_otchet <- function(patch) {
    prompt <- list(
      modelUri = sprintf("gpt://%s/yandexgpt-32k", YANDEX_FOLDER_ID),
      completionOptions = list(
        stream = FALSE,
        temperature = 0,
        maxTokens = "2000",
        reasoningOptions = list(
          mode = "ENABLED_HIDDEN"
        )
      ),
      messages = list(
        list(
          role = "system",
          text = paste0("Ты получишь изменения из коммита (GitHub API patch), который был идентифицирован, как аномальный ",
                        "(обращай на это внимание, однако помни, что коммиты могут быть и ложно-аномальными). ",
                        "Тебе нужно составить список файлов, которые требуют отдельного/внимательного анализа (не обязательно все; ты должен быть уверен в важности анализа; стоит обращать внимание на потенциально опасные файлы), ",
                        "а также дать краткий отчёт по типам вносимых изменений. ",
                        "В отличие от файлов, на которые 'стоит обратить внимание', в список изменений обязательно должны попасть все файлы коммита.\n",
                        "Типы вносимых изменения: 'Документация', 'Fix', 'Расширение', 'Вредоносный код', 'Ошибка кода', 'NULL', 'Другое'.\n",
                        "Формат ввода:\n",
                        "```\n",
                        "patch ({имя_файла_1}): ```{patch_1}```;\n",
                        "patch ({имя_файла_2}): ```{patch_2}```;\n",
                        "...\n",
                        "Ожидаемый формат отчёта:\n",
                        "```\n",
                        "Стоит обратить внимание:\n",
                        "* `{аномальный_файл_1}`: **{на что стоит обратить внимание}**;\n",
                        "* `{аномальный_файл_2}`: **{на что стоит обратить внимание}**;\n",
                        "...\n",
                        "Изменения:\n",
                        "* `{имя_файла_1}`: **{тип_изменения}** ({объяснение (1-2 предложения)});\n",
                        "* `{имя_файла_2}`: **{тип_изменения}** ({объяснение (1-2 предложения)});\n",
                        "...\n",
                        "```"
                        )
          ),
        list(
          role = "user",
          text = patch
        )
      )
    )
    
    response <- POST(
      url = "https://llm.api.cloud.yandex.net/foundationModels/v1/completion",
      add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Api-Key", YANDEX_API_KEY)
      ),
      body = toJSON(prompt, auto_unbox = TRUE, pretty = TRUE),
      encode = "json"
    )
    
    if (status_code(response) == 200) {
      return(content(response, "parsed")$result$alternatives[[1]]$message$text)
    } else {
      return(paste0("ERROR: ", status_code(response)))
    }
  }
}
