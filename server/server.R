library(shiny)
library(shinyjs)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)

source("server/ETL.R")
source("server/ML.R")

filter_repos <- function(repos, filters) {
  repos %>%
    Filter(function(repo) {
      if (filters$language != "Все" && repo$language != filters$language) {
        return(FALSE)
      }

      if (repo$stars < filters$stars[1] || repo$stars > filters$stars[2]) {
        return(FALSE)
      }

      if (repo$created_at < filters$created_date_range[1] ||
          repo$created_at > filters$created_date_range[2]) {
        return(FALSE)
      }

      if (repo$updated_at < filters$updated_date_range[1] ||
          repo$updated_at > filters$updated_date_range[2]) {
        return(FALSE)
      }

      if (repo$open_issues < filters$issues[1] ||
          repo$open_issues > filters$issues[2]) {
        return(FALSE)
      }

      if (repo$contributors < filters$contributors[1] ||
          repo$contributors > filters$contributors[2]) {
        return(FALSE)
      }

      repo_size_mb <- repo$size / 1024
      if (repo_size_mb < filters$size[1] || repo_size_mb > filters$size[2]) {
        return(FALSE)
      }

      if (filters$license != "Все" && repo$license != filters$license) {
        return(FALSE)
      }

      return(TRUE)
    }, .)
}

server <- function(input, output, session) {
  shinyjs::hide("filters")

  data <- reactiveValues(
    repos = NULL,
    user_profile = NULL,
    commits = NULL,
    activity_data = NULL,
    language_data = NULL,
    commit_heatmap_data = NULL
  )

  observeEvent(input$toggle_filters, {
    shinyjs::toggle("filters")
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
      repos <- get_user_repos(user_text, setProgress)
      if (!is.null(repos)) {
        data$user_profile <- get_user_profile(user_text)
        data$repos <- filter_repos(repos, filters())
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
          data$commits <- commits
          data$commit_heatmap_data <- prepare_commit_heatmap_data(data$commits)
        } else {
          showNotification("Коммиты не найдены", type = "warning")
        }
      })
    }
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0(data$user_profile$name, "_github_report.pdf")
    },
    content = function(file) {
      showNotification("Идет генерация отчета...", type = "message")

      # Создаем временный файл отчета
      report_path <- tempfile(fileext = ".Rmd")
      file.copy("report_template.Rmd", report_path)

      # Параметры для отчета
      params <- list(
        profile = data$user_profile,
        repos = data$repos,
        commits = data$commits
      )

      # Рендерим отчет
      rendered_report <- rmarkdown::render(
        report_path,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )

      return(rendered_report)
    }
  )

  # Новый рендер для отчета
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
    } else {
      tags$p("Профиль пользователя не найден.")
    }
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
    } else {
      tags$p("Репозитории не найдены.")
    }
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
      outliers <- detect_outliers(pca_data)

      output$pca_outliers <- renderUI({
        if (nrow(outliers) > 0) {
          tags$div(
            tags$h4("Выявленные выбросы:"),
            tags$ul(
              lapply(1:nrow(outliers), function(i) {
                tags$li(paste("ID:", outliers$id[i],
                              "| Автор:", outliers$author[i],
                              "| Расстояние:", round(outliers$distance[i], 2)))
              })
            )
          )
        } else {
          tags$p("Выбросы не обнаружены.")
        }
      })

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
  })
}
