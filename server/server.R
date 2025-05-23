library(shiny)
library(shinyjs)
library(httr2)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(stringr)
library(jsonlite)
library(markdown)
library(readr)

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
  needs_restart <- FALSE
  sidebar_state <- reactiveVal(FALSE)
  
  # Обработчик клика по кнопке
  observeEvent(input$toggle_sidebar, {
    sidebar_state(!sidebar_state())
    shinyjs::toggleClass("main_layout", "sidebar-collapsed")
    
    # Меняем иконку кнопки
    icon_name <- if (sidebar_state()) "angle-double-right" else "angle-double-left"
    updateActionButton(session, "toggle_sidebar", icon = icon(icon_name))
  })
  # Проверка и инициализация .Renviron
  init_renviron <- function() {
    renv_path <- file.path(getwd(), ".Renviron")
    
    # Если файла нет - создаем с дефолтными значениями
    if (!file.exists(renv_path)) {
      writeLines(
        c(
          "GITHUB_TOKEN=",
          "YANDEX_FOLDER_ID=",
          "YANDEX_API_KEY=",
          "DUCK_DB=KPZ",
          "COMMITS_TABLE=commits"
        ),
        renv_path
      )
      message("Created .Renviron file with default values")
    } else {
      # Если файл есть - проверяем наличие обязательных переменных
      renv_lines <- readLines(renv_path)
      
      # Функция для проверки и добавления переменной
      check_add_var <- function(var_name, default_value) {
        pattern <- paste0("^", var_name, "=")
        if (!any(grepl(pattern, renv_lines))) {
          write(paste0(var_name, "=", default_value), renv_path, append = TRUE)
          message("Added missing variable: ", var_name)
        } else {
          # Проверяем что после = есть значение
          line <- renv_lines[grep(pattern, renv_lines)]
          if (grepl(paste0("^", var_name, "=$"), line)) {
            # Заменяем пустое значение на дефолтное
            renv_lines[grep(pattern, renv_lines)] <- paste0(var_name, "=", default_value)
            writeLines(renv_lines, renv_path)
            message("Updated empty variable: ", var_name)
          }
        }
      }
      
      # Проверяем обязательные переменные
      check_add_var("DUCK_DB", "KPZ")
      check_add_var("COMMITS_TABLE", "commits")
    }
    
    # Загружаем обновленный .Renviron
    readRenviron(renv_path)
  }
  
  # Инициализируем .Renviron перед запуском приложения
  init_renviron()
  
  # Обработка текущего уровня логгирования
  observeEvent(input$log_level, {
    new_level <- input$log_level
    flog.threshold(new_level)
  })
  
  # Реактивные значения для текущих вкладок
  current_main_tab <- reactiveVal("report")
  current_settings_tab <- reactiveVal("env_vars")
  
  # Обработчики переключения вкладок Главного меню
  observeEvent(input$tab_report, { current_main_tab("report") })
  observeEvent(input$tab_repos, { current_main_tab("repos") })
  observeEvent(input$tab_commits, { current_main_tab("commits") })
  observeEvent(input$tab_events, { current_main_tab("events") })
  observeEvent(input$tab_languages, { current_main_tab("languages") })
  observeEvent(input$tab_activity, { current_main_tab("activity") })
  observeEvent(input$tab_pca, { current_main_tab("pca") })
  
  # Обработчики переключения вкладок Настроек
  observeEvent(input$tab_env_vars, { current_settings_tab("env_vars") })
  observeEvent(input$tab_other_settings, { current_settings_tab("other_settings") })
  
  # Рендеринг контента для Главного меню
  output$main_content <- renderUI({
    switch(current_main_tab(),
           "report" = withSpinner(uiOutput("user_report")),
           "repos" = withSpinner(uiOutput("repo_info")),
           "commits" = withSpinner(dataTableOutput("commits_table")),
           "events" = withSpinner(plotlyOutput("activity_plot")),
           "languages" = withSpinner(plotlyOutput("language_plot")),
           "activity" = withSpinner(plotlyOutput("commit_heatmap")),
           "pca" = tagList(
             tags$div(
               style = "display: flex; height: calc(100vh - 200px);",
               tags$div(
                 style = "flex: 1 1 60%;",
                 withSpinner(plotlyOutput("pca_plot"))
               ),
               tags$div(
                 style = "flex: 1 1 40%; display: flex; flex-direction: column;",
                 tags$div(
                   style = "padding: 8px 0;",
                   uiOutput("pca_outliers")
                 ),
                 tags$div(
                   style = "overflow-y: auto; padding-right: 8px;",
                   withSpinner(uiOutput("outlier_cards"))
                 )
               )
             )
           )
    )
  })
  
  # Рендеринг контента для Настроек
  output$settings_content <- renderUI({
    switch(current_settings_tab(),
           "env_vars" = tagList(
             fluidRow(
               column(6,
                      # Первая колонка - основные настройки
                      textInput("github_token", "GitHub Token", value = Sys.getenv("GITHUB_TOKEN")),
                      textInput("yandex_folder_id", "Yandex Folder ID", value = Sys.getenv("YANDEX_FOLDER_ID")),
                      textInput("yandex_api_key", "Yandex API Key", value = Sys.getenv("YANDEX_API_KEY")),
                      textInput("duck_db", "DuckDB Name", value = Sys.getenv("DUCK_DB")),
                      textInput("commits_table", "Commits Table Name", value = Sys.getenv("COMMITS_TABLE"))
               ),
               column(6,
                      # Вторая колонка - статистика
                      uiOutput("github_rate_limit")
               )
             ),
             actionButton("save_env", "Сохранить настройки (перезагрузка при закрытии)",icon = icon("save"), class = "btn-primary"),
             verbatimTextOutput("env_status")
           ),
           "other_settings" = tagList(
             h3("Другие настройки приложения"),
             p("Здесь могут быть дополнительные параметры конфигурации")
           )
    )
  })
  
  # Обработчик сохранения переменных окружения
  observeEvent(input$save_env, {
    env_vars <- c(
      paste0("GITHUB_TOKEN=", input$github_token),
      paste0("YANDEX_FOLDER_ID=", input$yandex_folder_id),
      paste0("YANDEX_API_KEY=", input$yandex_api_key),
      paste0("DUCK_DB=", input$duck_db),
      paste0("COMMITS_TABLE=", input$commits_table)
    )
    
    tryCatch({
      writeLines(env_vars, ".Renviron")
      readRenviron(".Renviron")
      
      # Вместо немедленной перезагрузки устанавливаем флаг
      needs_restart <<- TRUE
      
      output$env_status <- renderText({
        "Настройки успешно сохранены! Перезагрузка произойдет при закрытии приложения."
      })
    }, error = function(e) {
      output$env_status <- renderText({
        paste("Ошибка при сохранении настроек:", e$message)
      })
    })
  })
  
  # Проверка лимита запросов
  update_github_rate_limit <- function() {
    token <- Sys.getenv("GITHUB_TOKEN")
    if (nzchar(token)) {
      tryCatch({
        req <- request("https://api.github.com/rate_limit") %>%
          req_headers(
            Accept = "application/vnd.github+json",
            Authorization = paste("Bearer", token)
          )
        
        resp <- req %>% req_perform()
        
        if (resp_is_error(resp)) {
          stop("Request failed")
        }
        
        limits <- resp %>% resp_body_json()
        core_limit <- limits$resources$core
        remaining <- core_limit$remaining
        limit <- core_limit$limit
        reset_time <- as.POSIXct(core_limit$reset, origin = "1970-01-01")
        
        output$github_rate_limit <- renderUI({
          tags$div(
            class = "rate-limit-box",
            style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
            tags$h4("GitHub API Rate Limit", style = "margin-top: 0;"),
            tags$p(
              style = "margin-bottom: 5px;",
              tags$b("Использовано: "),
              sprintf("%d/%d запросов", limit - remaining, limit)
            ),
            tags$p(
              style = "margin-bottom: 0;",
              tags$b("Сброс: "),
              format(reset_time, "%H:%M:%S")
            )
          )
        })
      }, error = function(e) {
        output$github_rate_limit <- renderUI({
          tags$div(
            class = "alert alert-warning",
            "Не удалось проверить лимит запросов GitHub API"
          )
        })
      })
    } else {
      output$github_rate_limit <- renderUI({
        tags$div(
          class = "alert alert-info",
          "Введите GitHub Token для проверки лимита запросов"
        )
      })
    }
  }
  
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
    
    withProgress(message = "", value = 0, {
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
  
  # Вызов при загрузке
  observe({
    if (!is.null(data$repos)) {
      withProgress(message = "", value = 0, {
        commits <- get_user_commits_df(data$repos, setProgress)
        if (!is.null(commits)) {
          data$commits <- commits %>% arrange(desc(date))
          data$commit_heatmap_data <- prepare_commit_heatmap_data(data$commits)
        } else {
          showNotification("Коммиты не найдены", type = "warning")
        }
      })
    }
    update_github_rate_limit()
  })
  
  # Рендеринг контента для Отчета
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
            ),
            tags$div(
              style = "margin-top: 30px;",
              tags$a(
                href = profile$html_url,
                target = "_blank",
                class = "btn btn-primary",
                style = "margin-right: 10px;",
                icon("github"), "Профиль GitHub"
              )
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
        )
      )
    }
  })
  
  output$commits_table <- renderDataTable({
    commits <- data$commits
    if (!is.null(commits)) {
      # Ограничиваем длину текста в столбце patch
      commits$patch <- sapply(commits$patch, function(x) {
        if (nchar(x) > 50) {
          paste0(substr(x, 1, 50), "...")
        } else {
          x
        }
      })
      
      # Ограничиваем длину текста в столбце filename
      commits$filename <- sapply(commits$filename, function(x) {
        if (nchar(x) > 200) {
          paste0(substr(x, 1, 200), "...")
        } else {
          x
        }
      })
      
      datatable(
        commits,
        options = list(
          pageLength = 5,
          columnDefs = list(
            list(targets = which(names(commits) == "patch") - 1,
                 render = JS(
                   "function(data, type, row, meta) {",
                   "  return type === 'display' && data.length > 50 ?",
                   "    '<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                   "}")),
            list(targets = which(names(commits) == "filename") - 1,
                 render = JS(
                   "function(data, type, row, meta) {",
                   "  return type === 'display' && data.length > 200 ?",
                   "    '<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                   "}"))
          )
        ),
        filter = list(
          position = 'top',
          clear = FALSE,
          options = list(
            columns = ':not(:first-child)'
          )
        ),
        extensions = 'Select',
        selection = 'none'
      ) %>%
        formatStyle(columns = names(commits), fontSize = '12px')
    } else {
      NULL
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
    } else { }
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
      
      lang_data <- data$language_data %>%
        mutate(
          percentage = round(count / sum(count) * 100, 2),
          
          color = sapply(language, function(lang) {
            set.seed(nchar(lang))
            
            paste0("#", substr(digest::digest(lang, algo = "xxhash32"), 1, 6))
          })
        ) %>%
        arrange(desc(count))
      
      
      annotations <- lapply(1:nrow(lang_data), function(i) {
        list(
          x = 1.15,
          
          y = 1 - (i * 0.05),
          
          text = paste0(
            "<span style='color:", lang_data$color[i], "'>■ </span>",
            
            lang_data$language[i], 
            
            " (", sprintf("%.2f%%", lang_data$percentage[i]), ")"
          ),
          
          showarrow = FALSE,
          
          xref = "paper",
          
          yref = "paper",
          
          font = list(size = 12),
          
          align = "left"
        )
      })
      
      
      plot_ly(
        data = lang_data,
        
        labels = ~language,
        
        values = ~count,
        
        marker = list(
          colors = ~color,
          
          line = list(color = "#FFFFFF", width = 1)
        ),
        
        textinfo = "none",
        
        hoverinfo = "label+percent+value",
        
        type = "pie",
        
        hole = 0,
        
        sort = FALSE
      ) %>% 
        layout(
          showlegend = FALSE,
          
          margin = list(t = 40, b = 20, r = 200),
          
          annotations = c(
            list(
              list(
                x = 1.05,
                
                y = 1.05,
                
                text = "Языки программирования:",
                
                showarrow = FALSE,
                
                xref = "paper",
                
                yref = "paper",
                
                font = list(size = 14, weight = "bold")
              )
            ),
            
            annotations
          )
        )
    }
  })
  
  output$commit_heatmap <- renderPlotly({
    req(data$commit_heatmap_data)
    
    # Рассчитываем общее количество коммитов по дням
    daily_stats <- data$commit_heatmap_data %>%
      group_by(day) %>%
      mutate(
        day_total = sum(count),
        percent_of_day = ifelse(day_total > 0, round(100 * count / day_total, 1), 0)
      ) %>%
      summarise(
        day_total = first(day_total),
        peak_hour = hour[which.max(count)],
        peak_count = max(count),
        .groups = "drop"
      )
    
    # Объединяем с основными данными
    heatmap_data <- data$commit_heatmap_data %>%
      left_join(daily_stats, by = "day") %>%
      mutate(
        percent_of_day = ifelse(day_total > 0, round(100 * count / day_total, 1), 0),
        hour_label = sprintf("%02d:00-%02d:00", hour, hour+1)
      )
    
    max_count <- max(heatmap_data$count)
    
    plot_ly() %>%
      add_heatmap(
        data = heatmap_data,
        x = ~hour, 
        y = ~day, 
        z = ~count,
        colors = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", 
                   "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"),
        hoverinfo = "text",
        text = ~paste0(
          "<b>", day, "</b><br>",
          "<b>Время:</b> ", hour_label, "<br>",
          "<b>Коммитов:</b> ", count, " (", percent_of_day, "% дня)<br>",
          "<b>Пиковая активность:</b> ", peak_hour, ":00 (", peak_count, " коммитов)<br>",
          "<b>Всего за день:</b> ", day_total, " коммитов"
        ),
        showscale = TRUE,
        colorbar = list(
          title = "Коммитов",
          tickvals = seq(0, max_count, length.out = min(5, max_count + 1)),
          tickformat = ",d"
        )
      ) %>%
      add_annotations(
        data = daily_stats,
        x = 24.5,
        y = ~day,
        text = ~paste0(
          "📊 Всего: ", day_total, "\n",
          "🕒 Пик: ", peak_hour, ":00"
        ),
        showarrow = FALSE,
        xanchor = "left",
        font = list(size = 10)
      ) %>%
      layout(
        title = list(
          text = "<b>Распределение коммитов по времени</b><br><sub>Процент от общего числа коммитов в день</sub>",
          font = list(size = 18),
          x = 0.05
        ),
        xaxis = list(
          title = "Час дня",
          tickvals = 0:23,
          range = c(-0.5, 27)
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 12)
        ),
        margin = list(l = 100, r = 150, t = 80),
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$pca_plot <- renderPlotly({
    if (!is.null(data$commits)) {
      pca_data <- perform_pca(data$commits)
      
      plot_ly(
        data = pca_data,
        x = ~PC1,
        y = ~PC2,
        color = ~author,
        text = ~sprintf(
          "<b>ID</b>: %s<br><b>PC1</b>: %.2f<br><b>PC2</b>: %.2f<br><b>МГК</b>: %.2f",
          substr(id, 1, 7), round(PC1, 2), round(PC2, 2), round(distance, 2)
        ),
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
                tags$em(ifelse(nchar(commit$message) > 30, paste0(substr(commit$message, 1, 30), "..."), commit$message))
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
                      get_response(., n_files = commit$files_changed) %>%
                      format_response() %>%
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
  
  system_prompt <- read_file("server/data/prompt")
  schema <- read_json("server/data/schema.json")
  get_response <- function(patch, n_files) {
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
          text = system_prompt
        ),
        list(
          role = "user",
          text = paste("Анализируй следующий git-patch (", n_files," файлов):\n```\n", patch, "\n```")
        )
      ),
      json_schema = schema
    )
    
    response <- NULL
    attempt <- 1
    max_attempts <- 3
    
    while (attempt <= max_attempts) {
      tryCatch({
        req <- request("https://llm.api.cloud.yandex.net/foundationModels/v1/completion") %>%
          req_headers(
            "Content-Type" = "application/json",
            "Authorization" = paste("Api-Key", YANDEX_API_KEY)
          ) %>%
          req_body_json(prompt)
        
        response <- req %>% 
          req_error(is_error = \(resp) FALSE) %>% 
          req_perform()
        
        # Парсинг ответа
        if (resp_status(response) != 200) {
          return(paste("ERROR:", resp_status(response)))
        }
        
        response_content <- resp_body_string(response)
        if (!jsonlite::validate(response_content)) {
          stop("Invalid JSON response")
        }
        
        parsed_response <- fromJSON(response_content, simplifyVector = FALSE)
        return(parsed_response$result$alternatives[[1]]$message$text %>% 
               fromJSON(simplifyVector = FALSE))
      }, error = function(e) {
        attempt <<- attempt + 1
      })
    }
    
    return("ERROR: max attempts")
  }
  
  format_response <- function(data) {
    if (is.character(data) ) return(data)
    
    report <- c(sprintf("**Отчёт** (`%s`):\n", data$status))
    
    if (length(data$analysis) > 0) {
      # Группировка по уровням риска
      risks <- list(
        CRITICAL = list(),
        WARNING = list(),
        INFO = list(),
        SAFE = list()
      )
      
      for (item in data$analysis) {
        risks[[item$risk_level]] <- c(risks[[item$risk_level]], list(item))
      }
      
      # Генерация секций для каждого уровня риска
      for (risk_level in names(risks)) {
        if (length(risks[[risk_level]]) > 0) {
          # Заголовок уровня риска с иконкой
          risk_icon <- switch(risk_level,
                              "CRITICAL" = "🔴",
                              "WARNING" = "🟠", 
                              "INFO" = "🔵",
                              "SAFE" = "🟢")
          
          report <- c(report, sprintf("\n%s **%s**", risk_icon, risk_level))
          
          # Вывод каждого случая
          for (finding in risks[[risk_level]]) {
            report <- c(report,
                        sprintf("\n**File:** `%s`\n", finding$path),
                        sprintf("- **Reason:** %s\n", finding$reason),
                        sprintf("- **Code:**\n```\n%s\n```\n", finding$code_snippet),
                        paste("  -", finding$recommendations, collapse = "\n") %>%
                          sprintf("- **Recommendations:**\n%s\n", .),
                        "---") # Разделитель между находками
          }
        }
      }
    } else {
      report <- c(report, "Опасные файлы не обнаружены")
    }
    
    return(paste0(report, collapse = ""))
  } 
  
  format_report <- function(text) {
    text <- gsub("```r\n", "```\n", text, fixed = TRUE)
    text <- gsub("\n", "  \n", text)
    markdownToHTML(
      text = text,
      fragment.only = TRUE,
      options = c("escape", "fragment_only")
    ) %>%
      str_replace_all("&lt;", "<") %>%
      str_replace_all("&gt;", ">")
  }
  
  # Обработчик закрытия приложения
  session$onSessionEnded(function() {
    if (needs_restart) {
      # Добавляем небольшую задержку перед перезагрузкой
      Sys.sleep(1)
      .rs.restartR()
    }
  })
}
