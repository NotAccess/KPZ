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
        response <- GET(
          "https://api.github.com/rate_limit",
          add_headers(
            Accept = "application/vnd.github+json",
            Authorization = paste("token", token)
          )
        )
        
        if (status_code(response) == 200) {
          limits <- content(response)
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
        }
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
      tagList(
      tags$div(
        class = "user-report",
        style = "max-width: 1012px; margin: 0 auto; padding: 32px 16px; font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif;",
        
        # Заголовок профиля
        tags$div(
          style = "display: flex; gap: 32px; margin-bottom: 32px;",
          tags$img(
            src = profile$avatar_url,
            style = "width: 260px; height: 260px; border-radius: 50%; border: 1px solid #e1e4e8;"
          ),
          
          tags$div(
            style = "flex: 1;",
            tags$h1(
              style = "font-size: 32px; font-weight: 600; margin: 0 0 8px 0;",
              profile$name
            ),
            tags$p(
              style = "font-size: 20px; color: #57606a; margin: 0 0 16px 0;",
              profile$bio
            ),
            
            tags$div(
              style = "display: flex; gap: 24px; margin-bottom: 16px;",
              tags$div(
                style = "display: flex; align-items: center; gap: 4px; color: #24292f;",
                icon("users", class = "fa-lg"),
                tags$span(style = "font-weight: 600;", profile$followers),
                tags$span("подписчиков")
              ),
              tags$div(
                style = "display: flex; align-items: center; gap: 4px; color: #24292f;",
                icon("user-plus", class = "fa-lg"),
                tags$span(style = "font-weight: 600;", profile$following),
                tags$span("подписки")
              ),
              tags$div(
                style = "display: flex; align-items: center; gap: 4px; color: #24292f;",
                icon("building", class = "fa-lg"),
                tags$span(profile$company %||% "Не указана")
              )
            ),
            
            tags$div(
              style = "display: flex; gap: 16px;",
              tags$a(
                href = profile$html_url,
                target = "_blank",
                class = "btn btn-primary",
                style = paste(
                  "background: #2da44e; color: white;",
                  "padding: 8px 16px; border-radius: 6px;",
                  "text-decoration: none; font-weight: 600;",
                  "display: flex; align-items: center; gap: 8px;"
                ),
                icon("github"),
                "Профиль GitHub"
              )
            )
          )
        ),
        
        # Основная статистика
        tags$div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 16px; margin-bottom: 32px;",
          tags$div(
            style = "background: #f6f8fa; border: 1px solid #e1e4e8; border-radius: 6px; padding: 24px;",
            tags$h3(style = "font-size: 20px; margin: 0 0 16px 0;", "📊 Основные метрики"),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(3, minmax(140px, 1fr)); gap: 8px;",
              # Всего репозиториев
              tags$div(
                style = "display: flex; flex-direction: column; margin-right: 12px;",
                tags$div(
                  style = paste(
                    "color: #586069;",
                    "white-space: nowrap;", # Запрет переноса текста
                    "letter-spacing: -0.1px;" # Сжатие межбуквенного пространства
                  ),
                  "Всего репозиториев"
                ),
                tags$div(
                  style = paste(
                    "font-size: 32px;",
                    "font-weight: 600;",
                    "margin-top: -2px;" # Сдвиг числа вверх
                  ),
                  profile$public_repos
                )
              ),
              # Всего звёзд
              tags$div(
                tags$div(style = "color: #57606a;", "Всего звёзд"),
                tags$div(
                  style = "font-size: 32px; font-weight: 600;",
                  sum(sapply(data$repos, function(r) r$stars))
                )
              ),
              # Топ языков
              tags$div(
                tags$div(style = "color: #57606a; margin-bottom: 4px;", "Топ языков"),
                tags$div(
                  style = "display: flex; flex-direction: column; gap: 4px;",
                  if (!is.null(data$language_data)) {
                    top_langs <- head(arrange(data$language_data, desc(count)), 3)
                    lapply(1:nrow(top_langs), function(i) {
                      tags$div(
                        style = "display: flex; align-items: center; gap: 8px;",
                        tags$span(style = paste(
                          "width: 12px; height: 12px; border-radius: 50%;",
                          "background:", switch(top_langs$language[i],
                                                "R" = "#276DC3",
                                                "Python" = "#3572A5",
                                                "JavaScript" = "#F1E05A",
                                                "Java" = "#B07219",
                                                "C#" = "#178600",
                                                "C++" = "#F34B7D",
                                                "PHP" = "#4F5D95",
                                                "Swift" = "#FFAC45",
                                                "Kotlin" = "#A97BFF",
                                                "Go" = "#00ADD8",
                                                "Rust" = "#DEA584",
                                                "TypeScript" = "#3178C6",
                                                "Ruby" = "#701516",
                                                "SQL" = "#E38C00",
                                                "Dart" = "#00B4AB",
                                                "Scala" = "#DC322F",
                                                "Perl" = "#39457E",
                                                "Haskell" = "#5E5086",
                                                "Lua" = "#000080",
                                                "MATLAB" = "#E16737",
                                                "Shell" = "#89E051",
                                                "#000000" # Дефолтный цвет
                          )
                        )),
                        tags$span(top_langs$language[i]),
                        tags$span(style = "color: #57606a; font-size: 0.9em;", paste0("(", top_langs$count[i], ")"))
                      )
                    })
                  } else {
                    tags$em("Недостаточно данных", style = "color: #57606a;")
                  }
                )
              )
            )
          ),
          tags$div(
            style = "background: #f6f8fa; border: 1px solid #e1e4e8; border-radius: 6px; padding: 24px;",
            tags$h3(style = "font-size: 20px; margin: 0 0 16px 0;", "📅 Активность"),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 16px;",
              tags$div(
                tags$div(style = "color: #57606a;", "Создан аккаунт"),
                tags$div(style = "font-weight: 600;", format(as.Date(profile$created_at), "%d.%m.%Y"))
              ),
              tags$div(
                tags$div(style = "color: #57606a;", "Последняя активность"),
                tags$div(style = "font-weight: 600;", format(as.Date(profile$updated_at), "%d.%m.%Y"))
              )
            )
          )
        ),
        tags$div(
          style = "border-top: 1px solid #e1e4e8; padding-top: 32px;",
          tags$h2(style = "font-size: 24px; margin: 0 0 8px 0;", "📦 Репозитории"),
          tags$p(
            style = paste(
              "color: #57606a;",
              "font-size: 14px;",
              "margin: 0 0 16px 0;",
              "border-bottom: 1px solid #f0f0f0;",
              "padding-bottom: 12px;"
            ),
            "Список публичных репозиториев пользователя (всего ", 
            length(data$repos), 
            ")"
          ),
          # Кнопка управления всеми репозиториями
          tags$div(
            style = "display: flex; justify-content: flex-end; margin-bottom: 16px;",
            actionButton(
              "toggle_all_repos",
              "Свернуть",
              class = "btn-link",
              style = "color: #0969da; border: none; font-weight: 500;"
            )
          ),
          
          # Блок с репозиториями
          tags$div(
            id = "repos_container",
            style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 16px; transition: 0.3s all ease;",
            lapply(seq_along(data$repos), function(i) {
              repo <- data$repos[[i]]
              repo_id <- paste0("repo_", i)
              
              tags$div(
                id = repo_id,
                class = "repo-item",
                style = "transition: all 0.3s ease;",
                tags$div(
                  class = "repo-card",
                  style = "border: 1px solid #e1e4e8; border-radius: 6px; background: white;",
                  tags$div(
                    class = "repo-header",
                    style = "padding: 16px; cursor: pointer; border-bottom: 1px solid #e1e4e8;",
                    onclick = paste0("$('#", repo_id, "').collapse('toggle')"),
                    tags$div(
                      style = "display: flex; justify-content: space-between; align-items: center;",
                      tags$div(
                        style = "display: flex; align-items: center; gap: 8px;",
                        tags$h3(
                          style = "font-size: 16px; font-weight: 600; margin: 0; color: #0969da;",
                          repo$name
                        ),
                        tags$div(
                          style = "display: flex; align-items: center; gap: 4px; color: #57606a;",
                          icon("star"),
                          tags$span(repo$stars),
                          if(repo$is_fork) {
                            tags$span(
                              style = paste(
                                "font-size: 0.75em;",
                                "background: #f0f0f0;",
                                "border-radius: 12px;",
                                "padding: 2px 8px;",
                                "margin-left: 6px;",
                                "color: #586069;"
                              ),
                              "Fork"
                            )
                          }
                        )
                      )
                    )
                  ),
                  tags$div(
                    id = repo_id,
                    class = "collapse show",
                    tags$div(
                      style = "padding: 16px;",
                      tags$p(
                        style = "color: #57606a; font-size: 14px; margin: 0 0 16px 0; min-height: 40px;",
                        repo$description
                      ),
                      tags$div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        tags$div(
                          style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
                          # Блок языка
                          if(repo$language != "Не указан") {
                            tags$span(
                              style = "display: flex; align-items: center; gap: 4px;",
                              tags$span(style = paste(
                                "width: 12px; height: 12px; border-radius: 50%;",
                                "background:", switch(repo$language,
                                                      "R" = "#276DC3",
                                                      "Python" = "#3572A5",
                                                      "JavaScript" = "#F1E05A",
                                                      "Java" = "#B07219",
                                                      "C#" = "#178600",
                                                      "C++" = "#F34B7D",
                                                      "PHP" = "#4F5D95",
                                                      "Swift" = "#FFAC45",
                                                      "Kotlin" = "#A97BFF",
                                                      "Go" = "#00ADD8",
                                                      "Rust" = "#DEA584",
                                                      "TypeScript" = "#3178C6",
                                                      "Ruby" = "#701516",
                                                      "SQL" = "#E38C00",
                                                      "Dart" = "#00B4AB",
                                                      "Scala" = "#DC322F",
                                                      "Perl" = "#39457E",
                                                      "Haskell" = "#5E5086",
                                                      "Lua" = "#000080",
                                                      "MATLAB" = "#E16737",
                                                      "Shell" = "#89E051",
                                                      "#000000" # Дефолтный цвет
                                )
                              )),
                              tags$span(style = "font-size: 12px; color: #57606a;", repo$language)
                            )
                          },
                          tags$div(
                            style = "display: flex; align-items: center; gap: 8px;",
                            # Форки
                            tags$span(
                              style = "display: flex; align-items: center; gap: 4px;",
                              icon("code-branch", style = "font-size: 0.9em;"),
                              tags$span(
                                style = "font-size: 12px; color: #57606a;",
                                repo$forks
                              )
                            ),
                            
                            # Issues
                            tags$span(
                              style = "display: flex; align-items: center; gap: 4px;",
                              icon("exclamation-circle", style = "font-size: 0.9em; color: #d73a49;"),
                              tags$span(
                                style = "font-size: 12px; color: #57606a;",
                                repo$open_issues
                              )
                            )
                          )
                        ),
                        tags$span(
                          style = "font-size: 12px; color: #57606a;",
                          format(as.Date(repo$updated_at), "%d.%m.%Y")
                        )
                      )
                    )
                  )
                )
              )
            })
          ),
          
          # Обновляем JavaScript:
          tags$script(HTML("
            $(document).on('click', '#toggle_all_repos', function() {
              let button = $(this);
              let isHidden = button.text().includes('Развернуть');
              $('.repo-item').stop(true).fadeToggle(300, 'swing', function() {
                if($(this).css('opacity') == 0) {
                  $(this).css('display', 'none');
                }
              });
              button.html(isHidden ? 
                'Свернуть' : 
                'Развернуть');
            });
          
            $(document).on('click', '.repo-header', function() {
              $(this).closest('.repo-item').stop(true).fadeToggle(300);
            });
          ")),
          
          # Добавляем CSS:
          tags$style(HTML("
            .repo-item {
              opacity: 1;
              transition: opacity 0.3s ease, transform 0.3s ease;
            }
            .repo-item.hidden {
              opacity: 0;
              transform: scale(0.9);
              pointer-events: none;
              display: none !important;
            }
          "))
        )
      ),
      
      # Секция с визуализациями
      tags$div(
        style = "max-width: 1012px; margin: 0 auto; padding: 32px 16px;",
        
        # Графики активности
        tags$div(
          style = "margin-bottom: 40px;",
          tags$h2("📈 Активность", style = "font-size: 24px; border-bottom: 1px solid #eee; padding-bottom: 8px;"),
          withSpinner(plotlyOutput("activity_plot", height = "400px"))
        ),
        
        # Языки программирования
        tags$div(
          style = "margin-bottom: 40px;",
          tags$h2("📚 Языки программирования", style = "font-size: 24px; border-bottom: 1px solid #eee; padding-bottom: 8px;"),
          withSpinner(plotlyOutput("language_plot", height = "400px"))
        ),
        
        # Тепловая карта
        tags$div(
          style = "margin-bottom: 40px;",
          tags$h2("🌡️ Тепловая карта коммитов", style = "font-size: 24px; border-bottom: 1px solid #eee; padding-bottom: 8px;"),
          withSpinner(plotlyOutput("commit_heatmap", height = "400px"))
        ),
        
        # Анализ PCA
        tags$div(
          style = "margin-bottom: 40px;",
          tags$h2("🔍 Анализ аномалий (PCA)", style = "font-size: 24px; border-bottom: 1px solid #eee; padding-bottom: 8px;"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
            withSpinner(plotlyOutput("pca_plot", height = "500px")),
            withSpinner(uiOutput("outlier_cards"))
          )
        ),
        
        # Таблица коммитов
        tags$div(
          style = "margin-bottom: 40px;",
          tags$h2("📄 История коммитов", style = "font-size: 24px; border-bottom: 1px solid #eee; padding-bottom: 8px;"),
          withSpinner(dataTableOutput("commits_table"))
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
      
      datatable(
        commits,
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(targets = which(names(commits) == "patch") - 1,
                 render = JS(
                   "function(data, type, row, meta) {",
                   "  return type === 'display' && data.length > 50 ?",
                   "    '<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                   "}")
            )
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
        response <- POST(
          url = "https://llm.api.cloud.yandex.net/foundationModels/v1/completion",
          add_headers(
            "Content-Type" = "application/json",
            "Authorization" = paste("Api-Key", YANDEX_API_KEY)
          ),
          body = toJSON(prompt, auto_unbox = TRUE, pretty = TRUE),
          encode = "json"
        )
        
        if (status_code(response) != 200) {
          return(paste("ERROR:", status_code(response)))
        }
        
        # Парсинг и валидация JSON
        response_content <- content(response, "text", encoding = "UTF-8")
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
