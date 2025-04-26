library(shiny)
library(shinycssloaders)
library(plotly)
library(shinyjs)
library(shinyBS)

texts <- list(
  title = "Анализ GitHub пользователя",
  user_input_label = "Введите имя пользователя GitHub:",
  user_input_placeholder = "Например, i2z1",
  submit_button = "Анализировать",
  toggle_filters_button = "Показать фильтры",
  language_filter_label = "Язык программирования:",
  stars_filter_label = "Количество звёзд:",
  issues_filter_label = "Количество открытых issues:",
  contributors_filter_label = "Количество участников:",
  size_filter_label = "Размер репозитория (MB):",
  license_filter_label = "Лицензия:",
  created_date_label = "Дата создания:",
  updated_date_label = "Дата последнего обновления:"
)

languages <- c("Все", "Python", "JavaScript", "R", "HTML", "Другой")
licenses <- c("Все", "MIT", "Apache 2.0", "GPL", "Другая")

ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
    $(document).on('keyup', '#user_input', function(e) {
      if(e.keyCode == 13) {
        $('#submit_button').click();
      }
    });
  ")),
  titlePanel(texts$title),

  sidebarPanel(
    # Поле ввода пользователя
    textInput("user_input", texts$user_input_label, placeholder = texts$user_input_placeholder),

    # Контейнер для кнопок
    tags$div(
      class = "button-container",
      actionButton("submit_button", icon = icon("search"), texts$submit_button, class = "submit-button", style = "background-color: #0366d6; color: white;"),
      actionButton("toggle_filters", icon = icon("filter"), texts$toggle_filters_button, class = "toggle-button", style = "background-color: #28a745; color: white;")
    ),

    # Контейнер для фильтров
    tags$div(
      id = "filters",
      style = "margin-top: 20px;",

      # Группа фильтров по датам
      tags$div(
        class = "filter-group",
        style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; margin-bottom: 16px;",
        tags$h4(icon("calendar"), " Фильтры по датам", style = "margin-top: 0;"),

        dateRangeInput(
          "created_date_range",
          label = tags$span(icon("plus-circle"), texts$created_date_label),
          start = Sys.Date() - 365,
          end = Sys.Date(),
          format = "dd.mm.yyyy",
          language = "ru"
        ),
        dateRangeInput(
          "updated_date_range",
          label = tags$span(icon("sync"), texts$updated_date_label),
          start = Sys.Date() - 365,
          end = Sys.Date(),
          format = "dd.mm.yyyy",
          language = "ru"
        )
      ),

      # Группа фильтров по языкам и лицензиям
      tags$div(
        class = "filter-group",
        style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; margin-bottom: 16px;",
        tags$h4(icon("code"), " Фильтры по языкам и лицензиям", style = "margin-top: 0;"),

        selectInput(
          "language_filter",
          label = tags$span(icon("file-code"), texts$language_filter_label),
          choices = languages,
          selectize = TRUE
        ),
        selectInput(
          "license_filter",
          label = tags$span(icon("balance-scale"), texts$license_filter_label),
          choices = licenses,
          selectize = TRUE
        )
      ),

      # Группа фильтров по метрикам
      tags$div(
        class = "filter-group",
        style = "border: 1px solid #ddd; border-radius: 8px; padding: 12px; margin-bottom: 16px;",
        tags$h4(icon("chart-bar"), " Фильтры по метрикам", style = "margin-top: 0;"),

        sliderInput(
          "stars_filter",
          label = tags$span(icon("star"), texts$stars_filter_label),
          min = 0,
          max = 100,
          value = c(0, 100)
        ),
        sliderInput(
          "issues_filter",
          label = tags$span(icon("exclamation-circle"), texts$issues_filter_label),
          min = 0,
          max = 10,
          value = c(0, 10)
        ),
        sliderInput(
          "contributors_filter",
          label = tags$span(icon("users"), texts$contributors_filter_label),
          min = 0,
          max = 10,
          value = c(0, 10)
        ),
        sliderInput(
          "size_filter",
          label = tags$span(icon("weight"), texts$size_filter_label),
          min = 0,
          max = 1000,
          value = c(0, 1000)
        )
      )
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Отчет", icon = icon("user"), withSpinner(uiOutput("user_report"))),
      tabPanel("Репозитории", icon = icon("folder"), withSpinner(uiOutput("repo_info"))),
      tabPanel("Коммиты", icon = icon("code"), withSpinner(dataTableOutput("commits_table"))),
      tabPanel("События", icon = icon("chart-line"), withSpinner(plotlyOutput("activity_plot"))),
      tabPanel("Языки", icon = icon("language"), withSpinner(plotlyOutput("language_plot"))),
      tabPanel("Активность", icon = icon("calendar"), withSpinner(plotlyOutput("commit_heatmap"))),
      tabPanel("МГК", icon = icon("project-diagram"), withSpinner(plotlyOutput("pca_plot")), uiOutput("pca_outliers"))
    )
  )
)
