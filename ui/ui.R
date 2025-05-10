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
  tags$head(
    tags$script(HTML("
    $(document).on('shiny:connected', function() {
      // Добавляем выпадающий список для выбора столбца поиска
      var columnSelect = $('<select id=\"columnSelect\" class=\"form-control\" style=\"display: inline-block; width: auto; margin-left: 10px;\">' +
                           '<option value=\"all\">Все столбцы</option>' +
                           '<option value=\"0\">ID</option>' +
                           '<option value=\"1\">Patch</option>' +
                           '<option value=\"2\">Репозиторий</option>' +
                           '<option value=\"3\">Автор</option>' +
                           '<option value=\"4\">Дата</option>' +
                           '<option value=\"5\">Файл</option>' +
                           '<option value=\"6\">Статус</option>' +
                           '<option value=\"7\">Добавления</option>' +
                           '<option value=\"8\">Удаления</option>' +
                           '<option value=\"9\">Изменения</option>' +
                           '<option value=\"10\">Сообщение</option>' +
                           '<option value=\"11\">Ветка</option>' +
                           '</select>');

      // Вставляем выпадающий список рядом с поиском
      $('.dataTables_filter').prepend(columnSelect);

      // Обработчик изменения выбора столбца
      $('#columnSelect').on('change', function() {
        var table = $('.dataTable').DataTable();
        var col = $(this).val();

        if (col === 'all') {
          table.columns().search('').draw();
          $('.dataTables_filter input').off('keyup search');
          $('.dataTables_filter input').on('keyup search', function() {
            table.search(this.value).draw();
          });
        } else {
          $('.dataTables_filter input').off('keyup search');
          $('.dataTables_filter input').on('keyup search', function() {
            table.column(col).search(this.value).draw();
          });
        }
      });
    });
  "))
  ),

  useShinyjs(),
  titlePanel(texts$title),
  tags$script(HTML("
    $(document).on('keyup', '#user_input', function(e) {
      if(e.keyCode == 13) {
        $('#submit_button').click();
      }
    });
  ")),

  sidebarLayout(
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
            start = "2008-04-10", # Дата начала работы GitHub
            end = Sys.Date() + 1,
            format = "dd.mm.yyyy",
            language = "ru"
          ),
          dateRangeInput(
            "updated_date_range",
            label = tags$span(icon("sync"), texts$updated_date_label),
            start = Sys.Date() - 365,
            end = Sys.Date() + 1,
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
        id = "mainTabs",
        type = "tabs",
        tabPanel("Главное",
                 tags$div(
                   style = "display: flex; flex-wrap: wrap; background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                   actionLink("tab_report", "Отчет", icon = icon("user"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_commits", "Коммиты", icon = icon("code"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_events", "События", icon = icon("chart-line"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_languages", "Языки", icon = icon("language"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_activity", "Активность", icon = icon("calendar"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_pca", "МГК", icon = icon("project-diagram"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;")
                 ),

                 uiOutput("main_content")
        ),
        tabPanel("Настройки",
                 tags$div(
                   style = "display: flex; flex-wrap: wrap; background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                   actionLink("tab_env_vars", "Переменные", icon = icon("cog"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;"),
                   actionLink("tab_other_settings", "Другие настройки", icon = icon("sliders-h"),
                              style = "padding: 10px 15px; margin: 5px; border-radius: 4px; background: #fff; border: 1px solid #ddd;")
                 ),

                 uiOutput("settings_content")
        )
      )
    )
  )
)
