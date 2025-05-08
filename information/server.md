# Реализованные функции server

## **filter_repos**
```R
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
```
Функция фильтрует список репозиториев GitHub по комплексным критериям, включая метаданные, активность и технические характеристики.

Особенности: 
- Поддерживает специальное значение "Все" для language и license
- Автоматически конвертирует размер из KB в MB
- Проверяет вхождение в диапазоны (включительно)
- Работает с частичными данными (если какие-то поля отсутствуют в репозитории)

Входные данные: 
- `repos` - список репозиториев, где каждый элемент содержит:
   - `language` - основной язык программирования
   - `stars` - количество звёзд
   - `created_at` - дата создания (формат, конвертируемый в Date)
   - `updated_at` - дата последнего обновления
   - `open_issues` - количество открытых issues
   - `contributors` - количество контрибьюторов
   - `size` - размер репозитория в KB
   - `license` - тип лицензии
- `filters` - список критериев фильтрации:
   - `language` - язык ("Все" или конкретный язык)
   - `stars` - вектор из 2 чисел: min и max звёзд
   - `created_date_range` - вектор из 2 дат: min и max дата создания
   - `updated_date_range` - вектор из 2 дат: min и max дата обновления
   - `issues` - вектор из 2 чисел: min и max открытых issues
   - `contributors` - вектор из 2 чисел: min и max контрибьюторов
   - `size` - вектор из 2 чисел: min и max размер в MB
   - `license` - лицензия ("Все" или конкретная лицензия)

Выходные данные:
- отфильтрованный список репозиториев, соответствующих всем критериям
- пустой список, если ни один репозиторий не соответствует условиям

## **server**
```R
server <- function(input, output, session) {}
```
Shiny Server Module для анализа GitHub активности. Этот модуль реализует серверную логику приложения для анализа GitHub активности,
включая визуализацию репозиториев, коммитов и обнаружение аномалий.

Функциональность:
- Управление вкладками и настройками
- Загрузка и обработка данных GitHub
- Визуализация активности и языков программирования
- Анализ главных компонент (PCA) для коммитов
- Обнаружение аномальных коммитов
- Работа с переменными окружения

Основные компоненты:
- Вкладки - управление переключением между разделами приложения
- Данные - реактивные значения для хранения загруженных данных
- Фильтры - обработка и применение фильтров к репозиториям
- Визуализации - генерация графиков и таблиц
- Настройки - управление переменными окружения

Входные данные: 
- `input` - входные параметры Shiny
- `output` - выходные элементы Shiny
- `session` - сессия Shiny