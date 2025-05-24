library(shiny)
library(futile.logger)

source("ui/ui.R")
source("server/server.R")

flog.threshold(INFO)
flog.appender(appender.file("log.log"))
flog.layout(layout.format('~t [~l] ~m'))

shinyApp(ui = ui, server = server)
