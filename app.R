library(shiny)

source("ui/ui.R")
source("server/server.R")

shinyApp(ui = ui, server = server)