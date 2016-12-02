library(shinydashboard)
library(shiny)
library(data.table)
library(rhandsontable)
library(traittools)
library(sbformula)
library(openxlsx)
library(shinyFiles)
library(date)
library(agricolae)
library(doBy)
library(readxl)
library(shinyTree)
library(fbdesign)
tabNameS <- "data_processing"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbcheck::fbcheck_server(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Data Processing"),
                    dashboardSidebar(width = 350,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Data Processing", icon = icon("location-arrow"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        fbcheck::fbcheck_ui(name = tabNameS)
                      )
                    )
)

shinyApp(ui = ui, server = server)


