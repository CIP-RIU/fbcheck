#' UI for traittools
#' Returns user friendly ui
#' @author Omar Benites
#' @param type type of UI element, deault is a tab in a shinydashboard
#' @param title display title name
#' @param name UI TabName
#' @export

fbcheck_ui <- function (type = "tab", title = "Data Quality and Processing", 
                        name = "data_processing") 
{
  shinydashboard::tabItem(tabName = name, 
                          h2(title),
                          box(title = " ", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                                         width = NULL, 
                              
                              shinyWidgets::awesomeRadio(inputId = "fbcheck_type_Import", 
                                                         label = "Radio buttons", choices = c("HIDAP", "FieldBookApp-SPBase"),
                                                         selected = "HIDAP", inline = TRUE, checkbox = TRUE),
                              
                              conditionalPanel(condition = "input.fbcheck_type_Import == 'HIDAP'",
                              
                                            try(shinyFiles::shinyFilesButton("file","File select", "Please select a file", FALSE)),
                                            shiny::actionButton("calculate","Calculate", icon("play-circle-o")),
                                            HTML("<div style=\"float: right; margin: 0 5px 5px 10px;\">"),
                                            shiny::actionLink("exportButton", "Download"),
                                            HTML("</div>")#,
                             ),
                             
                             conditionalPanel(condition = "input.fbcheck_type_Import == 'FieldBookApp-SPBase'",
                             
                                              shiny::fileInput(inputId = "file_fbapp_sbase", label = "Choose CSV File", multiple = FALSE,
                                                               accept = c("text/csv","text/comma-separated-values,text/plain", ".csv")),
                                              selectInput(inputId = "fbcheck_fbapp_ExportFormat",label = "Spreadsheet format download",choices = c("Simple","Standard"), selected = "Standard"), 
                                              
                                              #actionButton('reset', 'Reset Input'),
                                              
                                              HTML('<div style="float: right; margin: 0 6px 6px 11px;">'),
                                              shiny::downloadLink('downloadData', 'Download FieldBookApp file'),
                                              HTML('</div>')         
                                              
                             
                             ), 
                              
                                                         br(), 
                                                        br(), 
                              tabBox(
                                         width = 12,
                                          tabPanel("Standard Modules",
                                                             
                                                             # try(shinyFiles::shinyFilesButton("file","File select", "Please select a file", FALSE)), 
                                                             # shiny::actionButton("calculate","Calculate", icon("play-circle-o")), 
                                                             # HTML("<div style=\"float: right; margin: 0 5px 5px 10px;\">"),
                                                             # shiny::actionLink("exportButton", "Download data"), 
                                                             # HTML("</div>"),
                                                             
                                                             
                                                      conditionalPanel(condition = "input.fbcheck_type_Import == 'HIDAP'",
                                                             
                                                             uiOutput("fbcheck_genofilter"),
                                                             uiOutput("fbcheck_factorfilter"),
                                                             shinysky::shinyalert("alert_fb_warning", FALSE, auto.close.after = 4),
                                                             box(
                                                               rHandsontableOutput("hot_btable", height = "1400px",
                                                                                   width = "1000px"),
                                                               height = "3400px",
                                                               width = "2400px"
                                                             ),
                                                             tags$style(type = "text/css", "#file { width:150px; margin-top: 25px;}"),
                                                             tags$style(HTML(
                                                               "#file {background-color:#0099cc; color: #ffffff}"
                                                             )),
                                                             tags$style(type = "text/css", "#calculate { width:150px; margin-top: 25px;}"),
                                                             tags$style(HTML(
                                                               "#calculate {background-color:#21b073; color: #ffffff}"
                                                             ))
                                                     ),
                                                     conditionalPanel(condition = "input.fbcheck_type_Import == 'FieldBookApp-SPBase'",  
                                                                      shinysky::shinyalert("alert_fbapp_warning_sbase", FALSE, auto.close.after = 4),          
                                                                      box(rHandsontableOutput("hot_btable_fbapp_sbase",height = "100%",width = "100%"),
                                                                          height = "3400px",width ="2400px")
                                                                        
                                                     )       
                                                             
                                                           ),
                                        tabPanel("Special Modules", fluidRow(
                                                             shinydashboard::tabBox(
                                                               title = "PVS",
                                                               id = "tabset1",
                                                               width = "2400px",
                                                               tabPanel(
                                                                 "Sel_Criteria",
                                                                 "Selection Criteria",
                                                                 br(),
                                                                 shinyTree::shinyTree(
                                                                   "fbcheckSelect_criteria",
                                                                   search = TRUE,
                                                                   checkbox = TRUE
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_1",
                                                                 "Selection Criteria",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f1_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "3400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_2",
                                                                 "Select Clones at Flowering Stage",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f2_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_3",
                                                                 "Select Clones at Harvest Stage",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f3_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_4",
                                                                 "Harvest Mother",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f4_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_5",
                                                                 "Harvest Baby",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f5_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_6",
                                                                 "Organoleptic_mother",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f6_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_7",
                                                                 "Organoleptic_baby",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f7_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_8",
                                                                 "Dormancy",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f8_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               ),
                                                               tabPanel(
                                                                 "Form_9",
                                                                 "Storage",
                                                                 shinydashboard::box(
                                                                   rHandsontableOutput("hot_f9_btable",
                                                                                       height = "1400px", width = "1400px"),
                                                                   height = "3400px",
                                                                   width = "2400px"
                                                                 )
                                                               )
                                                             )
                                                           ))#,
                                       
                                        
                                        
                                                         )), br(), br(), br()
        )
}
