

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com


library(shiny)

shinyUI(fluidPage(
  
  navbarPage("Temporal Mining",
             tabPanel("Interpolation",
                 
                      #Interpolation page components   
                      
                      # Application title
                      titlePanel("Interpolation Method"),
                      
                      # Sidebar with file upload
                      sidebarLayout(
                        sidebarPanel(
                          fileInput(
                            'file1', 'Choose CSV File',
                            accept = c('text/csv',
                                       'text/comma-separated-values,text/plain',
                                       '.csv')
                          ),
                          tags$hr(),
                          checkboxInput('header', 'Header', TRUE),
                          radioButtons('sep', 'Separator',
                                       c(
                                         Comma = ',',
                                         Semicolon = ';',
                                         Tab = '\t'
                                       ),
                                       ','),
                          radioButtons(
                            'quote', 'Quote',
                            c(
                              None = '',
                              'Double Quote' = '"',
                              'Single Quote' = "'"
                            ),
                            '"'
                          ),
                          class = 'leftAlign'
                        ),
                        
                        
                        
                        mainPanel(
                          radioButtons(
                            'method', 'Interpolation Methods:',
                            c(
                              'None' = '"',
                              'MICE(pmm) Interpolation' = 'mice.pmm',
                              'MICE(norm) Interpolation' = 'mice.norm',
                              'MICE(mean) Interpolation' = 'mice.mean',
                              'EOF Interpolation' = 'eof'
                            ),
                            '"'
                          ),
                          # Only show this panel if data is uploaded
                          br(),
                          h5("NOTE: Please unselect all non-numeric columns!", style = "color:blue"), br(),
                          downloadButton('downloadOriginalData', 'Download Original'),
                          downloadButton('downloadInterpolatedData', 'Download Interpolated'),
                          uiOutput("choose_columns"),
                          
                          uiOutput("errorPlots"),
                          
                          uiOutput("errorPlot"),
                          
                          tabsetPanel(
                            type = "tabs",
                            tabPanel("Original Data", tableOutput("table")),
                            tabPanel("Data with NAs", tableOutput("missing_table")),
                            tabPanel("Interpolated Data", tableOutput("intp_table")), 
                            tabPanel("Interpolated Data", tableOutput("sub_table"))
                          )
                        )
                        
                      )
                      
                      ),
             
             #satscan page components
             tabPanel("SaTScan")
  )
  
  
))
