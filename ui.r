require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)
require(plotly)


### SHINY UI ###
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Project Intro to Ds</a>'), id="nav",
             windowTitle = "Data Analysis Project",
             
             tabPanel("Welcome",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),     
                          
                      ),
                      tags$img(src = "background.png", width = "100%", height = "100%")
                      
             ),
             
             tabPanel("Processing",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(condition = "input.tabsetpanel == 'Import'",
                                           span(tags$i(h5("Upload Your csv file")), style="color:#045a8d"),

                                           # Input: Select a file ----
                                           fileInput("file1", "Choose CSV File",
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")),
                                           span(tags$i(h5("include header display")), style="color:#045a8d"),
                                           
                                           # Input: Checkbox if file has header ----
                                           checkboxInput("header", "Header", TRUE),
                                           
                                           span(tags$i(h5("Choose the type of dataset cause it is important for the next operations")), style="color:#045a8d"),
                                           
                                           pickerInput("type_dataset", "Type of dataset:",   
                                                       choices = c("X_movement", "O_movement", "Noise"), 
                                                       selected = c("X_movement"),
                                                       multiple = FALSE),
                                           
                                           # Input: Select number of rows to display ----
                                           radioButtons("disp", "Display",
                                                        choices = c(Head = "head",
                                                                    All = "all"),inline=TRUE,
                                                        selected = "head"),
                                           
                                           "You can display the header or the whole dataframe"
                                           
                          ),
                          
                          conditionalPanel(condition = "input.tabsetpanel == 'view' ",
                                           
                                           pickerInput("column", "Column to display:",   
                                                       choices = c("all","time", "gFx", "gFy","gFz","ax","ay","az","wx","wy","wz"), 
                                                       selected = c("all"),
                                                       multiple = FALSE),
                                           
                                           
                                           # Horizontal line ----
                                           tags$hr(),
                                           
                                           # Input: Checkbox if file has header ----
                                           checkboxInput("filter", "Filter", TRUE),
                                           
                                           conditionalPanel(condition= "input.filter == 1",
                                                            textAreaInput("filter_text", "Filter", width = "1000px",placeholder = "Provide a Filter (e.g., price>5000)"),
                                           ),
                                           
                                           
                                           "Provide a filtering condition to enhance your search in the dataset"
                                           
                          ),
                          
                          conditionalPanel(condition = "input.tabsetpanel == 'Visualisation' ",
                                           span(tags$i(h5("Choose the type of sensor for the 3d graph")), style="color:#045a8d"),

                                           #
                                           pickerInput("sensor", "Sensor ",   
                                                       choices = c("G-ForceMeter","Accelerometer", "Gyroscope"), 
                                                       selected = c("G-ForceMeter"),
                                                       multiple = FALSE),
                                           # Horizontal line ----
                                           tags$hr(),
                                           
                                           "You can make some geometrical operations on the 3d graph (rotation,zoom ....)"
                                           
                          ),
                          conditionalPanel(condition = "input.tabsetpanel == 'Processing' ",
                                           span(tags$i(h5("Choose the type of extraction and don't forget to provide the different threesholds for the processing")), style="color:#045a8d"),
                                           
                                           #
                                           pickerInput("feature", "Feature",   
                                                       choices = c("Long","Large"), 
                                                       selected = c("Long"),
                                                       multiple = FALSE),
                                           # Horizontal line ----
                                           tags$hr(),
                                           textInput("threeshold_mean", label = h6("Threeshold_mean"), placeholder ="Enter Value" ),
                                           textInput("threeshold_sd", label = h6("Threeshold_sd"),  placeholder ="Enter Value"),
                                           textInput("threeshold_time", label = h6("Threeshold_time"),  placeholder ="Enter Value"),
                                           tags$hr(),
                                           radioButtons("sensor_2", label = h6("Sensor 3d Visualisation for x_long"), 
                                                        choices = list("G-ForceMeter" = 1, "Accelerometer" = 2, "Gyroscope" = 3),
                                                        selected = 1),
                                           tags$hr(),
                                           actionButton("process", label = "Process"),
                                           
                                           
                          ),
                          
                          
                        ),
                        
                        mainPanel(
                          tabsetPanel(id="tabsetpanel",
                                      tabPanel("Import", 
                                               tags$br(),
                                               h4("Observations"),
                                               tags$br(),
                                               tableOutput("contents"),
                                               tags$br(),
                                               h4("Summary"),
                                               verbatimTextOutput("summary"),
                                               tags$br(),
                                               h4("Description"),
                                               textOutput("description"),
                                               tags$br(),
                                               h4("Variables"),
                                               tags$li("gFx = X-axis of the G-Force sensor"),
                                               tags$li("gFy = y-axis of the G-Force sensor"),
                                               tags$li("gFz = z-axis of the G-Force sensor"),
                                               tags$li("ax = X-axis of the Accelerometer sensor"),
                                               tags$li("ay = y-axis of the Accelerometer sensor"),
                                               tags$li("az = z-axis of the Accelerometer sensor"),
                                               tags$li("wx = X-axis of the Gyroscope sensor"),
                                               tags$li("wy = Y-axis of the Gyroscope sensor"),
                                               tags$li("wz = Z-axis of the Gyroscope sensor"),
                                                     
                                      ),
                                      tabPanel("view", DT::dataTableOutput("mytable1")),
                                      tabPanel("Visualisation", plotlyOutput("visualisation")),
                                      tabPanel("Processing", 
                                               conditionalPanel(condition = "input.process>=1",
                                                                h4("Result"),
                                                                verbatimTextOutput("str"),
                                                                h4("ggplot"),
                                                                plotOutput("ggplot"),
                                                                h4("3D Visualisation"),
                                                                plotlyOutput("processing_visualisation"),
                                                                
                                               )
                                               
                                      ),
                                      tabPanel("Download",
                                               tags$br(),
                                               h4("You have to enter the group names and the lables for non null pid"),
                                               tags$br(),
                                               h4("groups"),
                                               textInput("group_1", label = h6("Group 1"), placeholder ="Enter Value" ),
                                               textInput("group_2", label = h6("Group_2"),  placeholder ="Enter Value"),
                                               tags$br(),
                                               h4("Non null Pid Labels (3 Max)"),
                                               textInput("pid_1", label = h6("Label 1"), placeholder ="Enter Value" ),
                                               textInput("pid_2", label = h6("Label 2"),  placeholder ="Enter Value"),
                                               textInput("pid_3", label = h6("Label 3"),  placeholder ="Enter Value"),
                                               tags$br(),
                                               downloadButton("download_data", label = "Download"),
                                      )
                                      
                                      
                          )
                        )
                      )
             ),
             
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Context"), 
                        "This project is part of an academic project at", 
                        tags$a(href="https://www.univ-gustave-eiffel.fr/","Gustave Eiffel University"),
                        "Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
                        
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                        tags$br(),tags$br(),tags$h4("Dedication"),
                        tags$b("We express our gratitute to Mr Etienne Come , Professor at Gustave Eiffel University, for the supervision and all the support: "), tags$a(href="https://www.ifsttar.fr/menu-haut/annuaire/fiche-personnelle/personne/come-etienne/", "profile,"),
                        
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Romuald Motcheho Kamguia, Souhir Arous, Ayoub Bouallagi",tags$br(),
                        
                      )
             )
             
  )          
)
