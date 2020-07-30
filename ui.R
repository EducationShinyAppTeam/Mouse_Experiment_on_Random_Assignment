library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(boastUtils)
library(DT)

##Mouse app

ui <- list(
  tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css"
      )
  ),
  dashboardPage(
      skin = "red",
      dashboardHeader(
          title = "Random Assignment",
          titleWidth = 250,
          tags$li(class = "dropdown", actionLink("info", icon("info"))),
          tags$li(class = "dropdown",
                tags$a(href='https://shinyapps.science.psu.edu/',
                       icon("home")))
      ),
      dashboardSidebar(
          width = 250,
          sidebarMenu(
            id = "tabs",
            menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem(text = "Hand Selection", tabName = "hand", icon = icon("wpexplorer")),
            menuItem(text = "Hand-Selected Data Exploration", tabName = "summary", icon = icon("wpexplorer")),
            menuItem(text = "Computer Generated Simulation", tabName = "computer", icon = icon("wpexplorer")),
            menuItem(text = "References", tabName = "references", icon = icon("leanpub"))
          ),
          tags$div(
            class = "sidebar-logo",
            boastUtils::psu_eberly_logo("reversed")
          )
      ),
      dashboardBody(
          tabItems(
          tabItem(
            tabName = "overview",
            h1("Mouse Experiment on Random Assignment"),
            p("This app is designed to help you see the benefit in terms of 
            reduced bias when random assignment is used instead of haphazard 
            assignment to treatment groups. The app also demonstrates the
            distribution of means and proportions when random assignment 
            is used."),
            p("Raspberries have a high content of many beneficial compounds
                like vitamins C and E, folic and ellagic acid, calcium, selenium, 
                etc. As a result, researchers have recently been investigating their 
                anti-cancer properties. All of the twenty mice in the picture have a 
                tumor growing just under the skin on their backs. To test if raspberries 
                can help reduce the growth of these tumors, ten mice will be chosen to 
                have raspberries added in their diet and the remaining ten will 
                eat a normal diet without the raspberries."),
            h2("Instructions"), 
            tags$ul(
              tags$li("Please pick the ten mice that will receive the raspberry diet 
                    (quickly click on mice you want to include in the raspberry group 
                    until you have selected ten, then click the submit button)."),
              tags$li("If you select more than 10 mice, please click RESET to select again."),
              tags$li("Explore the data from the ten mice that you selected."),
              tags$li("Click RAW DATA to copy the data if you need to do further analysis."),
              tags$li("Click 'Compare with Random Selection' to explore computer generated data using random selection."),
              tags$li(strong("Note:"), "This app is intended to be used in full screen mode.")
            ),
            div(
              style = "text-align: center",
              bsButton(
                inputId = "go",
                label = "Go",
                icon = icon("bolt"),
                size = "large"
              )
            ),
          br(),
          h2("Acknowledgements"),
          p("This app was oringinally developed and coded by Yuxin Zhang and updated by Luxin 
            Wang and Thomas McIntyre based on extending the idea by Dennis Pearl and Tom Santner. 
            The current version of the app was modified by Chenese Gray."),
          div(class = "updated", "Last Update: 07/16/20 by C.G.")
          ),
          tabItem(
            tabName = "hand",
              #Use jscode to for reset button to reload the app
              useShinyjs(),
              h2("Hand Selection"),
              p("Below are 20 mice. Please select any 10 of them by clicking on them.
                Those that you select will be highlighted with  raspberry coloring."),
              br(),
              #Display all the mice in main panel
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = "btn1",
                    type='action',
                    label = tags$img(src = 'brown3.png',width = 82, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn18",
                    label = tags$img(src='brown3.png', width = 142, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn3",
                    label = tags$img(src='black3.png', width = 90, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn4",
                    label = tags$img(src='black3.png', width = 90, style = info, alt = "Black mouse.")
                  )
                )
              ), br(),br(),
              fluidRow(
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn6",
                    label = tags$img(src='brown3.png', width = 97, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 3,
                  offset = 1,
                  bsButton(
                    inputId = "btn20",
                    label = tags$img(src='black3.png', width = 162, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 3,
                  offset = 1,
                  bsButton(
                    inputId = "btn5",
                    label = tags$img(src='brown3.png', width = 95, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn8",
                    label = tags$img(src='black3.png', width = 92, style = info, alt = "Black mouse.")
                  )
                )
              ), br(),br(),
              fluidRow(
                column(
                  width =2,
                  offset = 1,
                  bsButton(
                    inputId = "btn10",
                    label = tags$img(src='black3.png', width = 101, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 3,
                  offset = 1,
                  bsButton(
                    inputId = "btn7",
                    label = tags$img(src='brown3.png', width = 101, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn11",
                    label = tags$img(src='brown3.png', width = 102, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn12",
                    label = tags$img(src='brown3.png', width = 113, style = info, alt = "Brown mouse.")
                  )
                )
              ), br(),br(),
              
              fluidRow(
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn13",
                    label = tags$img(src='black3.png', width = 122, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn14",
                    label = tags$img(src='brown3.png', width = 124, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn15",
                    label = tags$img(src='brown3.png', width = 126, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  width = 3,
                  bsButton(
                    inputId = "btn16",
                    label = tags$img(src='black3.png', width = 126, style = info, alt = "Black mouse.")
                  )
                ),
              ), br(),br(),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = "btn17",
                    label = tags$img(src='brown3.png', width = 127, style = info, alt = "Brown mouse.")
                  )
                ),
                column(
                  wdith = 2,
                  offset = 1,
                  bsButton(
                    inputId = "btn19",
                    label = tags$img(src='black3.png', width = 154, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 3,
                  offset = 1,
                  bsButton(
                    inputId = "btn9",
                    label = tags$img(src='black3.png', width = 101, style = info, alt = "Black mouse.")
                  )
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "btn2",
                    label = tags$img(src='brown3.png', width = 84, style = info, alt = "Brown mouse.")
                  )
                )
              ),
              br(),hr(),
                uiOutput("num"),
              fluidRow(
                column(
                  width = 2,
                  offset = 1,
                  bsButton(
                    inputId = "reset_button",
                    label = "Reset",
                    size = "large"
                  )
                ),
                bsPopover(
                  id = "reset_button",
                  title = "Notice",
                  content = "Click Reset to start again.",
                  placement = "top"
                ),
                column(
                  width = 2,
                  bsButton(
                    inputId = "submit",
                    label = "Submit Selection",
                    size = "large",
                    disabled = TRUE
                  )
                )
              )
          ),
          tabItem(
            tabName = "summary",
              h2("Summary of Hand-Selected  for Comparison"),
              br(),br(),br(),
              conditionalPanel(
                condition = "input.submit == 0",
                wellPanel(
                  div(
                    style = "position: relative; text-align: center;",
                    icon("lock"), 
                    p("Please choose 10 mice to unlock this page.")
                  )
                )
              ),
              conditionalPanel(
                condition = "(output.number >= 10) & (input.submit != 0)",
                DT::DTOutput(outputId = "miceSum"),
                  # fluidRow(
                  #   column(
                  #     width = 12, 
                  #     wellPanel(
                  #       fluidRow(
                  #          column(2,""), 
                  #          column(2,strong("Total selected")), 
                  #          column(2,strong("Mean Weight (g)")), 
                  #          column(2,strong("Mean Age (wks)")), 
                  #          column(2,strong("Mean Tumor Mass (mg)")), 
                  #          column(1,strong("Proportion Female")), 
                  #          column(1,strong("Proportion Brown"))
                  #         ), 
                  #         fluidRow(
                  #           column(2,strong("Rspb.Group")), 
                  #           column(2,"10"), 
                  #           column(2,textOutput("aveWeight")), 
                  #           column(2,textOutput("aveAge")), 
                  #           column(2,textOutput("aveTu")), 
                  #           column(1,textOutput("gend")), 
                  #           column(1,textOutput("col"))
                  #         ),
                  #         fluidRow(
                  #           column(2,strong("Control Group")), 
                  #           column(2,"10"), 
                  #           column(2,textOutput("aveWeightC")), 
                  #           column(2,textOutput("aveAgeC")), 
                  #           column(2,textOutput("aveTuC")), 
                  #           column(1,textOutput("gendC")), 
                  #           column(1,textOutput("colC"))
                  #       )
                  #     )
                  #   )
                  # ),
                  fluidRow(
                    column(
                      width = 3,
                      plotOutput("weight"),
                      tags$script(HTML(
                        "$(document).ready(function() {
                        document.getElementById('weight').setAttribute('aria-label',
                        `Comparison of mean weight between raspberry and control group.`)
                        })"
                      ))
                    ),
                     column(
                       width = 3,
                       plotOutput("age"),
                       tags$script(HTML(
                         "$(document).ready(function() {
                        document.getElementById('age').setAttribute('aria-label',
                        `Comparison of mean age between raspberry and control group.`)
                        })"
                       ))
                      ),
                     column(
                       width = 6,
                       plotOutput("tumor"),
                       tags$script(HTML(
                         "$(document).ready(function() {
                        document.getElementById('tumor').setAttribute('aria-label',
                        `Comparison of mean tumor mass between raspberry and control group.`)
                        })"
                       ))
                      )
                  ),
                  fluidRow(
                      column(
                        width = 3,
                        plotOutput("gender"),
                        tags$script(HTML(
                          "$(document).ready(function() {
                        document.getElementById('gender').setAttribute('aria-label',
                        `Comparison of gender between raspberry and control group.`)
                        })"
                        ))
                      ),
                      column(
                        width = 3,
                        plotOutput("color"),
                        tags$script(HTML(
                          "$(document).ready(function() {
                        document.getElementById('color').setAttribute('aria-label',
                        `Comparison of colors between raspberry and control group.`)
                        })"
                        ))
                      ),
                      column(
                        width = 6,
                        sliderInput(
                          inputId = "theta",
                          label = "Choose a Treatment Effect",
                          min = 0,
                          max = 1,
                          value = 0.6,
                          width = '90%'
                        )
                      )
                  ),
                  br(), br(), br(),
                  fluidRow(
                    column(4, offset = 2,
                           bsButton("getdata","Use Raw Data", size = "large")
                    ),
                    column(4,
                      bsButton("compare","Compare with Random Selection", size = "large")
                    )
                ),
                conditionalPanel("input.getdata != 0", verbatimTextOutput("dataf"))
              )
          ),
          tabItem(
            tabName = "computer",
            useShinyjs(),
              wellPanel(
                fluidRow(
                  column(
                    width = 2,
                    sliderInput(
                      inputId = "times",
                      label = "Number of Simulations",
                      value = 1,
                      min = 1,
                      max = 1000
                    )
                  ),
                  column(
                    width = 8,
                    p("Select a number to run multiple trials. (Hint: Try
                      selecting 10, 100, 1000, etc.)")
                  ),
                )
              ),
              conditionalPanel(
                condition = "input.times < 1",
                  wellPanel(
                    div(
                      style = "position: relative; text-align: center;", 
                      icon("lock"), 
                      p("Warning: The input number has to be at least one.")
                    )
                  )
              ),
              conditionalPanel(
                condition = "input.times >= 1",
                fluidRow(
                  column(
                    width = 6,
                    tableOutput("computerTable")
                  ),
                  column(
                    width = 6,
                    sliderInput(
                      "compTheta",
                      label = "Choose a Treatment Effect",
                      min = 0,
                      max = 1,
                      value = 0.6,
                      width = '400px'
                    )
                  )
                ),
                br(), br(), br(), br(), br(), br(), br(),
                fluidRow(
                  column(
                    width = 3,
                    plotOutput("compWeightBar"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('compWeightBar').setAttribute('aria-label',
                        `Comparison of mean computed generated weight between raspberry and control group.`)
                        })"
                    ))
                  ),
                  column(
                    width = 3,
                    plotOutput("compAgeBar"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('compAgeBar').setAttribute('aria-label',
                        `Comparison of mean computer generated age between raspberry and control group.`)
                        })"
                    ))
                  ),
                  column(
                    width = 6,
                    plotOutput("compTumorBar"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('compTumorBar').setAttribute('aria-label',
                        `Comparison of mean computer generated tumor mass between raspberry and control group.`)
                        })"
                    ))
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    plotOutput("compWeightHist"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('CompWeightHist').setAttribute('aria-label',
                        `Histogram of differences in weight between groups.`)
                        })"
                    ))
                  ),
                  column(
                    width = 4,
                    plotOutput("compAgeHist"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('compAgeHist').setAttribute('aria-label',
                        `Histogram of differences in age between groups.`)
                        })"
                    ))
                  ),
                  column(
                    width = 4,
                    plotOutput("compTumorHist"),
                    tags$script(HTML(
                      "$(document).ready(function() {
                        document.getElementById('compTumorHist').setAttribute('aria-label',
                        `Histogram of differences in tumor mass between groups.`)
                        })"
                    ))
                  )
                ),
                  div(style = "position:relative; top: -35px;", 
                      bsButton(
                        inputId = "compGetdata",
                        label = "Use the data from last trial"
                      )
                  ),
                 
                conditionalPanel(
                  condtion = "input.compGetdata != 0", 
                  verbatimTextOutput("compDataf")
                )
              )
          ),
          tabItem(
            tabName = "references", 
            h2("References"),
            p(
              class = "hangingindent",
              "Attali, D. (2020). shinyjs: Easily Improve the User Experience of 
              Your Shiny Apps in Seconds. R package version 1.1. Available from 
              https://CRAN.R-project.org/package=shinyjs"
            ),
            p(
              class = "haningindent",
              "Bailey, E. (2015). shinyBS: Twitter Bootstrap Components for Shiny. 
               R package version 0.61. Available from https://CRAN.R-project.org/package=shinyBS"
            ),
            p(
              class = "hangingindent",
              "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. R 
              package version 0.1.6.1. Available from https://github.com/EducationShinyAppTeam/boastUtils"
            ),
            p(
              class = "hangingindent",
              "Chang, W. and Borges Ribeiro, B. (2018). shinydashboard: Create 
              Dashboards with 'Shiny'. R package version 0.7.1. Available from 
              https://CRAN.R-project.org/package=shinydashboard"
            ),
            p(
              class = "hangingindent", 
              "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2020). 
              shiny: Web Application Framework for R. R package version 1.5.0. Available 
              from https://CRAN.R-project.org/package=shiny"
            )
          )
        )
      )
    )
  )
