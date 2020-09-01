library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(tidyverse)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Mouse Experiment for Random Assignment"
APP_DESCP  <<- paste(
  "This app will allow users to explore what happens when they attempt to do their",
  "own 'random' selection rather than a computer."
)
## End App Meta Data------------------------------------------------------------

# Global Functions/Constants ----
raspPalette <- c("#BC204B", "#F5F5DC", "#1E407C" )
small <- 100
medium <- 130
large <-160

## Extract tumor mass from the raspberry experiment model ----
model <- function(data, val, theta){
  TuMass = data[,4]
  for (i in 1:20){
    if (is.null(isolate(val$btn[i]))) {
      # skip
    } else if (isolate(val$btn[i]) == 0) {
      TuMass[i] = TuMass[i] + rnorm(1, mean = 0, sd = 0.05 * TuMass[i])
    } else {
      TuMass[i] = TuMass[i] * (1 - theta) + rnorm(1, mean = 0, sd = 0.05 * (TuMass[i] * (1 - theta)))
    }
  }
  return(TuMass)
}

compModel <- function(data, index, theta){
  TuMass = data[,4]
  for (i in 1:20){
    if (any(i == index)){
      TuMass[i] = TuMass[i] + rnorm(1, mean = 0, sd = 0.05 * TuMass[i])
    }
    else{
      TuMass[i] = TuMass[i] * (1 - theta) + rnorm(1, mean = 0, sd = 0.05 * (TuMass[i] * (1 - theta)))
    }
  }
  return(TuMass)
}

# Read in the data ----
data <- read.csv("database.csv", stringsAsFactors = FALSE)
colnames(data) = c("Color", "Weight", "Age", "Tumor", "Gender")
data$Color <- dplyr::recode(data$Color, "brown" = 1, "black" = 0)
data$Gender <- dplyr::recode(data$Gender, "Female" = 1, "Male" = 0)
data$handPicked <- "Control"
data$compPicked <- "Control"
origTumor <- data$Tumor

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "red",
    ## Header ----
    dashboardHeader(
      title = "Random Assignment",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem(text = "Select by Hand", tabName = "hand", icon = icon("wpexplorer")),
        menuItem(text = "Explore Hand Selection", tabName = "summary", icon = icon("wpexplorer")),
        menuItem(text = "Computer Selection", tabName = "computer", icon = icon("wpexplorer")),
        menuItem(text = "References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
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
          div(class = "updated", "Last Update: 8/31/20 by NJH.")
        ),
        ### Hand Selection Page ----
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
              offset = 3,
              bsButton(
                inputId = "btn1",
                label = tags$img(src = "blackMouse.png",
                                 width = medium,
                                 alt = "medium black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 1,
              offset = 1,
              bsButton(
                inputId = "btn2",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn3",
                label = tags$img(src = "brownMouse.png",
                                 width = large,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn4",
                label = tags$img(src = "blackMouse.png",
                                 width = medium,
                                 alt = "medium black mouse"
                ),
                style = "info"
              )
            )
          ), br(),
          fluidRow(
            column(
              width = 2,
              offset = 4,
              bsButton(
                inputId = "btn5",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 1,
              offset = 2,
              bsButton(
                inputId = "btn6",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "btn7",
                label = tags$img(src = "blackMouse.png",
                                 width = large,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn8",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            )
          ), br(),
          fluidRow(
            column(
              width = 1,
              offset = 3,
              bsButton(
                inputId = "btn9",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              offset = 2,
              bsButton(
                inputId = "btn10",
                label = tags$img(src = "blackMouse.png",
                                 width = small,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn11",
                label = tags$img(src = "brownMouse.png",
                                 width = medium,
                                 alt = "medium brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn12",
                label = tags$img(src = "blackMouse.png",
                                 width = small,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            )
          ), br(),
          fluidRow(
            column(
              width = 1,
              offset = 1,
              bsButton(
                inputId = "btn13",
                label = tags$img(src = "blackMouse.png",
                                 width = small,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "btn14",
                label = tags$img(src = "blackMouse.png",
                                 width = large,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 1,
              bsButton(
                inputId = "btn15",
                label = tags$img(src = "brownMouse.png",
                                 width = small,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              bsButton(
                inputId = "btn16",
                label = tags$img(src = "blackMouse.png",
                                 width = small,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            ),
          ),
          fluidRow(
            column(
              width = 1,
              offset = 5,
              bsButton(
                inputId = "btn17",
                label = tags$img(src = "blackMouse.png",
                                 width = small,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              offset = 0,
              bsButton(
                inputId = "btn18",
                label = tags$img(src = "brownMouse.png",
                                 width = medium,
                                 alt = "medium brown mouse"
                ),
                style = "info"
              )
            )
          ),
          fluidRow(
            column(
              width = 1,
              offset = 3,
              bsButton(
                inputId = "btn19",
                label = tags$img(src = "brownMouse.png",
                                 width = medium,
                                 alt = "medium brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = "btn20",
                label = tags$img(src = "brownMouse.png",
                                 width = medium,
                                 alt = "medium brown mouse"
                ),
                style = "info"
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
        ### Explore Hand Selection Page ----
        tabItem(
          tabName = "summary",
          h2("Summary of Hand-Selected Mice for Comparison"),
          conditionalPanel(
            condition = "input.submit == 0",
            br(),br(),br(),
            wellPanel(
              div(
                style = "position: relative; text-align: center;",
                icon("lock"),
                p("Please choose 10 mice to unlock this page.")
              )
            )
          ),
          conditionalPanel(
            condition = "input.submit != 0",
            DT::DTOutput(outputId = "miceSum"),
            fluidRow(
              column(
                width = 4,
                plotOutput("weight"),
                tags$script(HTML(
                  "$(document).ready(function() {
                        document.getElementById('weight').setAttribute('aria-label',
                        `Comparison of mean weight between raspberry and control group.`)
                        })"
                ))
              ),
              column(
                width = 4,
                plotOutput("age"),
                tags$script(HTML(
                  "$(document).ready(function() {
                        document.getElementById('age').setAttribute('aria-label',
                        `Comparison of mean age between raspberry and control group.`)
                        })"
                ))
              ),
              column(
                width = 4,
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
                width = 4,
                plotOutput("gender"),
                tags$script(HTML(
                  "$(document).ready(function() {
                        document.getElementById('gender').setAttribute('aria-label',
                        `Comparison of gender between raspberry and control group.`)
                        })"
                ))
              ),
              column(
                width = 4,
                plotOutput("color"),
                tags$script(HTML(
                  "$(document).ready(function() {
                        document.getElementById('color').setAttribute('aria-label',
                        `Comparison of colors between raspberry and control group.`)
                        })"
                ))
              ),
              column(
                width = 4,
                ## Hiding this slider for now
                # sliderInput(
                #   inputId = "theta",
                #   label = "Choose a Treatment Effect",
                #   min = 0,
                #   max = 1,
                #   value = 0.6,
                #   width = '90%'
                # )
              )
            ),
            br(), br(), br(),
            fluidRow(
              column(
                width = 4,
                offset = 2,
                bsButton(
                  inputId = "getHandData",
                  label = "Display Raw Data",
                  icon = icon("table"),
                  size = "large",
                  type = "toggle",
                  value = 0
                )
              ),
              column(
                width = 4,
                bsButton(
                  inputId = "compare",
                  label = "Compare with Random Selection",
                  size = "large"
                )
              )
            ),
            DT::DTOutput(outputId = "handData")
          )
        ),
        ### Computer Selection Page ----
        tabItem(
          tabName = "computer",
          useShinyjs(),
          h2("Summary of Computer-Selected Mice for Comparison"),
          p("The following table and graph are the same as on the Explore Hand
            Selection page except this time a computer randomly assigned
            the treatments."),
          DT::DTOutput(outputId = "compMiceSum"),
          fluidRow(
            column(
              width = 4,
              plotOutput("compWeight"),
              tags$script(HTML(
                "$(document).ready(function() {
                        document.getElementById('compWeight').setAttribute('aria-label',
                        `Comparison of mean weight between raspberry and control group.`)
                        })"
              ))
            ),
            column(
              width = 4,
              plotOutput("compAge"),
              tags$script(HTML(
                "$(document).ready(function() {
                        document.getElementById('compAge').setAttribute('aria-label',
                        `Comparison of mean age between raspberry and control group.`)
                        })"
              ))
            ),
            column(
              width = 4,
              plotOutput("compTumor"),
              tags$script(HTML(
                "$(document).ready(function() {
                        document.getElementById('compTumor').setAttribute('aria-label',
                        `Comparison of mean tumor mass between raspberry and control group.`)
                        })"
              ))
            )
          ),
          fluidRow(
            column(
              width = 4,
              plotOutput("compGender"),
              tags$script(HTML(
                "$(document).ready(function() {
                        document.getElementById('compGender').setAttribute('aria-label',
                        `Comparison of gender between raspberry and control group.`)
                        })"
              ))
            ),
            column(
              width = 4,
              plotOutput("compColor"),
              tags$script(HTML(
                "$(document).ready(function() {
                        document.getElementById('compColor').setAttribute('aria-label',
                        `Comparison of colors between raspberry and control group.`)
                        })"
              ))
            )
          ),
          bsButton(
            inputId = "getCompData",
            label = "Display Raw Data",
            icon = icon("table"),
            size = "large",
            type = "toggle",
            value = 0
          ),
          DT::DTOutput(outputId = "compData"),
          ## Hiding Simulation Tools for now
          # wellPanel(
          #   fluidRow(
          #     column(
          #       width = 2,
          #       sliderInput(
          #         inputId = "times",
          #         label = "Number of Simulations",
          #         value = 1,
          #         min = 1,
          #         max = 1000
          #       )
          #     ),
          #     column(
          #       width = 8,
          #       p("Select a number to run multiple trials. (Hint: Try
          #             selecting 10, 100, 1000, etc.)")
          #     ),
          #   )
          # ),
          # conditionalPanel(
          #   condition = "input.times < 1",
          #   wellPanel(
          #     div(
          #       style = "position: relative; text-align: center;",
          #       icon("lock"),
          #       p("Warning: The input number has to be at least one.")
          #     )
          #   )
          # ),
          # conditionalPanel(
          #   condition = "input.times >= 1",
          #   fluidRow(
          #     column(
          #       width = 6,
          #       tableOutput("computerTable")
          #     ),
          #     column(
          #       width = 6,
          #       sliderInput(
          #         "compTheta",
          #         label = "Choose a Treatment Effect",
          #         min = 0,
          #         max = 1,
          #         value = 0.6,
          #         width = '400px'
          #       )
          #     )
          #   ),
          #   br(), br(), br(), br(), br(), br(), br(),
          #   fluidRow(
          #     column(
          #       width = 3,
          #       plotOutput("compWeightBar"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('compWeightBar').setAttribute('aria-label',
          #               `Comparison of mean computed generated weight between raspberry and control group.`)
          #               })"
          #       ))
          #     ),
          #     column(
          #       width = 3,
          #       plotOutput("compAgeBar"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('compAgeBar').setAttribute('aria-label',
          #               `Comparison of mean computer generated age between raspberry and control group.`)
          #               })"
          #       ))
          #     ),
          #     column(
          #       width = 6,
          #       plotOutput("compTumorBar"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('compTumorBar').setAttribute('aria-label',
          #               `Comparison of mean computer generated tumor mass between raspberry and control group.`)
          #               })"
          #       ))
          #     )
          #   ),
          #   fluidRow(
          #     column(
          #       width = 4,
          #       plotOutput("compWeightHist"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('CompWeightHist').setAttribute('aria-label',
          #               `Histogram of differences in weight between groups.`)
          #               })"
          #       ))
          #     ),
          #     column(
          #       width = 4,
          #       plotOutput("compAgeHist"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('compAgeHist').setAttribute('aria-label',
          #               `Histogram of differences in age between groups.`)
          #               })"
          #       ))
          #     ),
          #     column(
          #       width = 4,
          #       plotOutput("compTumorHist"),
          #       tags$script(HTML(
          #         "$(document).ready(function() {
          #               document.getElementById('compTumorHist').setAttribute('aria-label',
          #               `Histogram of differences in tumor mass between groups.`)
          #               })"
          #       ))
          #     )
          #   ),
        ),
      ### Reference Page ----
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
            class = "hangingindent",
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

# Define the Server ----
server <- function(input, output,session) {
  mouseCount <- reactiveVal(0)
  miceBtns <- NA
  for(i in 1:20) {
    miceBtns[i] <- paste0("btn", i)
  }
  localData <- data

  ## Info Button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions",
      text = "Quickly click on 10 mice to select the treatment group
      and then compare the results with what happens when the mice are
      assigned to groups randomly by the computer",
      type = "info"
    )
  })

  ## Go button ----
  observeEvent(input$go,{
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "hand")
  })

  ## Reset Mice Button ----
  observeEvent(input$reset_button, {
    mouseCount(0)
    localData$handPicked <<- "Control"
    lapply(miceBtns, updateButton,
           session = session,
           style = "info",
           disabled = FALSE)
  })

  ## Mouse Buttons ----
  observeEvent(mouseCount(), {
    if(mouseCount() == 10) {
      lapply(miceBtns, updateButton, session = session, disabled = TRUE)
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
    }
  })

  ### Individual Observers
  observeEvent(input$btn1, {
    updateButton(
      session = session,
      inputId = "btn1",
      style = "default",
    )
    localData[1, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn2, {
    updateButton(
      session = session,
      inputId = "btn2",
      style = "default",
    )
    localData[2, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn3, {
    updateButton(
      session = session,
      inputId = "btn3",
      style = "default",
    )
    localData[3, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn4, {
    updateButton(
      session = session,
      inputId = "btn4",
      style = "default",
    )
    localData[4, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn5, {
    updateButton(
      session = session,
      inputId = "btn5",
      style = "default",
    )
    localData[5, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn6, {
    updateButton(
      session = session,
      inputId = "btn6",
      style = "default",
    )
    localData[6, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn7, {
    updateButton(
      session = session,
      inputId = "btn7",
      style = "default",
    )
    localData[7, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn8, {
    updateButton(
      session = session,
      inputId = "btn8",
      style = "default",
    )
    localData[8, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn9, {
    updateButton(
      session = session,
      inputId = "btn9",
      style = "default",
    )
    localData[9, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn10, {
    updateButton(
      session = session,
      inputId = "btn10",
      style = "default",
    )
    localData[10, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn11, {
    updateButton(
      session = session,
      inputId = "btn11",
      style = "default",
    )
    localData[11, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn12, {
    updateButton(
      session = session,
      inputId = "btn12",
      style = "default",
    )
    localData[12, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn13, {
    updateButton(
      session = session,
      inputId = "btn13",
      style = "default",
    )
    localData[13, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn14, {
    updateButton(
      session = session,
      inputId = "btn14",
      style = "default",
    )
    localData[14, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn15, {
    updateButton(
      session = session,
      inputId = "btn15",
      style = "default",
    )
    localData[15, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn16, {
    updateButton(
      session = session,
      inputId = "btn16",
      style = "default",
    )
    localData[16, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn17, {
    updateButton(
      session = session,
      inputId = "btn17",
      style = "default",
    )
    localData[17, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn18, {
    updateButton(
      session = session,
      inputId = "btn18",
      style = "default",
    )
    localData[18, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn19, {
    updateButton(
      session = session,
      inputId = "btn19",
      style = "default",
    )
    localData[19, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })
  observeEvent(input$btn20, {
    updateButton(
      session = session,
      inputId = "btn20",
      style = "default",
    )
    localData[20, "handPicked"] <<- "Treatment"
    mouseCount(mouseCount() + 1)
  })

  ### Display count of selected mice ----
  output$num <- renderUI({
    paste("You have selected", mouseCount(), "mice.")
  })

  ## Submit button ----
  #When the submit button is clicked, redirect to the next page.
  observeEvent(input$submit,{
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "summary")
    ## Do the Computer Assignment
    localData$compPicked <<- sample(rep(c("Control", "Treatment"), 10), size = 20)
  })

  ## Hand Picked Summaries ----
  observeEvent(input$tabs, {
    if(input$tabs == "summary") {
      handSummary <- localData %>%
        dplyr::group_by(handPicked) %>%
        dplyr::summarize(
          .groups = "rowwise",
          samWeight = mean(Weight),
          samAge = mean(Age),
          samTumor = mean(Tumor),
          propBrown = mean(Color),
          propFemale = mean(Gender)
        )

      ## Hand Selection Table ----
      handSummary <- tibble::remove_rownames(handSummary)
      handSummary <- tibble::column_to_rownames(handSummary, var = "handPicked")
      names(handSummary) <- c("Mean Weight (g/mouse)",
                              "Age (wks/mouse)",
                              "Tumor Mass (mg/mouse)",
                              "Proportion Brown",
                              "Proportion Female")
      output$miceSum <- DT::renderDT(
        expr = round(handSummary, digits = 3),
        caption = "Descriptive Statistics for Hand Selected Groups",
        style = "bootstrap4",
        rownames = TRUE,
        autoHideNavigation = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(handSummary))
          )
        )
      )

      ## Hand Selection Plots ----
      output$weight <- renderPlot({
        ggplot(data = aggregate(Weight ~ handPicked, data = localData, FUN = mean),
               mapping = aes(y = Weight, x = handPicked, fill = handPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean weight (g/mouse)") +
          labs(title = "Comparison of Mean Weights") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$age <- renderPlot({
        ggplot(data = aggregate(Age ~ handPicked, data = localData, FUN = mean),
               mapping = aes(y = Age, x = handPicked, fill = handPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean age (wk/mouse)") +
          labs(title = "Comparison of Mean Ages") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$tumor <- renderPlot({
        ggplot(data = aggregate(Tumor ~ handPicked, data = localData, FUN = mean),
               mapping = aes(y = Tumor, x = handPicked, fill = handPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean Tumor Mass (mg/mouse)") +
          labs(title = "Comparison of Mean Tumor Masses") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$gender <- renderPlot({
        localData %>%
          mutate(genderCat = case_when(
            Gender == 1 ~ "Female",
            Gender == 0 ~ "Male"
          )) %>%
          ggplot(mapping = aes(y = genderCat,
                               x = handPicked,
                               fill = genderCat)) +
          geom_bar(stat = "identity", position = "stack") +
          theme_bw() +
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            title = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          ) +
          xlab("Raspberry treatment") +
          labs(title = "Comparison of Gender",
               fill = "Gender")
      })

      output$color <- renderPlot({
        localData %>%
          mutate(colorCat = case_when(
            Color == 1 ~ "Brown",
            Color == 0 ~ "Black"
          )) %>%
          ggplot(mapping = aes(y = colorCat,
                               x = handPicked,
                               fill = colorCat)) +
          geom_bar(stat = "identity", position = "stack") +
          theme_bw() +
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            title = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          ) +
          xlab("Raspberry treatment") +
          labs(title = "Comparison of Color",
               fill = "Color") +
          scale_fill_manual(values = c("Brown" = "#BB9B7A",
                                       "Black" = "black"))
      })
    }
  })

  ## Display hand selected data ----
  observeEvent(input$getHandData, {
    if (input$getHandData %% 2 == 0){
      output$handData <- NULL
    } else if (input$getHandData %% 2 == 1) {
      temp1 <- localData %>%
        mutate(Gender = case_when(
        Gender == 1 ~ "Female",
        Gender == 0 ~ "Male"
      )) %>%
        mutate(Color = case_when(
          Color == 1 ~ "Brown",
          Color == 0 ~ "Black"
        ))
      output$handData <- DT::renderDT(
        expr = temp1,
        caption = "Hand Selected Mouse Data",
        style = "bootstrap4",
        rownames = TRUE,
        autoHideNavigation = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(localData))
          )
        )
      )
    }
  })

  ## Compare button, move to computer simulation page ----
  observeEvent(input$compare,{
    updateTabItems(
      session = session,
      inputId = "tabs",
      selected = "computer")
  })

  ## Computer Picked Summaries ----
  observeEvent(input$tabs, {
    if(input$tabs == "computer") {
      compSummary <- localData %>%
        dplyr::group_by(compPicked) %>%
        dplyr::summarize(
          .groups = "rowwise",
          samWeight = mean(Weight),
          samAge = mean(Age),
          samTumor = mean(Tumor),
          propBrown = mean(Color),
          propFemale = mean(Gender)
        )

      ## Computer Selection Table ----
      compSummary <- tibble::remove_rownames(compSummary)
      compSummary <- tibble::column_to_rownames(compSummary, var = "compPicked")
      names(compSummary) <- c("Mean Weight (g/mouse)",
                              "Age (wks/mouse)",
                              "Tumor Mass (mg/mouse)",
                              "Proportion Brown",
                              "Proportion Female")
      output$compMiceSum <- DT::renderDT(
        expr = round(compSummary, digits = 3),
        caption = "Descriptive Statistics for Computer Selected Groups",
        style = "bootstrap4",
        rownames = TRUE,
        autoHideNavigation = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(compSummary))
          )
        )
      )

      ## Computer Selection Plots ----
      output$compWeight <- renderPlot({
        ggplot(data = aggregate(Weight ~ compPicked, data = localData, FUN = mean),
               mapping = aes(y = Weight, x = compPicked, fill = compPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean weight (g/mouse)") +
          labs(title = "Comparison of Mean Weights") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$compAge <- renderPlot({
        ggplot(data = aggregate(Age ~ compPicked, data = localData, FUN = mean),
               mapping = aes(y = Age, x = compPicked, fill = compPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean age (wk/mouse)") +
          labs(title = "Comparison of Mean Ages") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$compTumor <- renderPlot({
        ggplot(data = aggregate(Tumor ~ compPicked, data = localData, FUN = mean),
               mapping = aes(y = Tumor, x = compPicked, fill = compPicked)) +
          geom_bar(stat = "identity", color = "black") +
          theme_bw() +
          theme(legend.position = "none",
                title = element_text(size = 14),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 14)) +
          xlab("Raspberry treatment") +
          ylab("Mean Tumor Mass (mg/mouse)") +
          labs(title = "Comparison of Mean Tumor Masses") +
          scale_fill_manual(values = c("Control" = raspPalette[2],
                                       "Treatment" = raspPalette[1],
                                       "Difference" = raspPalette[3]))
      })

      output$compGender <- renderPlot({
        localData %>%
          mutate(genderCat = case_when(
            Gender == 1 ~ "Female",
            Gender == 0 ~ "Male"
          )) %>%
          ggplot(mapping = aes(y = genderCat,
                               x = compPicked,
                               fill = genderCat)) +
          geom_bar(stat = "identity", position = "stack") +
          theme_bw() +
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            title = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          ) +
          xlab("Raspberry treatment") +
          labs(title = "Comparison of Gender",
               fill = "Gender")
      })

      output$compColor <- renderPlot({
        localData %>%
          mutate(colorCat = case_when(
            Color == 1 ~ "Brown",
            Color == 0 ~ "Black"
          )) %>%
          ggplot(mapping = aes(y = colorCat,
                               x = compPicked,
                               fill = colorCat)) +
          geom_bar(stat = "identity", position = "stack") +
          theme_bw() +
          theme(
            legend.position = "top",
            legend.direction = "horizontal",
            title = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank()
          ) +
          xlab("Raspberry treatment") +
          labs(title = "Comparison of Color",
               fill = "Color") +
          scale_fill_manual(values = c("Brown" = "#BB9B7A",
                                       "Black" = "black"))
      })
    }
  })

  ## Display computer selected data ----
  observeEvent(input$getCompData, {
    if (input$getCompData %% 2 == 0){
      output$compData <- NULL
    } else if (input$getCompData %% 2 == 1) {
      temp2 <- localData %>%
        mutate(Gender = case_when(
          Gender == 1 ~ "Female",
          Gender == 0 ~ "Male"
        )) %>%
        mutate(Color = case_when(
          Color == 1 ~ "Brown",
          Color == 0 ~ "Black"
        ))
      output$compData <- DT::renderDT(
        expr = temp2,
        caption = "Computer Selected Mouse Data",
        style = "bootstrap4",
        rownames = TRUE,
        autoHideNavigation = TRUE,
        options = list(
          responsive = TRUE,
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 1:ncol(localData))
          )
        )
      )
    }
  })

  # #use the "model" function and input theta
  # TuM <- model(data, val, input$theta)
  #
  # ##Print raw data with assigned group
  # output$dataf = renderPrint({
  #   data$Group = val$btn
  #   for (i in 1:20){
  #     if (data$Group[i] == 1){data$Group[i] = "Raspberry"}
  #     else {data$Group[i] = "Control"}
  #   }
  #   TuM = model(data,val,input$theta)
  #   data[,4] = TuM
  #
  #   #Change the column color and gender from numeric into string
  #   for (i in 1:20){
  #     if (data$Color[i] == 1){data$Color[i] = "Brown"}
  #     else {data$Color[i] = "Black"}
  #   }
  #   for (i in 1:20){
  #     if (data$Gender[i] == 1){data$Gender[i] = "Female"}
  #     else {data$Gender[i] = "Male"}
  #   }
  # })
  #
  # ###########################################################
  # #Save all the vectors that will be used later in reactive environment
  # table <- reactive({
  #   compWeight = c()
  #   compWeightC = c()
  #   compAge = c()
  #   compAgeC = c()
  #   compTum = c()
  #   compTumC = c()
  #   diffWeight = c()
  #   diffAge = c()
  #   diffTum = c()
  #
  #   #Use for loop to simulate many times
  #   # withProgress(message = "Simulating Experiments", value = 0, {
  #   #   n <- input$times
  #   for (i in 1:input$times){
  #     #Use random sampling to get 10 mice for experimental group each time
  #     exp = sample(1:20,10)
  #     #Get the average weight, age, tumor mass for both groups in each simulation
  #     # and save as vectors
  #     compWeight[i] = mean(data[exp,"Weight(g)"])
  #     compWeightC[i] = mean(data[-exp,"Weight(g)"])
  #     compAge[i] = mean(data[exp,"Age(wks)"])
  #     compAgeC[i] = mean(data[-exp,"Age(wks)"])
  #     Tumor = compModel(data,exp,input$compTheta)
  #     compTum[i] = mean(Tumor[exp])
  #     compTumC[i] = mean(Tumor[-exp])
  #     #Get the difference between two groups in average weight, average age, average tumor mass
  #     diffWeight[i] = compWeight[i] - compWeightC[i]
  #     diffAge[i] = compAge[i] - compAgeC[i]
  #     diffTum[i] = compTum[i] - compTumC[i]
  #     # incProgress(1/n, detail = paste("Simulating Experiment", i))
  #   }
  #   # })
  #
  #   #Create a dataframe
  #   meanWeight = c(mean(compWeightC), mean(compWeight))
  #   meanAge = c(mean(compAgeC),mean(compAge))
  #   meanTum = c(mean(compTumC),mean(compTum))
  #   meanDiff = mean(diffTum)
  #   compTable = data.frame(Group = c("Raspberry Group","Control Group"),
  #                          Selected = c(10,10),
  #                          Weight = meanWeight,
  #                          Age = meanAge,
  #                          Tumor = meanTum)
  #   names(compTable) = c("Group","Total Selected","Weight (g)","Age (wks)","Tumor Mass (mg)")
  #
  #   #Return these values
  #   list(aveWeight = meanWeight, aveAge = meanAge, aveTum = meanTum, aveTable = compTable, Tumor = Tumor,
  #        diffWeight = diffWeight, diffAge = diffAge, diffTum = diffTum, aveDiff = meanDiff, exp = exp)
  #
  # })
  # #Using the Freedman-Diaconis Rule for bin widths
  # binwidth <- function(x) {
  #   2 * IQR(x) / (length(x)^(1/3))
  #
  # }
  #
  #
  # output$computerTable <- renderTable({
  #   table()$aveTable
  # })
  #
  # output$compWeightBar <- renderPlot({
  #   barplot(table()$aveWeight,
  #           names.arg = c("Raspberry Group","Control Group"),
  #           main = "Comparison of Average Weight",
  #           ylab = "Weight(g)",
  #           col = c("#C7053D","beige"),
  #           cex.axis = 1.25,
  #           cex.names = 1.25)
  # }, width = 250, height = 350)
  #
  # output$compAgeBar = renderPlot({
  #   barplot(table()$aveAge,
  #           names.arg = c("Raspberry Group","Control Group"),
  #           main = "Comparison of Average Age",
  #           ylab = "Age(wks)",
  #           col = c("#C7053D","beige"),
  #           cex.axis = 1.25,
  #           cex.names = 1.25
  #   )
  # }, width = 250, height = 350)
  #
  # output$compTumorBar = renderPlot({
  #   barplot(c(table()$aveTum,table()$aveDiff),
  #           names.arg = c("Raspberry Group","Control Group","Difference"),
  #           main = "Comparison of Tumor Mass",
  #           ylab = "Tumor Mass(mg)",
  #           col = c("#C7053D","beige","#1C2C5B"),
  #           width = 5, xlim = c(1,30),
  #           cex.axis = 1.25,
  #           cex.names = 1.25
  #   )
  #   legend(
  #     x = "right",
  #     c("Raspberry Group","Control Group","Difference"),
  #     col = c("#C7053D","beige","#1C2C5B"),
  #     fill=c("#C7053D","beige","#1C2C5B")
  #   )
  # },width = 500, height = 350)
  #
  # output$compWeightHist = renderPlot({
  #   qplot(table()$diffWeight,
  #         geom="histogram",
  #         binwidth = binwidth(table()$diffWeight),
  #         main = "Differences in Weight between groups (g)",
  #         xlab = "Weight (g)",
  #         fill=I("#1C2C5B"),
  #         col=I("#1C2C5B"),
  #         alpha=I(.8),
  #         cex.axis = 1.25,
  #         cex.names = 1.25
  #   )
  # }, height = 350)
  #
  #
  # output$compAgeHist = renderPlot({
  #   qplot(table()$diffAge,
  #         geom="histogram",
  #         binwidth = binwidth(table()$diffAge),
  #         main = "Differences in Age between groups (wks)",
  #         xlab = "Age (wks)",
  #         fill=I("#1C2C5B"),
  #         col=I("#1C2C5B"),
  #         alpha=I(.8),
  #         cex.axis = 1.25,
  #         cex.names = 1.25
  #   )
  # }, height = 350)
  #
  # output$compTumorHist = renderPlot({
  #   qplot(table()$diffTum,
  #         geom="histogram",
  #         binwidth = binwidth(table()$diffTum),
  #         main = "Differences in Tumor Mass between groups (mg)",
  #         xlab = "Tumor Mass (mg)",
  #         fill=I("#1C2C5B"),
  #         col=I("#1C2C5B"),
  #         alpha=I(.8),
  #         cex.axis = 1.25,
  #         cex.names = 1.25
  #   )
  # }, height = 350)
  #
  # ##Print raw data with assigned group
  # output$compDataf = renderPrint({
  #   data$Group = c()
  #   for (i in 1:20){
  #     if (any(data$Group[i] == table()$exp)){data$Group[i] = "Raspberry"}
  #     else {data$Group[i] = "Control"}
  #   }
  #   data[,4] = table()$Tumor
  #
  #   for (i in 1:20){
  #     if (data$Color[i] == 1){data$Color[i] = "Brown"}
  #     else {data$Color[i] = "Black"}
  #   }
  #   for (i in 1:20){
  #     if (data$Gender[i] == 1){data$Gender[i] = "Female"}
  #     else {data$Gender[i] = "Male"}
  #   }
  # })
}

# Call the app
boastUtils::boastApp(server = server, ui = ui)