library(shiny)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(boastUtils)

## App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Mouse Experiment for Random Assignment"
APP_DESCP  <<- paste(
  "This app will allow users to explore what happens when they attempt to do their",
  "own 'random' selection rather than a computer."
)
## End App Meta Data------------------------------------------------------------

# Global Functions/Constants ----
## Disable all the buttons ----
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

## Extract tumor mass from the raspberry experiment model ----
model <- function(data, val, theta){
  TuMass = data[,4]
  for (i in 1:20){
    if (val$btn[i] == 0){
      TuMass[i] = TuMass[i] + rnorm(1, mean = 0, sd = 0.05 * TuMass[i])
    }
    else{
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
colnames(data) = c("Color", "Weight(g)", "Age(wks)", "TumorMass(mg)",
                   "Gender")
# Change the column color and gender from factor to numeric
for (i in 1:20){
  data$Color[i] <- ifelse(data$Color[i] == "brown", 1, 0)
  data$Gender[i] <- ifelse(data$Gender[i] == "Female", 1, 0)
}

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
        menuItem(text = "Hand Selection", tabName = "hand", icon = icon("wpexplorer")),
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
              offset = 1,
              bsButton(
                inputId = "btn1",
                label = tags$img(src = "brown3.png",
                                 width = 82,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              bsButton(
                inputId = "btn2",
                label = tags$img(src = "brown3.png",
                                 width = 142,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              bsButton(
                inputId = "btn3",
                label = tags$img(src = "black3.png",
                                 width = 90,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              bsButton(
                inputId = "btn4",
                label = tags$img(src = "black3.png",
                                 width = 90,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            )
          ), br(),br(),
          fluidRow(
            column(
              width = 2,
              bsButton(
                inputId = "btn5",
                label = tags$img(src = "brown3.png",
                                 width = 97,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              offset = 1,
              bsButton(
                inputId = "btn6",
                label = tags$img(src = "black3.png",
                                 width = 162,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              offset = 1,
              bsButton(
                inputId = "btn7",
                label = tags$img(src = "brown3.png",
                                 width = 95,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn8",
                label = tags$img(src = "black3.png",
                                 width = 92,
                                 alt = "small black mouse"
                ),
                style = "info"
              )
            )
          ), br(),br(),
          fluidRow(
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = "btn9",
                label = tags$img(src = "black3.png",
                                 width = 101,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              offset = 1,
              bsButton(
                inputId = "btn10",
                label = tags$img(src = "brown3.png",
                                 width = 101,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn11",
                label = tags$img(src = "brown3.png",
                                 width = 82,
                                 alt = "small brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn12",
                label = tags$img(src = "brown3.png",
                                 width = 113,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            )
          ), br(),br(),
          fluidRow(
            column(
              width = 3,
              bsButton(
                inputId = "btn13",
                label = tags$img(src = "black3.png",
                                 width = 122,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn14",
                label = tags$img(src = "brown3.png",
                                 width = 124,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              bsButton(
                inputId = "btn15",
                label = tags$img(src = "brown3.png",
                                 width = 126,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              bsButton(
                inputId = "btn16",
                label = tags$img(src = "black3.png",
                                 width = 126,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
          ), br(),br(),
          fluidRow(
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = "btn17",
                label = tags$img(src = "brown3.png",
                                 width = 127,
                                 alt = "large brown mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              offset = 1,
              bsButton(
                inputId = "btn18",
                label = tags$img(src = "black3.png",
                                 width = 154,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 3,
              offset = 1,
              bsButton(
                inputId = "btn19",
                label = tags$img(src = "black3.png",
                                 width = 101,
                                 alt = "large black mouse"
                ),
                style = "info"
              )
            ),
            column(
              width = 2,
              bsButton(
                inputId = "btn20",
                label = tags$img(src = "brown3.png",
                                 width = 84,
                                 alt = "small brown mouse"
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
        ### Computer Selection Page ----
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
  ## Info Button ----
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
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
  observeEvent(input$reset_button, {js$reset()})

  ## Mouse Buttons ----
  # Save all the actionButton input into a vector for later convenience
  val <- reactiveValues(btn = c())
  observe({
    val$btn[1] = input$btn1
    val$btn[2] = input$btn2
    val$btn[3] = input$btn3
    val$btn[4] = input$btn4
    val$btn[5] = input$btn5
    val$btn[6] = input$btn6
    val$btn[7] = input$btn7
    val$btn[8] = input$btn8
    val$btn[9] = input$btn9
    val$btn[10] = input$btn10
    val$btn[11] = input$btn11
    val$btn[12] = input$btn12
    val$btn[13] = input$btn13
    val$btn[14] = input$btn14
    val$btn[15] = input$btn15
    val$btn[16] = input$btn16
    val$btn[17] = input$btn17
    val$btn[18] = input$btn18
    val$btn[19] = input$btn19
    val$btn[20] = input$btn20
  })

  # Create 20 updateButtons for 20 bsButtons in ui
  observeEvent(val$btn,({
    for (i in 1:20){
      if (val$btn[i] == 1){
        updateButton(session,paste("btn",i,sep = ""), style = "default", disabled = TRUE)
      }
    }
  }))

  #When 10 buttons have been clicked, enable the submit button.
  ###UPDATE: unable for clicking more mice
  observe({
    if (sum(val$btn) == 10){
      updateButton(
        session,
        inputId = "submit",
        label = "Submit Selection",
        size = "large",
        disabled = FALSE
      )
      updateButton(session,'btn1',disabled=TRUE)
      updateButton(session,'btn2',disabled=TRUE)
      updateButton(session,'btn3',disabled=TRUE)
      updateButton(session,'btn4',disabled=TRUE)
      updateButton(session,'btn5',disabled=TRUE)
      updateButton(session,'btn6',disabled=TRUE)
      updateButton(session,'btn7',disabled=TRUE)
      updateButton(session,'btn8',disabled=TRUE)
      updateButton(session,'btn9',disabled=TRUE)
      updateButton(session,'btn10',disabled=TRUE)
      updateButton(session,'btn11',disabled=TRUE)
      updateButton(session,'btn12',disabled=TRUE)
      updateButton(session,'btn13',disabled=TRUE)
      updateButton(session,'btn14',disabled=TRUE)
      updateButton(session,'btn15',disabled=TRUE)
      updateButton(session,'btn16',disabled=TRUE)
      updateButton(session,'btn17',disabled=TRUE)
      updateButton(session,'btn18',disabled=TRUE)
      updateButton(session,'btn19',disabled=TRUE)
      updateButton(session,'btn20',disabled=TRUE)
    }
  })
  # When more than 10 buttons have been clicked, disable the submit button.
  observe({
    if (sum(val$btn) > 10){
      updateButton(session, "submit", disabled = TRUE)
    }
  })

  ## Counter: count how many buttons have been clicked
  output$number <- renderText(sum(val$btn))
  output$num <- renderUI({
    paste("You have selected", sum(val$btn), "mice.")
  })

  ## Submit button
  #When the submit button is clicked, redirect to the next page.
  observeEvent(input$submit,{
    updateTabItems(session, "tabs", "summary")
  })

  ## Compare button
  # When the "compare" button is clicked, redirect to the computer page.
  observeEvent(input$compare,{
    updateTabItems(session, "tabs", "computer")
  })

  #use the "model" function and input theta
  #TuM <- model(data,val,input$theta)

  #Define vectors for the summary data frame
  totalSelected <- c(10, 10)
  aveWeight <- c(sum(val$btn * data[,"Weight(g)"])/10, sum((1-val$btn)*data[,"Weight(g)"])/10)
  aveAge <- c(sum(val$btn * data[,"Age(wks)"])/10, sum((1-val$btn)*data[,"Age(wks)"])/10)
  aveTu <- c(sum(val$btn * TuM)/10, sum((1-val$btn) * TuM)/10)
  gend <- c(sum(val$btn * data[,"Gender"])/10, sum((1-val$btn) * data[,"Gender"])/10)
  col <- c(sum(val$btn * data[,"Color"])/10, sum((1-val$btn) * data[,"Color"])/10)

  #Create dataframe for summary table
  # miceSumData <- data.frame(
  #   Group = c("Raspberry Group","Control Group"),
  #   totalSelected,
  #   aveWeight,
  #   aveAge,
  #   aveTu,
  #   gend,
  #   col)
  # names(miceSumData) <- c("Total Selected",
  #                         "Mean Weight (g)",
  #                         "Mean Age (wks)",
  #                         "Mean Tumor Mass (g)",
  #                         "Proportion Female",
  #                         "Proportion Brown")
  #Prepare summary table for summary tab
  # output$miceSum <- DT::renderDT(
  #   expr = miceSumData,
  #   caption = "Hand Selection Data",
  #   style = "bootstrap4",
  #   rownames = FALSE,
  #   options = list(
  #     responsive = TRUE,
  #     scrollX = TRUE,
  #     columnDefs = list(
  #       # Notice the use of ncol on your data frame; leave the 1 as is.
  #       list(className = 'dt-center', targets = 1:ncol(miceSumData))
  #     )
  #   )
  # )
  #
  # output$weight = renderPlot({
  #   wei = sum(val$btn * data[,"Weight(g)"])/10
  #   weiC = sum((1 - val$btn) * data[,"Weight(g)"])/10
  #   barplot(c(wei,weiC),
  #           names.arg = c("Raspberry","Control"),
  #           main = "Comparison of Average Weight",
  #           ylab = "Weight(g)",
  #           ylim = c(0,60),
  #           col = c("#C7053D","beige"),
  #           cex.axis = 1.25,
  #           cex.names = 1.25)
  # }, width = 250, height = 350)
  #
  # output$age = renderPlot({
  #   age = sum(val$btn * data[,"Age(wks)"])/10
  #   ageC = sum((1 - val$btn) * data[,"Age(wks)"])/10
  #   barplot(c(age,ageC),
  #           names.arg = c("Raspberry","Control"),
  #           main = "Comparison of Average Age",
  #           ylab = "Age(wks)",
  #           ylim = c(0,12),
  #           col = c("#C7053D","beige"),
  #           cex.axis = 1.25,
  #           cex.names = 1.25
  #   )
  # }, width = 250, height = 350)
  #
  # output$tumor = renderPlot({
  #   TuM = model(data,val,input$theta)
  #   Tum = sum(val$btn * TuM)/10
  #   TumC = sum((1-val$btn) * TuM)/10
  #   barplot(c(Tum,TumC,(TumC-Tum)),
  #           names.arg = c("Raspberry Group","Control Group","Difference"),
  #           main = "Comparison of Tumor Mass",
  #           ylab = "Tumor Mass(mg)",
  #           ylim = c(0,600),
  #           col = c("#C7053D","beige","#1C2C5B"),
  #           width = 5,
  #           xlim = c(1,30),
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
  # output$gender = renderPlot({
  #   barplot(prop.table(rbind(c((sum(val$btn * data[,"Gender"])),
  #                              (sum((1 - val$btn) * data[,"Gender"]))),
  #                            c((sum(val$btn * (1 - data[,"Gender"]))),
  #                              (sum((1 - val$btn) * (1 - data[,"Gender"]))))),2),
  #           col = c("#FBB4AE","#B3CDE3"),
  #           names.arg = c("Raspberry","Control"),
  #           main = "Comparison of gender",
  #           width = 6,xlim = c(1,16),
  #           cex.axis = 1.25,
  #           cex.names = 1.25
  #   )
  #   legend(
  #     x = "right",
  #     c("Female","Male"),
  #     col = c("#FBB4AE","#B3CDE3"),
  #     fill = c("#FBB4AE","#B3CDE3")
  #   )
  # },width = 270, height = 350)
  #
  # output$color = renderPlot({
  #   barplot(prop.table(rbind(c((sum(val$btn * data[,"Color"])),
  #                              (sum((1 - val$btn) * data[,"Color"]))),
  #                            c((sum(val$btn * (1 - data[,"Color"]))),
  #                              (sum((1 - val$btn) * (1 - data[,"Color"]))))),2),
  #           col = c("#BE996E","black"),
  #           names.arg = c("Raspberry","Control"),
  #           main = "Comparison of colors",
  #           width = 6,xlim = c(1,16),
  #           cex.axis = 1.25,
  #           cex.names = 1.25
  #   )
  #   legend(
  #     x = "right",
  #     c("Brown","Black"),
  #     col = c("#BE996E","black"),
  #     fill = c("#BE996E","black")
  #   )
  # },width = 270, height = 350)
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