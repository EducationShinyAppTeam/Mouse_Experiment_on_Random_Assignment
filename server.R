library(shiny)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(V8)
library(shinydashboard)
library(shinyWidgets)


# Mouse app

#Use jscode to for reset button to reload the app
# jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

#Define the function to disable all the button
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

#Define the function to extract tumor mass from the raspberry experiment model
model <- function(data,val,theta){
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

compModel <- function(data,index,theta){
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

##Load and edit the data in global environment for this app
data = read.csv("database.csv")
colnames(data) = c("Color","Weight(g)","Age(wks)","TumorMass(mg)","Gender")
#Change the column color and gender from factor to numeric
data$Color = as.character(data$Color)
for (i in 1:20){
  if (data$Color[i] == "brown"){data$Color[i] = 1}
  else {data$Color[i] = 0}
}
data$Color = as.numeric(data$Color)

data$Gender = as.character(data$Gender)
for (i in 1:20){
  if (data$Gender[i] == "Female"){data$Gender[i] = 1}
  else {data$Gender[i] = 0}
}
data$Gender = as.numeric(data$Gender)


####################################################
shinyServer(function(input, output,session) {
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
  
  ##go button
  observeEvent(input$go,{
    updateTabItems(session,"tabs","hand")
  })
  
 #  observeEvent(input$stop, {
 #      reset("times")
 # })



  #Save all the actionButton input into a vector for later convenience
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

  #reload the app when the reset button is clicked
  observeEvent(input$reset_button, {js$reset()})

  #Create 20 updateButtons for 20 bsButtons in ui
  observeEvent(val$btn,({
    for (i in 1:20){
      if (val$btn[i] == 1){
        updateButton(session,paste("btn",i,sep = ""), style = "danger", disabled = TRUE)
      }
    }
  }))

  #When 10 buttons have been clicked, enable the submit button.
  ###UPDATE: unable for clicking more mice
  observe({
    if (sum(val$btn) == 10){
      updateButton(session, "submit","Submit Selection", icon = icon("hand-o-up"), size = "large", disabled = FALSE)
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
      updateButton(session,'btn20',disabled=TRUE)}
  })
  #When more than 10 buttons have been clicked, disable the submit button.
  observe({
    if (sum(val$btn) > 10){
      updateButton(session, "submit", disabled = TRUE)

    }
  })
  #When the submit button is clicked, redirect to the next page.
  observeEvent(input$submit,{
    updateTabItems(session, "tabs", "summary")
  })
  #When the "compare" button is clicked, redirect to the computer page.
  observeEvent(input$compare,{
    updateTabItems(session, "tabs", "computer")
  })

  #Counter: count how many buttons have been clicked
  output$number = renderText(sum(val$btn))
  output$num <- renderUI({
    paste("You have selected", sum(val$btn), "mice.")
  })

  #Print the average weight for experimental group
  output$aveWeight = renderText(sum(val$btn * data[,"Weight(g)"])/10)
  #Print the average age for experimental group
  output$aveAge = renderText(sum(val$btn * data[,"Age(wks)"])/10)
  
  #Print the average tumor mass for experimental group
  output$aveTu = renderText({
    #use the "model" function and input theta
    TuM = model(data,val,input$theta)
    paste(sum(val$btn * TuM)/10)
  })
  output$gend = renderText(sum(val$btn * data[,"Gender"])/10)
  output$col = renderText(sum(val$btn * data[,"Color"])/10)

  #Print everything for control group
  output$aveWeightC = renderText(sum((1-val$btn)*data[,"Weight(g)"])/10)
  output$aveAgeC = renderText(sum((1-val$btn)*data[,"Age(wks)"])/10)
  
  output$aveTuC = renderText({
    TuM = model(data,val,input$theta)
    paste(sum((1-val$btn) * TuM)/10)
  })

  output$gendC = renderText(sum((1-val$btn) * data[,"Gender"])/10)
  output$colC = renderText(sum((1-val$btn) * data[,"Color"])/10)

  output$weight = renderPlot({
    wei = sum(val$btn * data[,"Weight(g)"])/10
    weiC = sum((1 - val$btn) * data[,"Weight(g)"])/10
    barplot(c(wei,weiC),
            names.arg = c("Raspberry","Control"),
            main = "Comparison of Average Weight",
            ylab = "Weight(g)", ylim = c(0,60), col = c("#C7053D","beige"))
  }, width = 250, height = 350)

  output$age = renderPlot({
    age = sum(val$btn * data[,"Age(wks)"])/10
    ageC = sum((1 - val$btn) * data[,"Age(wks)"])/10
    barplot(c(age,ageC),
            names.arg = c("Raspberry","Control"), main = "Comparison of Average Age",
            ylab = "Age(wks)", ylim = c(0,12), col = c("#C7053D","beige"))
  }, width = 250, height = 350)

  output$tumor = renderPlot({
    TuM = model(data,val,input$theta)
    Tum = sum(val$btn * TuM)/10
    TumC = sum((1-val$btn) * TuM)/10
    barplot(c(Tum,TumC,(TumC-Tum)),
            names.arg = c("Raspberry Group","Control Group","Difference"), main = "Comparison of Tumor Mass",
            ylab = "Tumor Mass(mg)", ylim = c(0,600), col = c("#C7053D","beige","#1C2C5B"),width = 5, xlim = c(1,30))
    legend("right",c("Raspberry Group","Control Group","Difference"),col = c("#C7053D","beige","#1C2C5B"),fill=c("#C7053D","beige","#1C2C5B")
           )
  },width = 500, height = 350)

  output$gender = renderPlot({
    barplot(prop.table(rbind(c((sum(val$btn * data[,"Gender"])),
                               (sum((1 - val$btn) * data[,"Gender"]))),
                             c((sum(val$btn * (1 - data[,"Gender"]))),
                               (sum((1 - val$btn) * (1 - data[,"Gender"]))))),2),
            col = c("#FBB4AE","#B3CDE3"),names.arg = c("Raspberry","Control"),main = "Comparison of gender",width = 6,xlim = c(1,16))
    legend("right",c("Female","Male"), col = c("#FBB4AE","#B3CDE3"),fill = c("#FBB4AE","#B3CDE3"))
  },width = 270, height = 350)

  output$color = renderPlot({
    barplot(prop.table(rbind(c((sum(val$btn * data[,"Color"])),
                               (sum((1 - val$btn) * data[,"Color"]))),
                             c((sum(val$btn * (1 - data[,"Color"]))),
                               (sum((1 - val$btn) * (1 - data[,"Color"]))))),2),
            col = c("#BE996E","black"),names.arg = c("Raspberry","Control"),main = "Comparison of colors",width = 6,xlim = c(1,16))
    legend("right",c("Brown","Black"), col = c("#BE996E","black"),fill = c("#BE996E","black"))
  },width = 270, height = 350)

  ##Print raw data with assigned group
  output$dataf = renderPrint({
    data$Group = val$btn
    for (i in 1:20){
      if (data$Group[i] == 1){data$Group[i] = "Raspberry"}
      else {data$Group[i] = "Control"}
    }

    TuM = model(data,val,input$theta)
    data[,4] = TuM

    #Change the column color and gender from numeric into string
    for (i in 1:20){
      if (data$Color[i] == 1){data$Color[i] = "Brown"}
      else {data$Color[i] = "Black"}
    }

    for (i in 1:20){
      if (data$Gender[i] == 1){data$Gender[i] = "Female"}
      else {data$Gender[i] = "Male"}
    }

    # print(data)
  })

###########################################################
#Save all the vectors that will be used later in reactive environment
  table <- reactive({
    compWeight = c()
    compWeightC = c()
    compAge = c()
    compAgeC = c()
    compTum = c()
    compTumC = c()
    diffWeight = c()
    diffAge = c()
    diffTum = c()

#Use for loop to simulate many times
  # withProgress(message = "Simulating Experiments", value = 0, {
  #   n <- input$times
    for (i in 1:input$times){
      #Use random sampling to get 10 mice for experimental group each time
      exp = sample(1:20,10)
      #Get the average weight, age, tumor mass for both groups in each simulation and save as vectors
      compWeight[i] = mean(data[exp,"Weight(g)"])
      compWeightC[i] = mean(data[-exp,"Weight(g)"])
      compAge[i] = mean(data[exp,"Age(wks)"])
      compAgeC[i] = mean(data[-exp,"Age(wks)"])
      Tumor = compModel(data,exp,input$compTheta)
      compTum[i] = mean(Tumor[exp])
      compTumC[i] = mean(Tumor[-exp])
      #Get the difference between two groups in average weight, average age, average tumor mass
      diffWeight[i] = compWeight[i] - compWeightC[i]
      diffAge[i] = compAge[i] - compAgeC[i]
      diffTum[i] = compTum[i] - compTumC[i]
      # incProgress(1/n, detail = paste("Simulating Experiment", i))
    }
  # })
    
      #Create a dataframe
      meanWeight = c(mean(compWeightC), mean(compWeight))
      meanAge = c(mean(compAgeC),mean(compAge))
      meanTum = c(mean(compTumC),mean(compTum))
      meanDiff = mean(diffTum)
      compTable = data.frame(Group = c("Raspberry Group","Control Group"),
                             Selected = c(10,10),
                             Weight = meanWeight,
                             Age = meanAge,
                             Tumor = meanTum)
      names(compTable) = c("Group","Total Selected","Weight (g)","Age (wks)","Tumor Mass (mg)")

      #Return these values
      list(aveWeight = meanWeight, aveAge = meanAge, aveTum = meanTum, aveTable = compTable, Tumor = Tumor,
           diffWeight = diffWeight, diffAge = diffAge, diffTum = diffTum, aveDiff = meanDiff, exp = exp)

  })
#Using the Freedman-Diaconis Rule for bin widths
binwidth <- function(x) {
    2 * IQR(x) / (length(x)^(1/3))
}

  output$computerTable <- renderTable({
    table()$aveTable
    })

  output$compWeightBar <- renderPlot({
    barplot(table()$aveWeight,
            names.arg = c("Raspberry Group","Control Group"), main = "Comparison of Average Weight",
            ylab = "Weight(g)", col = c("#C7053D","beige"))
  }, width = 250, height = 350)

  output$compAgeBar = renderPlot({
    barplot(table()$aveAge,
            names.arg = c("Raspberry Group","Control Group"), main = "Comparison of Average Age",
            ylab = "Age(wks)", col = c("#C7053D","beige"))
  }, width = 250, height = 350)

  output$compTumorBar = renderPlot({
    barplot(c(table()$aveTum,table()$aveDiff),
            names.arg = c("Raspberry Group","Control Group","Difference"), main = "Comparison of Tumor Mass",
            ylab = "Tumor Mass(mg)", col = c("#C7053D","beige","#1C2C5B"),width = 5, xlim = c(1,30))
    legend("right",c("Raspberry Group","Control Group","Difference"),col = c("#C7053D","beige","#1C2C5B"),fill=c("#C7053D","beige","#1C2C5B")
    )
  },width = 500, height = 350)

  output$compWeightHist = renderPlot({
    qplot(table()$diffWeight,
          geom="histogram",
          binwidth = binwidth(table()$diffWeight),
          main = "Differences in Weight between groups (g)",
          xlab = "Weight (g)",
          fill=I("#1C2C5B"),
          col=I("#1C2C5B"),
          alpha=I(.8)
          )
  }, height = 350)

  output$compAgeHist = renderPlot({
    qplot(table()$diffAge,
          geom="histogram",
          binwidth = binwidth(table()$diffAge),
          main = "Differences in Age between groups (wks)",
          xlab = "Age (wks)",
          fill=I("#1C2C5B"),
          col=I("#1C2C5B"),
          alpha=I(.8)
    )
  }, height = 350)

  output$compTumorHist = renderPlot({
    qplot(table()$diffTum,
          geom="histogram",
          binwidth = binwidth(table()$diffTum),
          main = "Differences in Tumor Mass between groups (mg)",
          xlab = "Tumor Mass (mg)",
          fill=I("#1C2C5B"),
          col=I("#1C2C5B"),
          alpha=I(.8)
    )
  }, height = 350)

  ##Print raw data with assigned group
  output$compDataf = renderPrint({
    data$Group = c()
    for (i in 1:20){
      if (any(data$Group[i] == table()$exp)){data$Group[i] = "Raspberry"}
      else {data$Group[i] = "Control"}
    }

    data[,4] = table()$Tumor


    for (i in 1:20){
      if (data$Color[i] == 1){data$Color[i] = "Brown"}
      else {data$Color[i] = "Black"}
    }

    for (i in 1:20){
      if (data$Gender[i] == 1){data$Gender[i] = "Female"}
      else {data$Gender[i] = "Male"}
    }

    # print(data)

  })

})
