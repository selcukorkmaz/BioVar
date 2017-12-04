#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(lme4)
library(multcomp)
library(nlme)
library(ggplot2)
library(dplyr)
library(prospectr)
library(nortest)
source("outlier.R")
source("normality.R")
source("subsetAnalysis.R")
source("analysisOfVariance.R")
source("wideToLong.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dataM <- reactive({  ## Data input.
    if(input$dataInput==1){  ## Load example data.
      
      inFile <- input$uploadLong
      
      mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
      
      if (is.null(input$uploadLong))  {return(NULL)}
      
   
    }
    
   else if(input$dataInput==2){  ## Upload data.
      
      inFile <- input$uploadWide
      
      mySep <- switch(input$fileSepDFWide, '1'=",",'2'="\t",'3'=";", '4'="")
      
      if (is.null(input$uploadWide))  {return(NULL)}
      
   
   }
    
    if (file.info(inFile$datapath)$size <= 10485800){
      data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE, na.strings = c("", "NA","."))
    }
    
    else print("File is bigger than 10MB and will not be uploaded.")

  }
  )
  
  
  dataWideToLong <- reactive({
    
    if(input$runWideToLong){
      dataLong = wideToLong(data = dataM(), subject = input$subjectWide, gender = input$genderWide, timeRange = input$timeRange, replicate = input$replicateWide, analyte = input$analyteWide)
      
      dataLong
      
    }
    
  })
  
  
  dataAnalysis <- reactive({
    
    if(input$dataInput==1){
      dataM()
      
    }
    
    else if(input$dataInput==2){
      
      if(input$runWideToLong){
        dataWideToLong()
        
      }
    }
    
    
  })
   
  output$RawData <- renderDataTable(
    
        dataAnalysis(), options = list(iDisplayLength = 10)
    
    )

  
  observe({
    
    updateSelectInput(session, "analyte", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[3])
    updateSelectInput(session, "time", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[7])
    updateSelectInput(session, "replicate", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[8])
    updateSelectInput(session, "subject", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[1])
    updateSelectInput(session, "gender", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[2])
    
  })
  
  
  observe({
    
    # updateSelectInput(session, "analyteWide", choices = colnames(dataAnalysis()), selected = colnames(dataAnalysis())[3])
    updateSelectInput(session, "timeRange", choices = colnames(dataM()), selected = colnames(dataM())[7])
    updateSelectInput(session, "replicateWide", choices = colnames(dataM()), selected = colnames(dataM())[8])
    updateSelectInput(session, "subjectWide", choices = colnames(dataM()), selected = colnames(dataM())[1])
    updateSelectInput(session, "genderWide", choices = colnames(dataM()), selected = colnames(dataM())[2])
    
  })
  
  
  
  
  ### Outlier results ##########
  
  
  outlierResults <- reactive({
    
    outlier(data=dataAnalysis(), analyte = input$analyte, replicate = input$replicate, time = input$time, gender = input$gender, subject = input$subject)
    
  }) 
  
  dataSetWithoutOutliers <- reactive({
    
    outlierResults()[[4]]    
  }) 
  
  
  ### Outlier results: Step1 ##########
  
  output$outlierStep1 <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(outlierResults()[[1]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
  })
  
  ### Outlier results: Step2 ##########
  
  output$outlierStep2 <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(outlierResults()[[2]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
  })
  
  ### Outlier results: Step3 ##########
  
  output$outlierStep3 <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(outlierResults()[[3]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
  })
  
  
  
  ### Normality ##########
  
  
  normalityResults <- reactive({
    
    normality(data=dataSetWithoutOutliers(), subject = input$subject, analyte = input$analyte)
    
  }) 
  
  ### Normality: Step1 ##########
  output$normalityStep1 <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(normalityResults()[[1]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
  })
  
  ### Normality: Step2 ##########
  
  output$normalityStep2 <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(normalityResults()[[2]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
  })
  
  
  ### Subset Analysis ##########
  
  subsetResults <- reactive({
    
    subsetAnalysis(data=dataSetWithoutOutliers(), subject = input$subject, gender = input$gender, analyte = input$analyte, CVresult = input$showResults)
  }) 
  
  
  ### Homogenity ##########
  
  output$homogenity <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(subsetResults()[[1]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
  })
  
  
  
  ### t test for means ##########
  
  output$ttest <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(subsetResults()[[2]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
  })
  
  
  
  ### Homogenity SIA ##########
  
  output$homogenitySIA <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(subsetResults()[[3]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
  })
  
  
  
  ### ttest SIA ##########
  
  output$ttestSIA <- DT::renderDataTable(server = FALSE, {
    
    if(input$run){
      
      datatable(subsetResults()[[4]], extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
  })
  
  
  
  ######################## Analysis of variance ############
  
  analysisOfVarinceResults <- reactive({
    
    analysisOfVariance(dataSetWithoutOutliers(), subject= input$subject, gender = input$gender, replicate = input$replicate, time = input$time, subset = input$subsetAnalysis, CVresult = input$showResults, method = input$method)
    # analysisOfVariance(data, subject= "subject", gender = "Gender", replicate = "replicate", time = "time", subset = T, CVresult = "transformed")
    
    
  })
  
  
  ######### All ##############
  
  output$CVtableAll <- DT::renderDataTable(server = FALSE, {
    
    res = analysisOfVarinceResults()
    
    if(input$run && input$showResults == "original"){
      
      result = datatable(res$resultsAllOriginal$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformed"){
      
      result =  datatable(res$resultsAllTransformed$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformBack"){
      
      result = datatable(res$resultsAllTransformBack$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
  })
  
  
  output$CVResultsAll <- DT::renderDataTable(server = FALSE, {
    
    res = analysisOfVarinceResults()
    
    if(input$run && input$showResults == "original"){
      
      result = datatable(res$resultsAllOriginal$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformed"){
      
      result = datatable(res$resultsAllTransformed$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformBack"){
      
      result = datatable(res$resultsAllTransformBack$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
  })
  
  
  output$ErrorTableAll <- DT::renderDataTable(server = FALSE, {
    
    res = analysisOfVarinceResults()
    
    if(input$run && input$showResults == "original"){
      
      result = datatable(res$resultsAllOriginal$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformed"){
      
      result = datatable(res$resultsAllTransformed$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
    else if(input$run && input$showResults == "transformBack"){
      
      result = datatable(res$resultsAllTransformBack$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
    }
    
    
  })
  
  
  ############  Gender1 ###########
  
  
  
  output$CVtableGender1 <- DT::renderDataTable(server = FALSE, {
    
    if(input$subsetAnalysis){
      
      res = analysisOfVarinceResults()
      
      if(input$run && input$showResults == "original"){
        
        result =  datatable(res$resultsOriginalGender1$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformed"){
        
        result = datatable(res$resultsTransformedGender1$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result = datatable(res$resultsTransformBackGender1$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
    }
  })
  
  
  output$CVResultsGender1 <- DT::renderDataTable(server = FALSE, {
    if(input$subsetAnalysis){
      res = analysisOfVarinceResults()
      
      if(input$run && input$showResults == "original"){
        
        result = datatable(res$resultsOriginalGender1$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      else if(input$run && input$showResults == "transformed"){
        
        
        result = datatable(res$resultsTransformedGender1$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result =  datatable(res$resultsTransformBackGender1$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
    }
    
  })
  
  
  output$ErrorTableGender1 <- DT::renderDataTable(server = FALSE, {
    
    if(input$subsetAnalysis){
      
      res = analysisOfVarinceResults()
      
      
      if(input$run && input$showResults == "original"){
        
        result = datatable(res$resultsOriginalGender1$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformed"){
        
        result = datatable(res$resultsTransformedGender1$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result = datatable(res$resultsTransformBackGender1$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
    }
  })
  
  
  
  
  ############  Gender 2 ###########
  
  
  
  output$CVtableGender2 <- DT::renderDataTable(server = FALSE, {
    
    if(input$subsetAnalysis){
      res = analysisOfVarinceResults()
      
      if(input$run && input$showResults == "original"){
        
        result =  datatable(res$resultsOriginalGender2$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformed"){
        
        result = datatable(res$resultsTransformedGender2$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result = datatable(res$resultsTransformBackGender2$CVTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
    }
  })
  
  
  output$CVResultsGender2 <- DT::renderDataTable(server = FALSE, {
    
    if(input$subsetAnalysis){
      
      result = res = analysisOfVarinceResults()
      
      if(input$run && input$showResults == "original"){
        
        result =  datatable(res$resultsOriginalGender2$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformed"){
        
        result =   datatable(res$resultsTransformedGender2$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result = datatable(res$resultsTransformBackGender2$CVresults, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
    }
  })
  
  
  output$ErrorTableGender2 <- DT::renderDataTable(server = FALSE, {
    
    if(input$subsetAnalysis){
      
      res = analysisOfVarinceResults()
      
      
      if(input$run && input$showResults == "original"){
        
        result =  datatable(res$resultsOriginalGender2$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformed"){
        
        result =  datatable(res$resultsTransformedGender2$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
      
      
      else if(input$run && input$showResults == "transformBack"){
        
        result = datatable(res$resultsTransformBackGender2$errorTable, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(           dom = 'Bfrtip',buttons = list('copy', 'print', list(extend = 'collection',           buttons = c('csv', 'excel', 'pdf'),text = 'Download')), keys = TRUE))
      }
    }
  })
  
  
  
  
  ################ Titles #################
  output$outlierTitle <- renderText({
    
    if (input$run){
      "1. Outlier Detection"
    }
    
  })
  
  
  output$outlierTitleStep1 <- renderText({
    
    if (input$run){
      "Step 1: Outliers in the sets of duplicate results using the Cochran test"
    }
    
  })
  
  
  output$outlierTitleStep2 <- renderText({
    
    if (input$run){
      "Step 2: Outliers in the variances of the results from each subject using the Cochran test"
    }
    
  })
  
  output$outlierTitleStep3 <- renderText({
    
    if (input$run){
      "Step 3: Reed's criterion to see if any individual has a mean value that differs greatly from the other subjects"
    }
    
  })
  
  
  output$normalityTitleStep1 <- renderText({
    
    if (input$run){
      "Step 1: On set of results from each individual "
    }
    
  })
  
  output$normalityTitleStep2 <- renderText({
    
    if (input$run){
      "Step 2: On mean values of subjects"
    }
    
  })
  
  
  output$subsetTitleStep1 <- renderText({
    
    if (input$run){
      "Step 1: Student's t test for mean differences of gender"
    }
    
  })
  
  
  output$subsetTitleStep2 <- renderText({
    
    if (input$run){
      "Step 2: Student's t test for average within-subject total variance"
    }
    
  })
  
  
  output$anovaTitleAllStep1 <- renderText({
    
    if (input$run){
      "Step 1: Coefficient of Variation Results"
    }
    
  })
  
  
  output$anovaTitleAllStep2 <- renderText({
    
    if (input$run){
      "Step 2: Analysis of Variance Table"
    }
    
  })
  
  
  output$anovaTitleAllStep3 <- renderText({
    
    if (input$run){
      "Step 3: Quality Measures"
    }
    
  })
  
  
  
  
  
  output$anovaTitleGender1Step1 <- renderText({
    
    if (input$run){
      "Step 1: Coefficient of Variation Results"
    }
    
  })
  
  
  output$anovaTitleGender1Step2 <- renderText({
    
    if (input$run){
      "Step 2: Analysis of Variance Table"
    }
    
  })
  
  
  output$anovaTitleGender1Step3 <- renderText({
    
    if (input$run){
      "Step 3: Quality Measures"
    }
    
  })
  
  
  
  output$anovaTitleGender2Step1 <- renderText({
    
    if (input$run){
      "Step 1: Coefficient of Variation Results"
    }
    
  })
  
  
  output$anovaTitleGender2Step2 <- renderText({
    
    if (input$run){
      "Step 2: Analysis of Variance Table"
    }
    
  })
  
  
  output$anovaTitleGender2Step3 <- renderText({
    
    if (input$run){
      "Step 3: Quality Measures"
    }
    
  })
  
  
  output$plotTitleStep0 <- renderText({
    
    if (input$run){
      "Step 0: Plot before outlier detection"
    }
    
  })
  
  output$plotTitleStep1 <- renderText({
    
    if (input$run){
      "Step 1: Plot after step 1 outlier detection"
    }
    
  })
  
  
  output$plotTitleStep2 <- renderText({
    
    if (input$run){
      "Step 2: Plot after step 2 outlier detection"
    }
    
  })
  
  output$plotTitleStep3 <- renderText({
    
    if (input$run){
      "Step 3: Plot after step 3 outlier detection"
    }
    
  })
  ############## Plots ##################
  
  ####### Step 0 #######
  
  output$plotData <- DT::renderDataTable({
    
    
  })
  
  
  output$plotStep0 <- renderPlot({
    
    if(input$run){  
      
      data = dataAnalysis()
      data$replicate.subject = as.numeric(paste0(data[,input$replicate], data[,input$subject]))
      
      
      names = c(input$subject, input$gender, input$time, input$replicate, "replicate.subject", input$analyte)
      
      
      dataFull = data[,names]
      names(dataFull)[6] = "value"
      
      dataFull = dataFull[complete.cases(dataFull),]
      table(dataFull$value <=0)
      dataFull[dataFull$value <=0,"value"] = NA
      
      dataFull = dataFull[complete.cases(dataFull),]
      
      tbl = table(dataFull$subject)
      
      naSubject = tbl[!(tbl%%2==0)]
      naSubject2 = as.numeric(names(naSubject))
      
      if(length(naSubject2) >0){
        for(i in 1:length(naSubject2)){
          
          nas3 = dataFull[dataFull$subject == naSubject2[i],]
          
          tbl2 = table(nas3$time)
          
          nas4 = tbl2[!(tbl2%%2==0)]
          nas5 = as.numeric(names(nas4))
          
          for(j in 1:length(nas5)){
            
            dataFull[dataFull[,input$subject] == naSubject2[i] & dataFull[,input$time] == nas5[j],"value"] = NA
            
            
          }
          
        }
        
      }
      
      dataFull = dataFull[complete.cases(dataFull),]
      
      dataFull = dataFull[with(dataFull, order(eval(parse(text=input$gender)))), ]
      
      
      
      data_grp <- eval(substitute(group_by(dataFull, eval(parse(text=input$subject)))))
      plotData <- dplyr::summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
      colnames(plotData)[1] = input$subject
      
      gender = as.data.frame(data_grp[,c(input$subject, input$gender)][!duplicated(data_grp[,c(input$subject, input$gender)]), ])
      
      plotData = merge(plotData, gender, by = input$subject)
      
      plotData[,input$subject] <- as.factor(plotData[,input$subject])
      
      plotData = plotData[with(plotData, order(eval(parse(text=input$gender)))), ]
      
      
      ggplot(plotData, aes_string(y = "Mean", x =input$subject, color = input$gender)) +
        geom_point() +
        geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
        coord_flip() +
        theme_bw(base_size = 14) +
        ylab(input$analyte) +
        xlab("Subjects")+
        scale_x_discrete(limits=plotData[,input$subject])
      
    }
  })
  
  ####### Step 1 #######
  
  output$plotStep1 <- renderPlot({
    
    data = outlierResults()[[5]]
    
    names = c(input$subject, input$gender, input$time, input$replicate, "replicate.subject", "value")
    
    
    dataFull = data[,names]
    
    
    data_grp <- eval(substitute(group_by(dataFull, eval(parse(text=input$subject)))))
    plotData <- dplyr::summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
    colnames(plotData)[1] = input$subject
    
    gender = as.data.frame(data_grp[,c(input$subject, input$gender)][!duplicated(data_grp[,c(input$subject, input$gender)]), ])
    
    plotData = merge(plotData, gender, by = input$subject)
    
    plotData[,input$subject] <- as.factor(plotData[,input$subject])
    
    plotData = plotData[with(plotData, order(eval(parse(text=input$gender)))), ]
    
    
    ggplot(plotData, aes_string(y = "Mean", x =input$subject, color = input$gender)) +
      geom_point() +
      geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
      coord_flip() +
      theme_bw(base_size = 14) +
      ylab(input$analyte) +
      xlab("Subjects")+
      scale_x_discrete(limits=plotData[,input$subject])
    
    
    
  })
  
  ####### Step 2 #######
  
  output$plotStep2 <- renderPlot({
    
    data = outlierResults()[[6]]
    
    names = c(input$subject, input$gender, input$time, input$replicate, "replicate.subject", "value")
    
    
    dataFull = data[,names]
    
    
    data_grp <- eval(substitute(group_by(dataFull, eval(parse(text=input$subject)))))
    plotData <- dplyr::summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
    colnames(plotData)[1] = input$subject
    
    gender = as.data.frame(data_grp[,c(input$subject, input$gender)][!duplicated(data_grp[,c(input$subject, input$gender)]), ])
    
    plotData = merge(plotData, gender, by = input$subject)
    
    plotData[,input$subject] <- as.factor(plotData[,input$subject])
    
    plotData = plotData[with(plotData, order(eval(parse(text=input$gender)))), ]
    
    
    ggplot(plotData, aes_string(y = "Mean", x =input$subject, color = input$gender)) +
      geom_point() +
      geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
      coord_flip() +
      theme_bw(base_size = 14) +
      ylab(input$analyte) +
      xlab("Subjects")+
      scale_x_discrete(limits=plotData[,input$subject])
    
    
    
  })
  
  
  ####### Step 3 #######
  
  output$plotStep3 <- renderPlot({
    
    data = outlierResults()[[4]]
    
    names = c(input$subject, input$gender, input$time, input$replicate, "replicate.subject", "value")
    
    
    dataFull = data[,names]
    
    
    data_grp <- eval(substitute(group_by(dataFull, eval(parse(text=input$subject)))))
    plotData <- dplyr::summarise(data_grp, N = n(), Mean = mean(value), Min = min(value), Max = max(value))
    colnames(plotData)[1] = input$subject
    
    gender = as.data.frame(data_grp[,c(input$subject, input$gender)][!duplicated(data_grp[,c(input$subject, input$gender)]), ])
    
    plotData = merge(plotData, gender, by = input$subject)
    
    plotData[,input$subject] <- as.factor(plotData[,input$subject])
    
    plotData = plotData[with(plotData, order(eval(parse(text=input$gender)))), ]
    
    
    ggplot(plotData, aes_string(y = "Mean", x =input$subject, color = input$gender)) +
      geom_point() +
      geom_errorbar(aes(ymax = Max, ymin = Min), width = 0.35) +
      coord_flip() +
      theme_bw(base_size = 14) +
      ylab(input$analyte) +
      xlab("Subjects")+
      scale_x_discrete(limits=plotData[,input$subject])
    
    
    
  })
  
  
  ###### Report ######
  
  
  output$downloadReport <- downloadHandler(
    filename <- function() { paste0(input$analyte,'_report.txt') },
    content <- function(file) {
      pdf(file)
      
      outlierResults = outlier(data=dataAnalysis(), analyte = input$analyte, replicate = input$replicate, time = input$time, gender = input$gender, subject = input$subject)
      
      
      dataSetWithoutOutliers = outlierResults()[[4]]
      
      ### Outlier results: Step1 ##########
      
      result = outlierResults[[1]]
      
      out <- capture.output(result)
      write.table(out, file, row.names=F, col.names=F, quote=F)
      
      
    })
  
})
