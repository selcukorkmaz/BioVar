library(shiny)
shinyUI(pageWithSidebar(
  
  headerPanel("BioVar: Biological Variation Analysis Tool v.1.0"),
  
  sidebarPanel(width=3,
    conditionalPanel(condition="input.tabs1=='Introduction'"
    ),

    conditionalPanel(condition="input.tabs1=='Analysis'",
                     
                     h4("Select Variables"),
                     
                     selectInput(inputId = "analyte", label = "Analyte", multiple = FALSE, choices = NULL, selected = NULL),
                     selectInput(inputId = "subject", label = "Subject", multiple = FALSE, choices = NULL, selected = NULL),
                     selectInput(inputId = "gender", label = "Gender", multiple = FALSE, choices = NULL, selected = NULL),
                     selectInput(inputId = "time", label = "Time", multiple = FALSE, choices = NULL, selected = NULL),
                     selectInput(inputId = "replicate", label = "Replicate", multiple = FALSE, choices = NULL, selected = NULL),
              
                      # checkboxInput("subsetAnalysis", "Subset Analysis", value = TRUE),

                     # selectInput(inputId = "method", label = "Analysis method", multiple = FALSE, choices = c("CV-ANOVA" = "cvAnova", "ANOVA" = "lme"), selected = "lme"),

                     selectInput(inputId = "showResult", label = " Choose calculation data", multiple = FALSE, choices = c("Original" = "original", "Log-transformed" = "lnTransformed", "Transform back to original" = "transformBack", "CV-ANOVA" = "cv", "MoM" = "mom", "lnMoM" = "lnmom"), selected = "original"),
                     
                     checkboxInput(inputId = "advancedOptions", label = "Advanced Options", value = FALSE),
                     
                     conditionalPanel(condition = "input.advancedOptions",
                          selectInput(inputId = "subgroupTest", label = "Select subgroup test", multiple = FALSE, choices = c("Student's t" = "ttest", "Mann-Whitney U" = "mw")),
                                      
                         # checkboxInput(inputId = "logTransform", label = "Apply log transformation", value = FALSE), 
                         selectInput(inputId = "normalityTest", label = "Select normality test", multiple = FALSE, choices = c("Shapiro-Wilk" = "sw", "Anderson-Darling" = "ad" ), selected = "sw"),
                         selectInput(inputId = "steadyStateCenter", label = "Select central tendency measure for steady state", multiple = FALSE, choices = c("Median" = "median", "Mean" = "mean" ), selected = "mean"),
                         selectInput(inputId = "homogeneityTest", label = "Select a homogeneity test", multiple = FALSE, choices = c("Bartlett" = "bartlett", "Cochran" = "cochran" ), selected = "bartlett"),
                         
                         
                         checkboxInput(inputId = "outlierOptions", label = "Options for outliers", value = FALSE),
                                      
                              conditionalPanel(condition = "input.outlierOptions",
                                               radioButtons("step1Options", "Remove outlying replicates at step 1", choices = c("Remove replicates" = "replicates", "Do not remove replicates" = "none" ), selected = "replicates"),
                                               checkboxInput("step2Options", "Remove outlying subjects at step 2", TRUE),
                                               checkboxInput("step3Options", "Remove outlying subjects at step 3", TRUE)
                                               
                                               ),
                                      
                     sliderInput("decimal", "Decimals", 0, 10, 3),
                     selectInput(inputId = "alphaLevel", label = "Alpha (type I error)", multiple = FALSE, choices = c(0.10, 0.05, 0.01), selected = 0.05)
                     
                     ),
                     
                     actionButton(inputId = "run",  label = "Run Analysis", icon = icon("play", lib = "glyphicon"))
                     
    ),

    conditionalPanel(condition="input.tabs1=='Manual'"
    ),

    conditionalPanel(condition="input.tabs1=='Outlier detection'",
                    
                     actionButton(inputId = "applyOD",  label = "Apply", icon = icon("play", lib = "glyphicon"))

    ),
    
    conditionalPanel(condition="input.tabs1=='Data upload'",
                     h4("Input data"),
                     radioButtons("selectData", "", list("Upload a file" = 1, "Load example data" = 2), selected=1),
                     
                     # conditionalPanel(condition="input.selectData=='2'",
                     #                  h4("Select an example dataset:"),
                     #                  radioButtons("sampleData", "", list("Long format"=1, "Wide format"=2), selected=1)
                     # ),

                     conditionalPanel(condition="input.selectData=='1'",
                                      
                                      h4("Select a data format:"),
                                      
                                      radioButtons("dataInput", "", list("Long format"=1,"Wide format"=2), selected=1),
                                      
                                      h5("Upload a delimited text file: "),

                                      conditionalPanel(condition="input.dataInput=='1'",
                                      
                                      fileInput("uploadLong", "", multiple = FALSE),
                                      #checkboxInput("groupVar", "Group variable (default is last column)", TRUE),
                                      #conditionalPanel(condition="input.groupVar",
                                      #radioButtons("firstLast", "", list("None"=0, "First column"=1, "Last column"=2),selected=2)),
                                      
                                      #radioButtons("firstLastUpload", "Group variable (default is none)", list("None"=0, "First column"=1, "Last column"=2),selected=0),  ## firstLast variable for uploaded data.
                                      #radioButtons("firstLast", "Group variable(default is last column)", list("None"=0, "First column"=1, "Last column"=2),selected=2),
                                      
                                      checkboxInput("delimiter", "Delimiter"),
                                      
                                      conditionalPanel(condition="input.delimiter",
                                                       
                                                       radioButtons("fileSepDF", "", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2)
                                                       
                                      )

                     ),
                     
                     conditionalPanel(condition="input.dataInput=='2'",

                                      fileInput("uploadWide", "", multiple = FALSE),
                           
                                      checkboxInput("delimiter", "Delimiter"),
                                      
                                      conditionalPanel(condition="input.delimiter",
                                      
                                        radioButtons("fileSepDFWide", "", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2)
                                      
                                      ),
                                      
                                      textInput(inputId = "analyteWide", label = "Enter an analyte name"),
                                      selectInput(inputId = "subjectWide", label = "Select header for subject", multiple = FALSE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "genderWide", label = "Select header for gender", multiple = FALSE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "replicateWide", label = "Select header for replicate", multiple = FALSE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "timeRange", label = "Select multiple time points", multiple = TRUE, choices = NULL, selected = NULL),
                                      
                                      
                                      
                                      br(),
                                      
                                      actionButton(inputId = "runWideToLong",  label = "Convert to long format", icon = icon("play", lib = "glyphicon"))
                              
                     ))
               
    )
    
    
    
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               
               h4("Analyze biological variation for analytes!"),
               
          HTML('<p> The steps of analyzing the BV data can be simply divided into seven steps: (i) detecting outliers, (ii) controlling normality assumption, (iii) checking steady-state condition, (iv) checking homogeneity assumptions, (v) performing subset analysis for genders, (vi) performing analysis of variance and (vii) identifying analytical performance specifications.</p>'),
          
          HTML('This online tool can perform statistical analysis for the BV data based on guideline by Bartlett et al. [1], the pipeline by Braga and Panteghini [2] and the updated checklist by Aarsand et al. [3].</p>'),

          HTML('<left><img src="intro/intro1.png" width = "50%"></left><left><img src="intro/intro3.png" width = "50%"></left>'),
          
          
          h6("[1] Bartlett WA, Braga F, Carobene A, Coskun A, Prusa R, et al. A checklist for critical appraisal of studies of biological variation. Clin Chem Lab Med 2015; 53(6):879-85."),
          h6("[2] Braga, F., & Panteghini, M. (2016). Generation of data on within-subject biological variation in laboratory medicine: an update. Critical reviews in clinical laboratory sciences, 53(5), 313-325."),
          h6("[3] Aarsand AK, Roraas T, Fernandez-Calle P, Ricos C, Diaz-Garzon J, et al. The biological variation data critical appraisal checklist: a standard for evaluating studies on biological variation. Clin Chem 2018; 64(3):501-14.")
               
      ),
      
      tabPanel("Data upload",
               navbarPage(
                 title = '',
                 tabPanel('Data', DT::dataTableOutput('RawData'))
               )
      ),
      
      
      tabPanel("Analysis",
               
               navbarPage(
                 title = '',
                 tabPanel('Outliers', 
               

                       # h4(textOutput(outputId = "outlierTitle")),
                       h4(textOutput(outputId = "outlierTitleStep1")),
                       DT::dataTableOutput("outlierStepRes1"),
        
                       h4(textOutput(outputId = "outlierTitleStep2")),
                       DT::dataTableOutput("outlierStepRes2"),
        
                        h4(textOutput(outputId = "outlierTitleStep3")),
                       DT::dataTableOutput("outlierStepRes3")
               
               ),
               
               tabPanel('Normality', 
               
                   h4(textOutput(outputId = "normalityTitleStep1")),
                   DT::dataTableOutput("normalityStep1"),
    
                   h4(textOutput(outputId = "normalityTitleStep2")),
                   DT::dataTableOutput("normalityStep2")
                   
               ),
               
               tabPanel('Steady State', 
                        
                        h4(textOutput(outputId = "steadyStateResText")),
                        DT::dataTableOutput("steadyStateRes"),
                        HTML("<br>"),
                        HTML("<br>"),
                        h4(textOutput(outputId = "steadyStatePlotText")),
                        shiny::plotOutput("steadyStatePlot")
                        
               ),
               
               
               tabPanel('Homogeneity', 
                        
                        h4(textOutput(outputId = "bartlettAnalyticalText")),
                        DT::dataTableOutput("bartlettAnalytical"),     
                        
                        h4(textOutput(outputId = "bartlettwithinText")),
                        DT::dataTableOutput("bartlettWithin")
                        
               ),
               
               
               tabPanel('Subset', 

                  h4(textOutput(outputId = "subsetTitleStepBetween")),
                  DT::dataTableOutput("subsetBetween"),     
                  
                  h4(textOutput(outputId = "subsetTitleStepWithin")),
                  DT::dataTableOutput("subsetWithin"),     
                  
                  h4(textOutput(outputId = "subsetTitleStepAnalytical")),
                  DT::dataTableOutput("subsetAnalytical"),     
                                                
                   h4(textOutput(outputId = "subsetTitleStep1")),
                   DT::dataTableOutput("ttest")
                   # 
                   # h4(textOutput(outputId = "subsetTitleStep2")),
                   # DT::dataTableOutput("ttestSIA"),
                   # 
                   # h4(textOutput(outputId = "subsetTitleStep3")),
                   # DT::dataTableOutput("homogenity"),
                   # 
                   # h4(textOutput(outputId = "subsetTitleStep4")),
                   # DT::dataTableOutput("homogenitySIA")
               
               ),
               
                tabPanel('ANOVA', 
               
                navbarPage(
                  title = '',
                        
                    tabPanel('All Subjects',       
                         
                    h4(textOutput(outputId = "RCVTitleAllStep1")),
                    DT::dataTableOutput("rcvAll"), 
                    
                    h4(textOutput(outputId = "anovaTitleAllStep1")),
                    DT::dataTableOutput("CVResultsAll"),         
                              
                    h4(textOutput(outputId = "anovaTitleAllStep2")),
                    DT::dataTableOutput("CVtableAll"),
   
                     
                    h4(textOutput(outputId = "anovaTitleAllStep3")),
                    DT::dataTableOutput("ErrorTableAll")
                     
                  ),
                  
                  tabPanel('Subgroup 1', 
                           
                       h4(textOutput(outputId = "RCVTitleGender1Step1")),
                       DT::dataTableOutput("RCVtableGender1"),       
                       
                       h4(textOutput(outputId = "anovaTitleGender1Step1")),
                       DT::dataTableOutput("CVResultsGender1"),
                           
                       h4(textOutput(outputId = "anovaTitleGender1Step2")),
                       DT::dataTableOutput("CVtableGender1"),
                       
                       h4(textOutput(outputId = "anovaTitleGender1Step3")),
                       DT::dataTableOutput("ErrorTableGender1")
                       
                  ),
                  
                  tabPanel('Subgroup 2', 
                           
                           
                   h4(textOutput(outputId = "RCVTitleGender2Step1")),
                   DT::dataTableOutput("RCVtableGender2"),        
                   
                   h4(textOutput(outputId = "anovaTitleGender2Step1")),
                   DT::dataTableOutput("CVResultsGender2"),
                           
                   h4(textOutput(outputId = "anovaTitleGender2Step2")),
                   DT::dataTableOutput("CVtableGender2"),
              
                   h4(textOutput(outputId = "anovaTitleGender2Step3")),
                   DT::dataTableOutput("ErrorTableGender2")
                  )
               )
               ),
               
               tabPanel('Plots',
                        
                      h4(textOutput(outputId = "plotTitleStep0")),  
                      downloadButton("downloadPlotStep0", "Download Plot"),
                      shiny::plotOutput("plotStep0"),
                      
                      
                      h4(textOutput(outputId = "plotTitleStep1")),  
                      downloadButton("downloadPlotStep1", "Download Plot"),
                      shiny::plotOutput("plotStep1"),
                      
                      h4(textOutput(outputId = "plotTitleStep2")),  
                      downloadButton("downloadPlotStep2", "Download Plot"),
                      shiny::plotOutput("plotStep2"),
                      
                      h4(textOutput(outputId = "plotTitleStep3")),  
                      downloadButton("downloadPlotStep3", "Download Plot"),
                      shiny::plotOutput("plotStep3")
                ),
               
               
               tabPanel('Report',
                        
                        downloadButton("downloadReport", "Download report"),
                        radioButtons('format', 'Document format', c('HTML'),#'PDF',  'Word'),
                                     inline = TRUE)
                        # downloadButton('downloadReport', "Download HTML Report")
                        
               )
               )
               ),
      
      
      tabPanel("Manual",
               h5("Usage of the web-tool"),
               HTML('<p>In order to use this application,</p>'),
               
               HTML('<p> <b>1. Load your data set using <em>Data upload</em> tab. You can upload your dataset in two different ways:</b></p>'),
               HTML('<p><b>(i) Long format:</b></p>'),
               HTML('<center><img src="manual/dataUploadLong.png" width = "100%"></center>'),
               br(),
               
               HTML('<p><b>(ii) Wide format:</b> enter an analyte name, select subject, gender, replicate columns and define the time range and click <b>Convert to long format button</b>.</p>'),
               HTML('<center><img src="manual/dataUploadWide.png" width = "100%"></center>'),
               
               br(),
               HTML('<p> <b>2. After uploading appropriate dataset, move on to the <em>Analysis</em> tab. 
                    Select <em>analyte</em>, <em>subject</em>, <em>gender</em>, <em>time</em> and <em>replicate</em> variables. 
                    Choose a calculation data based on original or transformation including <em>Log-transformed</em>, <em>CV-ANOVA</em>, <em>MoM</em> and <em>lnMoM</em>.'),
               HTML('<p><b>(i) get outlier results for:</b></p>'),
               HTML('<center><img src="manual/outliers.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(ii) get normality results:</b></p>'),
               HTML('<center><img src="manual/normality.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(iii) get steady-state results:</b></p>'),
               HTML('<center><img src="manual/steadystate.png" width = "100%"></center>'),
               
               
               br(),
               HTML('<p><b>(iv) get homogeneity results:</b></p>'),
               HTML('<center><img src="manual/homogeneity.png" width = "100%"></center>'),
               
               
               br(),
               HTML('<p><b>(v) get subset analysis results:</b></p>'),
               HTML('<center><img src="manual/subset.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(vi) get ANOVA, RCV and II results for all subjects:</b></p>'),
               HTML('<center><img src="manual/anovaAll.png" width = "100%"></center>'),

               br(),
               HTML('<p><b>(vii) get ANOVA, RCV and II results for females:</b></p>'),
               HTML('<center><img src="manual/anovaGender1.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(viii) get ANOVA, RCV and II results for males:</b></p>'),
               HTML('<center><img src="manual/anovaGender2.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(ix) get plots:</b></p>'),
               HTML('<center><img src="manual/plots1.png" width = "100%"></center>'),
               
               br(),
               HTML('<p><b>(x) generate and download the report for the analyte to be analyzed:</b></p>'),
               HTML('<center><img src="manual/report.png" width = "100%"></center>')
               
      ),
     
      
      id="tabs1"
    ),
    
    tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
              tags$style(type="text/css", "select { max-width: 200px; }"),
              tags$style(type="text/css", "textarea { max-width: 185px; }"),
              tags$style(type="text/css", ".jslider { max-width: 200px; }"),
              tags$style(type='text/css', ".well { max-width: 330px; }"),
              tags$style(type='text/css', ".span4 { max-width: 330px; }")),
    
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon-2.ico"))
    
  )
 )
)




