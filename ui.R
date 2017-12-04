library(shiny)
shinyUI(pageWithSidebar(
  
  
  headerPanel("Biological Variation Tool v.0.1"),
  
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
              
                      checkboxInput("subsetAnalysis", "Subset Analysis", value = TRUE),
                     
                     selectInput(inputId = "method", label = "Analysis method", multiple = FALSE, choices = c("ANOVA" = "anova", "Linear Mixed Effects Models" = "lme"), selected = "ANOVA"),
                     
                     selectInput(inputId = "showResults", label = "Show results for", multiple = FALSE, choices = c("Original" = "original", "Log-transformed" = "transformed", "Back log-transformed" =  "transformBack"), selected = "Original"),
                     
                     
                     actionButton(inputId = "run",  label = "Run Analysis", icon = icon("play", lib = "glyphicon"))
                     
                     
    ),
    
    
    
    conditionalPanel(condition="input.tabs1=='Manual'"
    ),

    
    # conditionalPanel(condition="input.tabs1=='Citation'"
    #                  
    # ),
    
    conditionalPanel(condition="input.tabs1=='Outlier detection'",
                    
                     
                     actionButton(inputId = "applyOD",  label = "Apply", icon = icon("play", lib = "glyphicon"))
                     
                     
    ),
    
    
    # conditionalPanel(condition="input.tabs1=='Authors & News'"
    # 
    # ),
    
    conditionalPanel(condition="input.tabs1=='Data upload'",
                     h4("Input data"),
                     radioButtons("dataInput", "", list("Long format"=1,"Wide format"=2), selected=1),
                     

                     conditionalPanel(condition="input.dataInput=='1'",
                                      h5("Upload a delimited text file: "),
                                      
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
                                      
                                      # HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                                      # HTML('<p>Note: First row must be header.</p>'),
                                      
                                      
                                      # h4("Select Variables"),
                                      # 
                                      # selectInput(inputId = "analyte", label = "Analyte", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "subject", label = "Subject", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "gender", label = "Gender", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "time", label = "Time", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "replicate", label = "Replicate", multiple = FALSE, choices = NULL, selected = NULL)
                                      
                     ),
                     
                     conditionalPanel(condition="input.dataInput=='2'",
                                      h5("Upload a delimited text file: "),
                                      
                                      fileInput("uploadWide", "", multiple = FALSE),
                                      #checkboxInput("groupVar", "Group variable (default is last column)", TRUE),
                                      #conditionalPanel(condition="input.groupVar",
                                      #radioButtons("firstLast", "", list("None"=0, "First column"=1, "Last column"=2),selected=2)),
                                      
                                      #radioButtons("firstLastUpload", "Group variable (default is none)", list("None"=0, "First column"=1, "Last column"=2),selected=0),  ## firstLast variable for uploaded data.
                                      #radioButtons("firstLast", "Group variable(default is last column)", list("None"=0, "First column"=1, "Last column"=2),selected=2),
                                      
                                      checkboxInput("delimiter", "Delimiter"),
                                      
                                      conditionalPanel(condition="input.delimiter",
                                      
                                        radioButtons("fileSepDFWide", "", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2)
                                      
                                      ),
                                      
                                      
                                      textInput(inputId = "analyteWide", label = "Analyte"),
                                      selectInput(inputId = "subjectWide", label = "Subject", multiple = FALSE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "genderWide", label = "Gender", multiple = FALSE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "timeRange", label = "Time", multiple = TRUE, choices = NULL, selected = NULL),
                                      selectInput(inputId = "replicateWide", label = "Replicate", multiple = FALSE, choices = NULL, selected = NULL),
                                      
                                      
                                      br(),
                                      
                                      actionButton(inputId = "runWideToLong",  label = "Convert to long format", icon = icon("play", lib = "glyphicon"))
                                      # HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
                                      # HTML('<p>Note: First row must be header.</p>'),
                                      
                                      
                                      # h4("Select Variables"),
                                      # 
                                      # selectInput(inputId = "analyte", label = "Analyte", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "subject", label = "Subject", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "gender", label = "Gender", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "time", label = "Time", multiple = FALSE, choices = NULL, selected = NULL),
                                      # selectInput(inputId = "replicate", label = "Replicate", multiple = FALSE, choices = NULL, selected = NULL)
                                      
                     )
                     # conditionalPanel(condition="input.dataInput=='3'",
                     #                  h5("Paste or enter your data below:"),
                     #                  tags$textarea(id="myData", rows=10, cols=5, ""),
                     #                  actionButton('clearText_button','Clear data'),
                     #                  HTML('<br>'),
                     #                  HTML('<br>'),
                     #                  
                     #                  #radioButtons("firstLastPaste", "Group variable (default is none)", list("None"=0, "First column"=1, "Last column"=2),selected=0), ## firstLast variable for pasted data.
                     #                  #radioButtons("firstLast", "Group variable(default is last column)", list("None"=0, "First column"=1, "Last column"=2),selected=2),
                     #                  radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3), selected=2),
                     #                  HTML('<p>You can paste or manually enter your data as separated by comma, tab or semicolon.</p>'),
                     #                  HTML('<p>Note: First row must be header.</p>')
                     # )
    ),
    
    
    conditionalPanel(condition="input.tabs1==''",
                     
                     conditionalPanel(condition = "input.firstLast != '0'",
                                      selectizeInput("subsetUni", "Select a sub-group for tests and plots", choices = NULL, multiple = FALSE)
                     ),
                     
                     h5("Choose a univariate normality test"),
                     selectizeInput("normTest", "", choices = c("Shapiro-Wilk"="SW", "Cramer-von Mises"="CVM", "Lilliefors"="Lillie", "Shapiro-Francia"="SF", "Anderson-Darling"="AD"), multiple = FALSE, selected = "SW"),
                     HTML('<br>'),
                     h5("Choose a univariate plot"),
                     selectizeInput("normPlot", "", choices = c("Q-Q plot"="qqplot", "Histogram"="histogram", "Box-plot "="box", "Scatterplot matrix"="scatter"), multiple = FALSE, selected = "qqplot"),
                     
                     conditionalPanel(condition="input.normPlot=='box'",
                                      helpText("Note: Box-plots are based on standardized values (centered and scaled).")
                     ),
                     
                     HTML('<br>'),
                     fluidRow(column(5,sliderInput("myheightUni", "Plot height:", value=400, min=200, max=1200 )),
                              column(2),
                              column(5,sliderInput("mywidthUni", "Plot width:", value=600, min=200, max=1200))
                     )),
    
    
    
    
    conditionalPanel(condition="input.tabs1=='Normality Test'",
                     
                     
                     
                     
                     actionButton(inputId = "applyNormality",  label = "Apply", icon = icon("play", lib = "glyphicon"))
                     
                     
                     
                     
    ),
    
    
    conditionalPanel(condition="input.tabs1=='Subset Analysis'",
                     
              checkboxInput("logtransform2", "Logarithmic Transformation", value = FALSE),       
                     
              actionButton(inputId = "applySubset",  label = "Apply", icon = icon("play", lib = "glyphicon"))
                     
                     
    )
    
  ),
  
  
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction"
               
               
               
      ),
      
      tabPanel("Data upload",
               navbarPage(
                 title = '',
                 tabPanel('Data', dataTableOutput('RawData'))
               )
      ),
      
      
      tabPanel("Analysis",
               
               navbarPage(
                 title = '',
                 tabPanel('Outliers', 
               

                       # h4(textOutput(outputId = "outlierTitle")),
                       h4(textOutput(outputId = "outlierTitleStep1")),
                       DT::dataTableOutput("outlierStep1"),
        
                       h4(textOutput(outputId = "outlierTitleStep2")),
                       DT::dataTableOutput("outlierStep2"),
        
                        h4(textOutput(outputId = "outlierTitleStep3")),
                       DT::dataTableOutput("outlierStep3")
               
               ),
               
               tabPanel('Normality', 
               
                   h4(textOutput(outputId = "normalityTitleStep1")),
                   DT::dataTableOutput("normalityStep1"),
    
                   h4(textOutput(outputId = "normalityTitleStep2")),
                   DT::dataTableOutput("normalityStep2")
                   
               ),
               
               tabPanel('Subset', 
               
                   h4(textOutput(outputId = "subsetTitleStep1")),
                   DT::dataTableOutput("ttest"),
                   
                   h4(textOutput(outputId = "subsetTitleStep2")),
                   DT::dataTableOutput("ttestSIA")
               
               ),
               
                tabPanel('ANOVA', 
               
                navbarPage(
                  title = '',
                        
                    tabPanel('All Subjects',       
                         
                             
                    h4(textOutput(outputId = "anovaTitleAllStep1")),
                    DT::dataTableOutput("CVResultsAll"),         
                              
                    h4(textOutput(outputId = "anovaTitleAllStep2")),
                    DT::dataTableOutput("CVtableAll"),
   
                     
                    h4(textOutput(outputId = "anovaTitleAllStep3")),
                    DT::dataTableOutput("ErrorTableAll")
                     
                  ),
                  
                  tabPanel('Gender 1', 
                           
                       h4(textOutput(outputId = "anovaTitleGender1Step1")),
                       DT::dataTableOutput("CVResultsGender1"),
                           
                       h4(textOutput(outputId = "anovaTitleGender1Step2")),
                       DT::dataTableOutput("CVtableGender1"),
                       
                       h4(textOutput(outputId = "anovaTitleGender1Step3")),
                       DT::dataTableOutput("ErrorTableGender1")
                       
                  ),
                  
                  tabPanel('Gender 2', 
              
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
                      shiny::plotOutput("plotStep0"),
                      
                      h4(textOutput(outputId = "plotTitleStep1")),  
                      shiny::plotOutput("plotStep1"),
                      
                      h4(textOutput(outputId = "plotTitleStep2")),  
                      shiny::plotOutput("plotStep2"),
                      
                      h4(textOutput(outputId = "plotTitleStep3")),  
                      shiny::plotOutput("plotStep3")
                ),
               
               
               tabPanel('Report',
                        
                        downloadButton("downloadReport", "Download report")
                        
               )
               
               
               )
               
              
               ),
      
      
      
      # tabPanel("Outlier detection",
      #          
      #          
      #          h4("Step 1: Outliers in the sets of duplicate results using the Cochran test"),
      #          DT::dataTableOutput("outlierStep1"),
      #          br(),
      #          
      #          h4("Step 2: Outliers in the variances of the results from each subject using the Cochran test"),
      #          DT::dataTableOutput("outlierStep2"),
      #          br(),
      #          
      #          h4("Step 3: Reed's criterion to see if any individual has a mean value that differs greatly from the other subjects"),
      #          DT::dataTableOutput("outlierStep3")
      #          
      # ),
      
      
      # tabPanel("Normality Test",
      #          
      #          h4("Step1: On set of results from each individual."),
      #          DT::dataTableOutput("normalityStep1"),
      #          br(),
      #          
      #          h4("Step 2: On mean values of subjects"),
      #          DT::dataTableOutput("normalityStep2")
      #          
      # ),
      
      
      # tabPanel("Subset Analysis",
      #          # h4("Table 1. Barttlet's Homogenity Test"),
      #          # DT::dataTableOutput("homogenity"),
      #          # br(),
      #          
      #          h4("Table 1. Student's t test for mean differences of gender"),
      #          DT::dataTableOutput("ttest"),
      #          
      #          # h4("Table 3: S2I+A On set of results from each individual."),
      #          # DT::dataTableOutput("homogenitySIA"),
      #           br(),
      #          
      #          h4("Table 2: Student's t test for average within-subject total variance"),
      #          DT::dataTableOutput("ttestSIA")
      # ),
      
      
      # tabPanel("Analysis of Variance"
      # ),
      
      tabPanel("Manual",
               
               h5("Usage of the web-tool"),
               HTML('<p>In order to use this application,</p>'),
               HTML('<p> (i) load your data set using <b>Data upload</b> tab. If data set has a group variable, users can define whether this variable is in the first or last column then the analysis will be performed in each sub-group,</p>'),
               HTML('<p> (ii) check univariate normality through univariate normality tests and plots in the <b>Univariate analysis</b> tab. Users also can get descriptive statistics using this tab, </p>'),
               HTML('<p> (iii) check multivariate outliers in the <b>Outlier detection</b> tab, </p>'),
               HTML('<p> (iv) check multivariate normality through MVN tests and plots in the <b>Multivariate analysis</b> tab.</p>'),
               HTML('<p align="justify"> Users can download univariate results (both descriptive statistics and univariate normality tests, as txt) and univariate plots (as pdf) from <b>Univariate analysis</b> tab, outlier set (as txt), data set without outliers (as txt) and chi-square QQ plot (as pdf) from <b>Outlier detection</b> tab, also MVN test results (as txt) and plots (as pdf or png) can be downloaded by using <b>Multivariate analysis</b> tab. </p>'),
               HTML('<p> <b>Please note that box-plots are based on standardized values (centered and scaled), and perspective and contour plots are only available for bivariate normal distributions.</b></p>'),
               HTML('<p> <b>If there are missing values in the data, a listwise deletion will be applied and a complete-case analysis will be performed.</b></p>')
      ),
      
      # tabPanel("Authors & News",
      #          h4("Authors"),
      #          HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
      #          HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
      #          HTML('<p><a href="mailto:selcuk.korkmaz@hacettepe.edu.tr" target="_blank">selcuk.korkmaz@hacettepe.edu.tr</a><p>'),
      #          HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Dincer_Goksuluk_CV_Eng.pdf" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
      #          HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
      #          HTML('<p><a href="mailto:dincer.goksuluk@hacettepe.edu.tr" target="_blank">dincer.goksuluk@hacettepe.edu.tr</a><p>'),
      #          #HTML('<br>'),
      #          #h4("Contributors"),
      #          #h5("Gokmen Zararsiz"),
      #          HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Gokmen_Zararsiz_CV_Eng.pdf" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
      #          HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
      #          HTML('<p><a href="mailto:gokmen.zararsiz@hacettepe.edu.tr" target="_blank">gokmen.zararsiz@hacettepe.edu.tr</a><p>'),
      #          h5("Izzet Parug Duru"),
      #          HTML('<p>Marmara University Faculty of Arts and Sciences<a href="http://fzk.fef.marmara.edu.tr/en/" target="_blank"> Department of Physics</a><p>'),
      #          HTML('<p><a href="mailto:izzet.duru@gedik.edu.tr" target="_blank">izzet.duru@gedik.edu.tr</a><p>'),
      #          h5("Vahap Eldem"),
      #          HTML('<p>Istanbul University Faculty of Science <a href="http://fen.istanbul.edu.tr/biyoloji/#" target="_blank"> Department of Biology</a><p>'),
      #          HTML('<p><a href="mailto:vahap.eldem@istanbul.edu.tr" target="_blank">vahap.eldem@istanbul.edu.tr</a><p>'),
      #          
      #          HTML('<br>'),
      #          
      #          
      #          h4("News"),
      #          
      #          h5("Version 1.6 (June 9, 2015)"),
      #          HTML('<p>(1) Advanced options have been added for both perspective and contour plots.<p>'),
      #          HTML('<br>'),
      #          
      #          h5("Version 1.5 (June 2, 2015)"),
      #          HTML('<p>(1) Advanced options have been added for the multivariate outlier detection.<p>'),
      #          HTML('<p> (2) Bug fixes.<p>'),
      #          HTML('<br>'),
      #          
      #          h5("Version 1.4 (January 20, 2015)"),
      #          HTML('<p>(1) <a href="http://journal.r-project.org/archive/2014-2/korkmaz-goksuluk-zararsiz.pdf" target="_blank">MVN paper </a> published at The R Journal. The complete reference information is at the <b>Citation</b> tab <p>'),
      #          HTML('<p> (2) Minor improvements and fixes.<p>'),
      #          HTML('<br>'),
      #          
      #          h5("Version 1.3 (November 20, 2014)"),
      #          HTML('<p>Univariate descriptive statistics, tests and plots have been added.<p>'),
      #          
      #          HTML('<br>'),
      #          h5("September 12, 2014"),
      #          HTML('<p>MVN web-tool presented at 16th National Biostatistics Congress in Antalya.<p>'),
      #          
      #          HTML('<br>'),
      #          h5("Version 1.2 (June 8, 2014)"),
      #          HTML('<p>(1) Sub-group analysis has been added.<p>'),
      #          
      #          HTML('<br>'),
      #          h5("Version 1.1 (May 13, 2014)"),
      #          HTML('<p>(1) Three different outlier detection methods, including Mahalanobis distance, adjusted quantile and PCOut, are available now. <p>'),
      #          HTML('<p>(2) New data set without outliers can be downloaded. <p>'),
      #          
      #          HTML('<br>'),
      #          h5("Version 1.0 (March 10, 2014)"),
      #          HTML('<p>(1) Web-tool version of the <a href="http://cran.r-project.org/web/packages/MVN/index.html" target="_blank">MVN </a> package has been released. <p>'),
      #          
      #          
      #          HTML('<br>'),
      #          
      #          h5("Other Tools"),
      #          
      #          HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/easyROC/" target="_blank"> <b>easyROC: a web-tool for ROC curve analysis </b></a><p>'),
      #          HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/MLViS/" target="_blank"> <b>MLViS: machine learning-based virtual screening tool </b></a><p>'),
      #          HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/DDNAA/" target="_blank"> <b>DDNAA: Decision support system for differential diagnosis of nontraumatic acute abdomen </b></a><p>'),
      #          HTML('<br>'),
      #          
      #          
      #          
      #          h6("Please feel free to send us bugs and feature requests.")
      #          
      # ),
      
      # tabPanel("Citation",
      #          verbatimTextOutput("cite")
      # ),
      
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
))




