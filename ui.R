shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Bhanu"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("glmnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("pls Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         tabPanel("gbm Model",
                                  verbatimTextOutput(outputId = "gbm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gbm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gbm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gbm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gbm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gbm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gbm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gbm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "gbm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gbm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "gbm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gbm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gbm_ModelPlots"),
                                  verbatimTextOutput(outputId = "gbm_Recipe"),
                                  verbatimTextOutput(outputId = "gbm_ModelSummary2"),
                                  plotOutput(outputId = "gbm_ModelTree")
                         ),
                         tabPanel("lm Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "lm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "lm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("glmStepAIC Model",
                                  verbatimTextOutput(outputId = "glmStepAIC_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           #The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmStepAIC_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmStepAIC_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmStepAIC_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmStepAIC_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmStepAIC_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmStepAIC_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmStepAIC_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmStepAIC_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmStepAIC_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "glmStepAIC_Recipe"),
                                  verbatimTextOutput(outputId = "glmStepAIC_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmStepAIC_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("Cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "cubist_ModelPlots"),
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2")
                         ),
                         tabPanel("Model Tree",
                                  verbatimTextOutput(outputId = "M5_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "M5_Preprocess",
                                           selectizeInput(inputId = "M5_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(M5_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = M5_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "M5_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "M5_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "M5_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "M5_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "M5_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "M5_ModelPlots"),
                                  verbatimTextOutput(outputId = "M5_Recipe"),
                                  plotOutput(outputId = "M5_ModelTree")
                         ),
                         tabPanel("svmLinear Model",
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmLinear_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmLinear_Metrics"),
                                  hr(),
                                  #plotOutput(outputId = "svmLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmLinear_Recipe"),
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary2")
                                  
                         ),
                         tabPanel("svmPoly Model",
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmPoly_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmPoly_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmPoly_Recipe"),
                                  verbatimTextOutput(outputId = "svmPoly_ModelSummary2")
                                  
                         ),
                         tabPanel("kNN Model",
                                  verbatimTextOutput(outputId = "kNN_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "kNN_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(kNN_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = kNN_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "kNN_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "kNN_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "kNN_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "kNN_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "kNN_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "kNN_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "kNN_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "kNN_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "kNN_ModelPlots"),
                                  verbatimTextOutput(outputId = "kNN_Recipe"),
                                  verbatimTextOutput(outputId = "kNN_ModelSummary2")
                                  
                         ),
                         tabPanel("gamSpline Model",
                                  verbatimTextOutput(outputId = "gamSpline_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gamSpline_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gamSpline_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gamSpline_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gamSpline_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gamSpline_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gamSpline_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gamSpline_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "gamSpline_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gamSpline_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "gamSpline_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gamSpline_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gamSpline_ModelPlots"),
                                  verbatimTextOutput(outputId = "gamSpline_Recipe"),
                                  verbatimTextOutput(outputId = "gamSpline_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "gamSpline_Coef")  
                                  )
                                  
                         ),
                         tabPanel("brnn Model",
                                  verbatimTextOutput(outputId = "brnn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "brnn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(brnn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = brnn_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "brnn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "brnn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "brnn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "brnn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "brnn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "brnn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "brnn_ModelPlots"),
                                  verbatimTextOutput(outputId = "brnn_Recipe"),
                                  verbatimTextOutput(outputId = "brnn_ModelSummary2")
                                  
                         ),
                         tabPanel("nnet Model",
                                  verbatimTextOutput(outputId = "NNet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "NNet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(NNet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = NNet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "NNet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NNet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "NNet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NNet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "NNet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "NNet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "NNet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "NNet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "NNet_ModelPlots"),
                                  verbatimTextOutput(outputId = "NNet_Recipe"),
                                  verbatimTextOutput(outputId = "NNet_ModelSummary2")
                                  
                         )
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""),selected="brnn", inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
