shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    ## updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "brnn")
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  # reactive getGbmRecipe ----
  
  getGbmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gbm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gbm_Go ----
  observeEvent(
    input$gbm_Go,
    {
      library(gbm)
      library(plyr)
      method <- "gbm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGbmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 10)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gbm_Load,
    {
      method  <- "gbm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gbm_Delete,
    {
      models[["gbm"]] <- NULL
      gc()
    }
  )
  
  # output gbm_ModelSummary (text) ----
  output$gbm_ModelSummary0 <- renderText({
    description("gbm")   # Use the caret method name here
  })
  
  # output gbm_Metrics (table) ----
  output$gbm_Metrics <- renderTable({
    req(models$gbm)
    models$gbm$results[ which.min(models$gbm$results[, "RMSE"]), ]
  })
  
  # output gbm_ModelPlots (plot) ----
  output$gbm_ModelPlots <- renderPlot({
    req(models$gbm)
    plot(models$gbm)
  })
  
  # output gbm_Recipe (print) ----
  output$gbm_Recipe <- renderPrint({
    req(models$gbm)
    models$gbm$recipe
  })  
  
  # output gbm_ModelSummary2 (print) ----
  output$gbm_ModelSummary2 <- renderPrint({
    req(models$gbm)
    print(models$gbm)
  })
  
  # output gbm_ModelTree (plot) ----
  output$gbm_ModelTree <- renderPlot({
    req(models$gbm)
    plot(models$gbm$finalModel)
  })
 
  #-------
  getLmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lm_Load,
    {
      method  <- "lm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lm_Delete,
    {
      models[["lm"]] <- NULL
      gc()
    }
  )
  
  # output lm_ModelSummary (text) ----
  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })
  
  # output lm_Metrics (table) ----
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # # output lm_ModelPlots (plot) ----
  # output$lm_ModelPlots <- renderPlot({
  #   req(models$lm)
  #   plot(models$lm)
  # })
  
  # output lm_Recipe (print) ----
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  # output lm_ModelSummary2 (print) ----
  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    print(models$lm)
  })
  
 
  # output lm_Coef (print) ----
  output$lm_Coef <- renderTable({
    req(models$lm)
    co <- coef(models$lm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # reactive getglmStepAICRecipe ----
  getglmStepAIC <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmStepAIC_Go ----
  observeEvent(
    input$glmStepAIC_Go,
    {
      method <- "glmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getglmStepAIC(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$glmStepAIC_Load,
    {
      method  <- "glmStepAIC"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmStepAIC_Delete,
    {
      models[["glmStepAIC"]] <- NULL
      gc()
    }
  )
  
  # output glmStepAIC_ModelSummary (text) ----
  output$glmStepAIC_ModelSummary0 <- renderText({
    description("glmStepAIC")   # Use the caret method name here
  })
  
  # output glmStepAIC_Metrics (table) ----
  output$glmStepAIC_Metrics <- renderTable({
    req(models$glmStepAIC)
    models$glmStepAIC$results[ which.min(models$glmStepAIC$results[, "RMSE"]), ]
  })
  

  # output glmStepAIC_Recipe (print) ----
  output$glmStepAIC_Recipe <- renderPrint({
    req(models$glmStepAIC)
    models$glmStepAIC$recipe
  })  
  
  # output glmStepAIC_ModelSummary2 (print) ----
  output$glmStepAIC_ModelSummary2 <- renderPrint({
    req(models$glmStepAIC)
    print(models$glmStepAIC)
  })
  
  # output glmStepAIC_Coef (print) ----
  output$glmStepAIC_Coef <- renderTable({
    req(models$glmStepAIC)
    co <- coef(models$glmStepAIC$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
  library(Cubist)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getCubistRecipe ----
  getCubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubist_Go ----
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      
      tryCatch({
        model <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", tuneLength = 5)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method  <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      models[["cubist"]] <- NULL
      gc()
    }
  )
  
  # output cubist_ModelSummary (text) ----
  output$cubist_ModelSummary0 <- renderText({
    description("cubist")   # Use the caret method name here
  })
  
  # output cubist_Metrics (table) ----
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubist_ModelPlots (plot) ----
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  # output cubist_Recipe (print) ----
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubist_ModelSummary2 (print) ----
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  
 
  # METHOD * M5 ---------------------------------------------------------------------------------------------------------------------------
  library(RWeka)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(partykit)
  
  # reactive getRpartRecipe ----
  getM5Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$M5_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$M5_Go,
    {
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
       
        model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5) 
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$M5_Load,
    {
      method  <- "M5"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$M5_Delete,
    {
      models[["M5"]] <- NULL
      gc()
    }
  )
  
  # output M5_ModelSummary0 (print) ----
  output$M5_ModelSummary0 <- renderText({
    description("M5")   # Use the caret method name here
  })
  
  # output M5_Metrics (table) ----
  output$M5_Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  # output M5_Recipe (print) ----
  output$M5_Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  # output M5_ModelPlots (plot) ----
  output$M5_ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })
  
  # output M5_ModelTree (plot) ----
  output$M5_ModelTree <- renderPlot({
    req(models$M5)
    plot(models$M5$finalModel)
  })
  

  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  
  # reactive getsvmLinearRecipe ----  
  getsvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmLinear_Go ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        tuneGrid <- expand.grid(
          C = c(0.01, 0.05, 0.1)
        )
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )
  
  # output svmLinear_ModelSummary (text) ----
  output$svmLinear_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })
  
  # output svmLinear_Metrics (table) ----
  output$svmLinear_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  # # output svmLinear_ModelPlots (plot) ----
  # output$svmLinear_ModelPlots <- renderPlot({
  #   req(models$svmLinear)
  #   plot(models$svmLinear)
  # })
  
  # output svmLinear_Recipe (print) ----
  output$svmLinear_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output svmLinear_ModelSummary2 (print) ----
  output$svmLinear_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })
  
  # # output svmLinear_Coef (print) ----
  # output$svmLinear_Coef <- renderPrint({
  #   req(models$svmLinear)
  #   varImp(models$svmLinear)
  # })
  
  
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  
  # reactive getsvmPoly ----  
  getsvmPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmPoly_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmPoly_Go ----
  observeEvent(
    input$svmPoly_Go,
    {
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength=3, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmPoly_Load,
    {
      method  <- "svmPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmPoly_Delete,
    {
      models[["svmPoly"]] <- NULL
      gc()
    }
  )
  
  # output svmPoly_ModelSummary (text) ----
  output$svmPoly_ModelSummary0 <- renderText({
    description("svmPoly")   # Use the caret method name here
  })
  
  # output svmPoly_Metrics (table) ----
  output$svmPoly_Metrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmPoly_ModelPlots (plot) ----
  output$svmPoly_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  # output svmPoly_Recipe (print) ----
  output$svmPoly_Recipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmPoly_ModelSummary2 (print) ----
  output$svmPoly_ModelSummary2 <- renderPrint({
    req(models$svmPoly)
    print(models$svmPoly)
  })
  # output$svmPoly_Coef <- renderPrint({
  #   req(models$svmPoly)
  #   varImp(models$svmPoly)
  # })
  
  # METHOD * k-NN ---------------------------------------------------------------------------------------------------------------------------

  # reactive getkNN ----  
  getkNNRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$kNN_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent kNN_Go ----
  observeEvent(
    input$kNN_Go,
    {
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getkNNRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength=5)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$kNN_Load,
    {
      method  <- "knn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$kNN_Delete,
    {
      models[["knn"]] <- NULL
      gc()
    }
  )
  
  # output kNN_ModelSummary (text) ----
  output$kNN_ModelSummary0 <- renderText({
    description("knn")   # Use the caret method name here
  })
  
  # output knn_Metrics (table) ----
  output$kNN_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  # output kNN_ModelPlots (plot) ----
  output$kNN_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })
  
  # output kNN_Recipe (print) ----
  output$kNN_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  # output kNN_ModelSummary2 (print) ----
  output$kNN_ModelSummary2 <- renderPrint({
    req(models$knn)
    print(models$knn)
  })
  
  # METHOD * gamSpline ---------------------------------------------------------------------------------------------------------------------------
  
  library(gam)
  # reactive getkNN ----  
  getgamSplineRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gamSpline_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gamSpline_Go ----
  observeEvent(
    input$gamSpline_Go,
    {
      method <- "gamSpline"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        tuneGrid <- expand.grid(df = seq(1,3))       
        model <- caret::train(getgamSplineRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gamSpline_Load,
    {
      method  <- "gamSpline"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gamSpline_Delete,
    {
      models[["gamSpline"]] <- NULL
      gc()
    }
  )
  
  # output gamSpline_ModelSummary (text) ----
  output$gamSpline_ModelSummary0 <- renderText({
    description("gamSpline")   # Use the caret method name here
  })
  
  # output gamSpline_Metrics (table) ----
  output$gamSpline_Metrics <- renderTable({
    req(models$gamSpline)
    models$gamSpline$results[ which.min(models$gamSpline$results[, "RMSE"]), ]
  })
  
  # output gamSpline_ModelPlots (plot) ----
  output$gamSpline_ModelPlots <- renderPlot({
    req(models$gamSpline)
    plot(models$gamSpline)
  })
  
  # output gamSpline_Recipe (print) ----
  output$gamSpline_Recipe <- renderPrint({
    req(models$gamSpline)
    models$gamSpline$recipe
  })  
  
  # output gamSpline_ModelSummary2 (print) ----
  output$gamSpline_ModelSummary2 <- renderPrint({
    req(models$gamSpline)
    print(models$gamSpline)
  })
  
  # output gamSpline_Coef (print) ----
  output$gamSpline_Coef <- renderTable({
    req(models$gamSpline)
    co <- coef(models$gamSpline$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * brnn ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getbrnn ----  
  getbrnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$brnn_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent $brnn_Go ----
  observeEvent(
    input$brnn_Go,
    {
      method <- "brnn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getbrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$brnn_Load,
    {
      method  <- "brnn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$brnn_Delete,
    {
      models[["brnn"]] <- NULL
      gc()
    }
  )
  
  # output brnn_ModelSummary (text) ----
  output$brnn_ModelSummary0 <- renderText({
    description("brnn")   # Use the caret method name here
  })
  
  # output brnn_Metrics (table) ----
  output$brnn_Metrics <- renderTable({
    req(models$brnn)
    models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  })
  
  # output brnn_ModelPlots (plot) ----
  output$brnn_ModelPlots <- renderPlot({
    req(models$brnn)
    plot(models$brnn)
  })
  
  # output brnn_Recipe (print) ----
  output$brnn_Recipe <- renderPrint({
    req(models$brnn)
    models$brnn$recipe
  })  
  
  # output brnn_ModelSummary2 (print) ----
  output$brnn_ModelSummary2 <- renderPrint({
    req(models$brnn)
    print(models$brnn)
  })
  
  
  
  
  # METHOD * NNet ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNNet ----  
  getNNetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$NNet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent NNet_Go ----
  observeEvent(
    input$NNet_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$NNet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$NNet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()
    }
  )
  
  # output NNet_ModelSummary (text) ----
  output$NNet_ModelSummary0 <- renderText({
    description("nnet")   # Use the caret method name here
  })
  
  # output NNet_Metrics (table) ----
  output$NNet_Metrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  # output NNet_ModelPlots (plot) ----
  output$NNet_ModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })
  
  # output NNet_Recipe (print) ----
  output$NNet_Recipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })  
  
  # output NNet_ModelSummary2 (print) ----
  output$NNet_ModelSummary2 <- renderPrint({
    req(models$nnet)
    print(models$nnet)
  })
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
