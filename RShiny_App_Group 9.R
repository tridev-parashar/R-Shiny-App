############library packages###############
library(caret)
library(shiny)         
library(shinyWidgets) 
library(rattle)
library(shinycssloaders)

#data loading and processing#
SPD.df<-read.csv("SPD_9-11_Customer_Satisfaction_Survey_Data_latest.csv")

#check missing values#
sum(is.na(SPD.df)) 

#defining target factor and transforming it#
#Any score of 4 or 5 is considered satisfactory hence, 1 for the same.
SPD.df <- transform(SPD.df, y= ifelse(Overall_sat<4, 0, 1))
SPD.df$y=factor(SPD.df$y)

SPD.df = SPD.df[, -c(1, 2,3)]
#View(SPD.df)

set.seed(23)

#define function to part data set given sample size and input data#
partition_data <- function(percentage, SPD.df) {
  
  
  # partition the data into training and testing sets
  set.seed(213)
  train.index <- createDataPartition(SPD.df$y, p = percentage, list = FALSE)
  
  # subset the data using the training indices
  train_data <- SPD.df[train.index, ]
  test_data <- SPD.df[-train.index, ]
  
  # return the training and testing datasets and the training indices
  return(list(train_data, test_data))
}



############input functions################
############decision tree model################
D_Tree = function(cv_number,cv_repeat,train_metric,train_data) {
  # rpart is performs the split calculations and returns the tree
  train_data = train_data
  fitcontrol=trainControl(method = "repeatedcv", number = cv_number,
                          repeats = cv_repeat)
  set.seed(123) 
  DTtree=train(train_data[,-ncol(train_data)],train_data[,ncol(train_data)], 
               method = "rpart", tuneLength=10, metric = train_metric, 
               trControl = fitcontrol)
  return(DTtree)
}

##############random forest tree model#################
RF_Tree = function(cv_number,cv_repeat, train_metric,train_data){
  train_data = train_data
  fitControl=trainControl( method = "repeatedcv", number = cv_number,
                           repeats = cv_repeat)
  set.seed(234) 
  RFtree=train(train_data[,-ncol(train_data)],train_data[,ncol(train_data)],
               method="rf",metric=train_metric,
               trControl=fitControl,tuneLength=5) # Have a look at the model
  return(RFtree)
}

#############apply test data###################
predict_D_Tree = function(DTtree, TestData) {
  testdata = TestData[,-ncol(TestData)]
  prediction = predict(DTtree, newdata = testdata)
  DTresults = data.frame(prediction, truth = TestData$y)
  return(DTresults)
}

predict_RF_Tree = function(RFtree, TestData) {
  testdata = TestData[,-ncol(TestData)]
  prediction = predict(RFtree, newdata = testdata)
  RFresults = data.frame(prediction, truth = TestData$y)
  return(RFresults)
}
##############calculate the accuracy#############
# Calculate the accuracy of the classification results
cal_accuracy <- function(results) {
  # Create a confusion matrix from the results
  conf_mat <- table(results)
  
  # Calculate the accuracy as the sum of true positives and true negatives
  acc <- sum(diag(conf_mat)) / sum(conf_mat)
  
  # Return the accuracy as a string with 2 decimal places
  return(sprintf("Overall Accuracy: %.2f%%", acc * 100))
}

################write results as a table#######################
resultsTable = function(results) {
  # shape the result as a table for further output
  
  data = table(results)
  Outcomes = c("Predicted Not satisfied with the 911 call", "Predicted satisfied with the 911 call", "Total")
  # restructure the data and get the actually accept and not accept data
  c1 = c(data[, 1], sum(data[, 1]))  # data[, 1] is a length 2 vector
  c2 = c(data[, 2], sum(data[, 1]))  # data[, 2] is a length 2 vector
  c3 = c(sum(data[, 1]), sum(data[2, ]), sum(data))
  
  # turn these columns back into a dataframe but with proper headers
  output = data.frame(Outcomes)
  output$"Actually Not Satisfied" = c1
  output$"Actually Satisfied"     = c2
  output$"Total"             = c3
  
  return(output)
}

################show CP vs metric plot#######################
DT_plot = function(DTtree) {
  DTplot = plot(DTtree)
  return(DTplot)
}

################show Random forest predictors plot#######################
RF_plot = function(RFtree) {
  RFplot = plot(RFtree)
  return(RFplot)
}

################show random forest variables in plot#######################
RFvarplot = function(RFtree, impvar) {
  varplot = plot(varImp(RFtree), top=impvar)
  return(varplot)
}

# SERVER LOGIC FUNCTION
# Logic of input and output

server = function(input, output, session) {
  # INPUT EVENT REACTIONS
  # reconstruct the decision tree every time when createDTModel is pressed
  # reconstruct the random forest every time when createRFModel is pressed
  
  # partition the data into training and testing sets
  partitioned <- reactive({
    partition_data(input$percentage, SPD.df)
  })
  # get the train and test data for dt and rf
  DTTrainData= eventReactive(
    eventExpr = input$createDTModel,
    valueExpr = partitioned()[[1]]
  )
  DTTestData= eventReactive(
    eventExpr = input$createDTModel,
    valueExpr = partitioned()[[2]]
  )
  RFTrainData= eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = partitioned()[[1]]
  )
  RFTestData= eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = partitioned()[[2]]
  )
  # build the decision tree model
  DTtree = eventReactive(
    eventExpr = input$createDTModel,
    valueExpr = D_Tree(input$cv_number, input$cv_repeat, input$train_metric,DTTrainData())
  )
  # build the random forest model
  RFtree = eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = RF_Tree(input$cv_number, input$cv_repeat, input$train_metric,RFTrainData())
  )
  # generate test results
  DTtest_results = eventReactive(
    eventExpr = input$createDTModel,
    valueExpr = predict_D_Tree(DTtree(),DTTestData())
  )
  RFtest_results = eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = predict_RF_Tree(RFtree(),RFTestData())
  )
  
  # generate plots
  
  # random forest variance importance plot
  RFvarimp_plot = eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = RFvarplot(RFtree(),input$impvar)
  )
  # decision tree plot
  DTplot = eventReactive(
    eventExpr = input$createDTModel,
    valueExpr = DT_plot(DTtree())
  )
  # random forest predictors plot
  RFplot = eventReactive(
    eventExpr = input$createRFModel,
    valueExpr = RF_plot(RFtree())
  )
  
  # OUTPUT DISPLAY
  # assessment scores are each collapsed to display on a new line
  output$DTtest_scores = renderText(
    paste(cal_accuracy(DTtest_results()), collapse = "\n")
  )
  output$RFtest_scores = renderText(
    paste(cal_accuracy(RFtest_results()), collapse = "\n")
  )
  
  # tables of outcome breakdows are static widgets
  output$DTtest_table = renderTable(
    resultsTable(DTtest_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  output$RFtest_table = renderTable(
    resultsTable(RFtest_results()),
    align = "lccc",  # left-align first column, centre rest
    striped = TRUE
  )
  
  # frame for a plot of the decision tree
  output$tree_plot = renderPlot(
    fancyRpartPlot(
      DTtree()$finalModel
    )
  )
  # random forest important variable plot
  output$rf_varimp_plot = renderPlot(
    RFvarimp_plot()
  )
  # decision tree complexity parameter and metrics plot
  output$cp_plot = renderPlot(DTplot())
  
  #random forest plot
  output$rf_plot = renderPlot(RFplot())
  
  # best cp value picked
  output$cp_value = renderText(
    paste("The CP value picked for the optimal model: ", DTtree()$bestTune)
  )
  
  # best mtry value picked
  output$mtry_value = renderText(
    paste("The number of randomly selected predictors of the optimal model: ", RFtree()$bestTune)
  )
}

# USER INTERFACE FUNCTION

ui = fluidPage(
  # title
  titlePanel("Seattle Police Department Customer Satisfaction Survey"),
  helpText("The dataset consisted of survey data gathered by telephonic interviews of randomly 
  selected 911 callers who had an officer dispatched to provide assistance (excluding sensitive cases). 
  The exploration of this dataset involves the utilization of two classification methods - 
           Decision Tree and Random Forest."),
  setSliderColor(c("Black","Black","Black","Black"), c(1,2,3,4)),
  # sidebar ui design
  sidebarLayout(
    sidebarPanel(
      h2("The Controls"),
      br(),
      
      # build the action button
      actionButton(
        inputId = "createDTModel",
        label = "Create Decision Tree Model",
        class = "btn-primary",
        style = "background-color: black;"  # sets the background color to orange 
      ),
      br(),
      br(),
      
      actionButton(
        inputId = "createRFModel",
        label = "Create Random Forest Model",
        class = "btn-primary",  
        style = "background-color: black;"  # sets the background color to orange 
        
      ),
      
      h3("Model Features"),
      helpText(
        "The model features can be adjusted to look at how different variables",
        " affect the prediction outcomes."
      ),
      br(),
      
      # set the slider and picker for input variables
      h4("DatasetSplit"),
      helpText("The proportion of original data used to train the model."),
      sliderInput(
        inputId = "percentage",
        label = NULL,  
        min = 0.6,       
        max = 0.8,      
        value = 0.7,
        step = 0.05
      ),
      
      br(),
      h4("Decision Tree Train Metric Choice"),
      helpText("Accuracy is the percentage of correctly predicted instances out of all instances in the testing set."),
      helpText("Kappa is a metric to compare an observed accuracy with an expected accuracy at 
               random chance."),
      pickerInput(
        inputId = "train_metric",
        label = NULL,  
        # choose from accuracy and kappa for the metrics
        choices = list("Accuracy","Kappa"), 
        selected = "Accuracy",
        options = list(`actions-box` = TRUE),
        multiple = FALSE
      ),
      
      br(),
      h4("Cross-Validation Number"),
      helpText("Cross-Validation is a resampling method to test and 
               train the classfier on different partitions of the training data in order to reduce error rate.
               Cross-validation number is the number of partitions that the training data is to be split to."),
      sliderInput(
        inputId = "cv_number",
        label = NULL,  
        min = 1,       
        max = 30,      
        value = 10      
      ),
      br(),
      h4("Cross-Validation Repeat"),
      helpText("The number of times to repeat the cross-validation process."),
      sliderInput(
        inputId = "cv_repeat",
        label = NULL,  
        min = 1,       
        max = 30,      
        value = 3      
      ),
      br(),
      h4("Random Forest Top Important Variable"),
      helpText("The number of variables to display on the important variables graph."),
      sliderInput(
        inputId = "impvar",
        label = NULL,  
        min = 1,       
        max = 15,      
        value = 5      
      ),
      br(),
      helpText("**Click \"Create Model\" again after adjusting the controls**"),
      br(),
      
    ),
    mainPanel(
      # set the mainpanel
      fluidRow(
        label = NULL,
        h1("Decision Tree"),
        helpText(
          " A decision tree is a type of supervised machine learning algorithm that uses a recursive binary 
          split to assign observations to specific categories, starting at the top of the tree. 
          As the split occurs, the algorithm evaluates the most informative features that separate 
          the data into distinct categories. Once the tree has been constructed, the terminal nodes, 
          which are located at the bottom of the tree, represent the final outcomes of the classification 
          process."
        ),
        br(),
        
        h3("Complexity Parameter(CP) vs Accuracy/Kappa Metric"),
        
        helpText("CP is a criterion used in decision tree construction to determine the minimum improvement
                 required at each node. If the costs associated with adding another node exceed the CP value,
                 the tree-building process stops. CP is a useful tool for determining the optimal tree size 
                 and assessing model performance. If the CP value is too small, the model may be overfitted,
                 resulting in poor generalization. Conversely, if the CP value is too large, the model 
                 may be underfitted, leading to poor performance on the test set."
        ),
        tagAppendAttributes(
          textOutput("cp_value"),
          style = "white-space: pre-wrap; font-size: 14px;"
        ),
        br(),
        withSpinner(plotOutput(outputId = "cp_plot"),type=4),
        br(),
        
        # plot of the decision tree
        h3("Decision Tree Plot"),
        withSpinner(plotOutput(outputId = "tree_plot"),type=4),
        br(),
        
        tagAppendAttributes(
          textOutput("DTtest_scores"),
          style = "white-space: pre-wrap; font-size: 17px;"
        ),
        helpText(
          "The accuracy score reflects how well the model can predict 
          whether the caller expressed satisfaction during the call or not. "
        ),
        withSpinner(tableOutput("DTtest_table"),type=4)
      ),
      
      
      # plot of random forest
      fluidRow(
        label = NULL,
        h1("Random Forest"),
        helpText("Random Forest is a classification algorithm that comprises numerous decision trees. 
                 The algorithm randomly selects a subset of features to build each tree, and each tree 
                 makes a class prediction. The model's overall prediction is determined by selecting the
                 class with the highest number of votes. To minimize feature correlation and reduce 
                 variance, it's important to select the optimal number of features for the random forest model."),
        br(),
        
        h3("Random Forest Plot"),
       
        tagAppendAttributes(
          textOutput("mtry_value"),
          style = "white-space: pre-wrap; font-size: 14px;"
        ),
        br(),
        withSpinner(plotOutput(outputId = "rf_plot"),type=4),
        br(),
       
        h3("Random Forest Test Results"),
        helpText(
          "This table evaluates the performance and the outcomes of the random forest model on the testing data"),
        tagAppendAttributes(
          textOutput("RFtest_scores"),
          style = "white-space: pre-wrap; font-size: 17px;"
        ),
        br(),
        withSpinner(tableOutput("RFtest_table"),type=4)
      ),
      br(),
      
      h3("Important Variables"),
      helpText(
        "The variables shown in the graph below represent
        the features with the highest explanation power for 
        predicting customer satisfaction. We can see that Info, Behaviour,
        Assist_sat and Availability are important in both Random Forest and
        Decision tree except Off_protocol. However, as Random forest is 
        more robust and individual decision tree outputs may change based
        on sampling, we take features derived from the former as better 
        indicator of satisfactory calls."
      ),
      withSpinner(plotOutput(outputId = "rf_varimp_plot"),type=4),
      br(),
    )
  )
)



options(shiny.port = 8100)  # when running locally
shinyApp(ui = ui, server = server)


