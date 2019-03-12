##########################################
## HELPER FUNCTIONS
##########################################
# DATA MUNGING FUNCTIONS
GrabData <- function(x, from = '20130225', to = '20181228'){
  # Retrieves historical price data from stooq.com.
  # Args:
  #   x: Stock symbol. Lowercase string.
  #   from: First date in the series. String in the format of %YYYY%MM%DD.
  #   to: Last date in the series. String in the format of %YYYY%MM%DD.
  # Returns: A dataframe with Date, Open, High, Low, Close, and Volume columns
  return(read.csv(sprintf('https://stooq.com/q/d/l/?s=%s.us&d1=%s&d2=%s&i=d', 
                          x, from, to)))
}
Frame2Xts <- function(x){
  # Convert dataframe pulled from stooq to xts object
  # Args:
  #   x: datafame object from stooq.com
  # Returns: xts equivalent of original data frame
  x$Date <- as.Date(x$Date)
  x.xts <- xts(x, order.by = x$Date)
  storage.mode(x.xts) <- "numeric"
  x.xts$Date <- NULL
  x.xts
}
CalculateSettlementPrice <- function(x){
  # Calculate the settlement price, (open + close)/2.
  # Args:
  #   x: A dataframe returned by the GrabData function.
  # Returns: A dataframe with Date, Open, High, Low, Close, Volume, and Set columns
  x$Set <- ((x$Open + x$Close)/2)
  return(x)
}
GetColumn <- function(x, name){
  # Retrieves a column from a dataframe.
  # Args:
  #   x: A dataframe.
  #   name: The name of the column to be retireived
  # Returns: A column of the data frame. 
  # Column type is variable, but prefer numeric typing.
  x[[name]]
}
PlotColumn <- function(x, name){
  # Plot column of a dataframe as a line
  # Args:
  #   x: A dataframe
  #   name: The name of the column to be plotted 
  # Returns: Null. Draws R base graphics plot
  plot(x[[name]], type = 'l')
}
# ANALYSIS HELPER FUNCTIONS
TimeSeriesBoot <- function(x, col = "Set", fun){
  # Performs a geometric time series block bootstrap for a given statistic.
  # Args:
  #   x: A dataframe
  #   col: String providing the name of the column to be used in the bootstrap
  # Returns: tsboot object
  means <- tsboot(x[,c(col)], fun, R = 100, l = 5, sim="geom")
}
BootstrapCis <- function(x){
  #Plot boot strapped confidence intervals.
  # Args:
  #   x: a boot object
  # Returns: list of of format list(mean, percnetile bootstrap)
  lambda <- boot.ci(x, type = "perc", conf = c(0.95, 0.66, 0.5))
  list(lambda$t0, lambda$percent)
}
VectorComapre <- function(v, x = 0, c = '>'){
  # Calculate percentage of times values contained in a vector 
  # are above (below) some constant
  # Args:
  #   v: A numeric vector
  #   x: A numeric value
  #   c: string accepts values '>' (or '<')
  # Returns: Numeric indicating fraction of times values in a 
  # vector are above (or below) some constant
  if (c == '>'){return(length(v[v>x])/length(v))}
  if (c == '<'){return(length(v[v<x])/length(v))}
}
VectorHistogram <- function(v, vline = mean(v)){
  # Plot vector values as a histogram.
  # Args:
  #   v: A numeric vector
  #   vline: A numeric value that indicates where a vertical line should be drawn
  # Returns: NULL, draws R base graphics plot
  hist(v, labels = TRUE)
  abline(v = vline, col = "red")
}
VectorNumberLine <- function(v, v.names, x = mean(v)){
  # Plot scatterplot of vector values.
  # Args:
  #   v: A numeric vector
  #   x: Numerical value to make comparisons against
  #   v.names: A caracter vector containing names coresponding to v
  # Returns: NULL, draws R base graphics plot
  plot(v, integer(length(v)), 
       ylim = c(-1,1),
       pch=18, col="blue", yaxt='n')
  abline(v=x)
  text(v, integer(length(v)), v.names, 
       cex=0.6, pos=4, col="red", srt = 270)
}
GetOutlay <- function(x, d, f = c(0,5,20,30)){
  # Returns rows for series a number of day out
  # Args:
  #   x = xts object
  #   d = inital date, that will be passed to xts object
  #   f = vector of delta for days out
  # Returns: xts with rows for days out from original
  i <- match(index(x[d]), index(x))
  frame <- x[f+i]
  comparisons <- ifelse(as.numeric(frame$Set[1]) > as.numeric(frame$Set), -1, 1)
  comparisons[1] <- 0
  differences <- round(as.vector(frame$Set) - as.numeric(frame$Set[1]), digits = 2)
  pctchange <- round((as.vector(frame$Set) / as.numeric(frame$Set[1]) - 1)*100, digits = 2)
  frame <- cbind(frame, differences, pctchange, comparisons)
  return(data.frame(date=index(frame), coredata(frame)))
}
KellyCriterion <- function(p.hat, p.given){
  # Proportion bets based on disparity between given odds and true odds
  # Args:
  #   p0 = estimated probabilty of event occuring
  #   p1 = given probability
  # Returns: Fraction of bankroll to be wagered on a particular bet
  out <- list(1/p.given, ((p.hat*(1/p.given))-(1-p.hat))*p.given)
  names(out) <- c('odds','bet')
  out
}
PLag4 <- function(x){
  # Calculate and store daily returns, whether return was positive or negative,
  # calulate lagged returns up to 4 lags, return new xts object
  # Args:
  #   x: an xts object with 'Close' column
  # Returns: xts object with daily returns, up to 4 lags, 
  # and dummy for whether return was positive or negative
  x$Return <- ((x$Close/lag(x$Close, 1)) - 1 )
  x$PosR <- ifelse(x$Return > 0.0, 1, 0)
  x$NegR <- ifelse(x$Return < 0.0, 1, 0)
  x$LR1 <- lag(x$Return, 1)
  x$LR2 <- lag(x$Return, 2)
  x$LR3 <- lag(x$Return, 3)
  x$LR4 <- lag(x$Return, 4)
  x
}
TTSplit <- function(x, p = 0.7){
  # Splits data set in training and testing sets. Produce index of split.point
  # Args:
  #   x: xts object
  #   p: proprtion of entries to be used in test data set
  # Returns: train data (xts), test data (xts), split.point (integer)
  nm <- c('train.data', 'test.data', 'split.point')
  split.point <- floor(nrow(x)*p)
  train.data <- x[(1:split.point),]
  test.data <- x[(split.point+1):nrow(x),]
  out <- list(train.data, test.data, split.point)
  names(out) <- nm
  out
}
LiveTestSplit <- function(x, start, stop){
  # Reduces data sets to index from start to stop
  # Args:
  #   x: xts object
  #   start: string providing start date in the format MM/DD/YY
  #   stop: string providing stop date in the format MM/DD/YY
  # Returns: xts object indexed from start to stop date
  dates <- c(start,stop)
  dates <- as.Date(dates, '%m/%d/%y')
  formated.date <- sprintf("%s/%s", dates[1], dates[2])
  x[formated.date]
}
#TODO(dkanu): This function may need some work
LogitEval <- function(model, model.factors, data, cut.point = 0.5){
  # Feed in logistic regression model, model factors, and data to be tested on
  # Args:
  #   model: logistic regresion model object
  #   model.factors: vecotr of strings that list out facotrs to be used in the logistic regression model
  #   test.data: data series for the model to be tested on
  #
  # Returns:
  # A list with ROC Analysis for each variable in the model, evaluation matrix copmaring predicted probabilities
  # and actual behavior. Number of correct predictions, and proportion of correct predictions
  
  # Grab row containing dependent varaible for later use ---------------
   test.check <- data[, c(strsplit(as.character(model$formula), ' ')[[1]][1])]
  # ROC ANALYSIS -------------------
  # Cover plotting behavior for varying number of factors
  ## logit.roc <- roc(model$formula, data = test.data)
  ## invisible(ifelse(length(model.factors) == 1, plot(logit.roc), lapply(logit.roc, plot, col = 'red')))
  # EVALUATION MATRIX: More useful to checking logic ----------------------
  ## logit.pred <- predict(model, test.data, type = "response")
  ## evaluation.vec <- cbind(logit.pred, as.vector(test.check))
  ## colnames(evaluation.vec) <- c('p', 'actual')
  ## evaluation.vec
  ## evaluation.xts <- as.xts(evaluation.vec)
  ## evaluation.xts$outcome <- ifelse(evaluation.xts$p > .5, 1, 0)
  ## evaluation.xts$correct <- ifelse(evaluation.xts$outcome == evaluation.xts$actual, 1, 0)
  ## evaluation.xts <- evaluation.xts[,c(1,3,2,4)]
  ## pct.cc <- sum(evaluation.xts$correct)/length(evaluation.xts$correct)
  # CONFUSION MATRIX ---------------
  pred <- predict(model, data, type = "response")
  pred <- ifelse(pred > cut.point, 1, 0)
  m.confusion <- table(Predicted = pred, Actual = test.check)
  accuracy <- as.numeric(sum(diag(m.confusion))/sum(m.confusion))
  # OUTPUT -----------------------
  ## out <- list(logit.roc, evaluation.xts, pct.cc, m.confusion)
  ## names(out) <- c("ROC","evaluation matrix", "accuracy", "confusion matrix")
  out <- list(accuracy, m.confusion)
  names(out) <- c("accuracy", "m.confusion")
  out
}
#TODO(dkanu): Add documentation comparing LogitEval to LogitEval2
LogitEval2 <- function(model, data, cut.point = 0.5){
  # Feed in logistic regression model, model factors, and data to be tested on
  # Args:
  #   model: logistic regresion model object
  #   data: data series for the model to be tested on
  # Returns: Accuracy, Confusion Matrix
  # CONFUSION MATRIX ---------------
  pred <- predict(model, data, type = "response")
  pred <- ifelse(pred > cut.point, 1, 0)
  m.confusion <- table(Predicted = pred, 
                       Actual = data[, c(strsplit(as.character(model$formula), ' ')[[1]][1])])
  accuracy <- as.numeric(sum(diag(m.confusion))/sum(m.confusion))
  # OUTPUT -----------------------
  out <- list(accuracy, m.confusion)
  names(out) <- c("accuracy", "m.confusion")
  out
}
#TODO(dkanu): Add documentations and comment
LogitPred <- function(model, data, tol = 0.5, action = 'long'){
  pred_prob <- predict(model, newdata = data, type = 'response')
  if(action == 'long'){decision <- ifelse(pred_prob > tol, 1, 0)}
  if(action == 'short'){decision <- ifesle(pred_prob < tol, 1, 0)}
  out <- list(action, pred_prob[[1]], decision[[1]])
  names(out) <- c('type', 'pred_prob', 'decision')
  out
}
SvmPred <- function(model, data, action = 'long'){
  pred_prob <- predict(model, newdata = data)
  if(action == 'long'){decision <- ifelse(pred_prob == 1, 1, 0)}
  if(action == 'short'){decision <- ifelse(pred_prob == 0, 1, 0)}
  out <- list(action, decision)
  names(out) <- c('type', 'classification')
  out
}
SvmEval2 <- function(model, data){
  # Feed in logistic regression model, model factors, and data to be tested on
  # Args:
  #   model: logistic regresion model object
  #   data: data series for the model to be tested on
  # Returns: Accuracy, Confusion Matrix
  # CONFUSION MATRIX ---------------
  pred <- predict(model, data)
  m.confusion <- table(Predicted = pred, 
                       Actual = data[, gsub('as.factor, ', '', 
                                            toString(get(toString(model$call$formula))[[2]])
                                            )
                                     ]
                       )
  accuracy <- as.numeric(sum(diag(m.confusion))/sum(m.confusion))
  # OUTPUT -----------------------
  out <- list(accuracy, m.confusion)
  names(out) <- c("accuracy", "m.confusion")
  out
}
SubsetAccuracy <- function(x, tol = 0.5){
  # Function to be used with base stats 'Filter' function,
  # returns objects with accuracy greter than 'tol'
  # Args:
  #   x: object with 'accuracy' property
  #   tol: tolerance used for filtering the property attribute
  # Returns: objects with accuracy property greater than tolerance
  # Alternatevly just use an anonymous function inline - more practical
  # 'Filter(function(x) x$accuracy > 0.5, evaluations)'
  x$accuracy > tol
}
