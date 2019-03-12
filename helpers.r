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
# ANALYSIS HELPER FUNCTIONS
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
#TODO(dkanu): Add documentations and comment
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
