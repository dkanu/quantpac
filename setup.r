###############################
## SetUp & Data Acquisition
###############################
###############################
## IMPORTS & WORKING DIRECTORY
###############################
imports <- c('xts','pROC','formula.tools', 'e1071')
invisible(lapply(imports, require, character.only = TRUE))
###############################
## DATA MINING - FROM STOOQ.COM
###############################
basket <- c('spy','qqq', 'tsla', 'pypl', 'sq', 'aapl', 'v', 'fb', 'amd','iq', 'uvxy')
basket2 <- c('tsla', 'pypl', 'sq', 'aapl', 'v', 'fb', 'amd')
start.date <- '20181001'
end.date <- '20190123'
###############################
## DATA MANIPULATION
###############################
stocks.df <- lapply(basket, GrabData, from = start.date, to = end.date)
stocks.df <- lapply(stocks.df, CalculateSettlementPrice) # Calulate Midpoint (i.e settlment)
names(stocks.df) <- basket
stocks.xts <- lapply(stocks.df, Frame2Xts) #Turn Data Frame int XTS Object
stocks.xts <- lapply(stocks.xts, PLag4) # Calcualte Lags
stocks.xts <- lapply(stocks.xts, na.omit) # Remove Entries with N/As
##############################
## TRAIN TEST SPLIT
##############################
c.stocks.xts <- as.xts(Reduce(merge, stocks.xts))
data.split <- TTSplit(c.stocks.xts['2018-10-11/2019-01-10'], p = 0.75)
oos.xts <- LiveTestSplit(c.stocks.xts,"01/14/19","01/18/2019") # Out of Sample/Live Test
