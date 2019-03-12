##############################
### SVM PREDICT
##############################
##############################
### FILENAME DETAILS
##############################
filename <- sprintf("%s PREDICT %s.txt", "SVM", toString(format(Sys.time(), "%Y-%m-%d %H-%M-%S")))
##############################
### PREDICTION DATAFRAME
##############################
predict.df <- #Prediction for 1/25/19
  data.frame("LR1.8" = c(5.3), # AMD LAG 1 - i.e. 1/24/19
             "LR3.8" = c(-4.86), # AMD LAG 3 - i.e. 1/22/19
             "LR1" = c(0.05), # SPY LAG 1 - i.e. 1/24/19
             "LR3" = c(-1.35), # SPY LAG 3 - i.e. 1/22/19
             "LR1.1" = c(0.65), # QQQ LAG 1 - i.e. 1/24/19
             "LR3.1" = c(-2.0), # QQQ LAG 3 - i.e. 1/22/19
             "LR1.3" = c(1.88), # QQQ LAG 1 - i.e. 1/24/19
             "LR3.3" = c(-1.29), # QQQ LAG 3 - i.e. 1/22/19
             "LR1.10" = c(-4.79), # UVXY LAG 1 - i.e. 1/24/19
             "LR3.10" = c(13.46) # UVXY LAG 3 - i.e. 1/22/19
  )
#############################
### SAVE TO FILE
#############################
models <- list(amd.svm.model, pypl.svm.model)
names(models) <- c('amd', 'pypl')
sink(filename)
lapply(models, SvmPred, predict.df)
sink()
