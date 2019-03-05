##############################
### SVM PREDICT
##############################
##############################
### FILENAME DETAILS
##############################
filename <- sprintf("%s PREDICT %s.txt",
                    "SVM",
                    toString(format(Sys.time(), "%Y-%m-%d %H-%M-%S")))
##############################
### PREDICTION DATAFRAME
##############################
predict.df <- 
  data.frame("LR1.8" = c(5.3),
             "LR3.8" = c(-4.86),
             "LR1" = c(0.05),
             "LR3" = c(-1.35),
             "LR1.1" = c(0.65),
             "LR3.1" = c(-2.0)
  )
#############################
### SAVE TO FILE
#############################
sink(filename)
SvmPred(amd.svm.model, predict.df)
sink()
