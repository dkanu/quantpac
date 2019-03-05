####################
## SVM Model
## Kernel types - linear, polynomial, radial, sigmoid
####################
##############################
## MODELS
##############################
kern_type = "polynomial"
#TSLA
tsla.svm.factors <- c('LR1.2', 'LR3.2','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
tsla.svm.formula <- as.formula(paste('as.factor(PosR.2)~', paste(tsla.svm.factors, collapse = '+')))
tsla.svm.model <- svm(tsla.svm.formula, data = data.split$train.data, kernel = kern_type)
tsla.svm.eval <- SvmEval2(tsla.svm.model, data.split$test.data)
#PYPL --------------
pypl.svm.factors <- c('LR1.3', 'LR3.3','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
pypl.svm.formula <- as.formula(paste('as.factor(PosR.3)~', paste(pypl.svm.factors, collapse = '+')))
pypl.svm.model <- svm(pypl.svm.formula, data = data.split$train.data, kernel = kern_type)
pypl.svm.eval <- SvmEval2(pypl.svm.model, data.split$test.data)
#SQ---------------
sq.svm.factors <- c('LR1.4', 'LR3.4','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
sq.svm.formula <- as.formula(paste('as.factor(PosR.4)~', paste(sq.svm.factors, collapse = '+')))
sq.svm.model <- svm(sq.svm.formula, data = data.split$train.data, kernel = kern_type)
sq.svm.eval <- SvmEval2(sq.svm.model, data.split$test.data)
#AAPL----------------------
aapl.svm.factors <- c('LR1.5', 'LR3.5','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
aapl.svm.formula <- as.formula(paste('as.factor(PosR.5)~', paste(aapl.svm.factors, collapse = '+')))
aapl.svm.model <- svm(aapl.svm.formula, data = data.split$train.data, kernel = kern_type)
aapl.svm.eval <- SvmEval2(aapl.svm.model, data.split$test.data)
#V-----------------------
v.svm.factors <- c('LR1.6', 'LR3.6','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
v.svm.formula <- as.formula(paste('as.factor(PosR.6)~', paste(v.svm.factors, collapse = '+')))
v.svm.model <- svm(v.svm.formula, data = data.split$train.data, kernel = kern_type)
v.svm.eval <- SvmEval2(v.svm.model, data.split$test.data)
#FB-----------------------
fb.svm.factors <- c('LR1.7', 'LR3.7','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
fb.svm.formula <- as.formula(paste('as.factor(PosR.7)~', paste(fb.svm.factors, collapse = '+')))
fb.svm.model <- svm(fb.svm.formula, data = data.split$train.data, kernel = kern_type)
fb.svm.eval <- SvmEval2(fb.svm.model, data.split$test.data)
#AMD-----------------------
amd.svm.factors <- c('LR1.8', 'LR3.8','LR1','LR3','LR1.1','LR3.1','LR1.10', 'LR3.10')
amd.svm.formula <- as.formula(paste('as.factor(PosR.8)~', paste(amd.factors, collapse = '+')))
amd.svm.model <- svm(amd.svm.formula, data = data.split$train.data, kernel = kern_type)
amd.svm.eval <- SvmEval2(amd.svm.model, data.split$test.data)
############################
## BULK LIST BASED ACTIONS
############################
evaluations <- list(tsla.svm.eval, pypl.svm.eval, sq.svm.eval, 
                    aapl.svm.eval, v.svm.eval, fb.svm.eval, amd.svm.eval)
names(evaluations) <- basket2
models <- list(tsla.svm.model, pypl.svm.model, sq.svm.model, 
               aapl.svm.model, v.svm.model, fb.svm.model, amd.svm.model)
names(models) <- basket2
##############################
## FILENAME DETAILS
##############################
filename <- sprintf("%s ANALYSIS %s.txt", 
                    "SVM", 
                    toString(format(Sys.time(), "%Y-%m-%d %H-%M-%S")))
############################
## SAVE TO FILE
############################
sink(filename)
Filter(function(x) x$accuracy > 0.5, evaluations)
sink()
