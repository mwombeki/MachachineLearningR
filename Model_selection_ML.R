needs(C50, caret, caretEnsemble)
data(churn)

#number of churn clients
table(churnTrain$churn)/nrow(churnTrain)

#create train/test indexes
set.seed(42)
folds <- createFolds(churnTrain$churn,k=10)

#compare distribution 
i <- folds$Fold1
table(churnTrain$churn[i])/length(i)

#create a train control to reuse same cross validation folds
myControl <- trainControl(
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        verboseIter = TRUE,
        savePredictions = TRUE,
        index = folds
)

#######Try first with glmnet-linear model with variable selection

#1. fits quickly (fast, simple)
#2. ignores nosiy variables
#3 provides interpretable coeffients the same way as the coefficients from an lm or glm model.

set.seed(42)
model_glmnet <- train(
        churn ~., churnTrain,
        metric = "ROC",
        method = "glmnet",
        tuneGrid = expand.grid(
                alpha = 0:1,
                lambda = 0:10/10
        ),
        trControl = myControl
)
# plot the model to see it
plot(model_glmnet)

#plot the final model to see how the coefficient improves
plot(model_glmnet$finalModel)


#######Try random forest NEXT

#1. slower to fit thn glmnet
#2. less interpretable
#3. often but (not always) more acurate
#4. easy to tune require little preprocessing( no need to log transform or normalize)
#5. Capture thresholds and variable interations

churnTrain$churn <- factor(churnTrain$churn, levels = c("yes","no"))
model_rf <- train(
        churn~.,churnTrain,
        metric = "ROC",
        method = "ranger",
        trControl = myControl
)
# plot model
plot(model_rf)


####### Then select a best fitting model
#since the same test and CrossValidation was used so possible to compare
#1. select a model with HIGHEST AUC and smaller range between min and max AUC.
#2.model with lower std in AUC 

model_list <- list (glmnet= model_glmnet, rf = model_rf)

# select the model using use resamples()
resamp <- resamples(model_list)
resamp

# do a box-and-whisker plot or a dot-plot picking higher AUC
bwplot(resamp, metric = "ROC")
dotplot(resamp, metric = "ROC") # bettter for lost of models

# do a density plot to show a distribution of AUC using a kernel plot showing outliers with high and low AUC
densityplot(resamp, metric = "ROC")

#do a scatter plot to compare all folds of cross-validations
xyplot(resamp, metric = "ROC")


stack <-caretStack(model_list, method = "glm") 
