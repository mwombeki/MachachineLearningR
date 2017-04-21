needs(
        caret, 
        mlbench, 
        caTools, 
        ranger, 
        glmnet, 
        Matrix, 
        foreach
        )
data("Sonar")

rows <- sample(nrow(Sonar))
# Randomly order data: Sonar
Sonar <- Sonar[rows, ]

# Identify row to split on: split
split <- round(nrow(Sonar) * .6)

# Create train
train <- Sonar[1:split,]

# Create test
test <- Sonar[(split+1):nrow(Sonar), ]

#create a model 
model <- glm(
        Class ~.,  train, 
        family = "binomial"
        )

# Predict on test set: p
p <- predict(model, test, type = "response")

#apply threshold of 0.90: p_class cor classfying
p_class <- ifelse(p > 0.9, "M", "R")

#confusionMatrix at 90%
confusionMatrix(p_class, test$Class)

#Area under the curve
colAUC(p, test$Class, plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
        method = "cv",
        number = 10,
        summaryFunction = twoClassSummary,#Dont use a default so you can use AURICC
        classProbs = TRUE, # IMPORTANT!
        verboseIter = TRUE
)

#Linear model
model <- train(Class ~., Sonar, 
               method = "glm",  
               trControl = myControl
               )
#Print model 
model
plot(model)

#Using RandomFrorrest with a unique traincontrol
model3 <- train(
        Class ~., Sonar, 
        method = "ranger",  
        trControl = myControl
        )

#Using RandomFrorrest after Defining a tuneGrid of mtry(variables/predictors to use)
myControl2 <- data.frame(mtry = c(2,5,10,20,60))
model3 <- train(
        Class ~., Sonar, 
        method = "ranger",  
        tuneGrid = myControl2
        )
model3
plot(model3)

#Using RandomFrorrest with a tunelenth/default
model2 <- train(
        Class ~., Sonar, 
        method = "ranger", 
        tuneLength = 10
        )
plot(model2)
model2

#using glmnet with trainControl
model4 <- train(
        Class ~., Sonar, 
        method = "glmnet", 
        trControl = myControl
        )
model4
plot(model4) # with default tuneGrid  values of 3- Alpha 3-lambda (ROC vs alpha for each lamda/regularazation)
max(model4$results$ROC)
model4$bestTune

# use a custom tuneGrid of defined alpha and lamda tune group 
tuneGridx = expand.grid(
        alpha = 0:1,
        lambda = seq(0.0001, 1, length = 20
                     )
        )

model5 <- train(
        Class ~., Sonar, 
        method = "glmnet", 
        tuneGrid = tuneGridx
        )

print(model5)
plot(model5) # alpha = 0 is pure ridge regression, and alpha = 1 is pure lasso regression
max(model5$result$Accuracy)
