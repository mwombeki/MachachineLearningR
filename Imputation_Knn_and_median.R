data("mtcars")
needs(caret,RANN)

# use Preprocc for medianImpute
mtcars[sample(1:nrow(mtcars),10),"hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[,2:4]
set.seed(42)
model <- train(X, Y) # will cause errors

# erros due to median
median_model <- train(
        X, Y, method = "glm", 
        preProcess = "medianImpute"
        ) 
median_model

# using knnImpute to avoid errors from mediam imputation 
mtcars[mtcars$disp < 140,"hp"] <- NA # create NA in the data
Y <- mtcars$mpg
X <- mtcars[,2:4]
set.seed(42)

knn_model <- train(
        X, Y, method = "glm", 
        preProcess = "knnImpute"
        ) 
knn_model

#create a sample to compare
resampes <- resamples(
        x = list(
                median_model = median_model, knn_model = knn_model)
        )
dotplot(resampes, metric = "RMSE")


# add other preprocessing
mtcars[sample(1:nrow(mtcars),10),"hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[,2:4]
X$addon <- 1
set.seed(42)
#observe the preposessing "zv"= removes near zero constants then follow the order as wirittten
prepsd_model <- train(
        X, Y, method = "glm", 
        preProcess = c("zv", "medianImpute", "center", "scale", "pca")
        )
min(prepsd_model$results$RMSE)

