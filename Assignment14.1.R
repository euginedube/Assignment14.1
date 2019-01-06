#a. Read the dataset and identify the right features
#b. Clean dataset, impute missing values and perform exploratory data analysis.
#c. Visualize the dataset and make inferences from that
#d. Perform any 3 hypothesis tests using columns of your choice, make conclusions
#e. Create a linear regression model to predict the number of comments in the next 24 hours (relative to base time)


#Libraries
library(caret)
library(data.table)
library(MatrixModels)
library(glmnet)
install.packages("glmnet")

#a. Read the dataset and identify the right features

### Read train dataset 
train = fread("blogData_train.csv")
###Read test files and combine them into one dataframe
test_filenames = list.files(pattern = "blogData_test")
test = foreach(i = 1:length(test_filenames), .combine = rbind) %do% {
  temp = fread(test_filenames[i], header = F)
}

str(train)
#b. Clean dataset, impute missing values and perform exploratory data analysis.
train[, V281 := log(1 + V281)]
test[, V281 := log(1 + V281)]
# drop continous variables without variation
drop = c(8, 13, 28, 33, 38, 40, 43, 50, 278)
train[, (drop) := NULL]
test[, (drop) := NULL]
write.csv(train, "bf-Train.csv", row.names = F)
write.csv(test, "bf-Test.csv", row.names = F)

#c. Visualize the dataset and make inferences from that

View(train)

#Our target variable for prediction is V281(which is the number of comments in the next 24hrs)
#Other variables are statistcal information (mean,sd,min,max)  of some variables on the oroginal blog posts

#d. Perform any 3 hypothesis tests using columns of your choice, make conclusions

chisq.test(table(train$V1, train$V2))

t.test(train$V4, train$V280, paired=TRUE)


#e. Create a linear regression model to predict the number of comments in the next 24 hours (relative to base time)
train = fread("bf-Train.csv")
test = fread("bf-Test.csv")
mse = function(y_hat, y) {
  mse = mean((y - y_hat)^2)
  return(mse)
}
# create design matrices
train_x = model.Matrix(V281 ~ . - 1, data = train, sparse = F)
train_x_sparse = model.Matrix(V281 ~ . - 1, data = train, sparse = T)
train_y = train$V281

test_x = model.Matrix(V281 ~ . - 1, data = test, sparse = F)
test_y = test$V281

# Linear Model Using LASSO
mdl_lasso = cv.glmnet(train_x_sparse, train_y, family = "gaussian", alpha = 1)
pred_lasso = predict(mdl_lasso, newx = test_x)
mse(pred_lasso, test_y)
