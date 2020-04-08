install.packages("keras")
library(keras)
install_keras()

#read data
data <- read.csv(file.choose(),header = T)
str(data)

#change to matrix
data <- as.matrix(data)
dimnames(data) <- NULL
data
str(data)

#normalize
data[,1:21] <- normalize(data[,1:21])
data[,22] <- as.numeric(data[,22])-1
summary(data)

#data partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(.7,.3))
table(ind)
training <- data[ind==1,1:21]
test <- data[ind==2,1:21]
trainingtarget <- data[ind==1, 22]
testtarget <- data[ind==2, 22]

# One Hot Encoding
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
print(testLabels)

# Create 1st sequential model
model <- keras_model_sequential()
model %>%
  layer_dense(units=8, activation = 'relu', input_shape = c(21)) %>%
  layer_dense(units = 3, activation = 'softmax') #converts outout into 1-0
summary(model)

# Compile
model %>%
  compile(loss = 'categorical_crossentropy', # binary_crossntropy if 2 category response variable, here are 3
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit model (Multiple Perceptron Neural Network for Multi-Class Softmax Classification)
history <- model %>%
  fit(training,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history)

# Evaluate model with test data
model1 <- model %>%
  evaluate(test, testLabels)

# Prediction & confusion matrix - test data
prob <- model %>%
  predict_proba(test)

pred <- model1 %>%
  predict_classes(test)
table1 <- table(Predicted = pred, Actual = testtarget)

cbind(prob, pred, testtarget)


# Create 2nd sequential model
model <- keras_model_sequential()
model %>%
  layer_dense(units=50, activation = 'relu', input_shape = c(21)) %>%
  layer_dense(units = 3, activation = 'softmax') #converts outout into 1-0
summary(model)

# Compile
model %>%
  compile(loss = 'categorical_crossentropy', # binary_crossntropy if 2 category response variable, here are 3
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit model (Multiple Perceptron Neural Network for Multi-Class Softmax Classification)
history <- model %>%
  fit(training,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history)

# Evaluate model with test data
model2 <- model %>%
  evaluate(test, testLabels)

# Prediction & confusion matrix - test data
prob <- model %>%
  predict_proba(test)

pred <- model %>%
  predict_classes(test)
table2 <- table(Predicted = pred, Actual = testtarget)

#create 3rd sequential model
model <- keras_model_sequential()
model %>%
  layer_dense(units=50, activation = 'relu', input_shape = c(21)) %>%
  layer_dense(units = 10,activation = 'relu')%>%
  layer_dense(units = 3, activation = 'softmax') #converts outout into 1-0
summary(model)

# Compile
model %>%
  compile(loss = 'categorical_crossentropy', # binary_crossntropy if 2 category response variable, here are 3
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit model (Multiple Perceptron Neural Network for Multi-Class Softmax Classification)
history <- model %>%
  fit(training,
      trainLabels,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
plot(history)

# Evaluate model with test data
model3 <- model %>%
  evaluate(test, testLabels)

# Prediction & confusion matrix - test data
prob <- model %>%
  predict_proba(test)

pred <- model %>%
  predict_classes(test)
table3 <- table(Predicted = pred, Actual = testtarget)
# Fine-tune model
table1
model1
table2
model2
table3
model3
