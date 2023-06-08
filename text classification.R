

# Load the comet package
library(cometr)

# Load the text2vec package
library(text2vec)
library(tm)
# Load the data
# Ensure that the dataset is saved in your local working directory
data <- read.table("Restaurant_Reviews.tsv", header = TRUE, sep = "\t")


#############################

library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(data$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = data$Liked


# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)

#Create a new project and experiment in comet
#Note the this method will work only if you have properly set up your credentials in the .yml file in your local working directory
cometr::create_project(project_name = "nlp", project_description = "nlp")

experiment = create_experiment(
  project_name = "nlp",
  keep_active = TRUE,
  log_output = TRUE,
  log_error = FALSE,
  log_code = TRUE,
  log_system_details = TRUE
)

# Split the data into train and test sets
set.seed(123)

experiment$log_parameter("seed", 123)


split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm =table(test_set[,692], y_pred)
cm <- table(data$Prediction, data$Actual)

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
precision <- cm[4] / sum(cm[4], cm[2])
sensitivity <- cm[4] / sum(cm[4], cm[3])
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
specificity <- cm[1] / sum(cm[1], cm[2])



accuracy <- mean(y_pred == test_set$Liked)
experiment$log_parameter("Accuracy",accuracy)

summa <- summary(classifier)
experiment$log_metric("Accuracy",accuracy)
experiment$log_metric("Precison",precision)
experiment$log_metric("Sensitivity",sensitivity)
experiment$log_metric("F1-score",fscore)
experiment$log_metric("Specificity",specificity)

saveRDS(classifier, file = "model.rds")
experiment$upload_asset("model.rds")

experiment$get_metrics_summary()

experiment$stop()
experimen
