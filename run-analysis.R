###############################################################################
#
#  1. Download and read data into R
#
###############################################################################

require(RCurl)
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (any(!file.exists("./data/train.csv") | !file.exists("./data/train.csv"))) {
        dir.create("data")
        download.file(trainURL,
                      destfile="./data/train.csv",
                      method = "curl")
        download.file(testURL,
                      destfile="./data/test.csv",
                      method = "curl")
}

if (any(!exists("train") | !exists("test"))) {
        test <- read.csv("./data/test.csv",
                         header=TRUE,
                         stringsAsFactors=F,
                         na.strings=c("", "NA", "#DIV/0!"))
        train <- read.csv("./data/train.csv",
                          header=TRUE,
                          stringsAsFactors=F,
                          na.strings=c("", "NA", "#DIV/0!"))
}

###############################################################################
#
#  2. Choose column data relating to sensors
#
###############################################################################
t <- train[grep("belt|forearm|arm|dumbbell|classe", colnames(train))]
te <- test[grep("belt|forearm|arm|dumbbell|problem_id", colnames(test))]


###############################################################################
#
#  3. Make sure data are in the right format for regression
#
###############################################################################
t1 <- as.data.frame(sapply(t[1:152], "as.numeric"))
te1 <- as.data.frame(sapply(te[1:152], "as.numeric"))
t1$classe <- factor(t$classe)
te1$problem_id <- factor(te$problem_id)

###############################################################################
#
#  4. Remove near-zero-variance variables and simplify variable names
#
###############################################################################
library(caret)
zeroVar <- nearZeroVar(t1)
t2 <- t1[, -zeroVar]
te2 <- te1[, -zeroVar]
t2Colnames <- colnames(t2)
t3 <- t2
te3 <- te2
colnames(t3)[1:117] <- as.character(1:117)
colnames(te3)[1:117] <- as.character(1:117)

###############################################################################
#
#  5. Train model
#
###############################################################################
library(doMC)
registerDoMC(cores = 2)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5,
                     number = 10)

# test-time model
inTrain <- createDataPartition(t3$classe, p=.1, list = FALSE)
t4 <- t3[inTrain,]
c50TuneCtrl <- train(x =t4[, -118],
                     y=t4[,118],
                     method = "C5.0",
                     trControl = ctrl)

# final model
c50TuneFinal <- train(x =t3[, -118],
                      y=t3[,118],
                      method = "C5.0",
                      preProc = c("center", "scale"),
                      trControl = ctrl)

# times
c50TuneCtrl$times$everything
c50TuneFinal$times$everything

# Accuracy of the final model
c50TuneFinal

###############################################################################
#
#  5. Predict (Coursera assignment)
#
###############################################################################

answers <- predict(c50TuneFinal, newdata=te3[,-118])
