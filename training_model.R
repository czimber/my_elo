source('~/Documents/R/my_elo/process_data.R')
##############################################
## WHITE WINS
##############################################
## White - White WINS ------------------------
##############################################
### Select columns ---------------------------
train_df <- cbind(df2, df_move, 
                  df_w_ALL_weak, df_w_undec_weak, df_w_ALL_no_weak, 
                  df_w_undec_no_weak, df_b_ALL_weak, df_b_undec_weak, 
                  df_b_ALL_no_weak, df_b_undec_no_weak)
#####################################################################################
########## Seperate out training set -----------------
      train_name <- train_df[1:25000, ]
      train_name_10 <- subset(train_name, WhiteScore == 1)
########## SELECT WhiteScore -------------------------
      train_name <- train_name_10 # WhiteScore == 1
#####################################################################################
########## createDataPartition for testing ---------
      set.seed(1234)
      splitIndex <- createDataPartition(train_name[, 2], p = .75, list = FALSE, times = 1)
      trainDF <- train_name[ splitIndex,]
      testDF  <- train_name[-splitIndex,]
      trainDF1 <- trainDF
      testDF1 <- testDF
########## Seperate out: Event WhiteElo BlackElo elo.avg w.elo.diff
      xTrain <- trainDF[, -c(1:6)]
      xTrain1 <- trainDF[, -c(1:6)]
########## Seperate out: Event WhiteElo BlackElo elo.avg w.elo.diff
      xTest <- testDF[, -c(1:6)]
      xTest1 <- testDF[, -c(1:6)]
#####################################################################################
## Train for White ---------------------------
yTrain <- trainDF1[, 2]
yTrain1 <- trainDF1[, 2]
yTest <- testDF1[, 2]
yTest1 <- testDF1[, 2]

##############################################
### Center and scale -------------------------
# Training data ------------------------------
preProcValues <- preProcess(xTrain, method = c("center", "scale"))
xTrain <- predict(preProcValues, xTrain)
# Testing data -------------------------------
preProcValues <- preProcess(xTest, method = c("center", "scale"))
xTest <- predict(preProcValues, xTest)


## Begin Models
## svm
##############################################
### svm model
### is.na to 0 = YES
### center and scale = YES 
### remove LinerarCombos = NO
##############################################
xTrain[is.na(xTrain)] <- 0
xTest[is.na(xTest)] <- 0

cvCtrl <- trainControl(method = "repeatedcv", repeats = 3)

?ksvm

svmTune <- train(x=xTrain,
                 y=yTrain,
                 method = "svmRadial",
                 tuneLength=9,
                 preProc = c("center", "scale"),
                 metric = "MAE",
                 trControl = cvCtrl)


ksvm_WWW <- ksvm(yTrain  ~. , data=xTrain, # [1] 182.9708
                 prob.model=TRUE, # [1] 182.945
                 type="nu-svr")

pred_ksvm_WWW <- predict(ksvm_WWW, xTest)  
print(mae(yTest, pred_ksvm_WWW)) # [1] 183.0288 
