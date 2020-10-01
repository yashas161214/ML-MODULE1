install.packages("mlbench")
library(mlbench)
data("HouseVotes84")


plot(as.factor(HouseVotes84[,2]))
title(main=' Votes cast for issue 1',xlab="vote", ylab=" num reps")


HouseVotes84$Class
repub <- HouseVotes84$Class=="republican"
democrat <- HouseVotes84$Class=="democrat"
repub
plot(as.factor(HouseVotes84[repub,2]))
title(main='republican votes for issue 1',xlab="vote", ylab=" num reps")
plot(as.factor(HouseVotes84[democrat,2]))
title(main='democrat votes for issue 1',xlab="vote", ylab=" num reps")





na_by_col_class <- function(col,cls){
  return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))
}

na_by_col_class



p_y_col_class <- function(col,cls){
  sum_y <- sum(HouseVotes84[,col]=="y" & HouseVotes84$Class==cls,na.rm = TRUE)
  sum_n <- sum(HouseVotes84[,col]=="n" & HouseVotes84$Class==cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))
}


p_y_col_class(5,"democrat")


p_y_col_class(5,"republican")


na_by_col_class(2,"democrat")
na_by_col_class(2,"republican")



for(i in 2:ncol(HouseVotes84)){
  if(sum(is.na(HouseVotes84[,i])>0)) {
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="democrat",arr.ind = TRUE)
    c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=="republican",arr.ind = TRUE)
    HouseVotes84[c1,i] <- 
      ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    HouseVotes84[c2,i] <- 
      ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")
      
  }
}



HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)



traincolnum <- grep("train",names(HouseVotes84))


trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-traincolnum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-traincolnum]



install.packages("e1071")
library(e1071)
nb_model <- naiveBayes(Class~.,data= trainHouseVotes84)


nb_model
summary(nb_model)
str(nb_model)


nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])

mean(nb_test_predict==testHouseVotes84$Class)


table(pred=nb_test_predict,true=testHouseVotes84$Class)


mean(nb_test_predict==testHouseVotes84$Class)



nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for(i in 1:n){
    HouseVotes84[,'train'] <- ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
    traincolnum <- grep('train',names(HouseVotes84))
    trainHouseVotes84 <- HouseVotes84{HouseVotes84$train==1,-traincolnum]
      testHouseVotes84 <-HouseVotes84{HouseVotes84$train==0,-traincolnum]
nb_model <- naiveBayes(Class~.,data=trainHouseVotes84)
nb_test_predict <- predict(nnb_model,testHouseVotes84[,-1])
fraction_correct[i] <- mean(nb_test_predict==testHouseVotes84$Class)
      }
    }
  
