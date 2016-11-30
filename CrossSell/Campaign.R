#==========================================================================#
#@Author: Ganji Kiran
#Email: ganji.kiran99@gmail.com
#Contact: +91 9495176026
#Code written for Data Analysis, Modeling and Insights generation
#=========================================================================#
#Libraries
library(data.table)#For data mainpulation purposes
library(ggplot2)#For visualizing
library(ROCR)#For constructing ROCR graphs
library(partykit)#For Conditional Inference models
library(caTools)#For Splitting the data
library(caret)#For Confusion Matrix
library(randomForest)#For Random Forest
library(rminer)#Probability plots
#Setting the Directory
rm(list = ls())
setwd("E:/Work/Gramener/Data Exercise_Analytics")

#Functions
varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
  vars <- character(0)
  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  
  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}

getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

collapse<-function(x){
  x<-sub(" ","_",x)
  return(x)
}

EveRate <- function(Df){
  eve <- nrow(Df[response == "yes",,])/(nrow(Df))
  return(eve)
}

multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {rbind(x,y)}, datalist)
}

#========================= Reading Data and getting variables type =========================#

DataMain <- fread("CampaignData.csv", stringsAsFactors = TRUE)

NumVar <- varlist(DataMain,type="numeric")
FacVars <- varlist(DataMain,type="factor")
CharVars <- varlist(DataMain,type="character")

#========================= Reading Data and getting varibales type ends here =========================#

#========================= Basic Summary starts here ======================#

#Getting the Cat and Numerical Variables
SummaryDf <- data.frame()
SummaryDf[1,"TotCols"] <- ncol(DataMain)
SummaryDf[1,"NumericalCols"] <- length(NumVar)
SummaryDf[1,"FactorCols"] <- length(FacVars)
SummaryDf[1,"CharCols"] <- length(CharVars)

#Summarizing Categorical variables
FacVarsDf <- as.data.frame(FacVars)
i<-1
for(catvar in FacVars){
  FacVarsDf[i,"TrainLevels"] <- length(levels(DataMain[,get(catvar),]))
  i <- i+1
}


#Summarizing Numerical Variables
NumVarsDf <- as.data.frame(NumVar)
k <- 1
for(i in NumVar){
  u <- summary(DataMain[,get(i),])
  l <- 1
  for(j in names(u)){
    NumVarsDf[k,j] <- u[[l]]
    l <- l+1
  }
  NumVarsDf[k,"Unique_Values"] <- length(unique(DataMain[,get(i),]))
  k <- k+1
}

write.csv(NumVarsDf,"NumericalAnalysis.csv", row.names = FALSE)

#========================= Basic Summary ends here ======================#

#========================== Associative Studies =========================#
#Find associations between cat and numerical
#Find associations between cat and cat

#Plots start here
#Numerical to Categorical variables associations
for(j in FacVars){
  for(i in NumVar){
    m <- ggplot(DataMain, aes_string(x=i))
    m + geom_histogram(aes(fill = response)) + facet_grid(as.formula(paste("response ~",j)))
    ggsave(paste0("Plots/",j,"~",i,".png"), width = 15, height = 8, units = "in")
  }
}


#All the numerical
for(i in NumVar){
  m <- ggplot(DataMain, aes_string(x=i))
  
  m + geom_histogram(aes(fill = response))
  ggsave(paste0("Plots/NumericalNormal/",i,".png"), width = 15, height = 8, units = "in")
  
  m + geom_histogram(aes(fill = response, y = ..density..)) + geom_density()
  ggsave(paste0("Plots/NumericalDensity/",i,".png"), width = 15, height = 8, units = "in")
  
  m + geom_histogram(aes(fill = response)) + facet_grid(~response)
  ggsave(paste0("Plots/NumericalFacets/",i,".png"), width = 15, height = 8, units = "in")
  
  m + geom_histogram(aes(fill = response, y = ..density..)) + geom_density() + facet_grid(~response)
  ggsave(paste0("Plots/NumericalFacetsDen/",i,".png"), width = 15, height = 8, units = "in")
}
#Plots end here

#Associative Studies for Cat Variables
DataMain2 <- DataMain[,as.vector(FacVars),with=FALSE]

DataMain2 <- DataMain2[job!= "unknown",,]
DataMain2 <- DataMain2[education!= "unknown",,]
DataMain2 <- DataMain2[marital!= "unknown",,]
DataMain2 <- DataMain2[default!= "unknown",,]
DataMain2 <- DataMain2[housing!= "unknown",,]
DataMain2 <- DataMain2[loan!= "unknown",,]

DataMain2 <- droplevels(DataMain2)

SquarTestDf <- data.frame()
counts <- 1
for(i in FacVars){
  for(j in FacVars){
    if(i!=j){
      u <- chisq.test(DataMain2[,get(i),], DataMain2[,get(j),])
      SquarTestDf[counts,"LeftVar"] <- i
      SquarTestDf[counts,"RightVar"] <- j
      SquarTestDf[counts,"X-Squared"] <- u$statistic
      SquarTestDf[counts,"p-value"] <- u$p.value
      SquarTestDf[counts,"Degrees"] <- u$parameter
      
      counts <- counts +1
    }
  }
}

write.csv(SquarTestDf, "ChisquareAnalysis.csv", row.names = FALSE)

#Assoicative Studies for Numerical variables
Numvar2 <- as.vector(NumVar)
NumVarsDf <- DataMain[,(Numvar2), with=FALSE]
CorMatrix <- cor(NumVarsDf)
write.csv(CorMatrix,"NumericalAssos.csv", row.names = FALSE)

#========================== Associative Studies end here =========================#

#======================== Misc studies start here ================================#
#Duration Analysis
DataMain[,DurationMinutes:=duration/60,]
DataMain[,DurationHour:=duration/3600,]

DataMain2 <- DataMain[duration>1500,,]
DataMain2 <- DataMain[response == "yes",,]
DataMain3 <- DataMain[response == "no",,]

#seconds
m <- ggplot(DataMain, aes(duration))
m + geom_histogram(binwidth = 100, aes(fill = response))+facet_grid(~response)


#minutes
m <- ggplot(DataMain2, aes(DurationMinutes))
m + geom_histogram(binwidth = 1, aes(fill = response, y=..density..)) + geom_density() + facet_grid(~response)
ggsave(paste0("Plots/DurationMinutesFacets1500.png"), width = 15, height = 8, units = "in")


#hour
m <- ggplot(DataMain2, aes(DurationHour))
m + geom_histogram(binwidth = 0.01, aes(fill = response))


#Getting the conversion rates
ConversionDf <- data.frame()
for(i in FacVars){
  u <- data.frame(unclass(summary(DataMain[,get(i),])))
  v <- data.frame(unclass(summary(DataYesResponse[,get(i),])))
  z <- cbind(u,v)
  colnames(z)[1] <- "Total"
  colnames(z)[2] <- "Yes Reponse"
  z$ConversionRate <- z$`Yes Reponse`/z$Total
  z$Column <- i
  
  ConversionDf <- rbind(z, ConversionDf)
}

ConversionDf <- cbind(row.names(ConversionDf), ConversionDf)
row.names(ConversionDf) <- NULL
colnames(ConversionDf)[1] <- "FactorName"
ConversionDf <- data.table(ConversionDf)
ConversionDf <- ConversionDf[Column!="response"]
write.csv(ConversionDf, "ConversionRates.csv")


#======================== Misc studies end here ================================#

#======================== Modeling Starts here =================================#
#Split the data into train and test (Going for 70 30 split)
set.seed(400)
split =sample.split(DataMain$response, SplitRatio=0.70)
DtTrain = subset(DataMain, split==TRUE)
DtTest = subset(DataMain, split==FALSE)

DtTrain <- droplevels(DtTrain)
DtTest <- droplevels(DtTest)

#Quick check to see if the split is proper
FacVarsDf <- as.data.frame(FacVars)
i<-1
for(catvar in FacVars){
  FacVarsDf[i,"TrainLevels"] <- length(levels(DtTrain[,get(catvar),]))
  i <- i+1
}

i <- 1
for(catvar in FacVars){
  FacVarsDf[i,"TestLevels"] <- length(levels(DtTest[,get(catvar),]))
  i <- i+1
}

#Check if the categorical variables present in Test are also in Train
i <- 1
for(catvar in FacVars){
  TMat <- levels(DtTest[,get(catvar),]) %in% levels(DtTrain[,get(catvar),])
  TFinal <- ifelse(FALSE %in% TMat, FALSE, TRUE)
  FacVarsDf[i,"DiffFacs"] <- TFinal
  if(TFinal == FALSE){
    FacVarsDf[i,"LevelsDiff"] <- length(TMat[TMat == FALSE])
  }else{
    FacVarsDf[i,"LevelsDiff"] <- 0
  }
  i <- i+1
}
#FacVarsDf proved that the levels are consistent and so can go ahead with the data


#Remove/Change of variables

DtTrain[,duration:=NULL,]
DtTest[,duration:=NULL,]

colstodelete <- c("housing", "loan", "previous","nr.employed", "emp.var.rate", "cons.price.idx")

DtTrain[,(colstodelete):=NULL,]
DtTest[,(colstodelete):=NULL,]


#===================Logistic Model
#Constructing the model
glmmodel <- glm(response ~ .,data=DtTrain, family=binomial)
glmstep <- step(glmmodel, sacle= 0, direction =c("both", "backward", "forward"))

glm_pred <- predict(glmstep, type="response")
predictTest <- predict(glmstep, type="response", newdata=DtTest)

#Testing the model
ROCRPred <- prediction(glm_pred,DtTrain$response)
ROCRpredTest = prediction(predictTest, DtTest$response)
ROCRperf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

l<-c(1:length(predictTest))
for(i in l){
  if(predictTest[i]<=0.2){#Change the threshold here
    predictTest[i]<-"no"
  }
  else{
    predictTest[i]<-"yes"
  }
}

#AUC
auc_test <- as.numeric(performance(ROCRpredTest, "auc")@y.values)

#Confusion Matrices
u <- table(predictTest, DtTest$response)
confusionMatrix(u, positive = "yes")

#=========Modeling CTree
TreeModel <- ctree(response ~., data = DtTrain)
plot(TreeModel, gp = gpar(fontsize = 6), inner_panel=node_inner, ip_args=list(abbreviate = TRUE, id = TRUE))
summary(TreeModel)

#Testing the model
Preds <- predict(TreeModel, DtTest)
CtreePreds <- as.data.frame(predict(TreeModel, DtTest, type = "prob"))$yes
ROCRPredtree <- prediction(CtreePreds,DtTest$response)

#AUC Values
auc_tree <- as.numeric(performance(ROCRPredtree, "auc")@y.values)

#Confusion Matrix
v <- table(Preds, DtTest$response)
confusionMatrix(v, positive = "yes")

#======================Random Forest Model
RfModel <- randomForest(response ~ ., data=DtTrain, importance=TRUE, ntree=400)
varImpPlot(RfModel)
Pred_rf <- predict(RfModel, DtTest)

#Confusion Matrix
w <- table(Pred_rf, DtTest$response)
confusionMatrix(w, positive='yes')

#ROC Curve
Pred_rfP <- as.data.frame(predict(RfModel, DtTest, type = "prob"))$yes
ROCRPredRf <- prediction(Pred_rfP,DtTest$response)
ROCRperfRf2 <- performance(ROCRPredRf,"tpr","fpr")
plot(ROCRperfRf2, col = "blue", main="ROC Curve",print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
abline(0,1,col="red")
grid(col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)

#Lift Chart
Pred_rfP <- as.data.frame(predict(RfModel, DtTest, type = "prob"))$yes
ROCRPredRf <- prediction(Pred_rfP,DtTest$response)
perf <- performance(ROCRPredRf,"lift","rpp")
plot(perf, main="Lift Chart", xlab="% Populations", ylab="Lift", col = "blue")
abline(1,0,col="red")
abline(v = 0, col = "blue")
grid(col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)

#Adjusting the sensitivity
Pred_rfP <- as.data.frame(predict(RfModel, DtTest, type = "prob"))$yes
for(i in 1:length(Pred_rfP)){
  if(Pred_rfP[i]<=0.16){#Change the threshold here
    Pred_rfP[i]<-"no"
  }
  else{
    Pred_rfP[i]<-"yes"
  }
}

z <- table(Pred_rfP, DtTest$response)
confusionMatrix(z, positive='yes')

#Extracting the rules from Random Forest
tree<-getTree(RfModel, k=1, labelVar=TRUE)
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
length(rules)

RulesDf <- data.frame()
counts <- 1
for(i in 1:length(rules)){
  if(grepl("yes", as.character(rules[i])) == TRUE){
    RulesDf[counts, "Rule"] <- as.character(rules[i])
    counts <- counts +1
  }  
}

write.csv(RulesDf, "RFRules.csv", row.names = FALSE)

#======================== Modeling ends here =================================#

#======================== Insights generation starts here =================================#

#Playing with CTrees to generate the insights
agebreaks <- c(1,20,35,60,150)
agelabels= c("Young","Young Adults","Adults","Old")
DataMain[,agegroups:= cut(age, breaks= agebreaks, right= FALSE, labels= agelabels)]

#Preparing the data
set.seed(400)
split =sample.split(DataMain$response, SplitRatio=0.70)
DtTrain = subset(DataMain, split==TRUE)
DtTest = subset(DataMain, split==FALSE)

DtTrain <- droplevels(DtTrain)
DtTest <- droplevels(DtTest)

colstodelete <- as.vector(NumVar)
colstodelete <- append(colstodelete, c("month","day_of_week","housing","loan","contact"))


DtTrain[,(colstodelete):=NULL,]
DtTest[,(colstodelete):=NULL,]
colnames(DtTrain)

model_form <- "response ~ euribor3m+cons.conf.idx+contact+agegroups"
TreeModel <- ctree(as.formula(model_form), data = DtTrain)
plot(TreeModel, gp = gpar(fontsize = 6), inner_panel=node_inner, ip_args=list(abbreviate = TRUE, id = TRUE))

#Extracting the rules and calculating the event rate in each node
ResultDf <- as.data.frame(partykit:::.list.rules.party(TreeModel))
colnames(ResultDf)[1] <- "Path"

EvrtRateDf <- data.frame()
counts <- 1
for(i in 1:nrow(ResultDf)){
  ep <- as.character(ResultDf[i,])
  EvrtRateDf[counts,"Node"] <- as.numeric(row.names(ResultDf)[i])
  EvrtRateDf[counts,"Path"] <- ep
  eval(parse(text = paste0("Df2 <- DataMain[",ep,",,]")))
  EvrtRateDf[counts,"EvertRate"] <- EveRate(Df2)
  EvrtRateDf[counts,"Formula"] <- model_form
  counts <- counts +1
}

write.csv(EvrtRateDf,"Insights/EventRatefailure2.csv", row.names = FALSE)

#Merging several files
failure <- multmerge(paste0(getwd(),"/Insights/Failure/"))
failure$Sucesstype <- "Failure"
sucess <- multmerge(paste0(getwd(),"/Insights/Success/"))
sucess$Sucesstype <- "Success"
Grah <- multmerge(paste0(getwd(),"/Insights/Graphs/"))
total <- rbind(failure, sucess)
write.csv(total, "Insights/COmbined.csv", row.names = FALSE)
write.csv(Grah,"Insights/Graph.csv", row.names = FALSE)


#Importance plot
M <- fit(response~.,DtTrain,model="randomForest")
imp <-Importance(M, DtTrain)
vecplot(imp,graph="VEC",xval=4,main="Call Duration Relevance",
        Grid=10,TC=2,sort="decreasing")

impDf <- data.frame()
for(i in 1:13){
  xvals <- as.data.frame(imp$sresponses[[i]]$x)
  yvals <- as.data.frame(imp$sresponses[[i]]$y)$yes
  
  vals <- cbind(xvals, yvals)
  colnames(vals)[1] <- "xVal"
  colnames(vals)[2] <- "Pr.Success"
  vals$colname <- imp$sresponses[[i]]$n
  
  impDf <- rbind(impDf,vals)
}

write.csv(impDf,"ImportaceDf.csv", row.names = FALSE)
#========================== Insights generation ends here ============================#




