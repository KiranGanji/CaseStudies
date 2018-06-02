#Libraries
library(data.table)
library(tm)
library(proxy)
library(RTextTools)
library(fpc)
library(cluster)
library(stringi)

#Setting the work directory
rm(list = ls())
setwd("D:/Personal/OLX")

#Main Code
DataDf <- fread("Data/za_sample_listings_incl_cat.csv")
DataDf[,V1:=NULL,]

#Data wrangling
MainDf <- DataDf[,-c("listing_description","category_l3_name_en","category_sk",
                     "category_l1_name_en"),with=F]

#Imputing missing values
MainDf[is.na(listing_price),listing_price:=-9999,]

#Creating a cluster for items
MainDfLevel1 <- MainDf[,c("item_id","listing_price","category_l2_name_en","listing_latitude",
                          "listing_longitude"),with=F]

#Creating one hot encoded values
uniquecats <- unique(MainDfLevel1$category_l2_name_en)
for(i in uniquecats){
  newcol <- i
  newcol <- gsub("&","",newcol)
  newcol <- gsub(" ","_",newcol)
  MainDfLevel1[,(newcol):=ifelse(category_l2_name_en == i,1,0),]
}
MainDfLevel1[,category_l2_name_en:=NULL,]

#Creating the clusters
set.seed(123)
clusters <- kmeans(MainDfLevel1[,-c("item_id"),with=F],110,iter.max = 100)
MainDfLevel1[,Cluster1:=clusters$cluster,]
MainDf[,Cluster1:=clusters$cluster,]
summary <- MainDfLevel1[,list(Counts =.N),by=list(Cluster1)]
setorder(summary,Counts)
# write.csv(MainDfLevel1[,c("item_id","Cluster1"),with=F],"ClustersLevel1.csv", row.names = F)

#Get the median count of the clusters
mediancounts <- median(summary$Counts)
clusters2 <- summary[Counts > mediancounts,Cluster1,]

FinalClusters <- data.frame()
for(i in clusters2){
  print(i)
  datamain <- MainDf[Cluster1 == i,c("item_id","listing_title"),with=F]
  colnames(datamain) <- c("doc_id","text")
  
  #Clean the documents
  docs1 <- Corpus(DataframeSource(datamain))
  docs1 <- tm_map(docs1, removePunctuation)
  docs1 <- tm_map(docs1, removeNumbers)
  docs1 <- tm_map(docs1, tolower)
  docs1 <- tm_map(docs1, removeWords, c(stopwords("english"),"get","got"))
  docs1 <- tm_map(docs1, stripWhitespace)
  dtm1 <- DocumentTermMatrix(docs1)
  m  <- as.matrix(dtm1)
  
  print("Calucalting the distance matrix")
  distMatrix <- dist(m, method="euclidean")
  groups <- hclust(distMatrix,method="ward.D")
  
  #Clustering
  cuttreeDf <- cutree(groups, k = 10)
  cuttreeDfMain <- data.table(item_id = datamain$doc_id, Clusters = cuttreeDf)
  cuttreeDfMain[,Clusters:=paste0("subcluster_",Clusters),]
  
  FinalClusters <- rbind(FinalClusters,cuttreeDfMain)
}

FinalMainClusters <- MainDf[,c("item_id","Cluster1"),with=F]
FinalMainClusters <- merge(FinalMainClusters,FinalClusters, by=c("item_id"), all.x=T)

#Cleaning up
FinalMainClusters[is.na(Clusters),Clusters:=Cluster1,]
FinalMainClusters[,FinalClusters:=paste0(Cluster1,"_",Clusters),]

#Numerically encode finalclusters
uniqueclusters <- unique(FinalMainClusters$FinalClusters)
FinalMainClusters[,FinalClustersNum:=match(FinalMainClusters,uniqueclusters),]

write.csv(FinalMainClusters[,c("item_id","FinalClustersNum"),with=F],"FinalClusters.csv", row.names = F)










