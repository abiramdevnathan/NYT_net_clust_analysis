library(tm)
library(igraph)
library(RTextTools)
library(ggdendro)

getwd()
setwd("D:\\Media\\Documents\\Data Science\\Repositories\\Text mining\\Network analysis")

nytimes<-read.csv("nytimes.csv",header = T,stringsAsFactors = F)

#Combining the columns
nytimes$comb<-paste(nytimes$Title,nytimes$Subject,sep = " ")

#Cleaning the data
#Creating corpus
nytimes.corpus<-Corpus(VectorSource(nytimes$comb))
inspect(nytimes.corpus[1:10])

#Cleaning the corpus
nytimes.corpus<-tm_map(nytimes.corpus,tolower)
nytimes.corpus<-tm_map(nytimes.corpus,stripWhitespace)
nytimes.corpus<-tm_map(nytimes.corpus,removePunctuation)
nytimes.corpus<-tm_map(nytimes.corpus,removeNumbers)
nytimes.corpus<-tm_map(nytimes.corpus,removeWords,c(stopwords("english"),"nyc","new","york","says"))

#termdocmat
nytimestdm<-TermDocumentMatrix(nytimes.corpus)
nytimestdm1<-removeSparseTerms(nytimestdm,sparse = 0.98)
nytimestdm1
nytimestdm2<-t(nytimestdm1)
nytmat<-as.matrix(nytimestdm1)

#finding the distance metric between terms in documents
distmatrix<-dist(scale(nytmat),method = "euclidean")

#Creating hclust
nyt.h<-hclust(distmatrix,method = "ward")


#Choosing the optimal value of k
withiness<-data.frame()
for(i in 1:20){
  kmean<-kmeans(nytimestdm2,i)
  withiness<-rbind(withiness,cbind(i,kmean$tot.withinss))
}
names(withiness)<-c("no of cluster","tot.withiness")

plot(withiness)
lines(withiness)
lines(stats::lowess(withiness))
#8 clusters is the best

#To cut the tree with 7 clusters
k<-cutree(nyt.h,k=7)
k
finaloutput<-as.data.frame(k)

#Plot dendogram
ggdendrogram(nyt.h,size=7,color="blue")
plot(as.dendrogram(nyt.h))
rect.hclust(nyt.h,7)
