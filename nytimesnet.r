library(tm)
library(igraph)
library(RTextTools)

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
nytimestdm1<-removeSparseTerms(nytimestdm,sparse = 0.99)
nytimestdm1

#creating matrix
nytimesmat<-as.matrix(nytimestdm1)                               

#creating boolean matrix
nytimesmat[which(nytimesmat>0)]<-1

#creating adjacency matrix
nytmat2<-nytimesmat%*%t(nytimesmat)
nytmat20<-nytmat2[1:20,1:20]

#Building an adjacency graph
nyt.g<-graph.adjacency(nytmat20,weighted = "TRUE", mode = "undirected")
class(nyt.g)

#removing loops
nyt.g<-simplify(nyt.g)

#About vertices and edges
V(nyt.g)
E(nyt.g)
ecount(nyt.g)
vcount(nyt.g)

#Checking for degree
degree(nyt.g)

#Checking for weights
E(nyt.g)$weight

#randomly assigning weight
E(nyt.g)$weight<-runif(ecount(nyt.g))

#setting the labels and degree for vertices
V(nyt.g)$label<-V(nyt.g)$name
V(nyt.g)$degree<-degree(nyt.g)

#setting the layout
layout<-layout.fruchterman.reingold(nyt.g,dim=2)

plot(nyt.g,layout=layout,vertex.size=2,vertex.label.color="darkred")

#Applying effects
#For vertices
V(nyt.g)$label.cex<-1.1
V(nyt.g)$label.color<-"black"
V(nyt.g)$size<-12*V(nyt.g)$degree/max(V(nyt.g)$degree)

#For edges
edge_weight<- (-log(E(nyt.g)$weight))/max(-log(E(nyt.g)$weight))
E(nyt.g)$color<-rgb(0.6,0.3,0,edge_weight)


#Final plot
plot(nyt.g, layout=layout,vertex.color="lightblue")
