library(phyloseq)
library(plyr)

refcom<-readRDS("~/Documents/GitHub/JellyBean_project/RefCommNormDist.rds")


categories<-categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
key<-cbind(colMeans(as.data.frame(as.matrix(otu_table(refcom)[1:5]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[6:10]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[11:15]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[16:20]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[21:25]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[26:30]))))

keyVar<-cbind(colMeans(as.data.frame(as.matrix(otu_table(refcom)[1:5]))), colVars(as.data.frame(as.matrix(otu_table(refcom)[6:10]))), colVars(as.data.frame(as.matrix(otu_table(refcom)[11:15]))), colVars(as.data.frame(as.matrix(otu_table(refcom)[16:20]))), colVars(as.data.frame(as.matrix(otu_table(refcom)[21:25]))), colVars(as.data.frame(as.matrix(otu_table(refcom)[26:30]))))
