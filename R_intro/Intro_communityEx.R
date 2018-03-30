#load packages
library(phyloseq)

#Import data
setwd("~/Downloads/")
#read.csv("file.csv")
dat<-readRDS("Group2_ps.rds")
head(sample_data(dat))
saveRDS(dat, "~/Desktop/file.rds")
#see sample data
sample_data(dat)

#see otu table
otu_table(dat)

sptab<-otu_table(dat)
sptab

#call species from species table

t(sptab)
sp1<-as.data.frame(as.matrix(t(sptab)))$Species5
sp1

?otu_table

sampledata<-sample_data(dat)
sampledata

#call componenets of the sample data table:
sampledata$GroupID

#barplot tutorial: https://joey711.github.io/phyloseq/plot_bar-examples.html

plot_bar(dat)

sum(as.data.frame(as.matrix(sptab))$Sample1)/sample_sums(dat)
sample_sums(dat)

#preprocess data tutorial: https://joey711.github.io/phyloseq/preprocess.html

#transform sample counts:
#inptus is data, function
#function is defined by user and can be any tranformation you want
#here are two examples:

#make relative abundance:
relabund<-transform_sample_counts(dat, function(OTU) OTU/sum(OTU))

f1<-function(input1, input2, c) {a+b-c}
f1(1,2,3)

otu_table(relabund)
#log transform
logabund<-transform_sample_counts(dat, function(OTU) log(OTU))
otu_table(logabund)



#####write linear models
##define variable
sp1<-as.data.frame(as.matrix(t(sptab)))$Species1
GroupID<-sampledata$SampleID
Altitude<-sampledata$Area
#build model sp1 dependent on site category + covariates
linear.model1<-lm(sp1~GroupID+Altitude)
summary(linear.model1)
anova(linear.model1)
#this is not the most elegant way of doing this...
anova<-anova(linear.model1)
summary(anova)
#to get covariates/coefficients
summary(linear.model1)

#objective 1: determine which species differ significantly between environments!

#objective 2: determine which environmental variables are important determinants!
