#Learn MVABUND
#download rarefied_com.rds from https://github.com/Djeppschmidt/JellyBean_project
data<-readRDS("~/File.path/to/rarefied_com.rds")

### first, let's explore the data###

#How many samples are there?

#how many sequences does each sample have? (hint: use sample_sums())

#what sort of metadata?

#What might we use to standardize this dataset?

##transform sample counts
#first, define the transformation factor

adjustFactor<-(sum(total_abund)/length(total_abund))/total_abund
#data2<-round(as.matrix(otu_table(NormdistCommPS))*adjustFactor)

#isolate otu_table and apply the transformation(use your own R objects- they will be different than this example)
dat<-as.matrix(otu_table(data))*adjustFactor
dat<-round(dat)

#replace otu table in phyloseq object
data2<-data
otu_table(data2)<-otu_table(dat, taxa_are_rows = FALSE)

#Check your OTU table output . Does it look reasonable? are there decimals?(answer should be no!)

#add categories to the sampld data
categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
sample_data(data2)$categories<-categories

#save your modified phyloseq object using saveRDS(); we will use this as a starting point for all analyses

#now we are ready to start MVABUND!!
#tutorial: http://environmentalcomputing.net/introduction-to-mvabund/

#Example workflow:

library(mvabund)


#check the abundance distribution of species...
boxplot(otu_table(data2), horizontal = T, main="Abundance")
#check the mean variance relationship...
meanvar.plot(otu_table(data2))

#build a statistical model for your (e.g. explore the model distributions)
?manyglm
manyglm
mod1<-manyglm(otu_table(data2)~sample_data(data2)$F1, family="poisson")
mod2<-manyglm(otu_table(data2)~sample_data(data2)$F1+sample_data(data2)$F2+sample_data(data2)$F3+sample_data(data2)$F4+sample_data(data2)$F5, family="poisson")

#examine residual plot
plot(mod1)
plot(mod2)

#try another distribution
mod3<-manyglm(otu_table(data2)~sample_data(data2)$F1, family="negative_binomial")
plot(mod3)
mod4<-manyglm(otu_table(data2)~sample_data(data2)$F1+sample_data(data2)$F2+sample_data(data2)$F3+sample_data(data2)$F4+sample_data(data2)$F5, family="negative_binomial")
plot(mod4)

#run statistical test
anova(mod3)

#run test on each species individually
anova(mod3, p.uni="adjusted")


########################################

### Run Multiple times!

#raw dataset without normalization
#dataset with QPCR adjustment
#rarefaction without adjustment
#rarefaction+QPCR adjustment

### what is the difference? ###

#######################################
