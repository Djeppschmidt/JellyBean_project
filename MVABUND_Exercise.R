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
data2<-as.matrix(otu_table(data))*adjustFactor
data2<-round(data2)

#replace otu table in phyloseq object
otu_table(data2)<-otu_table(data2, taxa_are_rows = FALSE)

#Check your OTU table output . Does it look reasonable? are there decimals?(answer should be no!)


#save your modified phyloseq object using saveRDS(); we will use this as a starting point for all analyses

#now we are ready to start MVABUND!!
#tutorial: http://environmentalcomputing.net/introduction-to-mvabund/

library(mvabund)


#check the abundance distri ution of species...
boxplot(otu_table(data2), horizontal = T, main="Abundance")
#check the mean variance relationship...
meanvar.plot(otu_table(data2))

#build a statistical model for your
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
