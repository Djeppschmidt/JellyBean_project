#Learn MVABUND

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
otu_table(data)<-otu_table(data2, taxa_are_rows = FALSE)
       
#Check your OTU table output . Does it look reasonable? are there decimals?(answer should be no!)


#save your modified phyloseq object using saveRDS(); we will use this as a starting point for all analyses

#now we are ready to start MVABUND!!
#tutorial: http://environmentalcomputing.net/introduction-to-mvabund/

library(mvabund)



