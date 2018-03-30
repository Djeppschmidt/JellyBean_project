#objective 1: determine which species differ significantly between environments!


#####Species abundance transformations


###
###
###
###    Here are the instructions for what you do!
###    The code is the correct manipulation
###    But does it use the appropriate factors?
###    You decide.

#Download and Import all the data
dat1<-readRDS("Group1_ps.rds")
dat2<-readRDS("Group2_ps.rds")
dat3<-readRDS("Group3_ps.rds")

#first, isolate species table from phyloseq
#(you only need to do this once)
table1<-as.data.frame(as.matrix(t(sptab)))

#extract relevant data from sample data in each file
#e.g:
Altitude<-sample_data(dat1)$Altitude

#you may make a dataframe for the sample data as follows:
#keep in mind that each component is an object you've created previously
#this can be extracted from previous datasets, for example
sample.data<-data.frame(SampleID, GroupID, relDens, ... etc)


#make ratios
#for this: should have an object of Area (or Density)
#e.g. Area<-sampledata$Area
Area<-sample_data(dat2)$Area
#calculate density for each? is this necessary?
dens<-sample_data(dat3)$TotalOrg/sample_data(dat2)$Area
#if this doesn't work, make sure you've loaded dat2 into the environment
#or if you have TotalOrg as an object: dens<-TotalOrg/Area

# calculate the relative density sampled for each sample
relDens<-dens/mean(dens)

#you may make a dataframe for the sample data as follows:
#keep in mind that each component is an object you've created previously
#this can be extracted from previous datasets, for example
SampleID<-sample_data(dat2)$SampleID
#put dataframe together:
sample.data<-data.frame(SampleID, GroupID, relDens, ... etc)
#name rows to match for later
rownames(sample.data)<-c("Sample1","Sample2","Sample3","Sample4","Sample5","Sample6")

#now multiply the relative density times species values
#this multiplies your vector "relDense" with each column of your dataframe
adjDensTab<-relDens*table1

#you can make a new phyloseq object with your new dataset (if you wish)
newdat<-phyloseq(otu_table(adjDensTab, taxa_are_rows = FALSE), sample_data(sample.data))


#you can add any data you forgot directly
#(or any other sample Data) to dataframe
sample_data(newdat)$relDens<-relDens
#check work:
sample_data(newdat)$relDens

#now us lm to test differences like you did in class