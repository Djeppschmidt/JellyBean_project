library(limma)
library(Glimma)
library(edgeR)

data<-readRDS("~/File.path/to/rarefied_com.rds")

adjustFactor<-(sum(total_abund)/length(total_abund))/total_abund
#data2<-round(as.matrix(otu_table(NormdistCommPS))*adjustFactor)

#isolate otu_table and apply the transformation(use your own R objects- they will be different than this example)
data2<-as.matrix(otu_table(data))*adjustFactor
data2<-round(data2)

########################################

### Run Multiple times!

#raw dataset without normalization
#dataset with QPCR adjustment
#rarefaction without adjustment
#rarefaction+QPCR adjustment

### what is the difference? ###

#######################################


###Sample workflow:
sData<-sample_data(data)

#?DGEList
otutab<-as.matrix(t(otu_table(data)))#prepare OTU table for analysis
head(otutab)#inspect the OTU table
?DGEList#figure out how to set up next step...
dge<-DGEList(counts=otutab)#make OTU table into DGEList object
#do a scale normalization
dge<-calcNormFactors(dge) #calculate the normalization factors via EdgeR

categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5)) #make more metadata
sData$cat<-categories #add treatment effects to metadata?
attach(sData)


design<-model.matrix(~categories) #specify the model
v <- voom(dge, design, plot=TRUE) #make the voom object to test

fit<-lmFit(v, design) #construct the lm fit
?makeContrasts
contr<-makeContrasts(categories , levels=colnames(coef(fit))) #make treatment contrasts

?contrasts.fit
tmp<-contrasts.fit(fit, contr) #run contrasts
 ?ebayes
tmp<-eBayes(tmp) #use bayesian statistics to determine significantly different taxa! ranks genes in order of evidence of differential expression!

tmp2<-topTable(tmp, coef=1, sort.by="P", n="INF") #make output table
tmp2
