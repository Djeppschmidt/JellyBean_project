rawdata<-readRDS("~/Documents/GitHub/JellyBean_project/rarefied_com.rds")

sample_data(rawdata)$categories<-categories

adjustFactor<-(sum(total_abund)/length(total_abund))/total_abund

adjustFactor


#adjusted data


rareData<-rarefy_even_depth(rawdata, sample.size=min(sample_sums(rawdata)), replace=FALSE)

adjData1<-as.matrix(otu_table(rawdata))*adjustFactor

adjdata<-rawdata
otu_table(adjdata)<-adjData1

adj.raredat1<-as.matrix(otu_table(adjdata))*adjustFactor
adj.raredat1<-round(adj.raredat1)
adj.raredat<-rareData
otu_table(adj.raredat)<-adj.raredat1

library(mvabund)

key<-cbind(colMeans(as.data.frame(as.matrix(otu_table(refcom)[1:5]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[6:10]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[11:15]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[16:20]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[21:25]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[26:30]))))

mod.rawmvab<-manyglm(otu_table(rawdata)~categories, family="negative_binomial")
mod.raremvab<-manyglm(otu_table(adj.raredat)~categories, family="negative_binomial")
mod.adjmvab<-manyglm(otu_table(adjdata)~categories, family="negative_binomial")

anova.adjrare<-anova(mod.adjmvab, p.uni="adjusted")
anova.rarabund<-anova(mod.raremvab, p.uni="adjusted")
anova.rawabund<-anova(mod.rawmvab, p.uni="adjusted")

anova.adjrare
anova.rarabund
anova.rawabund


#### DESeq2 ###

# raw
library(DESeq2)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

ddsRaw = phyloseq_to_deseq2(rawdata, ~categories)

RawgeoMeans = apply(counts(ddsRaw), 1, gm_mean)

ddsRaw = estimateSizeFactors(ddsRaw, geoMeans = RawgeoMeans)
ddsRaw = DESeq(ddsRaw, fitType="local")

res = results(ddsRaw)
res = res[order(res$padj, na.last=NA), ]
alpha = 0.01
sigtab = res[(res$padj < alpha), ]

#sigtab = cbind(as(sigtab, "data.frame"), as(tax_table(data)[rownames(sigtab), ], "matrix"))# we have no tax table!!
head(sigtab)

#rarefied

sample_data(adj.raredat)$categories<-categories
ddsRare = phyloseq_to_deseq2(adj.raredat, ~categories)

RaregeoMeans = apply(counts(ddsRare), 1, gm_mean)

ddsRare = estimateSizeFactors(ddsRare, geoMeans = RaregeoMeans)
ddsRare = DESeq(ddsRare, fitType="local")

res2 = results(ddsRare)
res2 = res2[order(res2$padj, na.last=NA), ]
alpha = 0.01
sigtabRare = res2[(res2$padj < alpha), ]
sigtabRare

#adjusted

sample_data(adjdata)$categories<-categories
ddsadj = phyloseq_to_deseq2(adjdata, ~categories)

AdjgeoMeans = apply(counts(ddsadj), 1, gm_mean)

ddsadj = estimateSizeFactors(ddsadj, geoMeans = AdjgeoMeans)
ddsadj = DESeq(ddsadj, fitType="local")

res3 = results(ddsadj)
res3 = res3[order(res3$padj, na.last=NA), ]
alpha = 0.01
sigtabAdj = res3[(res3$padj < alpha), ]
sigtabAdj
####

#IF YOU WANT TO USE STABILIZED DATA FOR OTHER APPROACHES:  ###
#will maybe test this approach later in the semester#

altmethod
ddsRaw = phyloseq_to_deseq2(rawdata, ~categories)
ddsEST = estimateSizeFactors(ddsRaw)
diaEST = estimateDispersions(ddsEST)
diagvst = getVarianceStabilizedData(diaEST)#generate var sabilized data


otu_table(data) <- otu_table(diagvst, taxa_are_rows = TRUE)#replace data with variance stabilized


#### Limma Voom ####


#raw
rawOTU<-as.matrix(t(otu_table(rawdata)))
sData<-sample_data(rawdata)
attach(sData)
rawdge<-DGEList(counts=rawOTU)
rawdge<-calcNormFactors(rawdge) #calculate the normalization factors via EdgeR (see EdgeR documentation)

design<-model.matrix(~categories)
rawV <- voom(rawdge, design, plot=TRUE)
rawFit<-lmFit(rawV, design)
rawContr<-makeContrasts(categories , levels=colnames(coef(rawFit)))
rawTmp<-contrasts.fit(rawFit, rawContr)
rawTmp<-eBayes(rawTmp)
rawTmp2<-topTable(rawTmp, coef=1, sort.by="P", n="INF")#add column for bayesian support
rawTmp2

#rare
rareOTU<-as.matrix(t(otu_table(adj.raredat)))
#sData<-sample_data(rawdata)
#attach(sData)
raredge<-DGEList(counts=rareOTU)
raredge<-calcNormFactors(raredge) #calculate the normalization factors via EdgeR (see EdgeR documentation)

#design<-model.matrix(~categories)
rareV <- voom(raredge, design, plot=TRUE)
rareFit<-lmFit(rareV, design)
rareContr<-makeContrasts(categories , levels=colnames(coef(rareFit)))
rareTmp<-contrasts.fit(rareFit, rareContr)
rareTmp<-eBayes(rareTmp)
rareTmp2<-topTable(rareTmp, coef=1, sort.by="P", n="INF")#add column for bayesian support
rareTmp2


#adjusted
adjOTU<-as.matrix(t(otu_table(adjdata)))
#sData<-sample_data(rawdata)
#attach(sData)
adjdge<-DGEList(counts=adjOTU)
adjdge<-calcNormFactors(adjdge) #calculate the normalization factors via EdgeR (see EdgeR documentation)

#design<-model.matrix(~categories)
adjV <- voom(adjdge, design, plot=TRUE)
adjFit<-lmFit(adjV, design)
adjContr<-makeContrasts(categories , levels=colnames(coef(adjFit)))
adjTmp<-contrasts.fit(adjFit, adjContr)
adjTmp<-eBayes(adjTmp)
adjTmp2<-topTable(adjTmp, coef=1, sort.by="P", n="INF")#add column for bayesian support
adjTmp2
