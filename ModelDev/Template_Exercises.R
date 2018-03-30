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
categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
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

#### Limma Voom ####



