library(phyloseq)

rawdata<-readRDS("~/Documents/GitHub/JellyBean_project/rarefied_com.rds")
categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
sample_data(rawdata)$categories<-categories

total_abund<-sample_data(rawdata)$total_abund

adjustFactor<-total_abund/(sum(total_abund)/length(total_abund))
sum(total_abund)/length(total_abund)
total_abund

adjustFactor

a2<-adjustFactor*15000
#adjusted data


rareData<-rarefy_even_depth(rawdata, sample.size=min(sample_sums(rawdata)), replace=FALSE)

adjData1<-as.matrix(otu_table(rawdata))*adjustFactor

adjdata<-rawdata
otu_table(adjdata)<-round(adjData1)

# rarefaction to uneven depth
rrarefy2<-
  function (x, sample, replace)   {
    if (!identical(all.equal(x, round(x)), TRUE))
      stop("function is meaningful only for integers (counts)")
    x <- as.matrix(x)
    if (ncol(x) == 1)
      x <- t(x)
    if (length(sample) > 1 && length(sample) != nrow(x))
      stop(gettextf("length of 'sample' and number of rows of 'x' do not match"))
    sample <- rep(sample, length = nrow(x))
    colnames(x) <- colnames(x, do.NULL = FALSE)
    nm <- colnames(x)
    if (any(rowSums(x) < sample))
      warning("Some row sums < 'sample' and are not rarefied")
    for (i in 1:nrow(x)) {
      if (sum(x[i, ]) <= sample[i])
        next
      row <- sample(rep(nm, times = x[i, ]), sample[i], replace)
      row <- table(row)
      ind <- names(row)
      x[i, ] <- 0
      x[i, ind] <- row
    }
    x
    }

r2dat<-rrarefy2(otu_table(rawdata), a2, replace = FALSE)
r2data<-rawdata
head(r2data)

otu_table(r2data)<-otu_table(r2dat, taxa_are_rows=FALSE)
sample_sums(r2data)


adj.raredat1<-as.matrix(otu_table(rareData))*adjustFactor
adj.raredat1<-round(adj.raredat1)
adj.raredat<-rareData
otu_table(adj.raredat)<-adj.raredat1

###
###
### KEY!!  ######
###
###

key<-cbind(colMeans(as.data.frame(as.matrix(otu_table(refcom)[1:5]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[6:10]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[11:15]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[16:20]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[21:25]))), colMeans(as.data.frame(as.matrix(otu_table(refcom)[26:30]))))

###
###
### KEY!!  ######
###
###

###
###
### Add adonis!! ####
###
###

library(vegan)

rawOut<-adonis()
RareOut<-adonis()
adjRareOut<-adonis()
R2Out<-adonis()

###
###
### Test MVABUND ####
###
###
library(mvabund)
head(otu_table(r2data))
mod.rawmvab<-manyglm(otu_table(rawdata)~categories, family="negative_binomial")
mod.adjrare<-manyglm(otu_table(adj.raredat)~categories, family="negative_binomial")
mod.rare<-manyglm(otu_table(rareData)~categories, family="negative_binomial")
mod.rare2mvab<-manyglm(otu_table(r2data)~categories, family="negative_binomial")

anova.adjrare<-anova(mod.adjrare, p.uni="adjusted")
anova.rawmvab<-anova(mod.rawmvab, p.uni="adjusted")
anova.rare<-anova(mod.rare, p.uni="adjusted")
anova.rare2mvab<-anova(mod.rare2mvab, p.uni="adjusted")

anova.adjrare
anova.rare
anova.rawmvab
anova.rare2mvab

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
alpha = 0.05
sigtab = res[(res$padj < alpha), ]
outraw<-data.frame(rownames(res), res$padj)

#sigtab = cbind(as(sigtab, "data.frame"), as(tax_table(data)[rownames(sigtab), ], "matrix"))# we have no tax table!!
head(sigtab)

#rarefied
ddsRare = phyloseq_to_deseq2(rareData, ~categories)

RaregeoMeans = apply(counts(ddsRare), 1, gm_mean)

ddsRare = estimateSizeFactors(ddsRare, geoMeans = RaregeoMeans)
ddsRare = DESeq(ddsRare, fitType="local")

res2 = results(ddsRare)
res2 = res2[order(res2$padj, na.last=NA), ]
alpha = 0.05
sigtabRare = res2[(res2$padj < alpha), ]
sigtabRare
outraw<-data.frame(rownames(res2), res2$padj)

#adjusted

ddsadj = phyloseq_to_deseq2(adj.raredat, ~categories)

AdjgeoMeans = apply(counts(ddsadj), 1, gm_mean)

ddsadj = estimateSizeFactors(ddsadj, geoMeans = AdjgeoMeans)
ddsadj = DESeq(ddsadj, fitType="local")

res3 = results(ddsadj)
res3 = res3[order(res3$padj, na.last=NA), ]
alpha = 0.05
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

#NEW Rarefaction

ddsRare2 = phyloseq_to_deseq2(r2data, ~categories)

Rare2geoMeans = apply(counts(ddsRare2), 1, gm_mean)

ddsRare2 = estimateSizeFactors(ddsRare2, geoMeans = Rare2geoMeans)
ddsRare2 = DESeq(ddsRare2, fitType="local")

res4 = results(ddsRare2)
res4 = res4[order(res4$padj, na.last=NA), ]
alpha = 0.01
alpha2 = 0.05
sigtabRare2 = res4[(res4$padj < alpha2), ]
sigtabRare2


#### Limma Voom ####
biocLite("limma")

library(limma)
library(edgeR)
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
total_abund


#rare
rareOTU<-as.matrix(t(otu_table(rareData)))
sData<-sample_data(rareData)
attach(sData)
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
adjOTU<-as.matrix(t(otu_table(adj.raredat)))
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

#rare2
rare2OTU<-as.matrix(t(otu_table(r2data)))
#sData<-sample_data(rawdata)
#attach(sData)
rare2dge<-DGEList(counts=rare2OTU)
rare2dge<-calcNormFactors(rare2dge) #calculate the normalization factors via EdgeR (see EdgeR documentation)
?calcNormFactors
#design<-model.matrix(~categories)
rare2V <- voom(rare2dge, design, plot=TRUE)
rare2Fit<-lmFit(rare2V, design)
rare2Contr<-makeContrasts(categories , levels=colnames(coef(rareFit)))
rare2Tmp<-contrasts.fit(rare2Fit, rareContr)
rare2Tmp<-eBayes(rare2Tmp)
rare2Tmp2<-topTable(rare2Tmp, coef=1, sort.by="P", n="INF")#add column for bayesian support
rare2Tmp2
