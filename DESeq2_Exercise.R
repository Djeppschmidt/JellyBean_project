#tutorial here: http://joey711.github.io/phyloseq-extensions/DESeq2.html
library(phyloseq)

data<-readRDS("~/Documents/GitHub/JellyBean_Project/rarefied_com.rds")

#DESeq2 assumes that there have been no transformations of the data
#therefore we don't modify counts based on environmental abundance

data

sample_data(data)
otu_table(data)
library(DESeq2)

#  http://www.bioconductor.org/packages/release/bioc/vignettes/phyloseq/inst/doc/phyloseq-mixture-models.html

diagdds = phyloseq_to_deseq2(data, ~categories)
# calculate geometric means prior to estimate size factors
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }

  #use equation here:
geoMeans = apply(counts(diagdds), 1, gm_mean)
diagdds = estimateSizeFactors(diagdds, geoMeans = geoMeans)
diagdds = DESeq(diagdds, fitType="local")

res = results(diagdds)
res = res[order(res$padj, na.last=NA), ]
alpha = 0.01
sigtab = res[(res$padj < alpha), ]
sigtab = cbind(as(sigtab, "data.frame"), as(tax_table(data)[rownames(sigtab), ], "matrix"))# we have no tax table!!
head(sigtab)

write.csv(sigtab, "/path/to/output.csv")

posigtab = sigtab[sigtab[, "log2FoldChange"] > 0, ]
posigtab = posigtab[, c("baseMean", "log2FoldChange", "lfcSE", "padj", "Phylum", "Class", "Family", "Genus")]

########################################

### Run Multiple times!

#raw dataset without normalization
#dataset with QPCR adjustment
#rarefaction without adjustment
#rarefaction+QPCR adjustment

### what is the difference? ###

#######################################




#IF YOU WANT TO USE STABILIZED DATA FOR OTHER APPROACHES:  ###
#will maybe test this approach later in the semester

diagdds<-phyloseq_to_deseq2(data, ~categories)#choose model to apply (what are the predicive factors?)

diagdds = estimateSizeFactors(diagdds)
diagdds = estimateDispersions(diagdds)
diagvst = getVarianceStabilizedData(diagdds)
dim(diagvst)

#use variance stabilized data to input into limma-Voom and MVABUND
