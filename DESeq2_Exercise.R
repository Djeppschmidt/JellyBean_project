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

diagdds = phyloseq_to_deseq2(data, ~F1+F2+F3+F4+F5)
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

#### Alternative method ###
#not sure if this one works

diagdds<-phyloseq_to_deseq2(data, ~F1+F2+F3+F4+F5)#choose model to apply (what are the predicive factors?)

diagdds = estimateSizeFactors(diagdds)
diagdds = estimateDispersions(diagdds)
diagvst = getVarianceStabilizedData(diagdds)
dim(diagvst)
#insert thing thing!!
data0<-data

otu_table(data) <- otu_table(diagvst, taxa_are_rows = TRUE)


diagdds = DESeq(diagdds)  #, fitType='local')
res = results(diagdds)
res = res[order(res$padj, na.last = NA), ]
alpha = 0.01
keepOTUs = rownames(res[res$padj > alpha, ])[1:50]
dataTrimvs = prune_taxa(keepOTUs, data)
dataTrim0 = prune_taxa(keepOTUs, data0)
plot_heatmap(dataTrimvs, sample.label = "DIAGNOSIS",
             sample.order = "DIAGNOSIS")
