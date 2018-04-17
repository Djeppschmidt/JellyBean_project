## Import community

com<-readRDS("path/to/file.rds")

### alpha diversity
alpha<-estimate_richness(com, measures = "Observed")

### beta diversity
# bray-curtis
dist<-vegdist(otu_table(com), method="bray")

# Permanova

?adonis
adonis(dist~category+f1+f2+f3+f4+f5, com)
# ordination

ord<-metaMDS(com, distance-"bray")

fit<-envfit(ord, sample_data(com))