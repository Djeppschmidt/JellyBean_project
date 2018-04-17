## Import community

com<-readRDS("path/to/file.rds")

### alpha diversity
alpha<-estimate_richness(com, measures = "Observed")
?estimate_richness
categories<-c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(6,5))
summary(aov(alpha$Observed~categories
            
            ))
?aov        

boxplot(alpha$Observed~categories)
        

)### beta diversity
# bray-curtis
dist<-vegdist(otu_table(com), method="bray")
?vegdist
# Permanova

?adonis
adonis(dist~categories+F1+F2+F3+F4+F5, data.frame(sample_data(com))) 
?adonis
sample_data(com)

#ordination

ord<-metaMDS(otu_table(com), distance="bray")
plot(ord, col= csample_data(com)$categories)

?plot

?plot_ordination
sample_data(com)$categories<-categories
plotord = plot_ordination(com, ord, color="categories")+scale_shape_identity()
plotord

fit<-envfit(ord, sample_data(com))

plot(ord)
plot(fit, add=TRUE)

