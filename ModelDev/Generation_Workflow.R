#architecture
#make functions/list
f1<-function(a,b) {output<-a-b
   		return(output)}
f2<-function(a,b) {output<-a+b
   		return(output)}

fun<-list(f1,f2)
 #make site data
   a<-c(1,2,3)
   b<-c(3,2,1)
   df2<-matrix(.,a,b)
   df<-data.frame(a,b)

#Calculate species abundance
saa<-matrix(,nrow=3, ncol=2)
saa

for(i in 1:length(fun)) {
  	for(row in 1:nrow(df)){
   saa[row,i]<-do.call(fun[[i]], list(df[row,1],df[row,2]))
      }
	}


###START HERE####


#DEfine species responses

##take from Equation Reference##


#sp1abund<-function(F1.value,F2.value,F3.value,F4.value,F5.value) {
#   output<- (20*a+b/c-d)^log(e)
#   return(output)}

#sp2abund<-function(F1.value,F2.value,F3.value,F4.value,F5.value) {
#   output<- (a/b*c/d*e)
#   return(output)}


#sp1abunda<-function(a,b,c,d,e) {
#   output<- (20*a+b/c-d)^log(e)
#   return(output)}
#sp2abundb<-function(a,b,c,d,e) {
#   output<- (a/b*c/d*e)
#   return(output)}
#spvec2<-c(sp1abunda,sp2abundb...)

#fun<-c(...)
### real responses
spp1<-function(a,b,c,d,e) {
  abun<-(0.001*(a-50)^3+1000)+(10*b)+(-0.1*(c-50)^2+2000)+d+(-1*e+500)
  abun
  } #calibrated
spp2<-function(a,b,c,d,e) {abun<-(0.001*(a-1)^3+3)*(10*b)+(-10*(c-20)^2+1000)+d+(-e+50)
  abun} #calibrated
spp3<-function(a,b,c,d,e) {abun<-((1/e)*(a-50)^3+300)+(6*b)+((c-50)^2+200)+d
  abun} #calibrated
spp4<-function(a,b,c,d,e) {abun<-((1/e)+(a-12)^2+3)+(10*b)+((1/d)+(c-12)^2+50)
  abun} #calibrated
spp5<-function(a,b,c,d,e) {abun<-((1/e)+(a-12)^4-50)+(10*b)+((1/d)+(c-12)^2+50)
  abun} #calibrated
spp6<-function(a,b,c,d,e) {abun<-((d)*(a-14)^3+3)+(-10*b)+((1/e)*(c-5)^2+50)
  abun} #calibrated
spp7<-function(a,b,c,d,e) {abun<-((-d)*(a-11)^3+3)+(-10*b)+((1/e)*(c-5)^2+50)
  abun} #calibrated
spp8<-function(a,b,c,d,e) {abun<-(-1*(((e^(d/12))*(a-13)^3+3)+(10*b)+((-1000)*(c-12)^2)))
  abun} #calibrated
spp9<-function(a,b,c,d,e) {abun<-((-100)*(d-7)^2+2000)+(10*b)*(1/a)+((e-10)^3+3)+c
  abun} #calibrated
spp10<-function(a,b,c,d,e) {abun<-(0.001*(a-13)^3+3)*(10*b)*(-0.001*(e-7)^2+50)+(0.00000001*(e-12)^5+10)+(0.1*(c-7)^2+50)+d
  abun} #calibrated

  ### random responses

spp11<-function(a,b,c,d,e) {abun<-rnorm(1, 50000, 50)-10+(0*(a+b+c+d+e))
  abun} #generate 1 number with mean=50, stdev=50
spp12<-function(a,b,c,d,e) {abun<-rnorm(1, 40, 40)-10+(0*(a+b+c+d+e))
  abun}
spp13<-function(a,b,c,d,e) {abun<-rnorm(1, 20, 20)-10+(0*(a+b+c+d+e))
  abun}
spp14<-function(a,b,c,d,e) {abun<-rnorm(1, 10, 10)-10+(0*(a+b+c+d+e))
  abun}
spp15<-function(a,b,c,d,e) {abun<-rnorm(1, 50000, 10000)-10+(0*(a+b+c+d+e))
  abun}
spp16<-function(a,b,c,d,e) {abun<-rnorm(1, 50000, 50000)-10+(0*(a+b+c+d+e))
  abun}
spp17<-function(a,b,c,d,e) {abun<-rnorm(1, 4000, 1000)-10+(0*(a+b+c+d+e))
  abun}
spp18<-function(a,b,c,d,e) {abun<-rnorm(1, 20000, 5000)-10+(0*(a+b+c+d+e))
  abun}
spp19<-function(a,b,c,d,e) {abun<-rnorm(1, 5, 50)-10+(0*(a+b+c+d+e))
  abun}
spp20<-function(a,b,c,d,e) {abun<-rnorm(1, 5, 5)-10+(0*(a+b+c+d+e))
  abun}

spp21<-function(a,b,c,d,e) {abun<-rnorm(1, 5, 50)-10+(0*(a+b+c+d+e))
  abun}
spp22<-function(a,b,c,d,e) {abun<-rnorm(1, 5, 50)-10+(0*(a+b+c+d+e))
  abun}
spp23<-function(a,b,c,d,e) {abun<-rnorm(1, 5, 50)-10+(0*(a+b+c+d+e))
  abun}
spp24<-function(a,b,c,d,e) {abun<-rnorm(1, 2, 50)-10+(0*(a+b+c+d+e))
  abun}
spp25<-function(a,b,c,d,e) {abun<-rnorm(1, 2, 50)-10+(0*(a+b+c+d+e))
  abun}
spp26<-function(a,b,c,d,e) {abun<-rnorm(1, 1, 50)-10+(0*(a+b+c+d+e))
  abun}
spp27<-function(a,b,c,d,e) {abun<-rnorm(1, 1, 50)-10+(0*(a+b+c+d+e))
  abun}
spp28<-function(a,b,c,d,e) {abun<-rnorm(1, 10, 50)-10+(0*(a+b+c+d+e))
  abun}
spp29<-function(a,b,c,d,e) {abun<-rnorm(1, 10, 50)-10+(0*(a+b+c+d+e))
  abun}
spp30<-function(a,b,c,d,e) {abun<-rnorm(1, 1, 100)-10+(0*(a+b+c+d+e))
  abun}
spp31<-function(a,b,c,d,e) {abun<-rnorm(1, 1, 100)-10+(0*(a+b+c+d+e))
  abun}
spp32<-function(a,b,c,d,e) {abun<-rnorm(1, 1, 5)-10+(0*(a+b+c+d+e))
    abun}
#linear responses (small)
spp33<-function(a,b,c,d,e) {abun<-10*a+0*b*c*d*e
  abun}
spp34<-function(a,b,c,d,e) {abun<-10*b+0*a*c*d*e
  abun}
spp35<-function(a,b,c,d,e) {abun<-10*c+0*b*a*d*e
  abun}

### make vector of species

fun<-c(spp1,spp2,spp3,spp4,spp5,spp6,spp7,spp8,spp9,spp10,spp11,spp12,spp13,spp14,spp15,spp16,spp17,spp18,spp19,spp20,spp21,spp22,spp23,spp24,spp25,spp26,spp27,spp28,spp29,spp30,spp31,spp32,spp33,spp34,spp35)

#make environmental table
set.seed(0398274972)
#conditionsF1 (eg som)
library(reshape2)
f1c1<-c(5,5,5,5,5,5)
f1c2<-c(1,3,10,15,3,15)
f1c3<-c(0.5,0.5,3,3,1,5)
F1.frame<-mapply(rnorm, f1c1,f1c2,f1c3)
F1<-melt(F1.frame)

#F2
f2c1<-c(5,5,5,5,5,5)
f2c2<-c(34,30,50,55,35,60)
f2c3<-c(0.5,0.5,3,3,1,5)
F2.frame<-mapply(rnorm, f2c1,f2c2,f2c3)
F2<-melt(F2.frame)

#F3
f3c1<-c(5,5,5,5,5,5)
f3c2<-c(1,3,10,15,3,15)
f3c3<-c(0.5,0.5,3,3,1,5)
F3.frame<-mapply(rnorm, f3c1,f3c2,f3c3)
F3<-melt(F3.frame)

#F4
f4c1<-c(5,5,5,5,5,5)
f4c2<-c(1,3,10,15,3,15)
f4c3<-c(0.5,0.5,3,3,1,5)
F4.frame<-mapply(rnorm, f4c1,f4c2,f4c3)
F4<-melt(F4.frame)

#F5
f5c1<-c(5,5,5,5,5,5)
f5c2<-c(1,3,10,15,3,15)
f5c3<-c(0.5,0.5,3,3,1,5)
F5.frame<-mapply(rnorm, f5c1,f5c2,f5c3)
F5<-melt(F5.frame)

Factors<-data.frame(F1$value,F2$value,F3$value,F4$value,F5$value)
Sites<-c(paste0("Site", 1:30))
rownames(Factors)<-Sites
colnames(Factors)<-c("F1","F2","F3","F4","F5")
head(Factors)
#### output response table###
otu<-matrix(data=NA, nrow=nrow(Factors), ncol = length(fun))

for(i in 1:length(fun)) {
  for(row in 1:nrow(Factors)){
   otu[row,i]<-do.call(fun[[i]], list(Factors[row,1],Factors[row,2],Factors[row,3],Factors[row,4],Factors[row,5]))
      }
}


Taxa<-c("spp1","spp2","spp3","spp4","spp5","spp6","spp7","spp8","spp9","spp10","spp11","spp12","spp13","spp14","spp15","spp16","spp17","spp18","spp19","spp20","spp21","spp22","spp23","spp24","spp25","spp26","spp27","spp28","spp29","spp30","spp31","spp32","spp33","spp34","spp35")

row.names(otu)<-Sites
colnames(otu)<-Taxa

head(otu)


### convert negative values to 0
otu[otu<0]<-0
#out
### round numbers
otu<-round(otu)
head(otu)

#save as phyloseq table

library(phyloseq)
OTU<-otu_table(otu, taxa_are_rows = FALSE)
Sa<-sample_data(Factors)
PopPS<-phyloseq(OTU, Sa) # define phyloseq for original population
PopPS

saveRDS(PopPS, "~/Documents/GitHub/JellyBean_Project/RefCommNormDist.rds")


rrarefy2<- function (x, sample, replace, prob) # can use sample function to model systematic bias ...
{
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
    row <- sample(rep(nm, times = x[i, ]), sample[i], replace, prob)
    row <- table(row)
    ind <- names(row)
    x[i, ] <- 0
    x[i, ind] <- row
  }
  x
}
#define rarefaction depth for each sample
#may need to repeat this step multiple times to generate no negative numbers
depth<-list(rnorm(30, 80000, 40000))
depth<-round(depth[[1]])
depth

#for modeling sequencing we will be sampling with replacement because this approximates pcr allowing the machine to sequence each individual more than once; rarefying without replacement assumes that the community is completely sampled and the machine does not double count (technically not true)


seq1.replace<-rrarefy2(otu, depth, replace=TRUE) # model sequencing process
head(seq1.replace)

#seq1.noreplace<-rrarefy2(out, depth, replace=FALSE) # model sequencing process
#head(seq1.noreplace)

sum(as.data.frame(t(rarefied))$Site1)
sum(as.data.frame(t(rareplaced))$Site1)


seq1.replace<-otu_table(seq1.replace, taxa_are_rows = FALSE)
seq1.replacePS<-phyloseq(seq1.replace, Sa)
seq1.replacePS

#rareplaced<-otu_table(rareplaced, taxa_are_rows = FALSE)
#replaced.NormdistCommPS<-phyloseq(rareplaced, Sa)
#replaced.NormdistCommPS

total_abund<-sample_sums(PopPS)
sample_data(seq1.replacePS)$total_abund<-total_abund



saveRDS(rare.NormdistCommPS, "~/Documents/GitHub/JellyBean_Project/rarefied_com.rds")
saveRDS(replaced.NormdistCommPS, "~/Documents/GitHub/JellyBean_Project/rareplaced_com.rds")


?rnorm

