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


#### output response table###
out<-matrix(data=NA, nrow=nrow(Factors), ncol = length(fun))

out<-for(i in 1:length(fun)) {
  	for(row in 1:nrow(Factors)){
   rt[row,i]<-do.call(fun[[i]], list(Factors[row,1],Factors[row,2],Factors[row,3],Factors[row,4],Factors[row,5]))
      }
}

rt
