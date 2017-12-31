### test subsampling procedure


#### function development space:


rrarefy2<-
  function (x, sample, replace) #added replace argument as input
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
      row <- sample(rep(nm, times = x[i, ]), sample[i], replace) #use replace argument
      row <- table(row)
      ind <- names(row)
      x[i, ] <- 0
      x[i, ind] <- row
    }
    x
  }



### test subsampling procedure

### build test table

a<- c(10,30,40,50,60)
b<- c(12,33,44,55,66)
c<- c(14,38,48,58,68)

test_table<- data.frame(a,b,c)

colnames(test_table)<- c("Site1","Site2","Site3")

rownames(test_table)<-c("Spp1","Spp2","Spp3","Spp4","Spp5")

test_table<-otu_table(test_table)

depth<-c(20,40,50)

test_table<-t(test_table)



gg<-rrarefy2(test_table, depth, replace = TRUE)

gg

