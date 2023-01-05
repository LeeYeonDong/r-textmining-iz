# prepare corpus
corpus <- matrix(c(1,2,0,0,0,0,
                   3,1,0,0,0,0,
                   2,0,0,0,0,0,
                   3,3,2,3,2,4,
                   0,0,3,2,0,0,
                   0,0,4,1,0,0,
                   0,0,0,0,4,3,
                   0,0,0,0,2,1,
                   0,0,0,0,3,2,
                   0,0,1,0,2,3), ncol=6, byrow=T)

# initialize parameters
k <- 3
docnames <- c('Doc1','Doc2','Doc3','Doc4','Doc5','Doc6')
termnames <- c('Baseball','Basketball','Boxing','Money','Interest','Rate','Democrat','Republican','Cocus','President')
colnames(corpus) <- docnames
ndocs <- length(docnames)
rownames(corpus) <- termnames
nterms <- length(termnames)
topicnames <- paste0('topic', 1:k)
dtnames <- c()

## posterior_init
for (i in 1:dim(corpus)[2]) {
  dtnames <- append(dtnames,paste0(docnames[i],' ',termnames))
}

posterior_init <- matrix(runif(dim(corpus)[1] * dim(corpus)[2] * k,min = 0, max = 1), ncol = k)

posterior_init_sum <- posterior_init %>% apply(1,sum)
posterior_init <- posterior_init/posterior_init_sum

colnames(posterior.init) <- topicnames
rownames(posterior.init) <- dtnames


# p(z)
pz_init <- matrix(runif(k, min = 0, max = 1), ncol = k)

pz_init_sum <- pz_init %>% apply(1,sum)
pz_init <- pz_init/pz_init_sum

colnames(pz_init) <- topicnames


# p(d|z)
pdz_init <- matrix(runif(dim(corpus)[2] * k, min = 0, max = 1), ncol = k)

pdz_init_sum <- pdz_init %>% apply(1,sum)
pdz_init <- pdz_init/pdz_init_sum

colnames(pdz_init) <- topicnames
rownames(pdz_init) <- docnames


# p(w|z)
pwz_init <- matrix(runif(dim(corpus)[1] * k, min = 0, max = 1),ncol = k)

pwz_init_sum <- pwz_init %>% apply(1,sum)
pwz_init <- pwz_init/pwz_init_sum

colnames(pwz_init) <- topicnames
rownames(pwz_init) <- termnames

# posterior parameter
parameter_init <- list(pwz_init, pdz_init, pz_init)


# Expectation Step
estep <- function(parameter , posterior) {
  pwz <- parameter[[1]]
  pdz <- parameter[[2]]
  pz <- parameter[[3]]
  for (i in 1:(dim(corpus)[1] * dim(corpus)[2])) {
    doc <- unlist(strsplit(dtnames[i],' '))[1]
    term <- unlist(strsplit(dtnames[i],' '))[2]
    denominator <- sum(pz * pwz[which(rownames(pwz)==term),] * pdz[which(rownames(pdz)==doc),])
    for (j in 1:k) {
      numerator <- pz[1,j] * pdz[which(rownames(pdz)==doc),j] * pwz[which(rownames(pwz)==term),j]
      posterior[i,j] <- numerator/denominator
    }
  }
  return(posterior)
}


# Maximization Step
mstep <- function(posterior, parameter) {
  pwz <- parameter[[1]]
  pdz <- parameter[[2]]
  pz <- parameter[[3]]
  for (i in 1:dim(pwz)[1]) {
    for (j in 1:dim(pwz)[2]) {
      pwznumerator <- sum(corpus[i,] * posterior[which(str_detect(rownames(posterior), termnames[i])),j])
      pwzdenominator <- sum(corpus * posterior[,j])
      pwz[i,j] <- pwznumerator/pwzdenominator
    }
  }
  
  for (i in 1:dim(pdz)[1]) {
    for (j in 1:dim(pdz)[2]) {
      pdznumerator <- sum(corpus[,i] * posterior[which(str_detect(rownames(posterior), docnames[i])),j])
      pdzdenominator <- sum(corpus * posterior[,j])
      pdz[i,j] <- pdznumerator/pdzdenominator
    }
  }
  
  for (i in 1:dim(pz)[2]) {
    pznumerator <- sum(posterior[,i] * corpus)
    pzdenominator <- sum(corpus)
    pz[1,i] <- pznumerator/pzdenominator
  }
  
  return(list(pwz,pdz,pz))
}


# calculate probs
posterior.iter <- estep(parameter.init, posterior.init)
parameter.iter <- mstep(posterior.init, parameter.init)

while(i < 20) {
  posterior.iter <- estep(parameter.iter, posterior.iter)
  parameter.iter <- mstep(posterior.iter, parameter.iter)
  i <- i + 1
}


