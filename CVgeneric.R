CVgeneric <- function(classifier, features, labels, k, loss.function){
  
  # combine the data to make things eaier after CV
  training <-  cbind(labels, features)
  
  num <- nrow(training)
  
  # define a new function to build k fold
  CVgroup <- function(k,datasize){
    cvlist <- list()
    n <- rep(1:k,ceiling(datasize/k))[1:datasize]        
    temp <- sample(n,datasize)
    x <- 1:k
    dataseq <- 1:datasize
    cvlist <- lapply(x,function(x) dataseq[temp==x])
    return(cvlist)
  }
  
  cvlist <- CVgroup(k, num)
  
  names <- colnames(training)
  error = data.frame(k = 1:k, error = 0)
  for (i in 1:k) {
    val <- training[cvlist[[i]],]
    tra <- training[-cvlist[[i]],]
    classifier.fit <- classifier(as.formula(paste(names[1], paste(names[-1], collapse = '+'), sep = " ~ ")), data = tra, family = binomial)
    if (identical(classifier,glm) | identical(classifier,svm)) {
      classifier.probs <- predict(classifier.fit, val, type = "response")
      classifier.pred <- rep(0, length(classifier.probs))
      classifier.pred[classifier.probs > 0.5] = 1 
      error.rate <- loss.function(classifier.pred != val$expert_label)
      error[i, 2] = error.rate
      
    }else{
      classifier.pred <- predict(classifier.fit, val)
      error.rate <- loss.function(classifier.pred$class != val$expert_label)
      error[i, 2] = error.rate
    }
  }
  return(error)
} 