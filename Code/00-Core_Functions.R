library(plyr)
library(pcaPP)


# Chatterjee's Methode
NNSearch_C <- function(X){
  X <- as.data.frame(X)
  n <- nrow(X)
  NN_X <- RANN::nn2(X, query = X, k = 3)
  NN_index_X <- NN_X$nn.idx[, 2]
  repeat_data <- which(NN_X$nn.dists[, 2] == 0)
  df_X <- data.table::data.table(id = repeat_data, group = NN_X$nn.idx[repeat_data,1])
  .randomNN <- function(ids){
    m <- length(ids)
    x <- sample(x = (m - 1), m, replace = TRUE)
    x <- x + (x >= (1:m))
    return(ids[x])
  }
  df_X[, `:=`(rnn, .randomNN(id)), by = "group"]
  NN_index_X[repeat_data] = df_X$rnn
  ties = which(NN_X$nn.dists[, 2] == NN_X$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  if (length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(as.matrix(X[a, ]), ncol = ncol(X)),matrix(as.matrix(X[-a, ]), ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }
    NN_index_X[ties] <- sapply(ties, helper_ties)
  }
  return(NN_index_X)
}

tau <- function(X,Y){
  Z <- as.data.frame(X)
  m <- nrow(Z)
  #z <- table(Z)
  #z.1 <- 1-sum(z*(z-1)/(m*(m-1)))
  z <- count(Z,!!!Z)
  z.1 <- 1-sum(z$n*(z$n-1)/(m*(m-1)))
  z.2 <- NNSearch_C(Z)
  Y_NN <- Y[z.2]
  z.3 <- 0
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      z.3 <- z.3 + sign(Y[i]-Y[j])*sign(Y_NN[i]-Y_NN[j])
    }
  }
  z.3
  return(sum(z.3)/(m*(m-1)/2)/z.1)
}

count_fun <- function(x, y){
  if(x < y){
    return(1)
  }
  else if(x == y){
    return(0.5)
  }
  else{
    return(0)
  }
}
count_fun <- Vectorize(count_fun)

rte <- function(Y_1, Y_2){
  return(mean(outer(Y_1, Y_2, FUN = count_fun)))
}

tau_rte <- function(X, Y){
  Z <- as.data.frame(X)
  m <- nrow(Z)
  #z <- table(Z)
  #z.1 <- 1-sum(z*(z-1)/(m*(m-1)))
  z <- count(Z,!!!Z)
  z.1 <- 1-sum(z$n*(z$n-1)/(m*(m-1)))
  z.2 <- NNSearch_C(Z)
  Y_NN <- Y[z.2]
  z.3 <- 0
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      #z.3 <- z.3 + (rte(Y_1 = c(Y[i], Y_NN[i]), Y_2 = c(Y[j], Y_NN[j])))^2
      z.3 <- z.3 + (0.5 * rte(Y_1 = c(Y[i]), Y_2 = c(Y[j]))  + 0.5 * rte(Y_1 = c(Y_NN[i]), Y_2 = c(Y_NN[j])))^2
    }
  }
  z.3 <- 4 * (2 * z.3 / (m * (m-1))/ z.1) - 1
  return(z.3)
  #return(sum(z.3)/(m*(m-1)/2)/z.1)
}


conventional_rte <- function(x, y){
  ranks_y <- rank(c(y, x), ties.method = "average")[1:length(y)]
  n <- length(x)
  m <- length(y)
  rte <- 1 / n * (mean(ranks_y) - (m + 1) / 2)
  return(rte)
}

tau_fast <- function(X,Y){
  Z         <- as.data.frame(X)
  Y_NN      <- Y[NNSearch_C(Z)]
  m         <- nrow(Z)
  norm_x    <- table(Z)
  norm_x    <- m*(m-1)-sum(norm_x*(norm_x-1))
  norm_y    <- table(Y)
  norm_y    <- m*(m-1)-sum(norm_y*(norm_y-1))
  norm_y_NN <- table(Y_NN)
  norm_y_NN <- m*(m-1)-sum(norm_y_NN*(norm_y_NN-1))
  norm      <- sqrt(norm_y*norm_y_NN)
  return(cor.fk(Y,Y_NN)*norm/norm_x)
}


tau_fast_Y <- function(X,Y){
  Z    <- as.data.frame(X)
  Y_NN <- Y[NNSearch_C(Z)]
  
  # Groups
  idx_group <- 1L:nrow(Z)
  for (i in nrow(Z):2L) {
    for (j in 1L:(i-1)) {
      if (identical(as.numeric(Z[i,]),as.numeric(Z[j,]))) {
        idx_group[i] <- idx_group[j]
        break
      }
    }
  }
  has_duplicates <- length(idx_group) != length(unique(idx_group))
  #
  m         <- nrow(Z)
  norm_x    <- table(Z)
  norm_x    <- m*(m-1)-sum(norm_x*(norm_x-1))
  norm_y    <- table(Y)
  norm_y    <- m*(m-1)-sum(norm_y*(norm_y-1))
  norm_y_NN <- table(Y_NN)
  norm_y_NN <- m*(m-1)-sum(norm_y_NN*(norm_y_NN-1))
  norm      <- sqrt(norm_y*norm_y_NN)
  #
  if (!has_duplicates) {
    return(cor.fk(Y,Y_NN)*norm/norm_x)
  }
  if (has_duplicates) {
    n <- length(Y); total_pairs <- n * (n - 1) / 2
    concordant <- 0; discordant <- 0
    for (i in 1L:(n-1)) {
      for (j in (i+1L):n) {
        
        if (idx_group[i] != idx_group[j]) {
          if ((Y[i] - Y[j]) * (Y_NN[i] - Y_NN[j]) > 0) {
            concordant <- concordant + 1
          } 
          if ((Y[i] - Y[j]) * (Y_NN[i] - Y_NN[j]) < 0) {
            discordant <- discordant + 1
          }
        }
      }
    }
    tau <- (concordant - discordant) / total_pairs
    return(tau*norm/norm_x)
  }
}

concordance <- function(X,Y){
  X1 <- as.data.frame(X)
  Y1 <- as.data.frame(Y)
  m <- nrow(X1)
  tab_x <- table(X1)
  norm_x <- m*(m-1)-sum(tab_x*(tab_x-1))
  tab_y <- table(Y1)
  norm_y <- m*(m-1)-sum(tab_y*(tab_y-1))
  norm <- sqrt(norm_x*norm_y)
  return(ifelse(is.na(cor.fk(X,Y)),0,cor.fk(X,Y)*norm) )
}

tau_fast_Z_old <- function(X,Y){
  Z <- as.data.frame(X)
  if(nrow(unique(Z))==1){
    return(NA)
  }
  if(nrow(unique(Z))>1){
    Y_NN <- Y[NNSearch_C(Z)]
    Conc <- concordance(Y,Y_NN)
    
    multipleX <- Z %>% table() %>% as.data.frame() %>% filter(Freq>1) %>% select(X) %>% unlist() %>% as.character() %>% as.numeric()
    data <- data.frame(X1=Z$X,Y=Y,Y_NN=Y_NN)
    conc <- numeric()
    if(length(multipleX)>1){
      for(i in 1:length(multipleX)){
        Z1 <- data %>% filter(X1 == multipleX[i])
        conc[i] <- concordance(Z1$Y,Z1$Y_NN)
      }
      conc <- sum(conc)
      Conc <- Conc - conc
    }
    m <- nrow(Z)
    tab_x <- table(Z)
    norm_x <- m*(m-1)-sum(tab_x*(tab_x-1))
    return(Conc/norm_x)
  }  
}

#tau_fast_Z <- function(X,Y){
#  Z <- as.data.frame(X)
#  if(nrow(unique(Z))==1){
#    return(NA)
#  }
#  if(nrow(unique(Z))>1){
#    Y_NN <- Y[NNSearch_C(Z)]
#    Conc <- concordance(Y,Y_NN)
#    
#    multipleX <- Z %>% table() %>% as.data.frame() %>% filter(Freq>1) %>% select(-Freq) %>% mutate(across(c(everything()), .fns = function(x) as.numeric(x)))
#    data <- data.frame(Y=Y,Y_NN=Y_NN)
#    data <- cbind(X, data)
#    conc <- numeric()
#    if(length(multipleX)>1){
#      for(i in 1:length(multipleX)){
#        temp_multipleX <- multipleX[i,]
#        Z1 <- data %>% semi_join(temp_multipleX)
#        conc[i] <- concordance(Z1$Y,Z1$Y_NN)
#      }
#      conc <- sum(conc)
#      Conc <- Conc - conc
#    }
#    m <- nrow(Z)
#    tab_x <- table(Z) %>% as.data.frame() %>% select(Freq) %>% unlist()
#    norm_x <- m*(m-1)-sum(tab_x*(tab_x-1))
#    return(Conc/norm_x)
#  }  
#}

tau_fast_Z_1 <- function(X,Y){
  Z         <- as.data.frame(X)
  if(nrow(unique(Z))==1){
    return(NA)
  }
  if(nrow(unique(Z))>1){
    Y_NN      <- Y[NNSearch_C(Z)]
    Conc      <- concordance(Y,Y_NN)
    data      <- cbind(Z,data.frame(Y=Y,Y_NN=Y_NN))
    conc      <- numeric()
    if(isTRUE(as.data.frame(table(Z))$Freq>1)){
      multipleX <- ddply(Z,colnames(Z),nrow) %>% filter(V1>1) %>% as.data.frame()
      for(i in 1:nrow(multipleX)){
        Z1      <- suppressMessages(left_join(multipleX[i,],Z))
        Z2      <- unique(suppressMessages(left_join(Z1,data,relationship = "many-to-many")))
        conc[i] <- concordance(Z2$Y,Z2$Y_NN)
      }
      conc      <- sum(conc)
      Conc      <- Conc - conc
    }
    m         <- nrow(Z)
    tab_x     <- ifelse(ncol(Z)==1,table(Z),ddply(Z,colnames(Z),nrow)$V1)
    norm_x    <- m*(m-1)-sum(tab_x*(tab_x-1))
    return(Conc/norm_x)
  }  
}

tau_fast_Z <- function(X,Y){
  Z         <- as.data.frame(X)
  n         <- nrow(Z)
  treshold  <- max(1,n/100)
  if(nrow(unique(Z))==1){
    return(NA)
  }
  if(nrow(unique(Z))>1){
    Y_NN      <- Y[NNSearch_C(Z)]
    Conc      <- concordance(Y,Y_NN)
    data      <- cbind(Z,data.frame(Y=Y,Y_NN=Y_NN))
    logic     <- tryCatch(any(as.data.frame(table(Z))$Freq>treshold), error=function(e) nrow(ddply(Z,colnames(Z),nrow) %>% filter(V1>treshold) %>% as.data.frame())>0)
    conc      <- numeric()
    if(logic){
      multipleX <- ddply(Z,colnames(Z),nrow) %>% filter(V1>treshold) %>% as.data.frame()
      for(i in 1:nrow(multipleX)){
        Z1      <- suppressMessages(left_join(multipleX[i,],Z))
        Z2      <- unique(suppressMessages(left_join(Z1,data,relationship = "many-to-many")))
        conc[i] <- concordance(Z2$Y,Z2$Y_NN)
      }
      conc      <- sum(conc)
      Conc      <- Conc - conc
    }
    m         <- nrow(Z)
    tab_x     <- tryCatch(table(Z), error=function(e) ddply(Z,colnames(Z),nrow)$V1)
    norm_x    <- m*(m-1)-sum(tab_x*(tab_x-1))
    return(Conc/norm_x)
  }  
}

tau_fast_Z <- function(X,Y){
  Z         <- as.data.frame(X)
  n         <- nrow(Z)
  treshold  <- max(1,n/100)
  if(nrow(unique(Z))==1){
    return(NA)
  }
  if(nrow(unique(Z))>1){
    Y_NN      <- Y[NNSearch_C(Z)]
    Conc      <- concordance(Y,Y_NN)
    data      <- cbind(Z,data.frame(Y=Y,Y_NN=Y_NN))
    logic     <- tryCatch(any(as.data.frame(table(Z))$Freq>treshold), error=function(e) nrow(ddply(Z,colnames(Z),nrow) %>% filter(V1>treshold) %>% as.data.frame())>0)
    conc      <- numeric()
    if(logic){
      multipleX <- ddply(Z,colnames(Z),nrow) %>% filter(V1>treshold) %>% as.data.frame()
      for(i in 1:nrow(multipleX)){
        Z1 <- suppressMessages(semi_join(data, multipleX[i,]))
        conc[i] <- concordance(Z1$Y,Z1$Y_NN)
      }
      conc      <- sum(conc)
      Conc      <- Conc - conc
    }
    m         <- nrow(Z)
    tab_x     <- tryCatch(table(Z), error=function(e) ddply(Z,colnames(Z),nrow)$V1)
    norm_x    <- m*(m-1)-sum(tab_x*(tab_x-1))
    return(Conc/norm_x)
  }  
}