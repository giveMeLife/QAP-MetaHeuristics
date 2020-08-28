#Lee archivo qap del OR Library
readQAP<-function(name){ 
  a <- read.delim(name,header=FALSE, sep ="")
  n<-as.integer(a[1,1])
  fl<-a[2:(n+1),1:n]
  dis<-a[(n+2):(n+n+1),1:n]
  d <- list(n=n, f= fl, d = dis)
  return(d)
}

#Evalúa el valor de la fción objetivo.
evaluarQAP<-function(sol, f, d){
  acum<-0
  n<-length(sol)
  for(i in 1:n){
    for(j in 1:n){
      acum = acum + f[i,j]*d[sol[i],sol[j]]   
    }
  }
  return(acum)
}

#Genera vecino con método swap
swap<-function(sol,i,j){
  piv<-sol[i]
  sol[i]<-sol[j]
  sol[j]<-piv
  return(sol)
}


generatePoblation <- function(sol, size){
  n <- length(sol)
  pob <- data.frame()
  pob <- rbind(pob, sol)
  for(i in 1:size){
    sol <- sample(sol)
    pob <- rbind(pob, sol)
  }
  return(pob)
}

probabilities <- function(poblation, instancia){
  n <- nrow(poblation)
  total <- 0
  p <- NULL
  for(i in 1:n){
    total <- total + evaluarQAP(as.numeric(poblation[i,]), instancia$f, instancia$d)
  }
  for(i in 1:n){
    p[i] <- evaluarQAP(as.numeric(poblation[i,]), instancia$f, instancia$d)/total
  }
  return(p)
}

rouletteSelection <- function(poblation, percent, instancia){
  n <- as.integer(nrow(poblation)*percent)
  selected <- data.frame()
  p <- probabilities(poblation, instancia)
  p <- 1-p
  sols <- c(1:nrow(poblation))
  for(i in 1:n){
    select <- sample(sols, 1, p, replace = TRUE )
    selected <- rbind(selected, poblation[select, ])
    sols <- sols[-select]
    p <- p[-select]
  }
  rownames(selected) <- NULL
  return(selected)
}

tournamentSelection <- function(poblation, instancia, numberOfFighters, numberOfparents){
  selected <- data.frame()
  fighters <- data.frame()
  for(i in 1:numberOfparents){
    for(j in 1:numberOfFighters){
      chosen <- sample(1:nrow(poblation), 1, replace = TRUE)
      fighters <- rbind(fighters, as.numeric(poblation[chosen,]))
    }
    best <- bestSolution(fighters, instancia)
    selected <- rbind(selected, best)
    fighters <- data.frame()
  }
  return(selected)
}

recombination <- function(parent1, parent2){
  n <- length(parent1)
  i <- sample(1:(n-1), 1)
  j <- sample((i+1):n, 1)
  index <- i:j
  son <- rep(-1, n)
  son[i:j] <- parent1[i:j]
  notAssigned <- setdiff(parent2[i:j], parent1[i:j])
  if(length(notAssigned)!=0){
    for(k in 1:length(notAssigned)){
      caracteristicPosition <- match(notAssigned[k], parent2)
      mirrorCaracteristic <- parent1[caracteristicPosition]
      temp <- TRUE
      while(temp){
        if(!mirrorCaracteristic %in% parent2[i:j]){
          position <- match(mirrorCaracteristic, parent2)
          son[position] <- notAssigned[k]
          temp <- FALSE
        }
        else{
          caracteristicPosition <- match(mirrorCaracteristic, parent2)
          mirrorCaracteristic <- parent1[caracteristicPosition]
        }
      }
    }
  }
  for(i in 1:n){
    if(son[i] == -1){
      son[i] = parent2[i]
    }
  }
  return(son)
}


reproduction <- function(poblation, size){
  sons <- data.frame()
  index <- 1:nrow(poblation)
  for(i in 1:size){
    parent1 <- sample(index, 1, replace = TRUE)
    parent2 <- sample(index[-parent1], 1, replace = TRUE)
    parent1 <- as.numeric(poblation[parent1,])
    parent2 <- as.numeric(poblation[parent2,])
    son1 <- recombination(parent1, parent2)
    son2 <- recombination(parent2, parent1)
    sons <- rbind(sons, son1)
    sons <- rbind(sons, son2)
  }
  return(sons)
}

replace <- function(poblation1, poblation2, proportion1, proportion2, instancia){
  
}

bestSolution <- function(poblation, instancia){
  objectives <- NULL
  for(i in 1:nrow(poblation)){
    objectives[i] <- evaluarQAP(as.numeric(poblation[i,]), instancia$f, instancia$d)
  }
  min <- which.min(objectives)
  return(as.numeric(poblation[min,]))
}

mutation <- function(solution){
  index <- 1:length(solution)
  i <- sample(index, 1)
  j <- sample(index[-i], 1)
  element_i <- solution[i]
  element_j <- solution[j]
  solution[i] <- element_j
  solution[j] <- element_i
  return(solution)
}

doMutations <- function(poblation, percent){
  for(i in 1:nrow(poblation)){
    temp <- runif(1, 0, 1)
    if(temp < percent){
      poblation[i,] <- rbind(data.frame(), mutation(as.numeric(poblation[i,])))
    }
  }
  return(poblation)
}

evaluate <- function(p, instance){
  value <- NULL
  for(i in 1:nrow(p)){
    value[i] <- evaluarQAP(as.numeric(p[i,]),instance$f, instance$d)
  }
  p <- cbind(p, value)
  return(p)
}

selection <- function(p, q, instance, size){
  numberQ <- as.integer(0.7*size)
  numberP <- as.integer(0.3*size)
  p <- evaluate(p, instance)
  q <- evaluate(q, instance)
  topq <- head(q[order(q$value , decreasing=FALSE), ], numberQ)
  topp <- head(p[order(p$value, decreasing=FALSE), ], numberP)
  colnames(topq) <- colnames(topp)
  rownames(topq) <- NULL
  rownames(topp) <- NULL
  
  top <- rbind(topq, topp)
  top$value <- NULL
  rownames(top) <- NULL
  return(top)
}
