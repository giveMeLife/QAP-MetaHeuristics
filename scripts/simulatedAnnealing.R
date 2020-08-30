
simulatedAnnealing <- function(instancia, sol, Tmax, Tmin, it, beta, prefix="", iteration=0){
  T <- Tmax
  actualSolution <- sol 
  actualCost <- evaluarQAP(actualSolution, instancia$f, instancia$d)
  n = 1
  k = 1
  costs <- NULL
  space <- data.frame()
  while(T > Tmin){
    for(i in 1:it){
      x <- sample(1:instancia$n, 2, replace=F)
      neighbor <- swap(actualSolution,x[1], x[2])
      cost <- evaluarQAP(neighbor, instancia$f, instancia$d)
      delta_E <- cost - actualCost
      
      space <- rbind(space, actualSolution)
      space <- rbind(space, neighbor)
      
      if(delta_E <= 0){
        actualCost <- cost
        actualSolution <- neighbor
      }
      else{
        bolztmanProb <- exp((-delta_E)/T)
        temp <- sample(c(0,1), 1, prob = c(1-bolztmanProb, bolztmanProb), replace = TRUE)
        if(temp == 1){
          actualCost <- cost
          actualSolution <- neighbor
        }
      }
      costs[k] <- actualCost
      k <- k+1
    }
    #T <- T - n*beta
    T <- T*beta
    print(T)
    n <- n + 1
  }
  write.table(costs, paste0(workdir, "output/bests/sa_", prefix,"_", iteration, ".csv"), sep = ";", row.names = F)
  write.table(space, paste0(workdir, "output/space/sa_", prefix, "_space_", iteration, ".csv"), sep=";", row.names = F)
  return(invisible(actualCost))
}