evolutiveAlgorithm <- function(instance, poblation, size, generations, fighters, parents, percent, iteration=0, prefix=""){
  p <- poblation
  t <- 0
  bests <- NULL
  space <- c()
  while(generations > t){
    s <- p
    q <- tournamentSelection(p, instance, fighters, parents )
    q <- reproduction(q, size)
    q <- doMutations(q, percent)
    p <- selection(s, q, instance, size)
    t <- t+1
    bests[t] <- evaluarQAP(bestSolution(p, instance), instance$f, instance$d)
    q$generation = t
    space <- rbind(space, q)
    print(t)
  }
  best <- bestSolution(p, instance)
  
  write.table(bests, paste0(workdir, "output/bests/evolutive_", prefix,"_", iteration, ".csv"), sep=";", row.names = F)
  write.table(space, paste0(workdir, "output/space/evolutive_", prefix,"_space_", iteration, ".csv"), sep=";", row.names = F)
  return(evaluarQAP(best, instance$f, instance$d))
}