evolutiveAlgorithm <- function(instance, poblation, size, generations, fighters, parents, percent){
  p <- poblation
  t <- 0
  bests <- NULL
  while(generations > t){
    q <- tournamentSelection(p, instancia, fighters, parents )
    q <- reproduction(q, size)
    q <- doMutations(q, percent)
    p <- q
    t <- t+1
    bests[t] <- evaluarQAP(bestSolution(p, instancia), instancia$f, instancia$d)
    print(c("generación: ", t))
  }
  best <- bestSolution(p, instancia)
  print(best)
  x <- 1:length(bests)
  plot(x, bests)
  print(evaluarQAP(best, instancia$f, instancia$d))
}