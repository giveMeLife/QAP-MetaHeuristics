evolutiveAlgorithm <- function(instance, poblation, size, generations, fighters, parents, percent){
  p <- poblation
  t <- 0
  bests <- NULL
  while(generations > t){
    s <- p
    q <- tournamentSelection(p, instance, fighters, parents )
    q <- reproduction(q, size)
    q <- doMutations(q, percent)
    p <- selection(s, q, instance, size)
    t <- t+1
    bests[t] <- evaluarQAP(bestSolution(p, instance), instance$f, instance$d)
    print(c("generación: ", t))
  }
  best <- bestSolution(p, instance)
  x <- 1:length(bests)
  plot(x, bests)
  print(evaluarQAP(best, instance$f, instance$d))
}