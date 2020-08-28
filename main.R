setwd("/home/juan/Escritorio/qap")
#leer instancia, crear y evaluar una solucion inicial
instancia<-readQAP("Chr12a.dat")
sol <- 1:instancia$n
poblation <- generatePoblation(sample(sol), 20)
sol <- sample(sol)

poblation <- generatePoblation(sol, 50)
e <- evolutiveAlgorithm(instancia, poblation, size = 50, generations = 20, fighters = 10, parents = 50, percent = 0.2)
sa <- simulatedAnnealing(instancia, sol, 10000, 10, 20, 0.95)