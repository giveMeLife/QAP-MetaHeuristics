#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

source("scripts/evolutiveAlgorithm.R")
source("scripts/qap.R")
source("scripts/simulatedAnnealing.R")

file = args[1]
metaheuristic = args[2]
iteration = args[3]
workdir = args[4]

print(args)

my_file = paste0(workdir, "instances/", file)
instancia = readQAP(my_file)

sol <- 1:instancia$n
poblation <- generatePoblation(sample(sol), 20)
sol <- sample(sol)


pid = Sys.getpid()
command = paste("psrecord", pid, "--log", paste0("logs/monitor/", file, "_", metaheuristic, "_", iteration,".txt"), "--plot", paste0("logs/", file, "_", metaheuristic, "_", iteration,".png"),"--interval 1")
system(command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = FALSE, input = NULL)


start_time = Sys.time()
if (metaheuristic == "SA") {
  tmax <- evaluarQAP(sol, instancia$f, instancia$d)
  tmin <- 10
  it <- 20
  beta <- 0.95
  e <- simulatedAnnealing(instancia, sol, tmax, tmin, it, beta, iteration = iteration, prefix = file)
} else {
  e <- evolutiveAlgorithm(instancia, poblation, size = 50, generations = 20, fighters = 10, parents = 50, percent = 0.2, iteration = iteration, prefix = file) 
}
#e <- simulatedAnnealing(instancia, sol, tmax, tmin, it, beta, iteration = i, prefix = file)
end_time = Sys.time()
total_time = end_time - start_time
total_time = as.numeric(total_time, units = "secs")

output_file = paste0(workdir, "output/results/", file, "_", iteration, "_", metaheuristic, ".csv")

write.table(c(e, total_time), file = output_file, row.names = F, sep=";", dec=".")