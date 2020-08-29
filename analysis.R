library("ggpubr")
# bur26a.dat Chr12a.dat esc64a.txt kra32.dat)
file = "esc64a.txt"


ev = data.frame()
for (i in 1:101) {
  my_file = paste0("output/results/", file, "_", i, "_", "evolutive", ".csv")
  aux = t(read.table(my_file, sep = ";", dec = "."))
  ev = rbind(ev, aux)
}
ev$met = "Evolutive"


sa = data.frame()
for (i in 1:101) {
  my_file = paste0("output/results/", file, "_", i, "_", "SA", ".csv")
  aux = t(read.table(my_file, sep = ";", dec = "."))
  sa = rbind(sa, aux)
}
sa$met = "Simulated Annealing"

data = rbind(ev, sa)

data$V3 = as.numeric(gsub(",", ".", data$V3))
data$V2 = as.numeric(gsub(",", ".", data$V2))


summary(data[data$met == "Evolutive", ]$V2)



ggboxplot(data = data,
          "met", 
          "V3",
          ylab = "Tiempo (segundos)",
          xlab = "Metaheurística",
          color = "met",
          add = "jitter",
          legend.title = "") %>% 
  ggexport(filename = paste0("plots/", file, "_time.png"), width = 500, height = 400)


ggboxplot(data = data,
          "met", 
          "V2",
          ylab = "Solución óptima",
          xlab = "Metaheurística",
          color = "met",
          add = "jitter",
          legend.title = "") %>% 
  ggexport(filename = paste0("plots/", file, "_solution.png"), width = 500, height = 400)


min(data$V2)




bests.sa = data.frame()
for (i in 68) {
  my_file = paste0("output/bests/sa_", file, "_", i,".csv")
  aux = t(read.table(my_file, sep = ";", dec = "."))
  aux = aux[2:length(aux)]
  aux = as.numeric(gsub(",", ".", aux))
  x = c(1:length(aux))
  df = data.frame(aux, x)
  df$sol = i
  bests.sa = rbind(bests.sa, df)
}

ggscatter(bests.sa,
          x = "x",
          y = "aux",
          color = "sol")


bests.ev = data.frame()
for (i in 66) {
  my_file = paste0("output/bests/evolutive_", file, "_", i,".csv")
  aux = t(read.table(my_file, sep = ";", dec = "."))
  aux = aux[2:length(aux)]
  aux = as.numeric(gsub(",", ".", aux))
  x = c(1:length(aux))
  df = data.frame(aux, x)
  df$sol = i
  bests.ev = rbind(bests.ev, df)
}

ggscatter(bests.ev,
          x = "x",
          y = "aux",
          color = "sol")



ev$V3 = as.numeric(gsub(",", ".", ev$V3))
which.min(ev$V3)

sa$V3 = as.numeric(gsub(",", ".", sa$V3))
which.min(sa$V3)





