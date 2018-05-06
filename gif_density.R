source("graph_density.R")

png(file="awesome_gif%02d.png", width = 1000, height = 1000)
for (yr in 2004:2016){
  dens_plot(obs, yr)
}
dev.off()
