source("graph_density.R")

png(file="awesome_gif%02d.png", width = 600, height = 600)
for (yr in 2004:2016){
  dens_plot(obs, yr)
}
dev.off()
