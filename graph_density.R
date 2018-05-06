library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)


dens_plot <- function(master_table,box1,box2,box3, year){
  
    table_1y <- filter(master_table, year == year)%>%
    unite("date",c("year", "month", "day"), sep="-")
    
    state1 <- filter(table_1y , box == box1)
    state2 <- filter(table_1y , box == box2)
    state3 <- filter(table_1y , box == box3)

  
    p1 <- ggplot(state1, aes(date, count), colour = species)+
    geom_density()+
    scale_x_date(date_breaks = "1 month", date_labels = "%M")+
    theme_bw()+
    theme(panel.grid.major = element_blank())+
    xlab("Time")+
    ylab("Density")

    p2 <- ggplot(state2, aes(date, count), colour = species)+
    geom_density()+
    scale_x_date(date_breaks = "1 month", date_labels = "%M")+
    theme_bw()+
    theme(panel.grid.major = element_blank()) +
    xlab("Time")+
    ylab("Density")
  
    p3 <- ggplot(state3, aes(date, count), colour = species)+
    geom_density()+
    scale_x_date(date_breaks = "1 month", date_labels = "%M")+
    theme_bw()+
    theme(panel.grid.major = element_blank())+
    xlab("Time")+
    ylab("Density")
    
    plot_boxes <- grid.arrange(p1, p2, p3, cols=2)
    return(plot_boxes)
}

