library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)


obs <- read.csv("Data/result_table.csv", header = T)

dens_plot <- function(master_table, yr){
  
    table_1y <- filter(master_table, year == yr, box == 1 )%>%
    select("box","month", "pollen","bird")%>%
    mutate(freq_poll = pollen/sum(pollen)) %>%
    mutate(freq_bird = bird/sum(bird)) %>%
    gather(species, count, freq_poll:freq_bird)
    
    table_2y <- filter(master_table, year == yr, box ==2)%>%
      select("box","month", "pollen","bird")%>%
      mutate(freq_poll = pollen/sum(pollen)) %>%
      mutate(freq_bird = bird/sum(bird)) %>%
      gather(species, count, freq_poll:freq_bird)
    
    table_3y <- filter(master_table, year == yr, box ==3 )%>%
      select("box","month", "pollen","bird")%>%
      mutate(freq_poll = pollen/sum(pollen)) %>%
      mutate(freq_bird = bird/sum(bird)) %>%
      gather(species, count, freq_poll:freq_bird)
  
    p1 <- ggplot(table_1y, aes(x= month, y = count, color = species, fill= species))+
    ylim(0,1) +
    geom_area(alpha=0.4)+
    theme_bw()+
    theme(panel.grid.major = element_blank())+
    scale_x_continuous("months", breaks=1:12 , labels=c("J", "F","M","A","M","J","JL","A","S","O","N","D"))+ 
    xlab("Month")+
    ylab("Observations") +
    scale_fill_manual(values = c("red","green"), labels=c("Bird", "Plant")) +
    scale_color_manual(values = c("darkred","darkgreen"), guide=F) +
    ggtitle("South")

    p2 <- ggplot(table_2y, aes(x= month, y = count, color = species, fill= species))+
        geom_area(alpha=0.4)+
        ylim(0,1) +
        theme_bw()+
        theme(panel.grid.major = element_blank())+
        scale_x_continuous("months", breaks=1:12 , labels=c("J", "F","M","A","M","J","JL","A","S","O","N","D"))+ 
        xlab("Month")+
        ylab("Observations") +
        scale_fill_manual(values = c("red","green"), labels=c("Bird", "Plant")) +
        scale_color_manual(values = c("darkred","darkgreen"), guide =F) +
        ggtitle("Mid-East")
  
    p3 <- ggplot(table_3y, aes(x= month, y = count, color = species, fill= species))+
        ylim(0,1) +
        geom_area(alpha=0.4)+
        theme_bw()+
        theme(panel.grid.major = element_blank())+
        scale_x_continuous("months", breaks=1:12 , labels=c("J", "F","M","A","M","J","JL","A","S","O","N","D"))+ 
        xlab("Month")+
        ylab("Observations") +
        scale_fill_manual(values = c("red","green"), labels=c("Bird", "Plant")) +
        scale_color_manual(values = c("darkred","darkgreen"), guide=F) +
        ggtitle("Northeast")
    
    
    plot_boxes <- grid.arrange(p1, p2, p3, top=as.character(yr))
    return(plot_boxes)
}

