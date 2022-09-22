#2022-07-27 mickael.dinclaux@inrae.fr

#v1.0 intial script 28/07/2022
#V1.1 added: support for replicas

#script to generate a template (plate) with or without the visualization 
#of replicas

# Reinitialize the session
rm(list=ls(all=TRUE))


#########################################################
###       Installing and loading required packages    ###
#########################################################


if (!require("xlsx")){
  install.packages("xlsx", dependencies = TRUE)
  library(xlsx)
}
if (!require("platetools")){
  install.packages("platetools", dependencies = TRUE)
  library(platetools)
}
if (!require("ggplot2")){
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}


#########################################################
###              working directory                    ###
#########################################################


setwd("C:/Users/mdinclaux/Documents/Script/TWB_Robot/transfert_list_plaque/")

data <- read.csv("test_MP2.csv", header = TRUE, sep = ";", dec = ".",
                 stringsAsFactors = TRUE)

plate_title <- "Plate 5" #Title

count_replicate <- TRUE #Color wells by number of replicas



#########################################################
###              Do not modify                        ###
#########################################################

data <-data[,-1]
data <-data[,-1]
ncols = 2



if(count_replicate == FALSE){
  data$Volume.source.well <- 50
 
  platemap <- plate_map_grid(data=data$Volume.source.well,
                             well =data$Destination.well,
                             plate_id = data$Destination.plate.ID)
  plt96(platemap, na_alpha = 0.6 ,na_fill = "#cccccc",  )+
    theme_bw() +
    theme(panel.spacing.x = unit(1,"lines"),
          panel.spacing.y = unit(0.5, "lines")) + 
    facet_wrap(~plate_label, ncol = ncols)+
    ggtitle(plate_title)+
    theme(legend.position="none")+
    theme(plot.title = element_text(hjust = 0.5))
}
if(count_replicate == TRUE) {
  
  platemap <- plate_map_grid(data=data$replica, well =data$Destination.well,
                             plate_id = data$Destination.plate.ID)
  platemap$values <- as.factor(platemap$values)
  plt96(platemap, na_alpha = 0.6 ,na_fill = "#cccccc",  )+
    theme_bw() +
    scale_fill_manual(values = c("#CCFF00", "#33CC00",
                                 "#006600"))+
    theme(panel.spacing.x = unit(1,"lines"),
          panel.spacing.y = unit(0.5, "lines")) + 
    facet_wrap(~plate_label, ncol = ncols)+
    ggtitle(plate_title)+
    theme(legend.position= c(0.8, 0.3))+
    labs(fill = "Nombre de réplicas")+
    theme(plot.title = element_text(hjust = 0.5))
}



