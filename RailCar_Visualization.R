#The goal of this script is to create a visualization for the average loading 
#time, while also showing the individual data points.

#Load Libraries
library(ggplot2)
library(dplyr)
library(hms)

railcar <- read.csv("RailCar_Loading_Data.csv")

railcar$LoadingTime <- hms(railcar$LoadingTimeSeconds)
railcar$Material <- as.factor(railcar$Material)
railcar <- railcar[railcar$Material != 'Pit Scrap', ]

#Visualization - Use jitter plot to show data points and bar chart to show the 
#different averages

railcar |> group_by(Material) |>
           summarise(m = round(mean(LoadingTimeSeconds), 0)) |> 
           ungroup() |> 
      ggplot(aes(x = Material, y = m, group = Material, color = Material)) +
              geom_bar(stat = "identity", fill = "white", color = "black") +
              geom_jitter(aes(Material,LoadingTimeSeconds), data = railcar, size = 2) +
              labs(title = "Time to Load a Railcar by Commodity", 
                   x = "Commodity", y = "Loading Time",
                   caption = "Average Load Times:   Bush = 21:11,   HMS = 10:12, 
                              P&S = 13:32,   RCS = 30:59,   Shred = 15:14") +
              scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                                 labels = function(x){
                                     format(as.POSIXct(hms(x)), "%M:%S")
                                  }) +
              theme(plot.title = element_text(size=16, hjust = 0.5),
                    axis.title.x = element_text(size = 13),
                    axis.title.y = element_text(size = 13))

          
              
        


