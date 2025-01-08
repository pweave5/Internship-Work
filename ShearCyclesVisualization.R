#Import the Libraries
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

#Import and clean the data
shearData <- read.csv("Shear_Cycles_Data.csv")
shearData$Date <- as.Date(shearData$Date, format = "%m/%d/%Y")
shearData$Hour <- as.factor(shearData$Hour)
shearData <- shearData[shearData$Date > "2024-05-31",]


#Divide into months
juneData <- shearData[shearData$Date > "2024-05-31" & shearData$Date < "2024-07-01",] 
julyData <- shearData[shearData$Date > "2024-06-30" & shearData$Date < "2024-08-01",] 
augustData <- shearData[shearData$Date > "2024-07-31" & shearData$Date < "2024-09-01",] 



####### Graphing Functions #########################################################################################################

#Function takes a data frame as input and returns the average cycles processed per day 
#This function is called in the graphing functions
getDailyAverage <- function(month){
  month <- month |> group_by(Date) |> summarise(Cylces = mean(n()))
  return(mean(month$Cylces))
}


#Bar Chart with Data Labels
#Function takes data frame and string as input and returns the visualization
createBarChartWithLabels <- function(monthDF, monthName){
  monthDF |>  group_by(Date) |> summarise(Cycles = n()) |>
    ggplot() + 
    geom_bar(aes(x = Date, y = Cycles), stat = "identity", fill = "#026148", color = "black") +
    geom_text(aes(x = Date, y = Cycles, label = Cycles), position = position_stack(vjust = 0.5), color = "white", size = 4.5) +
    ylim(0, 100) +
    labs(title = paste("Shear Cycles Completed per Day -", monthName),
         caption = paste("Average Number of Completed Cylces per Day =", round(getDailyAverage(monthDF), 1))) +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.caption = element_text(size = 10))
}


#Bar Chart without data labels
#Function takes data frame and string as input and returns the visualization
createBarChartWithoutLabels <- function(monthDF, monthName){
  monthDF |>  group_by(Date) |> summarise(Cycles = n()) |>
    ggplot() + 
    geom_bar(aes(x = Date, y = Cycles), stat = "identity", fill = "royalblue", color = "black") +
    ylim(0, 100) +
    labs(title = paste("Shear Cycles Completed per Day -", monthName),
         caption = paste("Average Number of Completed Cylces per Day =", round(getDailyAverage(monthDF), 1))) +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.caption = element_text(size = 10))
}


#Line Chart Visualization
#Function takes data frame and string as input and returns the visualization
createLineChart <- function(monthDF, monthName){
  monthDF |> group_by(Date) |> summarise(Cycles = n()) |>
    ggplot(aes(x = Date, y = Cycles)) + 
    geom_line(color = "royalblue", linewidth = 1) +
    geom_point(aes(x = Date, y = Cycles), color = "black", size = 2, shape = 16) + 
    ylim(0, 100) +
    labs(title = paste("Shear Cycles Completed per Day -", monthName),
         caption = paste("Average Number of Completed Cylces per Day =", round(getDailyAverage(monthDF), 1))) +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.caption = element_text(size = 10))
}


#This function creates bar chart of the average hourly cycles completed
createHourBarGraph <- function(dataframe, timePeriodName){
  Days <- dataframe |> group_by(Date) |> summarise(n = n()) |> nrow()
  hourData <- dataframe |> group_by(Hour) |> summarise(Cycles = n(), Average = round(n() / Days, 2))
  Hours <- c("1:00 PM", "2:00 PM", "3:00 PM", "7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM")
  hourData <- cbind(hourData, Hours)
  
  hourData |>
    ggplot(aes(x = factor(Hours, levels = c("7:00 AM", "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM")), y = Average)) +
    geom_bar(stat = "identity", fill = "orchid2", color = "black") +
    geom_text(aes(label = Average), position = position_stack(vjust = 0.5), color = "black", size = 5) +
    labs(title = paste("Shear Cycles Completed by Hour -", timePeriodName),
         caption = paste("Average Number of Completed Cycles per Day =", round(getDailyAverage(dataframe), 1))) +
    xlab("Hour") +
    ylab("Average Number of Completed Cycles") +
    theme(plot.title = element_text(size = 18, hjust = 0.5),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          plot.caption = element_text(size = 10))
}


#########################################################################################################################################

#All Time Record
createBarChartWithoutLabels(shearData, "Summer 2024")
createLineChart(shearData, "Summer 2024")
createHourBarGraph(shearData, "Summer 2024")

#June Visualizations
createBarChartWithLabels(juneData, "June 2024")
createLineChart(juneData, "June 2024")


#July Visualizations
createBarChartWithLabels(julyData, "July 2024")
createLineChart(julyData, "July 2024")


#August Visualizations
createBarChartWithLabels(augustData,"August 2024")
createLineChart(augustData, "August 2024")


#Find Daily Averages
cyclesPerDay <- getDailyAverage(shearData)
juneCyclesPerDay <- getDailyAverage(juneData)
julyCyclesPerDay <- getDailyAverage(julyData)
augustCyclesPerDay <- getDailyAverage(augustData)




