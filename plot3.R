###########################################################################
#
## This is the source code for the Plot3 of the 
## Programming Assignment 2
#
## The structure of the code is as follows:
#
#  1. loaddataset function to load data from file
#  2. cookdataset function to filter and/or operate data
#  3. generateplot function to generate the plot
#  4  plot3 function to execute in a single call all the process
#
# Note: the use of message() function is obviously not necessary.
#        this function is used as an indicator of progress of the plot 
#        generation
#
###########################################################################
#
###########################################################################
#
## Section 1: loaddataset function
#
# Note1: assumed that files to load on the working directory
#
# Note2: this function could be optimized loading only data needed for 
#       plotting and filtering the rest. It is not done that way as we are
#       working from an exploratory aata analysis point of view, and thus 
#       not much concerned about performance and optimization
#
###########################################################################
loaddataset <- function(){
  message("loaddataset. Start...................")
  
  if(exists("dataset")==FALSE){
    dataset <- readRDS("summarySCC_PM25.rds")  
  }
  
  message("loaddataset. End.....................")
  
  # return dataset
  return(dataset)
}

###########################################################################
#
## Section 2: cookdataset function
#
###########################################################################
cookdataset <- function(dataset){
  message("cookdataset. Start...................")
  
  # filter for Baltimore city
  df1 <- dataset[dataset$fips == "24510",]
  # calculate total emissions per year
  df2 <- aggregate(df1$Emissions,list(df1$year,df1$type),sum)
  # arrange column names 
  colnames (df2) <- c("year", "type", "emissions")
  # total emissions in thousands of tons
  df2$emissions <- df2$emissions/1000
  
  message("cookdataset. End.....................")
  
  # return cooked dataset
  return(df2)
}

###########################################################################
#
## Section 3: generateplot function
#
###########################################################################
generateplot <- function(dataset){
  message("generateplot. Start...............")
  # load ggplot2 assuming it is installed
  library(ggplot2)
  # set bg param transparent
  par(bg="transparent")
  # closes graphic device befor start plotting
  dev.off()  
  # opens png graphic device
  png(filename="plot3.png", width=960) 
  
  # plot
  g <- ggplot(dataset, aes(year, emissions))
  h <- g + geom_line(aes(color=type)) + facet_grid(. ~ type) + 
       labs(title="Total Emissions per type and year in Baltimore") + 
       labs(x="Year") + labs(y="Total Emissions (thousands of tons)") + 
       theme_bw(base_family="Times")
  print(h)
  
  # closes device
  dev.off()
  message("generateplot. End.................")
}
###########################################################################
#
# plot3 function
#
###########################################################################
plot3 <- function(){
  df1 <- loaddataset()
  df2 <- cookdataset(df1)
  generateplot(df2)
}