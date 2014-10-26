###########################################################################
#
## This is the source code for the Plot6 of the 
## Programming Assignment 2
#
## The structure of the code is as follows:
#
#  1. loaddataset function to load data from file
#  2. cookdataset function to filter and/or operate data
#  3. generateplot function to generate the plot
#  4  plot6function to execute in a single call all the process
#
# Note: the use of message() function is obviously not necessary.
#        this function is used as an indicator of progress of the plot 
#        generation
#
###########################################################################
#
###########################################################################
#
## Section 1: getdataset function. Loads and filters data as requested:
#             Motor Vehicle related sources
#
# Note1: assumed that files to load on the working directory
#
# Note2: this function could be optimized loading only data needed for 
#       plotting and filtering the rest. It is not done that way as we are
#       working from an exploratory aata analysis point of view, and thus 
#       not much concerned about performance and optimization
#
###########################################################################
getdataset <- function(){
  # load two datasets
  message("loaddataset.NEI. Start...................")
  if(exists("NEI")==FALSE){
    NEI <- readRDS("summarySCC_PM25.rds")  
  }
  message("loaddataset.NEI. End.....................")
  
  message("loaddataset.SCC. Start...................")
  if(exists("SCC")==FALSE){
    SCC <- readRDS("Source_Classification_Code.rds")  
  }
  message("loaddataset.NEI. End.....................")  
  
  # filter SCC. Filtering "vehicle" at SCC level 2
  motorVehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
  # subset SCC
  motorVehiclesSCC <- SCC[motorVehicles,]$SCC
  # subset NEI
  motorVehiclesNEI <- NEI[NEI$SCC %in% motorVehiclesSCC,]
  
  # return dataset
  return(motorVehiclesNEI)
}

###########################################################################
#
## Section 2: cookdataset function
#
###########################################################################
cookdataset <- function(dataset){
  message("cookdataset. Start...................")
  
  # filter data per Baltimore City
  dfBaltimore <- dataset[dataset$fips == "24510",]
  dfBaltimore$city <- "Baltimore City"
  # filter data per LA
  dfLA <- dataset[dataset$fips == "06037",]
  dfLA$city <- "Los Angeles County"
  # combine results from 2 cities
  df2Cities <- rbind(dfBaltimore, dfLA)
  # calculate total emissions per year
  df2 <- aggregate(df2Cities$Emissions,list(df2Cities$year,df2Cities$city),sum)
  View(df2)
  
  
  # arrange column names 
  colnames(df2) <- c("year", "city","emissions")
  # total emissions in tons
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
  png(filename="plot6.png", width=960) 
  
  # plot
  g <- ggplot(dataset, aes(year, emissions))
  h <- g + geom_line(aes(color=city)) + facet_grid(. ~ city) + 
    labs(title="Total Emissions (motor vehicle sources) per year in Baltimore City and LA County") + 
    labs(x="Year") + labs(y="Total Emissions (thousands of tons)") + 
    theme_bw(base_family="Times")
  print(h)
  
  # closes device
  dev.off() 
  message("generateplot. End.................")
}
###########################################################################
#
# plot6function
#
###########################################################################
plot6<- function(){
  df1 <- getdataset()
  df2 <- cookdataset(df1)
  generateplot(df2)
}


