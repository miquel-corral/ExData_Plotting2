###########################################################################
#
## This is the source code for the Plot5 of the 
## Programming Assignment 2
#
## The structure of the code is as follows:
#
#  1. loaddataset function to load data from file
#  2. cookdataset function to filter and/or operate data
#  3. generateplot function to generate the plot
#  4  plot5function to execute in a single call all the process
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
  df1 <- dataset[dataset$fips == "24510",]
  # calculate total emissions per year
  df2 <- aggregate(df1$Emissions,list(df1$year),sum)
  # arrange column names 
  colnames(df2) <- c("year", "emissions")
  # total emissions in tons
  df2$emissions <- df2$emissions
  
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
  # set bg param transparent
  par(bg="transparent")
  # closes graphic device befor start plotting
  dev.off()
  
  # opens png graphic device
  png(filename="plot5.png", width=600) 
  
  # plot
  plot(dataset$year,dataset$emissions,ylab="Total Emissions (tones)",xlab="Year", main="Total PM2.5 emissions per year (motor vehicles) in Baltimore City",type="n")
  lines(dataset$year,dataset$emissions,col="red")
  
  # closes device
  dev.off()
  message("generateplot. End.................")
}
###########################################################################
#
# plot5function
#
###########################################################################
plot5<- function(){
  df1 <- getdataset()
  df2 <- cookdataset(df1)
  generateplot(df2)
}

