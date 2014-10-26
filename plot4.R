###########################################################################
#
## This is the source code for the Plot4 of the 
## Programming Assignment 2
#
## The structure of the code is as follows:
#
#  1. loaddataset function to load data from file
#  2. cookdataset function to filter and/or operate data
#  3. generateplot function to generate the plot
#  4  plot4 function to execute in a single call all the process
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
#             Coal Combustion related sources
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
  
  # filter SCC. first for the "combustion" rows at level one (more generic)
  combustionRows <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
  # filter SCC. then for the "coal" rows at level four (more specific)
  coalRows <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
  # dataset combining combustion and coalRows
  coalCombustionRows <- (combustionRows & coalRows)
  # Subset SCC
  coalCombustionSCC <- SCC[coalCombustionRows,]$SCC
  # Subset NEI
  coalCombustionNEI <- NEI[NEI$SCC %in% coalCombustionSCC,]
  
  # return dataset
  return(coalCombustionNEI)
}

###########################################################################
#
## Section 2: cookdataset function
#
###########################################################################
cookdataset <- function(dataset){
  message("cookdataset. Start...................")
  
  # calculate total emissions per year
  df2 <- aggregate(dataset$Emissions,list(dataset$year,dataset$type),sum)
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
  # set bg param transparent
  par(bg="transparent")
  # closes graphic device befor start plotting
  dev.off()
  
  # opens png graphic device
  png(filename="plot4.png") 
  
  # plot
  plot(dataset$year,dataset$emissions,ylab="Total Emissions (thousands of tones)",xlab="Year", main="Total PM2.5 emissions per year (coal combustion sources)",type="n")
  lines(dataset$year,dataset$emissions,col="red")
  
  # closes device
  dev.off()
  message("generateplot. End.................")
}
###########################################################################
#
# plot4 function
#
###########################################################################
plot4 <- function(){
  df1 <- getdataset()
  df2 <- cookdataset(df1)
  generateplot(df2)
}
