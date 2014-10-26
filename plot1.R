###########################################################################
#
## This is the source code for the Plot1 of the 
## Programming Assignment 2
#
## The structure of the code is as follows:
#
#  1. loaddataset function to load data from file
#  2. cookdataset function to filter and/or operate data
#  3. generateplot function to generate the plot
#  4  plot1 function to execute in a single call all the process
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
  
  # calculate total emissions
  df <- aggregate(dataset$Emissions,list(dataset$year),sum)
  # arrange column names
  colnames(df) <- c("year", "emissions")
  # total emissions in millions of tones
  df$emissions <- df$emissions/1000000
  
  message("cookdataset. End.....................")
  
  # return cooked dataset
  return(df)
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
  png(filename="plot1.png") 
  
  # plot
  plot(dataset$year,dataset$emissions,ylab="Total Emissions (millions of tones)",xlab="Year", main="Total PM2.5 emissions per year (all sources)",type="n")
  lines(dataset$year,dataset$emissions,col="red")
  
  # closes device
  dev.off()
  message("generateplot. End.................")
}
###########################################################################
#
# plot1 function
#
###########################################################################
plot1 <- function(){
  df1 <- loaddataset()
  df2 <- cookdataset(df1)
  generateplot(df2)
}