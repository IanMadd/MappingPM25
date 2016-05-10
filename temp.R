
#Read the population estimate data into R. Because the census is conducted only once every 10 years the data for these years are estimates. 

CensusEstimate9899 <- fread('9899CensusEstimate.csv', header = TRUE)
CensusEstimate0008 <- fread('CO-EST2008-ALLDATA.csv', header = TRUE)

#The population data in the 98 - 99 data set is character data and has commas as a thousands separator. Just using the as.numeric function will return NA's because it can't handle the commas. First remove the commas using gsub then convert to numeric data. Convert FIPS codes to character to match yearly data.tables.

CensusEstimate9899$`7/1/99 Estimate` <- as.numeric(gsub(",", "", CensusEstimate9899$`7/1/99 Estimate`))

CensusEstimate9899$Fips <- sprintf("%05d", as.integer(CensusEstimate9899$Fips)) 



#Now we take the Emissions / SqMile data and divide it by the population of each county. Similar to the divideEmissionsBySqMiles function, this function finds the emissions / SqMile data and divides it by the population of each county by finding the population of each county.


divideEmissionsPerSqMileByPop <- function (emissionsData,  popData) {
  vec <- numeric()
  x <- 0
  for (i in emissionsData$fips){
    x <- x + 1
    vec <- append(vec, emissionsData[emissionsData$fips == i, emissionsSqMiles] / as.numeric(popData[popData$Fips == i, 4, with = FALSE ]))   
  }
  return(vec)
}

#Cbind the results of the function to the NinetyNine data.table.

NinetyNine <- cbind(NinetyNine, emissionsSqMilePopTons = divideEmissionsPerSqMileByPop(NinetyNine, CensusEstimate9899))
head(NinetyNine)

# Get FIPS code in 2000 - 2008 data. The fips code in this data set is split into two columns, State and County. States codes that are less than 10 are just one digit, and county codes that are less than 100 are two or one digit. Use sprintf to make them the correct two and three digit lengths.
CensusEstimate0008$STATE <- sprintf("%02d", as.integer(CensusEstimate0008$STATE))  
CensusEstimate0008$COUNTY <- sprintf("%03d", as.integer(CensusEstimate0008$COUNTY)) 

#Then concatenate them to create the full five digit Fips code. 

CensusEstimate0008$FIPS <- paste(CensusEstimate0008$STATE, CensusEstimate0008$COUNTY, sep="")

# Multiply tons by 2000 to get pounds.

NinetyNine$emissionsSqMilePopPounds <- NinetyNine$emissionsSqMilePopTons * 2000

# Change the divideEmisionsPerSqMileByPop function to work on these 3 columns 

# emissionsData is now ZeroTwo, ZeroFive and ZeroEight
# popData is CensusEstimate0008$POPESTIMATE2002, CensusEstimate0008$POPESTIMATE2005, and CensusEstimate0008$POPESTIMATE2008. 2002 is column 12, 2005 is column 15, and 2008 is column 18.

# emissionsData Fips codes are ZeroTwo$fips, etc...
# popData Fips codes are CensusEstimate0008$FIPS
# > ZeroTwo$fips[2] == CensusEstimate0008$FIPS[2]



divideEmissionsPerSqMileByPop <- function (emissionsData,  popData, columnNumber) {
  vec <- numeric()
  fips <- numeric()
  emissions <- numeric()
  pop <- numeric()
  x <- 0
  for (i in emissionsData$fips){
    x <- x + 1
    fips <- append(fips, i)
    emissions <- append(emissions, as.numeric(emissionsData[emissionsData$fips == i, 4, with=FALSE]))
    pop <- append(pop, as.numeric(popData[popData$FIPS == i, columnNumber, with=FALSE]))  
    vec <- append(vec, as.numeric(emissionsData[emissionsData$fips == i, 4, with=FALSE]) / as.numeric(popData[popData$FIPS == i, columnNumber, with=FALSE]))   
  }
  #output <- data.table(fips = fips, emissions = emissions, vec = vec, pop=pop)
  #return(output)
  return(vec)
}

#Output<- data.table(divideEmissionsPerSqMileByPop(ZeroTwo,CensusEstimate0008, 12))

ZeroTwo <- cbind(ZeroTwo, emissionsSqMilePopPounds = divideEmissionsPerSqMileByPop(ZeroTwo,CensusEstimate0008, 12) * 2000)

ZeroFive <- cbind(ZeroFive, emissionsSqMilePopPounds = divideEmissionsPerSqMileByPop(ZeroFive,CensusEstimate0008, 15) * 2000)

ZeroEight <- cbind(ZeroEight, emissionsSqMilePopPounds = divideEmissionsPerSqMileByPop(ZeroEight,CensusEstimate0008, 18) * 2000)


#Decile of Ninety Nine Data Pounds of Emissions

DecileEmissionsCapitaPounds <- quantile(c(NinetyNine$emissionsSqMilePopPounds,ZeroEight$emissionsSqMilePopPounds, ZeroFive$emissionsSqMilePopPounds, ZeroTwo$emissionsSqMilePopPounds), probs=seq(0,1, by=0.1), na.rm = TRUE)


#Create color buckets

NinetyNine$colorbucketsPoundsEmissions <- as.numeric(cut(NinetyNine$emissionsSqMilePopPounds, DecileEmissionsCapitaPounds))

ZeroTwo$colorbucketsPoundsEmissions <- as.numeric(cut(ZeroTwo$emissionsSqMilePopPounds, DecileEmissionsCapitaPounds))
ZeroFive$colorbucketsPoundsEmissions <- as.numeric(cut(ZeroFive$emissionsSqMilePopPounds, DecileEmissionsCapitaPounds))
ZeroEight$colorbucketsPoundsEmissions <- as.numeric(cut(ZeroEight$emissionsSqMilePopPounds, DecileEmissionsCapitaPounds))

NinetyNineColorsMatched <- NinetyNine$colorbucketsPoundsEmissions[match(cnty.fips, as.numeric(NinetyNine$fips))]
ZeroTwoColorsMatched <- ZeroTwo$colorbucketsPoundsEmissions[match(cnty.fips, as.numeric(ZeroTwo$fips))]
ZeroFiveColorsMatched <- ZeroFive$colorbucketsPoundsEmissions[match(cnty.fips, as.numeric(ZeroFive$fips))]
ZeroEightColorsMatched <- ZeroEight$colorbucketsPoundsEmissions[match(cnty.fips, as.numeric(ZeroEight$fips))]

#1999 Emissions Map

png (file = 'PM25Emissions1999PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[NinetyNineColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 1999")  
legend("top", fill = (Emissions_palette4), legend = (leg.txt), ncol = 2)  
dev.off() 

#2002 Emissions Map
png (file = 'PM25Emissions2002PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroTwoColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2002")  
legend("top", fill = (Emissions_palette4), legend = (leg.txt), ncol = 2)  
dev.off() 

#2005 Emissions Map
png (file = 'PM25Emissions2005PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroFiveColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2005")  
legend("top", fill = (Emissions_palette4), legend = (leg.txt), ncol = 2)  
dev.off() 

#2008 Emissions Map
png (file = 'PM25Emissions2008PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroEightColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2008")  
legend("top", fill = (Emissions_palette4), legend = (leg.txt), ncol = 2)  
dev.off() 



