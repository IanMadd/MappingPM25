
#Read the population estimate data into R. Because the census is conducted only once every 10 years the data for these years are estimates. 

CensusEstimate9899 <- fread('9899CensusEstimate.csv', header = TRUE)
CensusEstimate0008 <- fread('CO-EST2008-ALLDATA.csv', header = TRUE)

#The population data in the 98 - 99 data set is character data and has commas as a thousands separator. Just using the as.numeric function will return NA's because it can't handle the commas. Remove the commas using gsub and then convert to numeric data. 

CensusEstimate9899$`7/1/99 Estimate`<- as.numeric(gsub(",","",CensusEstimate9899$`7/1/99 Estimate`))

#The FIPS codes are numeric data which sometimes is only 4 digits for codes that start with a 0. Conver to five digit integers using sprintf and as.integer.

CensusEstimate9899$Fips <- sprintf("%05d", as.integer(CensusEstimate9899$Fips))

head(CensusEstimate9899)

#Now we take the Emissions / SqMile data and divide it by the population of each county. Similar to the divideEmissionsBySqMiles function, this function finds the emissions / SqMile data and divides it by the population of each county by finding the population of each county.


vec <- numeric()
for (i in NinetyNine$fips){
  vec <- append(vec, NinetyNine[NinetyNine$fips == i, emissionsSqMiles] / as.numeric(CensusEstimate9899[CensusEstimate9899$Fips == i, '7/1/99 Estimate', with=FALSE]))  
}

head(vec)

#Cbind the results of the for loop to the NinetyNine data.table and multiply by 100000 to get tons of emissions per 100,000 people.
NinetyNine <- cbind(NinetyNine, emissionsSqMilePerCapita = vec * 100000)
rm(vec,i)

# Get FIPS code in 2000 - 2008 data. The fips code in this data set is split into two columns, State and County. States codes that are less than 10 are just one digit, and county codes that are less than 100 are two or one digit. Use sprintf to make them the correct two and three digit lengths.
CensusEstimate0008$STATE <- sprintf("%02d", as.integer(CensusEstimate0008$STATE))  
CensusEstimate0008$COUNTY <- sprintf("%03d", as.integer(CensusEstimate0008$COUNTY)) 

#Then concatenate them to create the full five digit Fips code. 

CensusEstimate0008$FIPS <- paste(CensusEstimate0008$STATE, CensusEstimate0008$COUNTY, sep="")


# Change the divideEmisionsPerSqMileByPop function to work on these 3 columns 

# emissionsData is now ZeroTwo, ZeroFive and ZeroEight
# popData is CensusEstimate0008$POPESTIMATE2002, CensusEstimate0008$POPESTIMATE2005, and CensusEstimate0008$POPESTIMATE2008. 2002 is column 12, 2005 is column 15, and 2008 is column 18.

# emissionsData Fips codes are ZeroTwo$fips, etc...
# populationData Fips codes are CensusEstimate0008$FIPS
# ZeroTwo$fips[2] == CensusEstimate9899$Fips[3]



divideEmissionsPerSqMileByPopulation <- function (emissionsData,  populationData, columnNumber) {
  vec <- numeric()
  for (i in emissionsData$fips){
    vec <- append(vec, as.numeric(emissionsData[emissionsData$fips == i, 4, with=FALSE]) / as.numeric(populationData[populationData$FIPS == i, columnNumber, with=FALSE]))   
  }
  return(vec)
}


ZeroTwo <- cbind(ZeroTwo, emissionsSqMilePerCapita = divideEmissionsPerSqMileByPopulation(ZeroTwo,CensusEstimate0008, 12) * 100000)

ZeroFive <- cbind(ZeroFive, emissionsSqMilePerCapita = divideEmissionsPerSqMileByPopulation(ZeroFive,CensusEstimate0008, 15) * 100000)

ZeroEight <- cbind(ZeroEight, emissionsSqMilePerCapita = divideEmissionsPerSqMileByPopulation(ZeroEight,CensusEstimate0008, 18) * 100000)


#Decile of Ninety Nine Data Pounds of Emissions

DecileEmissionsCapitaTons <- quantile(c(NinetyNine$emissionsSqMilePerCapita, ZeroEight$emissionsSqMilePerCapita, ZeroFive$emissionsSqMilePerCapita, ZeroTwo$emissionsSqMilePerCapita), probs=seq(0,1, by=0.1), na.rm = TRUE)


#Create color buckets

NinetyNine$colorbucketsTonsEmissions <- as.numeric(cut(NinetyNine$emissionsSqMilePerCapita, DecileEmissionsCapitaTons))
ZeroTwo$colorbucketsTonsEmissions <- as.numeric(cut(ZeroTwo$emissionsSqMilePerCapita, DecileEmissionsCapitaTons))
ZeroFive$colorbucketsTonsEmissions <- as.numeric(cut(ZeroFive$emissionsSqMilePerCapita, DecileEmissionsCapitaTons))
ZeroEight$colorbucketsTonsEmissions <- as.numeric(cut(ZeroEight$emissionsSqMilePerCapita, DecileEmissionsCapitaTons))

NinetyNineColorsMatched <- NinetyNine$colorbucketsTonsEmissions[match(cnty.fips, as.numeric(NinetyNine$fips))]
ZeroTwoColorsMatched <- ZeroTwo$colorbucketsTonsEmissions[match(cnty.fips, as.numeric(ZeroTwo$fips))]
ZeroFiveColorsMatched <- ZeroFive$colorbucketsTonsEmissions[match(cnty.fips, as.numeric(ZeroFive$fips))]
ZeroEightColorsMatched <- ZeroEight$colorbucketsTonsEmissions[match(cnty.fips, as.numeric(ZeroEight$fips))]

EmissionsPerCapitaLegend.txt <- c("0-2.39", "2.39-3.79", "3.79-5.2", "5.2-6.71",
             "6.71-8.51", "8.51-10.92", "10.92-14.19", "14.19-19.66", "19.66-31.73", ">31.73")

#1999 Emissions Map

png (file = 'PM25Emissions1999PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[NinetyNineColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 1999")  
legend("top", fill = (Emissions_palette4), legend = (EmissionsPerCapitaLegend.txt), ncol = 2)  
dev.off() 

#2002 Emissions Map
png (file = 'PM25Emissions2002PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroTwoColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2002")  
legend("top", fill = (Emissions_palette4), legend = (EmissionsPerCapitaLegend.txt), ncol = 2)  
dev.off() 

#2005 Emissions Map
png (file = 'PM25Emissions2005PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroFiveColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2005")  
legend("top", fill = (Emissions_palette4), legend = (EmissionsPerCapitaLegend.txt), ncol = 2)  
dev.off() 

#2008 Emissions Map
png (file = 'PM25Emissions2008PerCapita.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[ZeroEightColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Pounds / SQ Mile / Per Capita / Year - 2008")  
legend("top", fill = (Emissions_palette4), legend = (EmissionsPerCapitaLegend.txt), ncol = 2)  
dev.off() 



