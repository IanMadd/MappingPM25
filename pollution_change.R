

#download NEI data: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# Fips county code and county area data (ACS_09_5YR_G001_with_ann.csv) came from factfinder.census.gov


library("data.table")
library("colorspace")
library("mapproj")
library("maps")

data(county.fips)

#### first create a data.frame that gives total tons of 2.5PM by year and fips code from NEI dataset
# for example FIPS 09001 would have have four differnt totals, one for each year. 

NEI <- readRDS('summarySCC_PM25.rds')
NEI <- data.table(NEI)


countyfips <- fread('ACS_09_5YR_G001_with_ann.csv', header = TRUE)
setnames(countyfips, old = colnames(countyfips), new = as.character(countyfips[1,]))
countyfips <- countyfips [-1, ]

# aggregates data returning total tons by fips and year 
#(see for more info about data.table aggregation http://rprogramming.net/aggregate-data-in-r-using-data-table/): 
NEI_fips_year_aggregate <- as.data.table(NEI[, j = list(Emissions = sum(Emissions, na.rm = TRUE)), by = list(fips, year)])


# I noticed that the county with the highest amount of PM2.5 emissions was the Yukon-Koyukuk Census Area in AK.
# This county has an area of 147,000 sq miles. It didn't make sense to me for an area that large to grow brighter than 
# a county like Los Angeles which has lower total emissions but probably more per square mile. 

# First I create data.tables for each year 

NinetyNine <- NEI_fips_year_aggregate[NEI_fips_year_aggregate$year == 1999, ]
ZeroTwo <- NEI_fips_year_aggregate[NEI_fips_year_aggregate$year == 2002, ]
ZeroFive <- NEI_fips_year_aggregate[NEI_fips_year_aggregate$year == 2005, ] 
ZeroEight <- NEI_fips_year_aggregate[NEI_fips_year_aggregate$year == 2008, ]


# Now I create a function. This function takes emissions data for a particular fips, 
# finds the area in square miles for that fips in the countyfips file,
# and returns a vector with emissions/square miles. This function does
# not work with data sets where a fips code is listed more than once.
# It can't be used with NEI_fips_year_aggregate because there are four Emission records 
# and four years for each fips code. I haven't figured out how to do this operation with
#that file although I'm sure there is a way.

divideEmissionsBySqMiles <- function (emissionsdata,  SqMilesData = countyfips) {
  vec <- numeric()
  x <- 0
  for (i in emissionsdata$fips){  
    x <- x + 1
    vec <- append(vec, emissionsdata[emissionsdata$fips == i, Emissions] / as.numeric(SqMilesData[SqMilesData$Id2 == i, 38, with = FALSE ]))        
  } 
  return (vec)
}

#cbind the vector results to the data.table

NinetyNine <- cbind(NinetyNine, emissionsSqMiles = divideEmissionsBySqMiles(NinetyNine))
ZeroTwo <- cbind(ZeroTwo, emissionsSqMiles = divideEmissionsBySqMiles(ZeroTwo))
ZeroFive <- cbind(ZeroFive, emissionsSqMiles = divideEmissionsBySqMiles(ZeroFive))
ZeroEight <- cbind(ZeroEight, emissionsSqMiles = divideEmissionsBySqMiles(ZeroEight))


# create a color palette I ended up using one I found in http://colorbrewer2.org
Emissions_palette4 <- c("#ffe4e1", "#ffc9bd", "#ffad98", "#ff8f73", "#ff6d4e", "#ff3d21", "#ee0001", "#cb0002", "#ab0002", "#8b0000")

#get the deciles for the Emissions/SqMile for the first year. Basing all years on the first year
#will show declining emissions over the subsequent years. 

TotalDecile <- c(NinetyNine$emissionsSqMiles, ZeroTwo$emissionsSqMiles, ZeroFive$emissionsSqMiles, ZeroEight$emissionsSqMiles)
EmissionDecile <- quantile(TotalDecile, probs=seq(0,1, by=0.1), na.rm = TRUE)


#create a column in each table called colorbuckets that sorts each fips county
#into one of the deciles
NinetyNine$colorbuckets <- as.numeric(cut(NinetyNine$emissionsSqMiles, EmissionDecile))
ZeroTwo$colorbuckets <- as.numeric(cut(ZeroTwo$emissionsSqMiles, EmissionDecile))
ZeroFive$colorbuckets <- as.numeric(cut(ZeroFive$emissionsSqMiles, EmissionDecile))
ZeroEight$colorbuckets <- as.numeric(cut(ZeroEight$emissionsSqMiles, EmissionDecile))

# matches fips codes from my NEI data set and the data map set that lists county square miles 
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    county.fips$polyname)]
head(county.fips$fips[match(map("county", plot = FALSE)$names, county.fips$polyname)])
cnty.fips[1] == as.numeric(NinetyNine[67,1, with=FALSE])


NinetyNineColorsMatched <- NinetyNine$colorbuckets[match(cnty.fips, as.numeric(NinetyNine$fips))]
ZeroTwoColorsMatched <- ZeroTwo$colorbuckets[match(cnty.fips, as.numeric(ZeroTwo$fips))]
ZeroFiveColorsMatched <- ZeroFive$colorbuckets[match(cnty.fips, as.numeric(ZeroFive$fips))]
ZeroEightColorsMatched <- ZeroEight$colorbuckets[match(cnty.fips, as.numeric(ZeroEight$fips))]

# legend text for the final map
leg.txt <- c("0-.39", ".39-.66", ".66-.95", ".95-1.26",
             "1.26-1.59", "1.59-1.99", "1.99-2.51", "2.51-3.38", "3.38-5.47", ">5.47")


#####1999 Map 

png (file = 'PM25Emissions1999.png', width = 800, height = 800, pointsize = 12)  
map("county", col = Emissions_palette4[NinetyNineColorsMatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic") 
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, projection="polyconic")  
title("PM2.5 Emissions in Tons / SQ Mile / Year - 1999")  
legend("top", fill = (Emissions_palette4), legend = (leg.txt), ncol = 2)  
dev.off() 


#####2002 Map

png (file = 'PM25Emissions2002.png', width = 800, height = 800, pointsize = 12)
map("county", col = Emissions_palette4[ZeroTwoColorsMatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")

title("PM2.5 Emissions in Tons / SQ Mile / Year - 2002")
legend("top", fill = (Emissions_palette4),
       legend = (leg.txt), ncol = 2)
dev.off()


######2005 map

png (file = 'PM25Emissions2005.png', width = 800, height = 800, pointsize = 12)
map("county", col = Emissions_palette4[ZeroFiveColorsMatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")

title("PM2.5 Emissions in Tons / SQ Mile / Year - 2005")
legend("top", fill = (Emissions_palette4),
       legend = (leg.txt), ncol = 2)
dev.off()


####### 2008 map

png (file = 'PM25Emissions2008.png', width = 800, height = 800, pointsize = 12)
map("county", col = Emissions_palette4[ZeroEightColorsMatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")

map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")

title("PM2.5 Emissions in Tons / SQ Mile / Year - 2008")
legend("top", fill = (Emissions_palette4),
       legend = (leg.txt), ncol = 2)
dev.off()

