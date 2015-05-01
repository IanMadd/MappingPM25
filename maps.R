map("Spain", interior = TRUE, boundary = TRUE)
map("state", boundary = TRUE, col="gray")

# Drawing Spain and Portugal
map(regions = c("Spain", 
                "Portugal"), boundary = TRUE, interior = TRUE)


# Drawing counties of iowa
map('county', 'iowa', fill = TRUE, col = palette())

#Drawing counties of New England
map('county', c('maine', "new hampshire", 'vermont', 'massachusetts'), 
    fill = TRUE, col = palette(gray(seq(0,.9,len = 25))))
#Changing fill to FALSE just shows the lines of the county borders
map('county', c('maine', "new hampshire", 'vermont', 'massachusetts'), 
    fill = FALSE, col = palette(gray(seq(0,.9,len = 25))))



#Drawing map of World with states
map('world', fill = TRUE, col = 1:100)

map("state", ".*dakota", myborder = 0)    # map of the dakotas
map.axes()   #draws lat/long on existing maps

#add axes
map('county', c('maine', "new hampshire", 'vermont', 'massachusetts', 'rhode island', 'connecticut'), 
    fill = TRUE, col = palette(gray(seq(0,.9,len = 25))))
map.axes()   #draws lat/long on existing maps

#change the projection. See help(mapproject)
if(require(mapproj))
        map('state', proj = 'bonne', param = 45) 

#This looks weird
if(require(mapproj))
        map('world', proj = 'bonne', param = 45) 

#But this looks better
if(require(mapproj))
        map('world', proj = 'lambert', param = c(0,1))

map('world', proj = 'lambert', param = c(0,0))

map('world', proj = 'mercator')

map("state",proj="albers",par=c(30,40))

map("world",proj="gnomonic",orient=c(0,-100,0))
map("world",proj="gnomonic",orient=c(0,90,0))


# Different line types
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)



# plot the ozone data on a base map
# (figure 4 in the reference)
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()


# Map projections showing unemployment by county


if(require(mapproj)) {    
        # mapproj is used for  projection="polyconic"
        # color US county map by 2009 unemployment rate
        # match counties to map using FIPS county codes
        # Based on J's solution to the "Choropleth Challenge"
        # <a href="http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html</p>
        #<p>' title="http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html</p>'
        #<p>">http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result....</p></a>  
        # load data
        # unemp includes data for some counties not on the "lower 48 states" county
        # map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
        #  cities
  data(unemp)
  data(county.fips)
 
  # define color buckets
  colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
        unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
        leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
        
        # align data with map definitions by (partial) matching state,county
        # names, which include multiple polygons for some counties
        cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                            county.fips$polyname)]
        colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]
        
        # draw map
        map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
            lty = 0, projection = "polyconic")
        map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
            projection="polyconic")
        title("unemployment by county, 2009")
        legend("topright", leg.txt, horiz = TRUE, fill = colors)
        
        # Choropleth Challenge example, based on J's solution, see:
        # <a href="http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html<br />
        #" title="http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
        #<br/>http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result....</a>  
        # To see the faint county boundaries, use RGui menu:  File/SaveAs/PDF
}