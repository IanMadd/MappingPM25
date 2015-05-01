#pal function
pal <- function (col, border = "light gray", ...) {
         n <- length(col)
         plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE, xlab = "", ylab = "", ...)
         rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border) 
        }


rainbow_hcl(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n, ...)

map('county', c('maine', "new hampshire", 'vermont', 'massachusetts'),
    fill = TRUE, col = palette(gray(seq(0,.9,len = 25))))

map('county', c('maine', "new hampshire", 'vermont', 'massachusetts'), 
    fill = TRUE, col = rainbow(12))

map('county', 'iowa', fill = TRUE, col = rainbow(12))
map('county', 'iowa', fill = TRUE, col = heat.colors(n=30, alpha = 1))
map('county', 'iowa', fill = TRUE, col = terrain.colors(n=20, alpha = 1))
map('county', 'iowa', fill = TRUE, col = col2rgb("peachpuff"))
map('county', 'iowa', fill = TRUE, col = col2rgb(c(blu = "royalblue", reddish = "tomato")))  # note: colnames

map('county', 'iowa', fill = TRUE, col = col2rgb(paste0("blue", 1:4)))  # note: colnames
map('county', 'iowa', fill = TRUE, col = col2rgb(1:10))

palette(gray(seq(0,.9,len = 25))) 

pallette <- colorRamp(c('red', 'blue'))
#The pal function at top doesn't work so well with colorRamp
# pallette(1) 
# pallette(0)
#>      [,1] [,2] [,3]
#>[1,]  255  255    0

#> pallette(.5)
#     [,1]  [,2]  [,3]
#[1,] 127.5 127.5 127.5

#> pallette(1)
#     [,1] [,2] [,3]
#[1,]    0    0  255

# pallette(seq(0,1, len = 10)) #gives a sequence of ten colors from pal(0) to pal(1)

palette <- colorRampPalette(c("blue", "red"))

#> palette(1)
#[1] "#FF0000"

#palette(10)
#[1] "#FF0000" "#FF1C00" "#FF3800" "#FF5500" "#FF7100" "#FF8D00" "#FFAA00" "#FFC600" "#FFE200" "#FFFF00"

#more than two colors as well
#palette <- colorRampPalette(c("blue","magenta", "red", "yellow"))


