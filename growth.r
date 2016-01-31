install.packages("xkcd")
install.packages("htmlTable")
install.packages("reshape2")
install.packages("extrafont")
install.packages("ggplot2")
install.packages("cowplot")
library("XML")       #readHTMLTable
library("extrafont")
library("zoo")       
library("htmlTable")
library("reshape2")
library("ggplot2")
library("xkcd")
library("cowplot")   #ggdraw

setwd("/projects_small/gdal-versions/")

# XKCD Font
download.file("http://simonsoftware.se/other/xkcd.ttf",
                             dest="xkcd.ttf", mode="wb")
font_import(".", prompt=FALSE)
if(.Platform$OS.type != "unix") {
  ## Register fonts for Windows bitmap output
  loadfonts(device="win")
} else {
  loadfonts()
}

xkcd_line <- function(x, y, color) {
  len <- length(x);
  rg <- par("usr");
  yjitter <- (rg[4] - rg[3]) / 1000;
  xjitter <- (rg[2] - rg[1]) / 1000;
  x_mod <- x + rnorm(len) * xjitter;
  y_mod <- y + rnorm(len) * yjitter;
  lines(x_mod, y_mod, col='white', lwd=10);
  lines(x_mod, y_mod, col=color, lwd=5);
}

xkcd_axis <- function() {
  rg <- par("usr");
  yaxis <- 1:100 / 100 * (rg[4] - rg[3]) + rg[3];
  xaxis <- 1:100 / 100 * (rg[2] - rg[1]) + rg[1];
  xkcd_line(1:100 * 0 + rg[1] + (rg[2]-rg[1])/100, yaxis,'black')
  xkcd_line(xaxis, 1:100 * 0 + rg[3] + (rg[4]-rg[3])/100, 'black')
}


data <- read.table(header = TRUE, text = "
version date
2.0.0 06-2015
1.11.0 04-2014
1.10.0 04-2013
1.9.0 12-2011
1.8.0 01-2011
1.7.1 02-2010
1.6.0 12-2008
1.5.0 12-2007
1.4.0 01-2007
1.3.0 08-2005
1.2.0 03-2004")

data$date <- as.Date(as.yearmon(data$date, "%m-%Y"))

countGdalFormats <- function(version){
  formats <- readHTMLTable(
    readLines(paste("sources/gdal-",version,"/frmts/formats_list.html", sep=""))
  )[[1]]
  NROW(formats)-1
}

countOgrFormats <- function(version){
  formats <- readHTMLTable(
    readLines(paste("sources/gdal-",version,"/ogr/ogrsf_frmts/ogr_formats.html", sep=""))
  )[[1]]
  NROW(formats)-1
}

data$gdal <- apply(data, 1, function(x){countGdalFormats(x[[1]])})

data$ogr <- apply(data, 1, function(x){countOgrFormats(x[[1]])})

melted <- melt(data, id.vars=c("version", "date"))

p <- ggplot(melted, aes(date, value, color = variable)) +
  ggtitle("GDAL-OGR: continues growth of format drivers") +
  geom_line(aes(group=variable)) +
  theme_xkcd() +
  #axes
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) +
  scale_x_date(name="version and date",
               breaks=data$date, 
               labels=paste(data$version, "-", format(data$date, "%b %Y"))) +
  scale_y_continuous(name="num of formats",
                     breaks=seq(0,150,25)) +
  #label
  geom_label(aes(fill= variable, label=value), colour = "white", fontface = "bold", family = "xkcd", label.size = 1, show.legend=F) +
  #legend
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        plot.title = element_text(size=30, face="bold")) +
  scale_colour_discrete(breaks=c("gdal", "ogr"),
                        labels=c("num of raster formats", "num of vector formats"))


highs <- read.table(header = TRUE, text = "
driver version date variable hight
WMS 1.5.0 12-2007 gdal 90
BigTIFF 1.5.0 05-2007 gdal 110
Grib 1.6.0 12-2008 gdal 110
R 1.7.1 02-2010 gdal 120
Rasterlite 1.7.1 10-2010 gdal 90
SpatiaLite 1.7.1 07-2010 ogr 25
CartoDB 1.11.0 04-2014 ogr 91
WFS 1.8.0 01-2011 ogr 66
ES 1.10.0 04-2013 ogr 55
GeoJSON 1.5.0 12-2007 ogr 44")

highs$date <- as.Date(as.yearmon(highs$date, "%m-%Y"))
p <- p + geom_label(data=highs, aes(date, hight, group=variable, label=driver), fill="yellow", fontface = "bold", size=9, family = "xkcd", label.size = 0, show.legend = F)


ggdraw(switch_axis_position(p, axis = 'y'))

