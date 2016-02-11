library("XML")       #readHTMLTable
library("extrafont") #loadfonts
library("zoo")       #as.yearmon
library("reshape2")  #melt
library("ggplot2")   #ggplot
library("xkcd")      #theme_xkcd
library("grid")      #grid.draw

setwd("/projects_small/gdal-ogr-format-driver-growth/")

#force english month abbreviations (font doesn't have any special charachters)
Sys.setlocale("LC_TIME", "C")

# XKCD Font
download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
font_import(".", prompt=FALSE)
if(.Platform$OS.type != "unix") {
  loadfonts(device="win")
} else {
  loadfonts()
}

data <- read.table(header = TRUE, text = "
version date
2.1.0 05-2016
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

x_date_labels <- function(currentDate) {
  trunkDate <- data[which(data$version == "2.1.0"),]$date
  currentData <- data[which(data$date == currentDate),]
  ifelse(
    trunkDate == currentDate,
    paste("future:",currentData$version, "-", format(currentData$date, "%b %Y")),
    paste(currentData$version, "-", format(currentData$date, "%b %Y"))
  )
}

p <- ggplot(melted, aes(date, value, color = variable)) +
  theme_xkcd() +
  #data
  geom_line(aes(group=variable)) +
  geom_label(aes(fill= variable, label=value), colour = "white", fontface = "bold", family = "xkcd", size=5, label.size = 1, show.legend=F) +
  #title
  labs(title="GDAL-OGR: continues growth of format drivers\n") +
  theme(plot.title = element_text(size=30, face="bold")) +
  #axes
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1)) +
  scale_x_date(name=NULL,
               breaks=data$date,
               labels=x_date_labels,
               limits=c(min(data$date)-150, max(data$date)+200)) +
  scale_y_continuous(name=NULL, breaks=NULL, expand=c(0, 0)) +
  theme(panel.grid=element_blank(), 
        panel.border=element_blank(),
        axis.ticks.length=unit(.2, "cm"),
        plot.margin=unit(c(1,.5,1,.5),"cm")) +
  #legend
  theme(legend.justification=c(0,.75), 
        legend.position=c(0,1), 
        legend.title=element_blank()) +
  scale_colour_discrete(breaks=c("gdal", "ogr"),
                        labels=c("\nnumber of\nraster formats\n", "number of\nvector formats"))


highs <- read.table(header = TRUE, text = "
driver version date variable hight
WMS 1.5.0 10-2007 gdal 90
BigTIFF 1.5.0 06-2007 gdal 107
Grib 1.6.0 12-2008 gdal 110
R 1.7.1 02-2010 gdal 120
Rasterlite 1.7.1 08-2010 gdal 90
WMTS 2.1.0 05-2016 gdal 131
SpatiaLite 1.7.1 07-2010 ogr 25
CartoDB 1.11.0 02-2014 ogr 91
WFS 1.8.0 12-2010 ogr 66
ElasticSearch 1.10.0 10-2013 ogr 55
GeoJSON 1.5.0 12-2007 ogr 45
MongoDB 2.1.0 05-2016 ogr 68")

highs$date <- as.Date(as.yearmon(highs$date, "%m-%Y"))
p <- p + geom_label(data=highs, aes(date, hight, group=variable, label=driver), fill="yellow", fontface = "bold", size=7, family = "xkcd", label.size = NA, show.legend = F)

# time axis arrow
p <- p + geom_segment(aes(x=as.Date("2003-12-01"), 
                                   y=0, 
                                   xend=as.Date("2016-11-01"), 
                                   yend=0),
                      arrow=arrow(length=unit(0.3,"cm")),
                      size=.5,
                      show.legend=F,
                      col="black")

# "plot created" tag
createdText <- paste("plot created", format(Sys.Date(), format="%Y-%m-%d"))
p <- p + annotation_custom(textGrob(createdText, 
                                    gp=gpar(fontsize=10, fontfamily="xkcd"),
                                    hjust=0),
                           xmin=as.numeric(as.Date(min(data$date))),
                           xmax=as.numeric(as.Date(min(data$date))),
                           ymin=-100,
                           ymax=0)


# hack to move the arrow out of the panel
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] <- "off"

grid.draw(gt)

ggsave("time-series.png", plot=gt, width=7, height=5, units="cm", dpi=300, scale=3)


#
# some simple statistics
#
numberVersion <- NROW(data)
numberVersionYears <- length(seq(min(data$date), max(data$date), "years"))

versionsPerYear <- round(numberVersion / numberVersionYears, 1)
paste(versionsPerYear, "versions per year")

gdalFormatsPerYear <- round((max(data$gdal) - min(data$gdal)) / numberVersionYears, 1)
ogrFormatsPerYear <- round((max(data$ogr) - min(data$ogr)) / numberVersionYears, 1)
paste(gdalFormatsPerYear, "format drivers per year")
paste(ogrFormatsPerYear, "format drivers per year")

gdalFormatsPerVersion <- round((max(data$gdal) - min(data$gdal)) / numberVersion, 1)
ogrFormatsPerVersion <- round((max(data$ogr) - min(data$ogr)) / numberVersion, 1)
paste(gdalFormatsPerVersion, "format drivers per version")
paste(ogrFormatsPerVersion, "format drivers per version")

