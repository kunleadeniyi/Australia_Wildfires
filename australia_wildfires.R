library(maps)
library(mapdata)
library(stringr)

# read data
fia <- read.csv("fire_nrt_M6_96062.csv", sep=",")
fia_original <- fia

acq_date <- as.Date(fia$acq_date, "%d/%m/%Y")
fia$acq_date <- acq_date

# important to mention
# date range of the first december 5 to jan 5
# normalize brightness and then use it to size the points. (if frp really refers to the heat

# scaler function converts the scale of any column from its original to a scale of 0 to 1
# 1 being the highest
# takes in vector or column
minmaxscaler <- function(val) {
  return ((val - min(val))/(max(val) - min(val)))
}


# For MODIS, the confidence value ranges from 0% to 100% and can be used to assign one of the three fire classes (low-confidence fire, nominal-confidence fire, or high-confidence fire) 
# Fires with confidence level lower than 30% are considered low confidence fires
fia <- fia[fia$confidence > 30,]

fia$frp_scaled <- minmaxscaler(fia$frp)
fia$brightness_scaled <- minmaxscaler((fia$brightness))

fia_2019 <- subset(fia, acq_date < '2020-01-01')
fia_2020 <- subset(fia, acq_date > '2019-12-31')

fia_tri_1 <- subset(fia, acq_date < '2019-12-12')
fia_tri_5 <- subset(fia, acq_date < '2019-12-12')
fia_tri_2 <- subset(fia, acq_date >= '2019-12-12' & acq_date < '2019-12-19')
fia_tri_6 <- subset(fia, acq_date < '2019-12-19')
fia_tri_3 <- subset(fia, acq_date >= '2019-12-19' & acq_date < '2019-12-26')
fia_tri_7 <- subset(fia, acq_date < '2019-12-26')
fia_tri_4 <- subset(fia, acq_date >= '2019-12-26' )
fia_tri_8 <- subset(fia )


fia$acq_time_2 <- as.character(fia$acq_time)

# left pad and make everything 4 digits
fia$acq_time_2 <- str_pad(fia$acq_time_2, 4, 'left', pad = "0")
fia$hour <- substr(fia$acq_time_2,1,2)


# aggregation by day
fia_by_day = aggregate(fia$acq_date,
                       by = list(fia$acq_date),
                       FUN = length)
names(fia_by_day) <- c('day', 'fires_detected')

# aggregation by hour
fia_by_hour = aggregate(fia$hour,
                        by = list(fia$hour),
                        FUN = length)
names(fia_by_hour) <- c('hour', 'fire_per_hour')


# insert missing hours of the day
hours_in_day <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", 
                  "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")

for (x in hours_in_day) {
  #print(x)
  if (! x %in% as.list(unique(fia_by_hour$hour))) {
    print(x)
    #fia_by_hour$hour = c(fia_by_hour$hour, x)
    #fia_by_hour$fire_per_hour = c(fia_by_hour$fire_per_hour, 0)
    fia_by_hour = rbind(fia_by_hour, list(x, 0))
  }
}

fia_by_hour <- fia_by_hour[order(fia_by_hour$hour),]

# set font
par(family='Avenir Black')

# boundary box
par(fig=c(0,1,0,1), bg="white")
par(mar=c(0,0,0,0))
par(oma=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
par(xpd=T)
plot(-99, -99, axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
box()

# layout

#dev.new()
# start parameters, blank plot with max x=1 and max y=1
par(fig=c(0,1,0,1), new=TRUE)
par(mar=c(0,0,0,0))
par(oma=c(1,1,1,1)) #bottom, left, top, right
      par(xpd=T)
plot(-99, -99, axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
title(main="Wildfires in Australia", line = -0.5, cex.main=2, outer=T)
#title(main="Wildfires in Australia", line = -1.25, cex.main=2, outer=T)


# (mastropi) mastropi. ‘Answer to “Adjust Plot Title and Sub-Title in Base R”’. 
# Stack Overflow, 8 Mar. 2019, https://stackoverflow.com/a/55059687.
viz_subtitle = "Between December 5, 2019 and January 5, 2020"
mtext(viz_subtitle, side=3, line=-1.5, adj=0.5, cex=0.7, family="Avenir", outer=T)



#layout
#x1, y1, x2, y2
#rect(0.5, 0, 1, 0.6, border=F, col='darkgrey') # bottom right plot

#rect(0, 0, 0.5, 0.6, border=F, col='white') # bottom left plot
#rect(0.00, 0.1, 0.25, 0.60, border=F, col='skyblue') # x1, y1, x2, y2
#rect(0.25, 0.1, 0.50, 0.60, border=F, col='skyblue') # x1, y1, x2, y2

# rect(0, 0.6, 1, 1, border=F, col='lightgrey') # top plot

#rect(0.00, 0.6, 0.25, 0.95, border=F, col='lightgrey') # top plot 1
#rect(0.25, 0.6, 0.50, 0.95, border=F, col='lightgrey') # top plot 2
#rect(0.50, 0.6, 0.75, 0.95, border=F, col='lightgrey') # top plot 3
#rect(0.75, 0.6, 1.00, 0.95, border=F, col='lightgrey') # top plot 4

#rect(0, 0.0, 0.5, 0.10, border=F, col='lightgrey') # bottom left corner

# colour
col_fire = rgb(0.98,0.30,0.25,0.2)
col_line = rgb(0.98,0.30,0.25)
col_bar = rgb(0.98,0.30,0.25)


# plots
# big plot
par(mar=c(0,2,1,1))
par(oma=c(1,1,1,1))
par(fig=c(0.5, 1, 0, 0.6), new=TRUE)
  # plot big map here
  plot(0,0, type='n', axes=F, ann=F, xlim=c(112, 155), ylim=c(-44, -10))
  #points(fia$longitude, fia$latitude, pch=19, col=rgb(0.98,0.30,0.25,fia$brightness_scaled))#, cex=fia$frp_scaled*3)
  points(fia$longitude, fia$latitude, pch=19, col=col_fire, cex=fia$brightness_scaled*2)
  map('world2', 'australia', add=T)
  title(main="Total burned areas")
  
# top plots
  # plot 4 small multiples map here
  #par(mar=c(0,0,2,0)) # bottom, left, top, right
  par(fig=c(0.00, 0.25, 0.6, 0.93), new=TRUE) # x1, x2, y1, y2
  
    plot(0,0, type='n', axes=F, ann=F, xlim=c(112, 155), ylim=c(-44, -10))
    # box()
    points(fia_tri_5$longitude, fia_tri_5$latitude, pch=19, col=col_fire, cex=fia$brightness_scaled)
    map('world2', 'australia', add=T)
    title(main="by December 12")

  par(fig=c(0.25, 0.50, 0.6, 0.93), new=TRUE) # x1, x2, y1, y2
      
    plot(0,0, type='n', axes=F, ann=F, xlim=c(112, 155), ylim=c(-44, -10))
    points(fia_tri_6$longitude, fia_tri_6$latitude, pch=19, col=col_fire, cex=fia$brightness_scaled)
    map('world2', 'australia', add=T)
    title(main="by December 19")

    
  par(fig=c(0.50, 0.75, 0.6, 0.93), new=TRUE) # x1, x2, y1, y2

    plot(0,0, type='n', axes=F, ann=F, xlim=c(112, 155), ylim=c(-44, -10))
    points(fia_tri_7$longitude, fia_tri_7$latitude, pch=19, col=col_fire, cex=fia$brightness_scaled)
    map('world2', 'australia', add=T)
    title(main="by December 26")
    
  par(fig=c(0.75, 1.00, 0.6, 0.93), new=TRUE) # x1, x2, y1, y2
      
    plot(0,0, type='n', axes=F, ann=F, xlim=c(112, 155), ylim=c(-44, -10))
    points(fia_tri_8$longitude, fia_tri_8$latitude, pch=19, col=col_fire, cex=fia$brightness_scaled)
    map('world2', 'australia', add=T)
    title(main="by January 5")

    
# bottom left 
    par(fig=c(0.00, 0.25, 0.1, 0.60), new=TRUE) # x1, x2, y1, y2
    par(mar=c(4,3,1,1)) # bottom, left, top, right
    par(oma=c(1,1,1,1))  
    par(xaxs="i", yaxs="i") # to join the x and y axis at 0,0
    # fires detected per day
    plot(fia_by_day$day, fia_by_day$fires_detected, 
         type='l', lwd=2,col=col_line,
         axes=F,  ylab="", xlab="",
         ylim=c(0,8000), 
         xlim=c(min(fia_by_day$day), max(fia_by_day$day)))
    axis(1, fia_by_day$day, format(fia_by_day$day, "%b %d"), las=2, tck=-0.02, cex.axis=0.5) #,labels = as.Date(fia_by_day$day))
    axis(2, at = seq(0,7500, by=500), labels=seq(0,7500, by=500),  
         tck=-0.02, cex.axis=0.5, padj = 2)
    title(main="Fires detected per day")
    title(ylab="Number of wildfires detected", line=1, cex.lab=1, family="Avenir")
    title(xlab="Days", line=2.5, cex.lab=1, family="Avenir")
    
    
    par(fig=c(0.25, 0.50, 0.1, 0.60), new=TRUE) 
    par(mar=c(4,3,1,1)) # bottom, left, top, right
    par(oma=c(1,1,1,0.5)) # 
    
    # fires per hour
    barplot(fia_by_hour$fire_per_hour, names.arg = c(fia_by_hour$hour), ylim=c(0,16000),
            cex.names = 0.5, col = col_bar, border = F, 
            axes = F, las=3) 
    axis(2, at = seq(0,15000, by=1000), labels=seq(0,15000, by=1000),  
         tck=-0.02, cex.axis=0.5, las=1, hadj=0.8)
    #axis(1, at=fia_by_hour$hour[c(1,24)], labels=c("Experi1","Experi13"),  tck=-0.02, cex.axis=0.5, las=2)
    title(main="Fires detected per hour")
    title(ylab="Number of wildfires detected", line=2.2, cex.lab=1, family="Avenir")
    title(xlab="Hour of day (24hr)", line=2.5, cex.lab=1, family="Avenir")


# Foot notes 
    par(fig=c(0.0, 0.5, 0.0, 0.10), new=TRUE) #x1, x2, y1, y2
    #par(xaxs="r", yaxs="r",)
    par(mar=c(0,0,0,0))
    par(oma=c(1,1,1,1))
    plot(0, 0, axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', type='n')
    text(0.0, 0.9," Notes", adj=0)
    text(0.0, 0.7, 
         " All data are from December 5, 2019, to January 5, 2020. \n All fires of low confidence have been excluded \n Source: NASA", 
         adj=c(0,1), family="Avenir", cex=0.85)
    
# (R Core Team) R Core Team. R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, 2022, https://www.R-project.org/.
    
    
    
    
    
