
#read data
climateData <- read.csv("ClimateData.csv")

#print data
#print(climateData)
#print summary of data
# summary (climateData)

#Whats the difference between print() and summary()
# summary contains min, median, mean, max. (data summarized)

#What does FG represent?
# number of foggy days

#How are empty values represented?
# represented with "NA"

#count NAs
# sum(is.na(climateData))
#total NA = 31

#print data type 
# print(class(climateData))
#variable type is data.frame, other variable types EX: character, integer, float

#DATA CLEANING (remove duplicates)
df <- read.csv("ClimateData.csv")
df[!duplicated(df), ]

#find mean and median
# summary(df)
#Year: median = 1986, mean = 1986
#T: median = 11.1, mean = 11.44
#TM: median = 16.4, mean = 16.3
#Tm: median = 6.7, mean = 6.76
#PP = median = 1011.6, mean = 1010.1
#V: median = 13.3, mean = 13.6
#RA: median = 188, mean = 186.8
#SN: median = 14, mean = 38.21
#TS: median = 5, mean = 6.233
#FG: median = 146, mean = 113.5
#TN: median = 0, mean = 0.2877
#GR: median = 0, mean = 0.7534

#find standard deviation of T 
# sd(df$T)
#returns NA b/c there are NAs in collumn

#find SD while ignoring NAs
# sd(df$T,na.rm=TRUE)
#returns 5.450711




#create pie charts


pp40s <- 0
pp50s <- 0 
pp60s <- 0
pp70s <- 0
pp80s <- 0
pp90s <- 0 
pp2000s<- 0
pp2010s <- 0
pp2020s <- 0 

for (x in (1:nrow(df))){
  #print(sum(df[row:(row+10),5],na.rm=TRUE))
  
  year <- df$Year[x]
  if (is.na(df$PP[x])){
    next
  }
  else if (year < 1950){
    pp40s <- pp40s+df$PP[x]
    
  }

  else if (year < 1960){
    pp50s <- pp50s+df$PP[x]
   
  }

  else if (year < 1970){
    pp60s<- pp60s+df$PP[x]
  }

  else if (year < 1980){
    pp70s<- pp70s+df$PP[x]
    
  }
  else if (year < 1990){
    pp80s<- pp80s+df$PP[x]
   
  }
  else if (year < 2000){
    pp90s<- pp90s+df$PP[x]
    
  }
  else if (year < 2010){
    pp2000s<- pp2000s+df$PP[x]
    
  }
  else if (year < 2020){
    pp2010s<- pp2010s+df$PP[x]
    
  }
  else if (year <= 2022){
    pp2020s<- pp2020s+df$PP[x]
  }
  
    
  }
chart <- c(pp40s,pp50s,pp60s,pp70s,pp80s,pp90s,pp2000s,pp2010s,pp2020s)
labels <- c("1940s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")
png(file="chart.png")
pie(chart,labels, main = "Precipitation")

totaldays<-c()
#make boxplot
for (i in (1:nrow(df))){
  total <- sum(df$SN[i], df$TS[i], df$TN[i], df$GR[i],na.rm=TRUE)
  totaldays <- c(totaldays, total)
 
  
  }

df<-cbind(df,totaldays)
print(df)

png(file="boxplot.png")

boxplot(df$totaldays,main="extreme weather per year",outline=FALSE)
#print(max(df$V,na.rm = TRUE))

#histogram
png(file="hist.png")
hist(df$V,main="Wind speeds",
     xlab = "Wind speeds",
     ylim = c(0,20),
     breaks=c(10,11,12,13,14,15,16,17,18,19,20))

#bar graph
colfunc <- colorRampPalette(c("deepskyblue","deepskyblue4"))
png(file="bargraph.png")
barplot(df$RA,
        xlab="years from 1948 to 2022",
        ylab="rainy days",
        main="rainy days per year",
        ylim=c(0,max(df$RA,na.rm=TRUE)),
        col=colfunc(80)
       )
#bar graph looks kinda weird since there are so many bars and lines,
#it would look better if it were a scatter plot

#scatter plot

png(file="scatterplot.png")
plot(x=df$TM,y=df$Year,
    main="max temp per year",
    xlab="max temp",
     ylab="year"
    )
#there is not enough evidence to conclude that the year causes increase in max temp.
dev.off()