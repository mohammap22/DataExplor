library(ggplot2)
library(shiny)
library(dplyr)
library(tidyverse)
library(tidyr)
library(leaflet)
library(WDI)
library(drc)

install.packages("corrplot")
library(corrplot)

countries <- read.csv("c1329fad-744d-4da3-80ff-b321ebe3aa35_Data.csv")
regions <- read.csv("15961dc9-2878-4637-837a-a6f27d6463e2_Data.csv")


clearWDI <- WDI(country = c("AFE","AFW","CEB",
                            "EAS","ECS", "LCN","MEA", "NAC","PSS","SAS","SSF"), indicator = c("NY.GDP.PCAP.KD",'NY.GDP.MKTP.PP.CD',
                                               "SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY"),start = 2014, end = 2014)


largeclearWDI <- WDI(country = c("AFE","AFW","CEB",
                            "EAS","ECS", "LCN","MEA", "NAC","PSS","SAS","SSF"), indicator = c("NY.GDP.PCAP.KD",'NY.GDP.MKTP.PP.CD',
                                                                                              "SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                                                                              "EN.POP.SLUM.UR.ZS",
                                                                                              "SI.POV.NAHC"),start = 2000, end = 2020)



convertAvg <- function(country, vals, dataset){
  data <- data.frame(Region = country)
  for (i in vals){
    new <- rep(i, nrow(data))
    data[, ncol(data)+1] = mean(dataset[dataset$country == country,i], na.rm = TRUE)
    colnames(data)[ncol(data)] <- paste0("Average",i)
  }
  return(data)
}

makeAvgDF <- function(regions, indicators, dataset){
  ret <- data.frame()
  for(i in regions){
    ret = rbind(ret,convertAvg(i, indicators, dataset))
  }
  return(ret)
}
myp <- makeAvgDF(c("North America", "Pacific island small states","South Asia","Sub-Saharan Africa","Central Europe and the Baltics",
                   "East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean", 
                   "Middle East & North Africa"), c("NY.GDP.PCAP.KD","NY.GDP.MKTP.PP.CD","SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                                    "EN.POP.SLUM.UR.ZS","SI.POV.NAHC"),
                 largeclearWDI)



ggplot(data = myp, aes(x = AverageNY.GDP.MKTP.PP.CD, y = AverageSE.PRM.TENR, color = Region,
                       size = AverageSI.POV.NAHC))+
  geom_point()+xlab("Regions Average GDP") + ylab("Percent of Children Enrolled in School")




ugeMan <- WDI(country = "all",indicator = c("NY.GDP.PCAP.KD","NY.GDP.MKTP.PP.CD","SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                            "EN.POP.SLUM.UR.ZS","SI.POV.NAHC"), 
              start = 2010, end = 2020)

ugeCopy <- ugeMan
sum(is.na(ugeCopy$NY.GDP.MKTP.PP.CD))

P <- subset(ugeCopy, select = -SL.TLF.0714.SW.TM)
ugeCopy <- subset(ugeCopy, select = -SL.TLF.0714.SW.TM)[complete.cases(P), ]

ugeCopy <- unique(ugeCopy)


bigboy <- makeAvgDF(countries$Country.Name, c("NY.GDP.PCAP.KD","NY.GDP.MKTP.PP.CD","SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                      "EN.POP.SLUM.UR.ZS","SI.POV.NAHC"),ugeMan)


regionBig <- makeAvgDF(c("North America", "Pacific island small states","South Asia","Sub-Saharan Africa","Central Europe and the Baltics",
                       "East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean", 
                       "Middle East & North Africa"), c("NY.GDP.PCAP.KD","NY.GDP.MKTP.PP.CD","SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                              "EN.POP.SLUM.UR.ZS","SI.POV.NAHC"),ugeMan)

bigboy <- unique(bigboy)
regionBig <- unique(regionBig)

cor(bigboy$AverageNY.GDP.PCAP.KD, bigboy$AverageSL.TLF.0714.SW.TM, use = "complete.obs")

ggplot(data = (bigboy), aes(x = AverageNY.GDP.PCAP.KD, y = AverageEN.POP.SLUM.UR.ZS, color = Region))+
  geom_point()+xlab("Net Enrollment %") + ylab("GDP") +
  theme(legend.position = 'none')
  
AverageNY.GDP.PCAP.KD
AverageSI.POV.NAHC

prob5 <- function(n){
  range = pbinom(1:n,n,.5) - pbinom(n-(1:n)-1,n,.5)
  return(min(which(range >=.95)))
}
prob5(10)


recent <- ugeMan[ugeMan$year == 2019,]

recentRegions <- makeAvgDF(c("North America", "Pacific island small states","South Asia","Sub-Saharan Africa","Central Europe and the Baltics",
                         "East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean", 
                         "Middle East & North Africa"), c("NY.GDP.PCAP.KD","NY.GDP.MKTP.PP.CD","SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY",
                                                          "EN.POP.SLUM.UR.ZS","SI.POV.NAHC"),recent)

corData <- cor(ugeCopy[,-c(1:4)], use = 'complete.obs')

corrplot(corData)

ggplot(data = (ugeCopy), aes(x = SE.PRM.TENR, y = EN.POP.SLUM.UR.ZS, color = country, size = SI.POV.DDAY))+
  geom_point()+xlab("Net School Enrollment %") + ylab("% of Pop Living in the Slums") +
  theme(legend.position = 'none')


ggplot(data = (ugeCopy), aes(x = SE.PRM.TENR, y = EN.POP.SLUM.UR.ZS))+
  geom_point()+xlab("Net School Enrollment %") + ylab("% of Pop Living in the Slums") +
  theme(legend.position = 'none')+ geom_smooth(method=lm, se=FALSE)


