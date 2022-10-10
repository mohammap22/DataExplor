library(ggplot2)
library(shiny)
library(dplyr)
library(tidyverse)
library(tidyr)
library(leaflet)
library(WDI)
library(drc)


#C02 <- ('EN.ATM.CO2E.KD.GD', 'EN.ATM.CO2E.PP.GD.KD', 'EN.ATM.CO2E.PP.GD', 'EN.ATM.CO2E.KT', 'EN.ATM.CO2E.PC', 
 #       'EN.CO2.ETOT.ZS', 'EN.ATM.CO2E.GF.ZS', 'EN.ATM.CO2E.GF.KT', 'EN.ATM.CO2E.LF.ZS', 'EN.ATM.CO2E.LF.KT', 
  #      'EN.CO2.MANF.ZS', 'EN.CO2.OTHX.ZS', 'EN.CO2.BLDG.ZS', 'EN.ATM.CO2E.SF.ZS', 'EN.ATM.CO2E.SF.KT', 
   #     'EN.CO2.TRAN.ZS', 'EN.ATM.CO2E.EG.ZS')

#look at air pollution too 

test <- WDI(country = "all", indicator = 'NY.GDP.PCAP.KD', start = 2015, end = 2015)
clearWDI <- WDI(country = c("AFE","AFW","CEB",
                            "EAS","ECS", "LCN","MEA", "NAC","PSS","SAS","SSF"), indicator = c("NY.GDP.PCAP.KD",'NY.GDP.MKTP.PP.CD',
                                               "SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY"),start = 2014, end = 2014)


largeclearWDI <- WDI(country = c("AFE","AFW","CEB",
                            "EAS","ECS", "LCN","MEA", "NAC","PSS","SAS","SSF"), indicator = c("NY.GDP.PCAP.KD",'NY.GDP.MKTP.PP.CD',
                                                                                              "SE.PRM.TENR","SL.TLF.0714.SW.TM","SI.POV.DDAY"),start = 2000, end = 2020)


FullclearWDI <- clearWDI[! is.na(clearWDI$SE.PRM.TENR),]


rowMeans(largeclearWDI[largeclearWDI$country =="North America",], na.rm=T)


largeclearWDI[largeclearWDI$country =="North America",]


model <- drm(Y ~ X, fct = DRC.asymReg())
ggplot(data = FullclearWDI, aes(x = NY.GDP.MKTP.PP.CD, y = SE.PRM.TENR, color = as.factor(country)))+
  geom_point()+xlab("Regions Average GDP") + ylab("Percent of Children Enrolled in School")
  
SI.POV.DDAY

USA <- WDIData[WDIData$Country.Code == "USA",]
plot(as.numeric(as.list(USA[USA$Indicator.Code == 'EN.ATM.CO2E.KD.GD',])))


topoData <- geoJ (countries.geojson) %>% paste(collapse = "\n") 

leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
  addTiles() %>%
  addTopoJSON(topoData, weight = 1, color = "#444444", fill = FALSE)






prob5 <- function(n){
  range = pbinom(1:n,n,.5) - pbinom(n-(1:n)-1,n,.5)
  return(min(which(range >=.95)))
}
prob5(10)





