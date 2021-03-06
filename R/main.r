#############################################################################################
#############################################################################################
# ustawienia
#############################################################################################
#############################################################################################

# uzupelnijcie podaj?c nazwe swojego kompa i ?cie?k? do folderu, gdzie trzymacie projekt

komputer <- Sys.info()["nodename"]
{
  if(komputer == "DYM")
    setwd("C:\\Users\\Jakub\\SkyDrive\\GitHub\\delta_hedging\\")
  else if(komputer == "SYDNEYBOOK")
    setwd("C:\\Users\\Sydney\\Documents\\GitHub\\delta_hedging\\")
  else if (komputer == "") 
    setwd("")
  else
    setwd("")
}


source("R\\functions.r")  
source("R\\charts.r") 
source("R\\part_A.r")  
require("ggplot2")
require("gridExtra")

#############################################################################################
#############################################################################################
# parametry
#############################################################################################
#############################################################################################

set.seed(666)
param.ile.symulacji <- 1000
param.ile.symulacji.dla.opcji <- 10000
param.data.poczatek <- "2010-02-01"
param.data.koniec <- "2011-02-01"
param.data.sym.poczatek <- "2011-02-01"
param.data.sym.koniec <- "2012-02-01"
param.string <- "part_zero"
param.r <- 0.0424
param.T <- 159/252
param.numer.wiersza <- 153

#############################################################################################
#############################################################################################
# wczytanie danych
#############################################################################################
#############################################################################################

dane.KGHM <- read.csv("./R/data/kghm.csv")
dane.WIG20 <- read.csv("./R/data/wig20.csv")

dane.przyszle.KGHM <- read.csv("./R/data/kghm_future.csv")
dane.przyszle.WIG20 <- read.csv("./R/data/wig20_future.csv")

dane.ceny.akcji = data.frame(data=dane.KGHM[,1],
                             KGHM=dane.KGHM[,5],
                             WIG20=dane.WIG20[,5],
                             stringsAsFactors=FALSE)

dane.s0.WIG20 <- dane.ceny.akcji[254,3]

dane.zwroty.akcji <- fun.policz.zwroty(dane.ceny.akcji,
                                      param.data.poczatek,
                                      param.data.koniec)

#############################################################################################
#############################################################################################
# estymacja
#############################################################################################
#############################################################################################

dane.mean.WIG20 <- mean(dane.zwroty.akcji[,2]) * length(dane.zwroty.akcji[,2])
dane.mean.KGHM <- mean(dane.zwroty.akcji[,1]) * length(dane.zwroty.akcji[,2])
dane.sd.WIG20 <- sd(dane.zwroty.akcji[,2]) * sqrt(length(dane.zwroty.akcji[,2]))
dane.sd.KGHM <- sd(dane.zwroty.akcji[,1])  * sqrt(length(dane.zwroty.akcji[,2]))

dane.korelacja <- cor(dane.zwroty.akcji[,1],
                      dane.zwroty.akcji[,2])

dane.kowariancja <- cov(dane.zwroty.akcji[,1],
                        dane.zwroty.akcji[,2])

