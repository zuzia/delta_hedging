#############################################################################################
# ustawienia
#############################################################################################

# uzupelnijcie podaj?c nazwe swojego kompa i ?cie?k? do folderu, gdzie trzymacie projekt

komputer <- Sys.info()["nodename"]
{
  if(komputer == "DYM")
    setwd("C:\\Users\\Jakub\\SkyDrive\\GitHub\\delta_hedging\\")
  else if(komputer == "")
    setwd("")
  else if (komputer == "") 
    setwd("")
  else
    setwd("")
}
source("R\\functions.r")  


#############################################################################################
# parametry
#############################################################################################

set.seed(666)
param.ile.symulacji <- 1000
param.data.poczatek <- "2010-02-01"
param.data.koniec <- "2011-02-01"

#############################################################################################
# wczytanie danych
#############################################################################################

dane.KGHM <- read.csv("./R/data/kghm.csv")
dane.WIG20 <- read.csv("./R/data/wig20.csv")

dane.przyszle.KGHM <- read.csv("./R/data/kghm_future.csv")
dane.przyszle.WIG20 <- read.csv("./R/data/wig20_future.csv")

dane.ceny.akcji = data.frame(data=dane.KGHM[,2],
                             KGHM=dane.KGHM[,5],
                             WIG20=dane.WIG20[,5],
                             stringsAsFactors=FALSE)

dane.zwrotyAkcji <- fun.policz.zwroty(dane.ceny.akcji,
                                      param.data.poczatek,
                                      param.data.koniec)

#############################################################################################
# estymacja
#############################################################################################


dane.mean.WIG20 <- mean(dane.zwrotyAkcji[,2])
dane.mean.KGHM <- mean(dane.zwrotyAkcji[,1])
dane.sd.WIG20 <- sd(dane.zwrotyAkcji[,2])
dane.sd.KGHM <- sd(dane.zwrotyAkcji[,1])

dane.korelacja <- cor(dane.zwrotyAkcji[,1],
                      dane.zwrotyAkcji[,2])

dane.kowariancja <- cov(dane.zwrotyAkcji[,1],
                        dane.zwrotyAkcji[,2])


#############################################################################################
# symulacja
#############################################################################################    	


dane.symulacja.WIG20 <- fun.symuluj.1dim(1,
                                         dane.WIG20[1,5],
                                         dane.mean.WIG20,
                                         dane.sd.WIG20,
                                         dane.przyszle.WIG20[,1],
                                         4)





