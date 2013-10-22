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
source("R\\charts.r")  
require("ggplot2")

#############################################################################################
# parametry
#############################################################################################

set.seed(666)
param.ile.symulacji <- 1000
param.data.poczatek <- "2010-02-01"
param.data.koniec <- "2011-02-01"
param.data.sym.poczatek <- "2011-02-01"
param.data.sym.koniec <- "2012-02-01"
param.string <- "part_zero"

dir.create(sprintf("./images/%s",param.string),showWarnings=F)

#############################################################################################
# wczytanie danych
#############################################################################################

dane.KGHM <- read.csv("./R/data/kghm.csv")
dane.WIG20 <- read.csv("./R/data/wig20.csv")

dane.przyszle.KGHM <- read.csv("./R/data/kghm_future.csv")
dane.przyszle.WIG20 <- read.csv("./R/data/wig20_future.csv")

dane.ceny.akcji = data.frame(data=dane.KGHM[,1],
                             KGHM=dane.KGHM[,5],
                             WIG20=dane.WIG20[,5],
                             stringsAsFactors=FALSE)

dane.zwroty.akcji <- fun.policz.zwroty(dane.ceny.akcji,
                                      param.data.poczatek,
                                      param.data.koniec)


#############################################################################################
# estymacja
#############################################################################################


dane.mean.WIG20 <- mean(dane.zwroty.akcji[,2])
dane.mean.KGHM <- mean(dane.zwroty.akcji[,1])
dane.sd.WIG20 <- sd(dane.zwroty.akcji[,2])
dane.sd.KGHM <- sd(dane.zwroty.akcji[,1])

dane.korelacja <- cor(dane.zwroty.akcji[,1],
                      dane.zwroty.akcji[,2])

dane.kowariancja <- cov(dane.zwroty.akcji[,1],
                        dane.zwroty.akcji[,2])


#############################################################################################
# symulacja
#############################################################################################    	



dane.symulacja.1dim.WIG20 <- fun.symuluj.1dim(1,
                                         dane.przyszle.WIG20[1,5],
                                         dane.mean.WIG20,
                                         dane.sd.WIG20,
                                         dane.przyszle.WIG20[,1],
                                         param.ile.symulacji)


#############################################################################################
# zwroty
#############################################################################################

dane.zwroty.WIG20.symulacje <- fun.policz.zwroty(dane.symulacja.1dim.WIG20,
                                      param.data.sym.poczatek,
                                      param.data.sym.koniec)
dane.zwroty.WIG20.historyczne <- dane.zwroty.akcji[,"WIG20"]


#############################################################################################
# opcje
#############################################################################################


dane.opcje.payoff.WIG20.A <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 0, 2400)
dane.opcje.payoff.WIG20.B <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 0, 2600)
dane.opcje.payoff.WIG20.C <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 1, 2900)
dane.opcje.payoff.WIG20.D <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 1, 3100)

#############################################################################################
# prezentacja (wykresy)
#############################################################################################   

kolor.kwantyle.WIG20 <- "#E1017B"
kolor.linia.WIG20 <- "#720FCC"

wykres.1dim.WIG20 <- rysuj.symulacje(dane.symulacja.1dim.WIG20,
                                 dane.przyszle.WIG20[,5],
                                 param.ile.symulacji,
                                 kolor.kwantyle.WIG20,
                                 kolor.linia.WIG20,
                                 "WIG20")

#TODO - histogramy dla opcji i dla zwrotów(porównanie)


#############################################################################################
# zapis
#############################################################################################   

zapisz.wykres(wykres.1dim.WIG20, 9, 18, 90)


