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


#############################################################################################
# parametry
#############################################################################################

set.seed(666)
param.ile.symulacji <- 1000
param.data.poczatek <- "2010-02-01"
param.data.koniec <- "2011-02-01"
param.data.sym.poczatek <- "2011-02-01"
param.data.sym.koniec <- "2012-02-01"
param.string <- "1"

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

dane.zwrotyAkcji <- fun.policz.zwroty(dane.ceny.akcji,
                                      param.data.poczatek,
                                      param.data.koniec)

#dane.ceny.akcji[159,2]

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


dane.symulacja.1dim.WIG20 <- fun.symuluj.1dim(1,
                                         dane.przyszle.WIG20[1,5],
                                         dane.mean.WIG20,
                                         dane.sd.WIG20,
                                         dane.przyszle.WIG20[,1],
                                         1000)


#############################################################################################
# zwroty
#############################################################################################

dane.zwroty.symulacji <- fun.policz.zwroty(dane.symulacja.1dim.WIG20[,1:4],
                                      param.data.sym.poczatek,
                                      param.data.sym.koniec)


#############################################################################################
# prezentacja
#############################################################################################   

kolor.kwantyle.WIG20 <- "#E1017B"
kolor.linia.WIG20 <- "#720FCC"

wykres.1dim.WIG20 <- rysuj.symulacje(dane.symulacja.1dim.WIG20,
                                 dane.przyszle.WIG20[,5],
                                 1000,
                                 kolor.kwantyle.WIG20,
                                 kolor.linia.WIG20,
                                 "WIG20")




#############################################################################################
# zapis
#############################################################################################   

zapisz.wykres(wykres.1dim.WIG20, 9, 18, 90)


