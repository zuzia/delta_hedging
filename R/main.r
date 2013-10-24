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
param.ile.symulacji.dla.opcji <- 10000
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


dane.mean.WIG20 <- mean(dane.zwroty.akcji[,2]) * length(dane.zwroty.akcji[,2])
dane.mean.KGHM <- mean(dane.zwroty.akcji[,1]) * length(dane.zwroty.akcji[,2])
dane.sd.WIG20 <- sd(dane.zwroty.akcji[,2]) * sqrt(length(dane.zwroty.akcji[,2]))
dane.sd.KGHM <- sd(dane.zwroty.akcji[,1])  * sqrt(length(dane.zwroty.akcji[,2]))

dane.korelacja <- cor(dane.zwroty.akcji[,1],
                      dane.zwroty.akcji[,2])

dane.kowariancja <- cov(dane.zwroty.akcji[,1],
                        dane.zwroty.akcji[,2])


#############################################################################################
# symulacja
#############################################################################################    	

dev.off()

dane.symulacja.1dim.WIG20 <- fun.symuluj.1dim(1,
                                         dane.przyszle.WIG20[1,5],
                                         dane.mean.WIG20,
                                         dane.sd.WIG20,
                                         dane.przyszle.WIG20[,1],
                                         param.ile.symulacji)


dane.symulacja.1dim.WIG20.dla.opcji <- fun.symuluj.1dim(159/253,
                                                        dane.przyszle.WIG20[1,5],
                                                        dane.mean.WIG20,
                                                        dane.sd.WIG20,
                                                        c("2011-02-01", "2011-09-16"),
                                                        param.ile.symulacji.dla.opcji)

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


dane.opcje.payoff.WIG20.A <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 0, 2400)
dane.opcje.payoff.WIG20.B <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 0, 2600)
dane.opcje.payoff.WIG20.C <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 1, 2900)
dane.opcje.payoff.WIG20.D <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 1, 3100)
#dane.opcje.payoff.WIG20.D <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 1, 3100)


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

# to poniżej trzeba ręcznie zapisać

par(mfrow = c(2,2))
wykres.histogram.WIG20.symulacje.1 <- hist(dane.zwroty.WIG20.symulacje[,1], freq = FALSE, nclass = 25, col="#556270", xlab ="", main = "Histogram symulacji 1")
wykres.histogram.WIG20.symulacje.2 <- hist(dane.zwroty.WIG20.symulacje[,2], freq = FALSE, nclass = 25, col="#4ECDC4", xlab ="", main = "Histogram symulacji 2")
wykres.histogram.WIG20.symulacje.3 <- hist(dane.zwroty.WIG20.symulacje[,3], freq = FALSE, nclass = 25, col="#C7F464", xlab ="", main = "Histogram symulacji 3")
wykres.histogram.WIG20.historyczne <- hist(dane.zwroty.WIG20.historyczne , freq = FALSE, nclass = 25, col="#FF6B6B", xlab ="", main = "Histogram zwrotów historycznych")

# to poniżej trzeba ręcznie zapisać
par(mfrow = c(2,2))
wykres.histogram.opcja.A <- hist(dane.opcje.payoff.WIG20.A , freq = FALSE, nclass = 25, col="#F56991", xlab ="", main = "Histogram zwrotów z opcji A")
wykres.histogram.opcja.B <- hist(dane.opcje.payoff.WIG20.B , freq = FALSE, nclass = 25, col="#FF9F80", xlab ="", main = "Histogram zwrotów z opcji B")
wykres.histogram.opcja.C <- hist(dane.opcje.payoff.WIG20.C , freq = FALSE, nclass = 25, col="#FFC48C", xlab ="", main = "Histogram zwrotów z opcji C")
wykres.histogram.opcja.D <- hist(dane.opcje.payoff.WIG20.D , freq = FALSE, nclass = 25,col="#EFFAB4", xlab ="", main = "Histogram zwrotów z z opcji D")


#############################################################################################
# zapis
#############################################################################################   

zapisz.wykres(wykres.1dim.WIG20, 9, 18, 90)


















