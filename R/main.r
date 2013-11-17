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
  else if(komputer == "")
    setwd("SYDNEYBOOK")
  else if (komputer == "C:\\Users\\Sydney\\Documents\\GitHub\\delta_hedging\\") 
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
param.r <- 0.032
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

#############################################################################################
#############################################################################################
# part A
#############################################################################################
#############################################################################################


dane.part.A.abstract.hist.1 <- fun.eval.loss.abstract(5000, 1, 0, 2400)
dane.part.A.abstract.hist.2 <- fun.eval.loss.abstract(5000, 4, 0, 2400)
dane.part.A.abstract.hist.3 <- fun.eval.loss.abstract(5000, 7, 0, 2400)
dane.part.A.abstract.hist.3 <- fun.eval.loss.abstract(5000, 10, 0, 2400)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

hist.1 <- rysuj.histogram(dane = dane.part.A.abstract.hist.1, szerokosc.faktyczna = 5)
hist.2 <- rysuj.histogram(dane = dane.part.A.abstract.hist.2, szerokosc.faktyczna = 5)
hist.3 <- rysuj.histogram(dane = dane.part.A.abstract.hist.3, szerokosc.faktyczna = 5)
hist.4 <- rysuj.histogram(dane = dane.part.A.abstract.hist.4, szerokosc.faktyczna = 5)

hist.0 <- grid.arrange(hist.1, hist.2, hist.3, hist.4, ncol = 2)



dane.part.A.abstract.2 <- fun.eval.loss.abstract(1000, 1:15, 0, 2600)
rysuj.kwantyle.straty(dane.part.A.abstract.2)

dane.part.A.reality.1 <- fun.eval.loss.reality(30, 1, 2600)[[1]][,5]
rysuj.linie(dane.part.A.reality.1)

dane.part.A.reality.2 <- fun.eval.loss.reality(30, 0, 2600)[[1]][,2]
rysuj.linie(dane.part.A.reality.2, szare = FALSE, bary = FALSE)







