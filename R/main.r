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

hist.strike <- 2400
hist.type <- 0
hist.iteration.no <- 5000

set.seed(5)
dane.part.A.abstract.hist.1 <- remove_outliers(fun.eval.loss.abstract(hist.iteration.no, 1, hist.type, hist.strike))
dane.part.A.abstract.hist.2 <- remove_outliers(fun.eval.loss.abstract(hist.iteration.no, 4, hist.type, hist.strike))
dane.part.A.abstract.hist.3 <- remove_outliers(fun.eval.loss.abstract(hist.iteration.no, 7, hist.type, hist.strike))
dane.part.A.abstract.hist.4 <- remove_outliers(fun.eval.loss.abstract(hist.iteration.no, 30, hist.type, hist.strike))

hist.1 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.1, -140, 140), szerokosc.faktyczna = 5, nazwa = "1 rehedging")
hist.2 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.2, -140, 140), szerokosc.faktyczna = 5, nazwa = "4 rehedgingi")
hist.3 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.3, -140, 140), szerokosc.faktyczna = 5, nazwa = "7 rehedgingów")
hist.4 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.4, -140, 140), szerokosc.faktyczna = 5, nazwa = "10 rehedgingów")
hist.0 <- grid.arrange(hist.1, hist.2, hist.3, hist.4, ncol = 2)


dane.part.A.abstract.2 <- fun.eval.loss.abstract(1000, 1:15, 1, 2900)
rysuj.kwantyle.straty(dane.part.A.abstract.2)

dane.part.A.reality.1 <- fun.eval.loss.reality(30, 0, 2600)[[1]][,5]
rysuj.linie(dane.part.A.reality.1)

rysuj.histogram(fun.eval.loss.abstract(hist.iteration.no, 30, hist.type, hist.strike), szerokosc.faktyczna = 5)

dane.part.A.reality.2 <- fun.eval.loss.reality(30, 1, 3000)[[1]][,2]
rysuj.linie(dane.part.A.reality.2, szare = FALSE, bary = FALSE)


fun.eval.loss.reality(31, 0, 2400)[[1]][33,5]
fun.eval.loss.reality(31, 0, 2600)[[1]][33,5]
fun.eval.loss.reality(31, 1, 2900)[[1]][33,5]
fun.eval.loss.reality(31, 1, 3000)[[1]][33,5]
# t1.call <- c()

# for(i in 1:11) {
#   t1.call <- c(t1.call, fun.eval.loss.reality(30, 0, (2000+i*100))[[1]][32,5])
# }
# t1.put <- c()
# for(i in 1:11) {
#   t1.put <- c(t1.put, fun.eval.loss.reality(30, 1, (2000+i*100))[[1]][32,5])
# }
# dane.part.a.reality.0 <- data.frame(strike = c(2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 3100), call = t1.call, put = t1.put)
# 
# 

#############################################################################################
#############################################################################################
# analiza wrażliwości
#############################################################################################
#############################################################################################
###########################################################################################

ile.dysk <- 20
d.r <- seq(from = 0, to = param.r, length.out = ile.dysk)
d.mean <- seq(from = 0, to = dane.mean.WIG20, length.out = ile.dysk)
d.sd <- seq(from = 0, to = dane.sd.WIG20, length.out = ile.dysk)

d.res.r <- sapply(d.r,
             fun.eval.loss.preprocess1,
             liczba.iteracji = 100,
             liczba.rehedg = 31,
             typ1 = 0,
             s = dane.s0.WIG20,
             strike = 2600,
             sd = dane.sd.WIG20,
             T = param.T,
             mean = dane.mean.WIG20)
d.r.result <- c()
for(i in 1:ile.dysk) {
  d.r.result <- c(d.r.result, mean(d.res.r[,i]))
}

d.res.sd <- sapply(d.sd,
                  fun.eval.loss.preprocess1,
                  liczba.iteracji = 100,
                  liczba.rehedg = 31,
                  typ1 = 0,
                  s = dane.s0.WIG20,
                  strike = 2600,
                  r = param.r,
                  T = param.T,
                  mean = dane.mean.WIG20)
d.sd.result <- c()
for(i in 1:ile.dysk) {
  d.sd.result <- c(d.sd.result, mean(d.res.sd[,i]))
}
d.res.mean <- sapply(d.mean,
                   fun.eval.loss.preprocess1,
                   liczba.iteracji = 100,
                   liczba.rehedg = 31,
                   typ1 = 0,
                   s = dane.s0.WIG20,
                   strike = 2600,
                   r = param.r,
                   T = param.T,
                   sd = dane.sd.WIG20)
d.mean.result <- c()
for(i in 1:ile.dysk) {
  d.mean.result <- c(d.mean.result, mean(d.res.mean[,i]))
}

d.x <- seq(from = 0, to = 1, length.out = ile.dysk)

d <- data.frame(axis.x.param = d.x, .mean = d.mean.result, .sd = d.sd.result, .rate = d.r.result)

wykres <- rysuj.analiza.wrazliwosci(d)
wykres

#############################################################################################
#############################################################################################
# premia za ryzko
#############################################################################################
#############################################################################################


hist.strike <- 2600
hist.type <- 0
hist.iteration.no <- 100

set.seed(5)
dane.part.A.abstract.risk.premium <- fun.eval.loss.abstract(hist.iteration.no, 1, hist.type, hist.strike)
t1 <- dane.part.A.abstract.risk.premium

quantile(t1, probs = c(0.9))
