source("R\\main.r") 

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

dane.part.A.reality.hist.1 <- fun.eval.loss.reality(1, 0, 2400)[[1]][1+2,5]
dane.part.A.reality.hist.2 <- fun.eval.loss.reality(4, 0, 2400)[[1]][4+2,5]
dane.part.A.reality.hist.3 <- fun.eval.loss.reality(7, 0, 2400)[[1]][7+2,5]
dane.part.A.reality.hist.4 <- fun.eval.loss.reality(30, 0, 2400)[[1]][30+2,5]

hist.1 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.1, -140, 140), szerokosc.faktyczna = 5, nazwa = "1 rehedging", reality = dane.part.A.reality.hist.1)
hist.2 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.2, -140, 140), szerokosc.faktyczna = 5, nazwa = "4 rehedgingi", reality = dane.part.A.reality.hist.2)
hist.3 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.3, -140, 140), szerokosc.faktyczna = 5, nazwa = "7 rehedgingów", reality = dane.part.A.reality.hist.3)
hist.4 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.4, -140, 140), szerokosc.faktyczna = 4, nazwa = "10 rehedgingów", reality = dane.part.A.reality.hist.4)
hist.0 <- grid.arrange(hist.1, hist.2, hist.3, hist.4, ncol = 2)

#jeden histogram bez odrzucania odstających (dla Kasi)
dane.part.A.abstract.hist.5 <- fun.eval.loss.abstract(hist.iteration.no, 7, hist.type, hist.strike)
hist.5 <- rysuj.histogram(dane = c(dane.part.A.abstract.hist.5, -140, 140), szerokosc.faktyczna = 5, nazwa = "1 rehedging", reality = dane.part.A.reality.hist.3)

dane.part.A.abstract.2 <- fun.eval.loss.abstract(1000, 1:15, 1, 2900)
rysuj.kwantyle.straty(dane.part.A.abstract.2)

dane.part.A.reality.1 <- fun.eval.loss.reality(30, 0, 2600)[[1]][,5]
rysuj.linie(dane.part.A.reality.1)

#####rysuj.histogram(fun.eval.loss.abstract(hist.iteration.no, 30, hist.type, hist.strike), szerokosc.faktyczna = 5)

dane.part.A.reality.2 <- fun.eval.loss.reality(30, 1, 3000)[[1]][,2]
dane.part.A.reality.3 <- fun.eval.loss.reality(30, 1, 2800)[[1]][,2]
dane.part.A.reality.4 <- fun.eval.loss.reality(30, 1, 2500)[[1]][,2]
dane.part.A.reality.5 <- fun.eval.loss.reality(30, 1, 2300)[[1]][,2]
dane.part.A.reality.6 <- data.frame(x = (1:length(dane.part.A.reality.2)),
                                    y1 = dane.part.A.reality.2,
                                    y2 = dane.part.A.reality.3,
                                    y3 = dane.part.A.reality.4,
                                    y4 = dane.part.A.reality.5)

rysuj.linie2(dane.part.A.reality.6)


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

sens.ile.dyskretnych <- 50
sens.mnoznik <- 10
sens.liczba.iteracji <- 100
sens.liczba.rehedg <- 31
sens.typ1 <- 0
sens.strike <- 2600

dane.part.A.sensivity <- fun.sensivity (sens.ile.dyskretnych, sens.liczba.iteracji, sens.liczba.rehedg, sens.typ1, sens.strike , sens.mnoznik)

rysuj.analiza.wrazliwosci(dane.part.A.sensivity)


#############################################################################################
#############################################################################################
# premia za ryzko
#############################################################################################
#############################################################################################

risk.iteration.no <- 10000
risk.kwantyle <- seq(from = 0, to = 1, by = 0.01)
data.part.A.risk <- fun.risk.premium(risk.strike, risk.type, risk.iteration.no, risk.rehedg.no, risk.kwantyle)

rysuj.premie.za.ryzyko(data.part.A.risk)


