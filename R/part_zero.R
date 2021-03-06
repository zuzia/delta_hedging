source("R\\main.r")  

#############################################################################################
# symulacja 1 dim
#############################################################################################      


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
# symulacja 2 dim
#############################################################################################    	

mean_vec <- c(dane.mean.WIG20, dane.mean.KGHM) ## dryf
sd_vec <- c(dane.sd.WIG20, dane.sd.KGHM) ## zmiennosc
S_0_vec <- c(dane.przyszle.WIG20[1,5], dane.przyszle.KGHM[1,5])

dane.symulacja.2dim <- fun.symuluj.2dim(1,
                                        S_0_vec,
                                        mean_vec,
                                        sd_vec,
                                        dane.korelacja,
                                        dane.przyszle.KGHM[,1],
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


dane.opcje.payoff.WIG20.A <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 0, 2400)
dane.opcje.payoff.WIG20.B <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 0, 2600)
dane.opcje.payoff.WIG20.C <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 1, 2900)
dane.opcje.payoff.WIG20.D <- payoff(t(dane.symulacja.1dim.WIG20.dla.opcji[2,2:(param.ile.symulacji.dla.opcji+1)])[1:param.ile.symulacji.dla.opcji], 1, 3100)
#dane.opcje.payoff.WIG20.D <- payoff(t(dane.symulacja.1dim.WIG20[159,2:(param.ile.symulacji+1)])[1:param.ile.symulacji], 1, 3100)


#############################################################################################
# prezentacja (wykresy) 1 dim
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

# zwroty
par(mfrow = c(2,2))
wykres.histogram.WIG20.symulacje.1 <- hist(dane.zwroty.WIG20.symulacje[,1], freq = FALSE, nclass = 25, col="#556270", xlab ="", main = "Histogram symulacji 1")
wykres.histogram.WIG20.symulacje.2 <- hist(dane.zwroty.WIG20.symulacje[,2], freq = FALSE, nclass = 25, col="#4ECDC4", xlab ="", main = "Histogram symulacji 2")
wykres.histogram.WIG20.symulacje.3 <- hist(dane.zwroty.WIG20.symulacje[,3], freq = FALSE, nclass = 25, col="#C7F464", xlab ="", main = "Histogram symulacji 3")
wykres.histogram.WIG20.historyczne <- hist(dane.zwroty.WIG20.historyczne , freq = FALSE, nclass = 25, col="#FF6B6B", xlab ="", main = "Histogram zwrotów historycznych")

# zwroty (więcej danych)
par(mfrow = c(1,2))
wykres.histogram.WIG20.symulacje.1 <- hist(c(t(dane.zwroty.WIG20.symulacje)), freq = FALSE, nclass = 50, col="#556270", xlab ="", main = "Histogram zwrotów z wszystkich symulacji")
lines(x, dnorm(x, mean=mean(c(t(dane.zwroty.WIG20.symulacje))), sd=sd(c(t(dane.zwroty.WIG20.symulacje)))), col="#FF6B6B", lwd = 3)

x <- seq(min(dane.zwroty.WIG20.historyczne), max(dane.zwroty.WIG20.historyczne), len=200)
wykres.histogram.WIG20.historyczne <- hist(dane.zwroty.WIG20.historyczne , freq = FALSE, nclass = 25, col="#FF6B6B", xlab ="", main = "Histogram zwrotów historycznych")
lines(x, dnorm(x, mean=mean(dane.zwroty.WIG20.historyczne), sd=sd(dane.zwroty.WIG20.historyczne)), col="#556270", lwd = 4)
# opcje z zerem
par(mfrow = c(2,2))
wykres.histogram.opcja.A <- hist(dane.opcje.payoff.WIG20.A , freq = FALSE, nclass = 25, col="#F56991", xlab ="", main = "Histogram zwrotów z opcji A")
wykres.histogram.opcja.B <- hist(dane.opcje.payoff.WIG20.B , freq = FALSE, nclass = 25, col="#FF9F80", xlab ="", main = "Histogram zwrotów z opcji B")
wykres.histogram.opcja.C <- hist(dane.opcje.payoff.WIG20.C , freq = FALSE, nclass = 25, col="#FFC48C", xlab ="", main = "Histogram zwrotów z opcji C")
wykres.histogram.opcja.D <- hist(dane.opcje.payoff.WIG20.D , freq = FALSE, nclass = 25,col="#EFFAB4", xlab ="", main = "Histogram zwrotów z z opcji D")

# opcje bez zera
par(mfrow = c(2,2))
wykres.histogram.opcja.A <- hist(dane.opcje.payoff.WIG20.A[! dane.opcje.payoff.WIG20.A %in% 0], freq = FALSE, nclass = 25, col="#F56991", xlab ="", main = "Histogram zwrotów z opcji A")
wykres.histogram.opcja.B <- hist(dane.opcje.payoff.WIG20.B[! dane.opcje.payoff.WIG20.B %in% 0] , freq = FALSE, nclass = 25, col="#FF9F80", xlab ="", main = "Histogram zwrotów z opcji B")
wykres.histogram.opcja.C <- hist(dane.opcje.payoff.WIG20.C[! dane.opcje.payoff.WIG20.C %in% 0] , freq = FALSE, nclass = 25, col="#FFC48C", xlab ="", main = "Histogram zwrotów z opcji C")
wykres.histogram.opcja.D <- hist(dane.opcje.payoff.WIG20.D[! dane.opcje.payoff.WIG20.D %in% 0] , freq = FALSE, nclass = 25,col="#EFFAB4", xlab ="", main = "Histogram zwrotów z z opcji D")


#############################################################################################
# zapis
#############################################################################################   

zapisz.wykres(wykres.1dim.WIG20, 9, 18, 90)

#############################################################################################
# prezentacja (wykresy) 2 dim WIG20 i KGHM
############################################################################################# 

##najpierw z kwantylami wykresy, osobno, będą prezentowane koło siebie
kolor.kwantyle.WIG20 <- "#E1017B"
kolor.linia.WIG20 <- "#720FCC"

dane.symulacja.2dim.WIG20 <- dane.symulacja.2dim[[1]]
dane.symulacja.2dim.KGHM <- dane.symulacja.2dim[[2]]

wykres.2dim.WIG20 <- rysuj.symulacje(dane.symulacja.2dim.WIG20,
                                     dane.przyszle.WIG20[,5],
                                     param.ile.symulacji,
                                     kolor.kwantyle.WIG20,
                                     kolor.linia.WIG20,
                                     "WIG20")
ggsave(plot=wykres.2dim.WIG20, file=sprintf("./images/part_zero/wykres.2dim.WIG20.png"),height=9,width=18,dpi=100)

wykres.2dim.KGHM <- rysuj.symulacje(dane.symulacja.2dim.KGHM,
                                    dane.przyszle.KGHM[,5],
                                    param.ile.symulacji,
                                    kolor.kwantyle.WIG20,
                                    kolor.linia.WIG20,
                                    "KGHM")
ggsave(plot=wykres.2dim.KGHM, file=sprintf("./images/part_zero/wykres.2dim.KGHM.png"),height=9,width=18,dpi=100)



##dwa jednocześnie, kawal porzadnego kreatywnego programowania ;)
rys_dwa <- rysuj_dwa(dane.symulacja.2dim, 4, 4) #na koncu powinno byc ile faktycznie mamy symulacji
ggsave(plot=rys_dwa, file=sprintf("./images/part_zero/rys_dwa.png"),height=9,width=18,dpi=100)
#############################################################################################
# zwroty 2 dim i policzone korealcje
#############################################################################################
dane.zwroty.WIG20.2dim.symulacje <- fun.policz.zwroty(dane.symulacja.2dim.WIG20,
                                                      param.data.sym.poczatek,
                                                      param.data.sym.koniec)

dane.zwroty.KGHM.2dim.symulacje <- fun.policz.zwroty(dane.symulacja.2dim.KGHM,
                                                     param.data.sym.poczatek,
                                                     param.data.sym.koniec)

dane.korelacja.symulacje <- cor(dane.zwroty.WIG20.2dim.symulacje[2],
                                dane.zwroty.KGHM.2dim.symulacje[2])

#dane.korelacja.symulacje
#          sym
#sym 0.7403541

# dane.korelacja.symulacje
#          sym.1
#sym.1 0.7814661

#dane.korelacja
#[1] 0.7865623

#############################################################################################
# testy normalnosci i niezaleznosci zwrotow
#############################################################################################
## qq-ploty
qq_wig20 <- qqnorm(dane.zwroty.akcji[2]$WIG20)
qq_kghm <- qqnorm(dane.zwroty.akcji[1]$KGHM)

#test Shapiro-Wilka
shapiro.test(dane.zwroty.akcji[1]$KGHM)
#> shapiro.test(dane.zwroty.akcji[1]$KGHM)
#
#	Shapiro-Wilk normality test
#
#data:  dane.zwroty.akcji[1]$KGHM 
#W = 0.9913, p-value = 0.1395

shapiro.test(dane.zwroty.akcji[2]$WIG20)
#> shapiro.test(dane.zwroty.akcji[2]$WIG20)
#
#	Shapiro-Wilk normality test
#
#data:  dane.zwroty.akcji[2]$WIG20 
#W = 0.979, p-value = 0.0008596

#test niezaleznosci chi-kwadrat
#chisq.test(dane.zwroty.akcji[2]$WIG20)                        

ks.test(dane.zwroty.akcji[1]$KGHM,pnorm,exact=TRUE)

#	One-sample Kolmogorov-Smirnov test
#
#data:  dane.zwroty.akcji[1]$KGHM 
#D = 0.4731, p-value = 2.887e-15
#alternative hypothesis: two-sided 
#
#Komunikat ostrzegawczy:
#In ks.test(dane.zwroty.akcji[1]$KGHM, pnorm, exact = TRUE) :
#  wartości powtórzone nie powinny być obecne w teście Kolmogorowa-Smirnowa

ks.test(dane.zwroty.akcji[2]$WIG20,pnorm,exact=TRUE)

#	One-sample Kolmogorov-Smirnov test
#
#data:  dane.zwroty.akcji[2]$WIG20 
#D = 0.4836, p-value = 2.887e-15
#alternative hypothesis: two-sided 