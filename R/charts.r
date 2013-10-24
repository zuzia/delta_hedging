
rysuj.symulacje <- function(notowania.symulacje, notowania.faktyczne, liczba.symulacji, kolor.kwantyle, kolor.linia, nazwa ="")
{

  
  rownames(notowania.symulacje) <- c()
  liczba.dni <- length(rownames(notowania.symulacje))
  pierwsza.symulacja <- 2 # numer kolumny, w ktorej znajduje sie 1 symulacja
  ostatnia.symulacja <- pierwsza.symulacja + liczba.symulacji - 1
  
  
  kwantyle <- t(apply(notowania.symulacje[, pierwsza.symulacja : ostatnia.symulacja], 1, quantile, probs = c(0.05, 0.3, 0.7, 0.95)))
  colnames(kwantyle)[1] <- "kwantyl.05"
  colnames(kwantyle)[2] <- "kwantyl.30"
  colnames(kwantyle)[3] <- "kwantyl.70"
  colnames(kwantyle)[4] <- "kwantyl.95"
  
  minimum <- apply(notowania.symulacje[, pierwsza.symulacja : ostatnia.symulacja], 1, min)
  maksimum <- apply(notowania.symulacje[, pierwsza.symulacja : ostatnia.symulacja], 1, max)
  
  dane.do.wykresu <- data.frame(numer.notowania = rep(1 : liczba.dni, times = liczba.symulacji),
                                notowanie = stack(notowania.symulacje[, pierwsza.symulacja : ostatnia.symulacja]))
  
  
  dane.do.wykresu <- cbind(dane.do.wykresu, kwantyle)
  
  dane.do.wykresu <- cbind(dane.do.wykresu, minimum)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "min"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, maksimum)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "max"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, notowania.faktyczne)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "faktyczne"
  
  wykres <- ggplot(dane.do.wykresu, aes(x = numer.notowania)) + xlab("Numer notowania") + ylab("Cena") # + ggtitle(paste("Symulacja", nazwa))
  wykres <- wykres + geom_line(aes(y = notowanie.values, group = notowanie.ind), alpha = .10)
  wykres <- wykres + geom_ribbon(aes(ymin = min, ymax = kwantyl.05), fill = kolor.kwantyle, alpha = .30) +
    geom_ribbon(aes(ymin = kwantyl.05, ymax = kwantyl.30), fill = kolor.kwantyle, alpha = .50) +
    geom_ribbon(aes(ymin = kwantyl.30, ymax = kwantyl.70), fill = kolor.kwantyle, alpha = .70) +
    geom_ribbon(aes(ymin = kwantyl.70, ymax = kwantyl.95), fill = kolor.kwantyle, alpha = .50) +
    geom_ribbon(aes(ymin = kwantyl.95, ymax = max), fill = kolor.kwantyle, alpha = .30)
  wykres <- wykres + geom_line(aes(y = faktyczne), colour = kolor.linia, size = 1)
  
  
  return( wykres )
  
}

rysuj.histogram <- function(dane, szerokosc.slupka, kolor.niski, kolor.wysoki, kolor.faktyczny, kolor.sigma, notowanie.faktyczne, szerokosc.faktyczne = 0.05, mi, sigma, nazwa)
{
  wLewo <- 1 - szerokosc.faktyczne
  wPrawo <- 1 + szerokosc.faktyczne
  
  if(notowanie.faktyczne == 0)
  {
    notowanie.faktyczne <- 0.001
  }
  
  wykres <- ggplot(dane, aes(x = payoff)) +
    xlab("wyplata") +
    ylab("liczba") +
    geom_rect(aes_string(xmin = mi - sigma, xmax = mi + sigma, ymin = 0, ymax = Inf), fill = kolor.sigma, alpha = .01) +
    #geom_vline(xintercept = mi, size = 2, colour = kolor.sigma, alpha = .3) +
    geom_rect(aes_string(xmin = mi*0.98 , xmax = mi*1.02, ymin = 0, ymax = Inf), fill = kolor.sigma) +
    geom_histogram(binwidth = szerokosc.slupka, aes(fill = ..count..)) +
    scale_fill_gradient("", low = kolor.niski, high = kolor.wysoki) +
    geom_rect(aes_string(xmin = notowanie.faktyczne*wLewo , xmax = notowanie.faktyczne*wPrawo, ymin = 0, ymax = Inf), fill = kolor.faktyczny) +
    theme(legend.position = "none") 
  #geom_vline(xintercept = notowanie.faktyczne, colour = kolor.faktyczny, size = 2)  
  
  
  return( wykres )
}


