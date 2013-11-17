
# input to wektor liczb (wartość portfela/delty/etc w kolejnych chwilach)
# szare: można dać FALSE, wtedy na górnym wykresie nie ma szrego tła w okolicy zera
# bary: można dać FALSe, wtedy jest sam wykres liniowy (ten górny)

rysuj.linie <- function(input, szare = TRUE, bary = TRUE) {
  
  t0 <- input
  maks <- max(abs(t0))/15
  t1 <- c(0)
  for(i in 2:length(t0)) {
    t1 <- c(t1, t0[i] - t0[i-1])
  }
  t2 <- data.frame(x = 1:length(t1), y1 = t0, y2 = t1, min = rep(-maks, length(t1)), max = rep(maks, length(t1)))
  
  w1 <- ggplot(data = t2)
  
  if(szare == TRUE)
    w1 <- w1 + geom_ribbon(aes(x = x, ymin = min, ymax = max), fill = "grey", alpha = .50)

  w1 <- w1 +
    geom_line(aes(x = x, y = y1), color = "black", size = 1) +
    theme_bw() +
    theme(legend.position = "none", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15)) +
    xlab("") +
    ylab("") +
    ggtitle("")
  
  if(bary == TRUE) {
    w2 <- ggplot() +
      geom_bar(data = t2, aes(x = x, y = y2 * (y2 > 0)), fill = "#282634", stat="identity") +
      geom_bar(data = t2, aes(x = x, y = y2 * (y2 < 0)), fill = "#FF4E44", stat="identity") +
      theme_bw() +
      theme(legend.position = "none", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15)) +
      xlab("") +
      ylab("") +
      ggtitle("")
  
    w3 <- grid.arrange(arrangeGrob(w1, w2, heights=c(0.4, 0.6), ncol=1),ncol = 1)
  } else {
    w3 <- w1
  }
  
  return (w3)  
}

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
  wykres <- wykres +
    geom_ribbon(aes(ymin = min, ymax = kwantyl.05), fill = kolor.kwantyle, alpha = .30) +
    geom_ribbon(aes(ymin = kwantyl.05, ymax = kwantyl.30), fill = kolor.kwantyle, alpha = .50) +
    geom_ribbon(aes(ymin = kwantyl.30, ymax = kwantyl.70), fill = kolor.kwantyle, alpha = .70) +
    geom_ribbon(aes(ymin = kwantyl.70, ymax = kwantyl.95), fill = kolor.kwantyle, alpha = .50) +
    geom_ribbon(aes(ymin = kwantyl.95, ymax = max), fill = kolor.kwantyle, alpha = .30)
  wykres <- wykres + geom_line(aes(y = faktyczne), colour = kolor.linia, size = 1)
  
  
  return( wykres )
}

rysuj.histogram <- function(dane, kolor.niski = "#D0E0EB", kolor.wysoki = "#88ABC2", kolor.sigma = "red", szerokosc.faktyczna = 5, nazwa = "wykres")
{
  mi <- mean(dane)
  sigma <- sd(dane)
  
  wykres <- ggplot(data.frame(ax = dane), aes(x = ax)) +
    xlab("") +
    ylab("") +
    ggtitle("") +
    geom_rect(aes_string(xmin = mi - sigma, xmax = mi + sigma, ymin = 0, ymax = Inf), fill = kolor.sigma, alpha = .005) + #odchylenie
    geom_rect(aes_string(xmin = mi-(sigma/30) , xmax = mi+(sigma/30), ymin = 0, ymax = Inf), fill = kolor.sigma, alpha = .05) + #srednia
    geom_histogram(binwidth = szerokosc.faktyczna, aes(fill = ..count..)) + #histogram
    scale_fill_gradient("", low = kolor.niski, high = kolor.wysoki) +
    theme(legend.position = "none", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15))
  
  return( wykres )
}

rysuj.kwantyle.straty <- function(dane, circle.kolor = "#547980", kolor.kwantyle = "#45ADA8", circle.alpha = .04, circle.size = 7) {
  
  pre.kwantyle <- data.frame(t(apply(t(dane), 1, quantile, probs = c(0.05, 0.3, 0.7, 0.95))))
  minima <- data.frame((apply(t(dane), 1, min)))
  maksima <- data.frame((apply(t(dane), 1, max)))
  
  kwantyle <- cbind(1:(ncol(dane)), pre.kwantyle, minima, maksima)
  
  colnames(kwantyle) <- c("ax", "kwantyl.5", "kwantyl.30", "kwantyl.70", "kwantyl.95", "min", "max")
  
  dane.do.wykresu <- data.frame(ax = sort(rep(1 : ncol(dane), times = nrow(dane))), value = stack(data.frame(dane))["values"])
  
  wykres <- ggplot(dane.do.wykresu, aes(x = ax)) +
    xlab("") +
    ylab("") +
    ggtitle("") +
    geom_point(aes(y = values), size = circle.size, alpha = circle.alpha, colour = circle.kolor) +
    #geom_ribbon(data = kwantyle, aes(x = ax, ymin = min, ymax = kwantyl.5), fill = kolor.kwantyle, alpha = .30) +
    geom_ribbon(data = kwantyle, aes(x = ax, ymin = kwantyl.5, ymax = kwantyl.30), fill = kolor.kwantyle, alpha = .50) +
    geom_ribbon(data = kwantyle, aes(x = ax, ymin = kwantyl.30, ymax = kwantyl.70), fill = kolor.kwantyle, alpha = .90) +
    geom_ribbon(data = kwantyle, aes(x = ax, ymin = kwantyl.70, ymax = kwantyl.95), fill = kolor.kwantyle, alpha = .50) +
    #geom_ribbon(data = kwantyle, aes(x = ax, ymin = kwantyl.95, ymax = max), fill = kolor.kwantyle, alpha = .30) +
    theme(legend.position = "none", axis.text.y = element_text(size=15), axis.text.x = element_text(size=15))

  return ( wykres )
}


rysuj_dwa <- function(notowania.zbiorcze, liczba.symulacji, liczba.do_mean){
  symulacje.WIG20 <- notowania.zbiorcze[[1]]
  symulacje.KGHM <- notowania.zbiorcze[[2]]
  
  liczba.dni <- length(symulacje.WIG20$sym)
  pierwsza.symulacja <- 2 # numer kolumny, w ktorej znajduje sie 1 symulacja
  ostatnia.symulacja <- pierwsza.symulacja + liczba.symulacji - 1
  ostatnia_mean <- pierwsza.symulacja + liczba.do_mean - 1
  
  minimum.WIG20 <- apply(symulacje.WIG20[, pierwsza.symulacja : ostatnia.symulacja], 1, min)
  maksimum.WIG20 <- apply(symulacje.WIG20[, pierwsza.symulacja : ostatnia.symulacja], 1, max)
  ##TODO to jest krzywe, sprawdzic
  mean.WIG20 <- apply(symulacje.WIG20[1 : liczba.dni, ][pierwsza.symulacja:ostatnia_mean], 1, mean)
  
  minimum.KGHM <- apply(symulacje.KGHM[, pierwsza.symulacja : ostatnia.symulacja], 1, min)
  maksimum.KGHM <- apply(symulacje.KGHM[, pierwsza.symulacja : ostatnia.symulacja], 1, max)
  ##TODO to jest krzywe, sprawdzic  
  mean.KGHM <- apply(symulacje.KGHM[1 : liczba.dni, ][pierwsza.symulacja:ostatnia_mean], 1, mean)
  
  WIG_col <- "blue"
  KGHM_col <- "red"
  
  dane.do.wykresu <- data.frame(numer.notowania = rep(1 : liczba.dni, times = liczba.symulacji),
                                notowanie = stack(symulacje.WIG20[, pierwsza.symulacja : ostatnia.symulacja]))
  
  dane.do.wykresu <- cbind(dane.do.wykresu, minimum.WIG20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "min"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, maksimum.WIG20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "max"
  ##teraz nadszedl czas na kreatywne programowanie bez znajomosci ggplota i R  
  dane.do.wykresu <- cbind(dane.do.wykresu, symulacje.KGHM[2]$sym*20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "kghm1"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, symulacje.KGHM[3]$sym*20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "kghm2"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, symulacje.KGHM[4]$sym*20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "kghm3"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, symulacje.KGHM[5]$sym*20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "kghm4"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, mean.KGHM*20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "kghm_m"
  
  dane.do.wykresu <- cbind(dane.do.wykresu, mean.WIG20)
  colnames(dane.do.wykresu)[length(dane.do.wykresu)] <- "wig20_m"
  
  wykres <- ggplot(dane.do.wykresu, aes(x = numer.notowania)) + xlab("Numer notowania") + ylab("Cena") # + ggtitle(paste("Symulacja", nazwa))
  wykres <- wykres + geom_line(aes(y = notowanie.values, group = notowanie.ind),colour=WIG_col, alpha = .30)
  
  wykres <- wykres + geom_line(aes(y = kghm1, group = notowanie.ind),colour=KGHM_col, alpha = .10)
  wykres <- wykres + geom_line(aes(y = kghm2, group = notowanie.ind),colour=KGHM_col, alpha = .10)
  wykres <- wykres + geom_line(aes(y = kghm3, group = notowanie.ind),colour=KGHM_col, alpha = .10)
  wykres <- wykres + geom_line(aes(y = kghm4, group = notowanie.ind),colour=KGHM_col, alpha = .10)
  
  wykres <- wykres + geom_line(aes(y = kghm_m, group = notowanie.ind),colour=KGHM_col, alpha = 1)
  wykres <- wykres + geom_line(aes(y = wig20_m, group = notowanie.ind),colour=WIG_col, alpha = 1)
  
  return( wykres )
}

rysuj.mi.tu.korelacje <- function(notowania1, notowania2, nazwa1, nazwa2) {
  zwroty1 <- (notowania1[2:length(notowania1)] - notowania1[1:length(notowania1)-1]) / notowania1[1:length(notowania1)-1]
  zwroty2 <- (notowania2[2:length(notowania2)] - notowania2[1:length(notowania2)-1]) / notowania2[1:length(notowania2)-1]
  
  l <- min(length(zwroty1), length(zwroty2))
  zwroty1 <- zwroty1[1:l]
  zwroty2 <- zwroty2[1:l]
  df <- data.frame(zwroty1, zwroty2)
  colnames(df)[1] <- nazwa1
  colnames(df)[2] <- nazwa2
  
  wykres <- ggplot(df, aes_string(x = nazwa1)) + geom_point(aes_string(y = nazwa2), colour="red", size = 5.0)
  
  return(wykres)
}


