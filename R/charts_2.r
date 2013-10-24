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
