# zwraca trajektorie symulacji wig20 i kghm jednoczesnie
# t - długość symulacji (1 to jeden rok) [u nas w zadaniu 1]
# S_0 - wektor wartosci w chwili zero (pierwsze dla wig20, drugie dla kghm)
# mean - wektor srednich
# sd - wektor odchylen standardowych
# daty - wektor dat [253 w naszym przypadku]
# ile - ile symulacji [u nas 1000]

fun.symuluj.2dim <- function (t, S_0, mean, sd, corr, daty, ile) {

  skok <- t/length(daty)
  results1 <- data.frame(data = daty)
	results2 <- data.frame(data = daty)
  
  for(i in 1:ile) {
    
    temp1 <- c(S_0[1], rep(0, length(daty)-1))
    temp2 <- c(S_0[2], rep(0, length(daty)-1))
    
    for(j in 2:length(daty)){
			norm <- two_dim_norm(c(0,0), 1, 1, corr) ##TODO zmienic to bo brzydal
      
      temp1[j] <- temp1[j-1]*exp((mean[1] - 1/2*sd[1]^2)*skok + sd[1]*(norm[1]*sqrt(skok) - norm[1]))
      temp2[j] <- temp2[j-1]*exp((mean[2] - 1/2*sd[2]^2)*skok + sd[2]*(norm[2]*sqrt(skok) - norm[2]))
    }
    
    results1 <- cbind(results1, sym = temp1)
    results2 <- cbind(results2, sym = temp2)
  }
    
  results1[1,1:ile+1] <- S_0[1]
	results2[1,1:ile+1] <- S_0[2]
  
  return (list(results1,results2))
}

## wywolanie
mean_vec <- c(dane.mean.WIG20, dane.mean.KGHM)
sd_vec <- c(dane.sd.WIG20, dane.sd.KGHM)
S_0_vec <- c(dane.WIG20[1,5], dane.KGHM[1,5])
## TODO nie 4 symulacje a 1000 ;)
dane.symulacja.2dim <- fun.symuluj.2dim(1,
                                         S_0_vec,
                                         mean_vec,
                                         sd_vec,
                                         dane.korelacja,
                                         dane.przyszle.KGHM[,1],
                                         4)
