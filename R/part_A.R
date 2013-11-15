#############################################################################################
# ustawienia
#############################################################################################
source("R\\main.r")  


#############################################################################################
# funkcje
#############################################################################################
fun.eval.d1 <- function(s0, strike, r, sd, T, t) {
  
  return ((log(s0/strike) + (r+sd/2)*(T-t))/(sqrt(sd*(T-t))))
}

fun.eval.d2 <- function(s0, strike, r, sd, T, t) {
  
  return ((log(s0/strike) + (r-sd/2)*(T-t))/(sqrt(sd*(T-t))))
}

fun.symuluj.1dim.1skok <- function(t, S_0, mean, sd, rnorm) {
  return (S_0*exp((mean -1/2*sd^2)*t + sd*rnorm*t))
}

fun.eval.option <- function(s, r, T, t, sd, strike) {
  
  d1 <- fun.eval.d1(s, strike, r, sd, T, t)
  d2 <- fun.eval.d2(s, strike, r, sd, T, t)
  
  return (s*pnorm(d1) - strike*exp(-r*(T-t))*pnorm(d2))
}

fun.eval.loss.abstract <- function(s, strike, r, mean, sd, T, liczba.rehedg, pusta.zmienna) {
  
  option <- fun.eval.option(s, r, T, 0, sd, strike)
  delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, 0))
  norisk <- option-s*delta
  portfel <- data.frame(s = s,delta = delta0, norisk = norisk)
  
  skok <- T/(liczba.rehedg+1)
  
  for(i in 1:(liczba.rehedg)){
    
    s <- fun.symuluj.1dim.1skok(skok, s, mean, sd, rnorm(1))
    new.delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, i*skok))
    norisk <- norisk*exp(r*skok) - (new.delta-delta)*s
    delta <- new.delta
    portfel <- rbind(portfel, c(s, delta, norisk))
    
  }
  
  s <- fun.symuluj.1dim.1skok(skok, s, mean, sd, rnorm(1))
  strata <- (delta*s + norisk*exp(r*skok) - max((s-strike),0))/option
  
  return (strata)
  
}

fun.eval.loss.reality <- function(s, strike, r, sd, T, liczba.rehedg, przyszle.dane, numer.wiersza) {

  option <- fun.eval.option(s, r, T, 0, sd, strike)
  delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, 0))
  norisk <- option-s*delta
  portfel <- data.frame(s = s,delta = delta0, norisk = norisk, value = option)

  skok <- T/(liczba.rehedg+1)
  
  for(i in 1:(liczba.rehedg)){
    
    s <- przyszle.dane[floor((numer.wiersza-1)/(liczba.rehedg+1)*(i+2))]
    value <- delta*s + norisk*exp(r*skok)
    new.delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, i*skok))
    norisk <- norisk*exp(r*skok) - (new.delta-delta)*s
    delta <- new.delta
    portfel <- rbind(portfel, c(s, delta, norisk, value))
    
  }
  
  s <- przyszle.dane[numer.wiersza]
  
  value <- delta*s + norisk*exp(r*skok) - max((s-strike),0)
  portfel <- rbind(portfel, c(s, 0, 0, value))

  return(portfel)
}


#############################################################################################
# czesc abstrakcyjna
#############################################################################################

fun.apply.for.loss <- function(liczba.iteracji, liczba.rehedg) {
  re <- sapply(rep(1,liczba.iteracji),
               fun.eval.loss.abstract,
               s = dane.s0.WIG20,
               strike = 2300,
               r = param.r,
               mean = dane.mean.WIG20,
               sd = dane.sd.WIG20,
               T = param.T,
               liczba.rehedg = liczba.rehedg)
  
  #colnames(re) <- as.strings(liczba.rehedg)
  
}
dane.liczba.rehedg <- c(1, 4, 12)
dane.liczba.iteracji <- 1000

dane.part.A.abstract <- sapply(dane.liczba.rehedg,
                               fun.apply.for.loss,
                               liczba.iteracji = dane.liczba.iteracji)

mean(dane.part.A.abstract[,1])
mean(dane.part.A.abstract[,2])
mean(dane.part.A.abstract[,3])

#############################################################################################
# czesc rzeczywista
#############################################################################################
dane.part.A.reality <- lapply(c(4,10),
                              fun.eval.loss.reality,
                              s = dane.s0.WIG20,
                              strike = 3000,
                              r = param.r,
                              sd = dane.sd.WIG20,
                              T = param.T,
                              przyszle.dane = dane.przyszle.WIG20[,5],
                              numer.wiersza = 153)

dane.part.A.reality[1]



#############################################################################################
# użycie wykresów
#############################################################################################

rysuj.histogram(dane = dane.part.A.abstract[,2])

rysuj.kwantyle.straty(dane.part.A.abstract, circle.alpha = .01, circle.size = 7)



