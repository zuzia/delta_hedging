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

fun.symuluj.1dim.1skok <- function (t, S_0, mean, sd, rnorm) {
  return (S_0*exp((mean -1/2*sd^2)*t + sd*rnorm*t))
}

fun.eval.loss.abstract <- function(s, strike, r, mean, sd, T, liczba.rehedg, pusta.zmienna) {
  
  skok <- T/(liczba.rehedg+1)
   
  for(i in 0:(liczba.rehedg)){
    
    new.delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, i*skok))
    
    if(i!=0){
      strata <- strata + (new.delta - delta)*s
    } else {
      strata <- 0
    }
    
    delta <- new.delta
    s <- fun.symuluj.1dim.1skok(skok,s, mean, sd, rnorm(1))
  }
  
  strata <- strata + (1-delta)*(s-strike)
  
  return (strata)
  
}

fun.eval.loss.reality <- function(s, strike, r, sd, T, liczba.rehedg, przyszle.dane, numer.wiersza) {

  skok <- T/(liczba.rehedg+1)
  result <- data.frame()
  
  for(i in 0:(liczba.rehedg)){
    
    new.delta <- pnorm(fun.eval.d1(s, strike, r, sd, T, i*skok))
    
    if(i!=0){
      strata <- strata + (new.delta - delta)*s
    } else {
      strata <- 0
    }
    
    delta <- new.delta
    
    result <- rbind(result, c(delta, strata, s))
    
    s <- przyszle.dane[floor((numer.wiersza-1)/(liczba.rehedg+1)*(i+2))]
  }
  strata <- strata + (1-delta)*(s-strike)
  
  result <- rbind(result, c(0, strata, s))
  colnames(result)[1] <- ("delta","strata","s")
  
  return(result)
}


#############################################################################################
# czesc abstrakcyjna
#############################################################################################

fun.apply.for.loss <- function(liczba.iteracji, liczba.rehedg) {
  re <- sapply(rep(1,liczba.iteracji),
               fun.eval.loss.abstract,
               s = dane.s0.WIG20,
               strike = 3000,
               r = param.r,
               mean = dane.mean.WIG20,
               sd = dane.sd.WIG20,
               T = param.T,
               liczba.rehedg = liczba.rehedg)
  
  #colnames(re) <- as.strings(liczba.rehedg)
  
}
dane.liczba.rehedg <- c(4,8)
dane.liczba.iteracji <- 1000

dane.part.A.abstract <- sapply(dane.liczba.rehedg,
                               fun.apply.for.loss,
                               liczba.iteracji = dane.liczba.iteracji)

#############################################################################################
# czesc rzeczywista
#############################################################################################
dane.part.A.reality <- sapply(c(4,10),
                              fun.eval.loss.reality,
                              s = dane.s0.WIG20,
                              strike = 3000,
                              r = param.r,
                              sd = dane.sd.WIG20,
                              T = param.T,
                              przyszle.dane = dane.przyszle.WIG20[,5],
                              numer.wiersza = 153)

dane.part.A.reality[,1]



#############################################################################################
# wykresy
#############################################################################################



rysuj.histogram(dane = dane.part.A.abstract[,1])


dane.part.A.abstract[1:10,]

