source("R\\main.r")  

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
  
  return(result)
}




sapply(1:1000,
       fun.eval.loss.abstract,
       s = dane.s0.WIG20,
       strike = 3000,
       r = param.r,
       mean = dane.mean.WIG20,
       sd = dane.sd.WIG20,
       T = param.T,
       liczba.rehedg = 10)


fun.eval.loss.reality(s = dane.s0.WIG20,
                      strike = 3000,
                      r = param.r,
                      sd = dane.sd.WIG20,
                      T = param.T,
                      liczba.rehedg = 151,
                      przyszle.dane = dane.przyszle.WIG20[,5],
                      numer.wiersza = 153)



# rysuj.histogram <- function(dane, szerokosc.slupka, kolor.niski, kolor.wysoki, kolor.faktyczny, kolor.sigma, notowanie.faktyczne, szerokosc.faktyczne = 0.05, nazwa)
# {
#   wLewo <- 1 - szerokosc.faktyczne
#   wPrawo <- 1 + szerokosc.faktyczne
#   
#   if(notowanie.faktyczne == 0)
#   {
#     notowanie.faktyczne <- 0.001
#   }
#   
#   wykres <- ggplot(dane, aes(x = payoff)) +
#     xlab("wyplata") +
#     ylab("liczba") +
#     geom_rect(aes_string(xmin = mi - sigma, xmax = mi + sigma, ymin = 0, ymax = Inf), fill = kolor.sigma, alpha = .01) +
#     #geom_vline(xintercept = mi, size = 2, colour = kolor.sigma, alpha = .3) +
#     geom_rect(aes_string(xmin = mi*0.98 , xmax = mi*1.02, ymin = 0, ymax = Inf), fill = kolor.sigma) +
#     geom_histogram(binwidth = szerokosc.slupka, aes(fill = ..count..)) +
#     scale_fill_gradient("", low = kolor.niski, high = kolor.wysoki) +
#     geom_rect(aes_string(xmin = notowanie.faktyczne*wLewo , xmax = notowanie.faktyczne*wPrawo, ymin = 0, ymax = Inf), fill = kolor.faktyczny) +
#     theme(legend.position = "none") 
#   #geom_vline(xintercept = notowanie.faktyczne, colour = kolor.faktyczny, size = 2)  
#   
#   
#   return( wykres )
# }
# 
# 
# histogram<- rysuj.histogram(dane 0.1, "#FFBAFF", "#E1017B", "#A0F401", "#4C78E5", zysk.faktyczny, 0.01, "wykres")
# 



