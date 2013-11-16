############ obliczenie d1 ###########
#####################################
fun.eval.d1 <- function(s0, strike, r, sd, T, t) {
  
  return ((log(s0/strike) + (r+sd^2/2)*(T-t))/(sd*sqrt(T-t)))
}

############ obliczenie d2 ###########
#####################################
fun.eval.d2 <- function(s0, strike, r, sd, T, t) {
  
  return ((log(s0/strike) + (r-sd^2/2)*(T-t))/(sd*sqrt(T-t)))
}

############ symulacja jednego skoku ###########
###############################################
# np. jezeli t = 0.5 to symuluje pol roku do przodu
fun.symuluj.1dim.1skok <- function(t, S_0, mean, sd, rnorm) {
  return (S_0*exp((mean -1/2*sd^2)*t + sd*rnorm*sqrt(t)))
}

############ wycena opcji ###########
####################################
# typ: 0 - call, 1 - put
fun.eval.option <- function(typ, s, r, T, t, sd, strike) {
  
  d1 <- fun.eval.d1(s, strike, r, sd, T, t)
  d2 <- fun.eval.d2(s, strike, r, sd, T, t)
  
  if(typ == 0) 
    return( s*pnorm(d1) - strike*exp(-r*(T-t))*pnorm(d2) )
  else if(typ == 1) 
    return( -s*pnorm(-d1) + strike*exp(-r*(T-t))*pnorm(-d2) ) 

}

############ obliczenie delty ###########
########################################
# typ: 0 - call, 1 - put
fun.eval.delta <- function(typ, s, r, T, t, sd, strike) {
  if(typ == 0)
    return( pnorm(fun.eval.d1(s, strike, r, sd, T, t)))
  else if(typ == 1)
    return( pnorm(fun.eval.d1(s, strike, r, sd, T, t)) - 1 )
}

############ policzenie loss/profit ###########
##############################################
# typ1: 0 - call, 1 - put
# typ2: 0 - abstract, 1 - reality
# jezeli typ2 = 0 to wtedy w zmiennej 'typ2.param' przekazujemy 'mean'
# jezeli typ2 = 1 to wtedy w zmiennej 'typ2.param' przekazujemy 'przyszle.dane'
fun.eval.loss <- function(typ1, typ2, s, strike, r, sd, T, liczba.rehedg, typ2.param) {
  
  if(typ2 == 0) 
    mean <- typ2.param
  else if(typ2 == 1)
    przyszle.dane <- typ2.param
    
  option <- fun.eval.option(typ1, s, r, T, 0, sd, strike)
  delta <- fun.eval.delta(typ1, s, r, T, 0, sd, strike)
  norisk <- option-s*delta
  data <- data.frame(s = s, delta = delta, norisk = norisk, option = option, portfel = delta*s + norisk - option)
  
  skok <- T/(liczba.rehedg+1)
  
  for(i in 1:(liczba.rehedg)){
    
    if(typ2 == 0)
      s <- fun.symuluj.1dim.1skok(skok, s, mean, sd, rnorm(1))
    else if(typ2 == 1)
      s <- przyszle.dane[floor((param.numer.wiersza-1)/(liczba.rehedg+1)*(i+2))]
    
    option <- fun.eval.option(typ1, s, r, T, i*skok, sd, strike)
    new.delta <- fun.eval.delta(typ1, s, r, T, i*skok, sd, strike)
    norisk <- norisk*exp(r*skok) - (new.delta-delta)*s
    delta <- new.delta
    data <- rbind(data, c(s, delta, norisk, option, portfel = delta*s + norisk - option))
    
  }
  
  if(typ2 == 0)
    s <- fun.symuluj.1dim.1skok(skok, s, mean, sd, rnorm(1))
  else if(typ2 == 1)
    s <- przyszle.dane[param.numer.wiersza]
  norisk <- norisk*exp(r*skok)
  option <- payoff(s, typ1, strike)
  
  strata <- delta*s + norisk - option
  
  data <- rbind(data, c(s, delta, norisk, option, strata))
  colnames(data) <- c("s", "delta", "norisk", "option", "portfel")
  
  return (data)
  
}

############ wyciagniecie koncowej straty z 'fun.eval.loss' iles razy ###########
################################################################################
# pusta.zmienna: wywołanie 'fun.eval.loss tyle razy ile wynosi dlugosc 'pusta.zmienna'
fun.eval.loss.simple <- function(typ1, typ2, s, strike, r, sd, T, liczba.rehedg, typ2.param, pusta.zmienna) {
  return(fun.eval.loss(typ1, typ2, s, strike, r, sd, T, liczba.rehedg, typ2.param)[(liczba.rehedg+2),5])
}

############ zastosowanie 'sapply' na 'fun.eval.loss.simple' dla liczby iteracji ###########
###########################################################################################
fun.eval.loss.preprocess1 <- function(liczba.iteracji, liczba.rehedg, typ1, s, strike, r, sd, T, mean) {
  return ( sapply(rep(1,liczba.iteracji),
                  fun.eval.loss.simple,
                  typ1 = typ1,
                  typ2 = 0,
                  s = s,
                  strike = strike,
                  r = r,
                  sd = sd,
                  T = T,
                  liczba.rehedg = liczba.rehedg,
                  typ2.param = mean) )
}

############ A.ABSTRACT czyli zastosowanie 'sapply' na 'fun.eval.loss.preprocess1' dla liczby reheegingow ###########
####################################################################################################################
fun.eval.loss.abstract <- function(liczba.iteracji, liczba.rehedg, typ1, strike) {
  return ( sapply(liczba.rehedg,
                  fun.eval.loss.preprocess1,
                  liczba.iteracji = liczba.iteracji,
                  typ1 = typ1,
                  strike = strike,                  
                  s = dane.s0.WIG20,
                  r = param.r,
                  sd = dane.sd.WIG20,
                  T = param.T,
                  mean = dane.mean.WIG20) )
}

############ A.REALITY czyli zastosowanie 'lapply' na 'fun.eval' dla liczby reheegingow ###########
###################################################################################################
fun.eval.loss.reality <- function(liczba.rehedg, typ1, strike) {
  return ( lapply(liczba.rehedg,
                  fun.eval.loss,
                  typ1 = typ1,
                  strike = strike,
                  typ2 = 1,
                  s = dane.s0.WIG20,
                  r = param.r,
                  sd = dane.sd.WIG20,
                  T = 1,                   
                  typ2.param = dane.przyszle.WIG20[,5]) )
}