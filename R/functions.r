
# zwraca ramke danych zwrotow, przyjmuje daty w formacie "YYYY-MM-DD"

fun.policz.zwroty <- function ( dane.ceny , data.poczatkowa , data.koncowa)
{

    p1 = which( dane.ceny$data == data.poczatkowa)
    p2 = which( dane.ceny$data == data.koncowa)

    zwroty <- data.frame (  (dane.ceny[(p1+1):p2,2:length(dane.ceny)] - dane.ceny[p1:(p2-1),2:length(dane.ceny)]) / dane.ceny[p1:(p2-1),2:length(dane.ceny)] )                                                   
    
    return(zwroty)                                      
}


# zwraca trajektorie symulacji
# t - długość symulacji (1 to jeden rok) [u nas w zadaniu 1]
# S_0 - wartość w chwili zero
# mean - srednia
# sd - odchylenie standardowe
# daty - wektor dat [253 w naszym przypadku]
# ile - ile symulacji [u nas 1000]

fun.symuluj.1dim <- function (t, S_0, mean, sd, daty, ile) {

  skok <- t/length(daty)
  results <- data.frame(data = daty)
  
  for(i in 1:ile) {
    
    temp <- c(S_0, rep(0, length(daty)-1))
    
    for(j in 2:length(daty)){
      unif1 <- runif(1)
      unif2 <- runif(1)
      norm  <- as.vector(mapply(function(u1,u2) {sqrt(-2*log(u1)) * c(cos(2*pi*u2), sin(2*pi*u2))}, unif1, unif2))
      
      temp[j] <- temp[j-1]*exp((mean - 1/2*sd^2)*skok + sd*(norm[2]*sqrt(skok)))
    }
    
    results <- cbind(results, temp)
  }
    
  results[1,1:ile+1] <- S_0
  
  return (results)
}



# zwraca trajektorie symulacji wig20 i kghm jednoczesnie
# t - długość symulacji (1 to jeden rok) [u nas w zadaniu 1]
# S_0 - wektor wartosci w chwili zero (pierwsze dla wig20, drugie dla kghm)
# mean - wektor srednich
# sd - wektor odchylen standardowych
# daty - wektor dat [253 w naszym przypadku]
# ile - ile symulacji [u nas 1000]

fun.symuluj.2dim <- function (t, S_0, mean, sd, corr, daty, ile) {
  ##tak naprawde mean to nie srednia, a dryf, sd - zmiennosc, w main sa przemnozone 
  skok <- t/length(daty)
  results1 <- data.frame(data = daty)
  results2 <- data.frame(data = daty)
  
  for(i in 1:ile) {
    
    temp1 <- c(S_0[1], rep(0, length(daty)-1))
    temp2 <- c(S_0[2], rep(0, length(daty)-1))
    
    for(j in 2:length(daty)){
      norm <- two_dim_norm(c(0,0), 1, 1, corr) ##TODO zmienic to bo brzydal
      
      temp1[j] <- temp1[j-1]*exp((mean[1] - 1/2*sd[1]^2)*skok + sd[1]*(norm[1]*sqrt(skok)))
      temp2[j] <- temp2[j-1]*exp((mean[2] - 1/2*sd[2]^2)*skok + sd[2]*(norm[2]*sqrt(skok)))
    }
    
    results1 <- cbind(results1, sym = temp1)
    results2 <- cbind(results2, sym = temp2)
  }
  
  results1[1,1:ile+1] <- S_0[1]
  results2[1,1:ile+1] <- S_0[2]
  
  return (list(results1,results2))
}
cholesky <- function(sd1, sd2, corr){
  covariance <- sd1*sd2*corr
  tmp <- matrix(data=c(sd1^2, covariance, covariance, sd2^2), nrow=2, ncol=2)
  a <- t(chol(tmp))
  return(a)
}

two_dim_norm <- function(mean_vector, sd1, sd2, corr){
  chol_a <- cholesky(sd1, sd2, corr)
  ##	norm_vector <- rnorm(2)
  unif1 <- runif(1)
  unif2 <- runif(1)
  norm_vector  <- as.vector(mapply(function(u1,u2) {sqrt(-2*log(u1)) * c(cos(2*pi*u2), sin(2*pi*u2))}, unif1, unif2))
  solution <- mean_vector + chol_a %*% norm_vector
  return(solution)
}


#### zapisuje wykres ####
zapisz.wykres <- function(x,h=9,w=18,d=100)
{
  ggsave(plot=x, file=sprintf("./images/%s/%s.png",param.string,deparse(substitute(x))),height=h,width=w,dpi=d)
}


#### funkcja do liczenia payoffu ####
payoff <- function(notowania, typ, strike)
{
  # typ 0 to call
  # typ 1 to put
  
  if(typ == 0) 
    return( pmax(notowania-strike, 0))
  if(typ == 1) 
    return( pmax(strike-notowania, 0) ) 
}




