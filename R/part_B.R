source("R\\part_A.r") 
source("R\\Black-Scholes.r")
source("R\\main.r")

######################################################################
# Podejscie 0 - delta hedging
######################################################################
##### wykres delty #####
S <- seq(1000,4000,len=300)
d <- -DCall(2700,2300,param.r,dane.sd.WIG20,param.T)
plot(S,-DCall(S,2300,param.r,dane.sd.WIG20,param.T)-d, xlab="kurs",ylab="delta",type='l')

##### funkcja symyluj?ca portfel #####
delta.hedging.0 <- function(S0,r,sd,T,mean) {
  delta <- -DCall(S0,2300,r,sd,T)
  prowizje <- 0.004*(abs(delta*S0) + abs(VCall(S0,2300,r,sd,T)))
  inwestycja <- -delta*S0 - VCall(S0,2300,r,sd,T) + prowizje
  
  portfel <- data.frame(S=S0,delta=delta,inwestycja=inwestycja)
  S=S0
  for(i in 1:floor(T*252/30)) {
    t <- i*30/252
    S <- fun.symuluj.1dim.1skok(30/252,S,mean,sd,rnorm(1))
    delta_nowa <- -DCall(S,2300,r,sd,T-t)
    prowizje <- abs(delta-delta_nowa)*S*0.004
    inwestycja <- inwestycja*exp(r*30/252)
    inwestycja <- inwestycja - (delta_nowa-delta)*S
    inwestycja <- inwestycja + prowizje
    delta <- delta_nowa
    portfel <- rbind(portfel,c(S,delta,inwestycja))
  }
  
  skok <- T-floor(T*252/30)*30/252
  S <- fun.symuluj.1dim.1skok(skok,S,mean,sd,rnorm(1))
  
  inwestycja <- inwestycja*exp(r*skok)
  zwrot <- -VCall(S,2300,r,sd,0) - delta*S - inwestycja
  portfel <- rbind(portfel,c(S,-VCall(S,2300,r,sd,0),zwrot))
  return (zwrot)
}

##### wywo?anie pojedyncze oraz histogram #####
delta.hedging.0(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20)

zwroty <- c()
for(i in 1:1000) {
  zwroty <- c(zwroty,delta.hedging.0(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20))
}


zwroty <- rep()

rysuj.histogram(zwroty)
mean(zwroty)
sd(zwroty)


######################################################################
# Podejscie I - 1 opcja binarna
######################################################################
##### wykres delty #####
S <- seq(1000,4000,len=300)
K <- GCall(2700,2300,param.r,dane.sd.WIG20,param.T)/GBinPut(2700,2300,param.r,dane.sd.WIG20,param.T)
d <- K*DBinPut(2700,2300,param.r,dane.sd.WIG20,param.T)-DCall(2700,2300,param.r,dane.sd.WIG20,param.T)
plot(S,-DCall(S,2300,param.r,dane.sd.WIG20,param.T)+K*DBinPut(S,2300,param.r,dane.sd.WIG20,param.T)-d, xlab="kurs",ylab="delta",type='l')

##### funkcja symyluj?ca portfel #####
gamma.hedging.1 <- function(S0,r,sd,T,mean,E) {
  K <- GCall(S0,2300,r,sd,T)/GBinPut(S0,E,r,sd,T)
  delta <- -DCall(S0,2300,r,sd,T) + K*DBinPut(S0,E,r,sd,T)
  prowizje <- 0.04*(abs(delta*S0) + abs(K*VBinPut(S0,E,r,sd,T)) + abs(VCall(S0,2300,r,sd,T)))
  inwestycja <- -delta*S0 + K*VBinPut(S0,E,r,sd,T) - VCall(S0,2300,r,sd,T) #+ prowizje
  
  portfel <- data.frame(S=S0,delta=delta,K=K,inwestycja=inwestycja)
  S=S0
  for(i in 1:floor(T*252/5)) {
    t <- i*5/252
    S <- fun.symuluj.1dim.1skok(5/252,S,mean,sd,rnorm(1))
    K_nowe <- GCall(S,2300,r,sd,T-t)/GBinPut(S,E,r,sd,T-t)
    delta_nowa <- -DCall(S,2300,r,sd,T-t) + K*DBinPut(S,E,r,sd,T-t)
    prowizje <- abs(delta-delta_nowa)*S*0.04
    prowizje <- prowizje + abs(K_nowe-K)*VBinPut(S,E,r,sd,T-t)*0.04
    inwestycja <- inwestycja*exp(r*5/252)
    inwestycja <- inwestycja + (K_nowe-K)*VBinPut(S,E,r,sd,T-t)
    inwestycja <- inwestycja - (delta_nowa-delta)*S
    inwestycja <- inwestycja + prowizje
    K <- K_nowe
    delta <- delta_nowa
    portfel <- rbind(portfel,c(S,delta,K,inwestycja))
  }
  
  skok <- T-floor(T*252/5)*5/252
  S <- fun.symuluj.1dim.1skok(skok,S,mean,sd,rnorm(1))

  inwestycja <- inwestycja*exp(r*skok)
  zwrot <- -VCall(S,2300,r,sd,0) + K*VBinPut(S,E,r,sd,0) - delta*S - inwestycja
  portfel <- rbind(portfel,c(S,-VCall(S,2300,r,sd,0),K*VBinPut(S,E,r,sd,0),zwrot))
  return (zwrot)
}

##### wywo?anie pojedyncze oraz histogram #####
gamma.hedging.1(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20,2300)

zwroty <- c()
for(i in 1:10000) {
  zwroty <- c(zwroty,gamma.hedging.1(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20,2300))
}
zwroty <- sort(zwroty)
min(zwroty)
max(zwroty)
zwroty <- zwroty[1000:9000]
rysuj.histogram(zwroty)
mean(zwroty)
sd(zwroty)


######################################################################
# Podejscie II - 2 opcje binarne w takiej samej ilo?ci
######################################################################
##### wykres delty #####
K <- GCall(2700,2300,param.r,dane.sd.WIG20,param.T)/(GBinPut(2700,2300,param.r,dane.sd.WIG20,param.T)+GBinCall(2700,3100,param.r,dane.sd.WIG20,param.T))
d <- K*DBinCall(2700,3100,param.r,dane.sd.WIG20,param.T)+K*DBinPut(2700,2300,param.r,dane.sd.WIG20,param.T)-DCall(2700,2300,param.r,dane.sd.WIG20,param.T)
plot(S,-DCall(S,2300,param.r,dane.sd.WIG20,param.T)+K*DBinPut(S,2300,param.r,dane.sd.WIG20,param.T)+K*DBinCall(S,3100,param.r,dane.sd.WIG20,param.T)-d,xlab="kurs",ylab="delta", type='l')

##### funkcja symyluj?ca portfel #####
gamma.hedging.2 <- function(S0,r,sd,T,mean,E1,E2) {
  K <- GCall(S0,2300,r,sd,T)/(GBinPut(S0,E1,r,sd,T)+GBinCall(S0,E2,r,sd,T))
  delta <- -DCall(S0,2300,r,sd,T) + K*(DBinPut(S0,E1,r,sd,T)+DBinCall(S0,E2,r,sd,T))
  prowizje <- 0.04*(abs(delta*S0) + abs(K*VBinPut(S0,E1,r,sd,T)) + abs(K*VBinCall(S0,E2,r,sd,T)) + abs(VCall(S0,2300,r,sd,T)))
  inwestycja <- -delta*S0 + K*(VBinPut(S0,E1,r,sd,T)+VBinCall(S0,E2,r,sd,T)) - VCall(S0,2300,r,sd,T) + prowizje
  
  portfel <- data.frame(S=S0,delta=delta,K=K,inwestycja=inwestycja)
  S=S0
  for(i in 1:floor(T*252/5)) {
    t <- i*5/252
    S <- fun.symuluj.1dim.1skok(5/252,S,mean,sd,rnorm(1))
    K_nowe <- GCall(S,2300,r,sd,T-t)/(GBinPut(S,E1,r,sd,T-t)+GBinCall(S,E2,r,sd,T-t))
    delta_nowa <- -DCall(S,2300,r,sd,T-t) + K*(DBinPut(S,E1,r,sd,T-t)+DBinCall(S,E2,r,sd,T-t))
    prowizje <- abs(delta-delta_nowa)*S*0.04
    prowizje <- prowizje + abs(K_nowe-K)*(VBinPut(S,E1,r,sd,T-t)+VBinCall(S,E2,r,sd,T-t))*0.04
    inwestycja <- inwestycja*exp(r*5/252)
    inwestycja <- inwestycja + (K_nowe-K)*(VBinPut(S,E1,r,sd,T-t)+VBinCall(S,E2,r,sd,T-t))
    inwestycja <- inwestycja - (delta_nowa-delta)*S
    inwestycja <- inwestycja + prowizje
    K <- K_nowe
    delta <- delta_nowa
    portfel <- rbind(portfel,c(S,delta,K,inwestycja))
  }
  
  skok <- T-floor(T*252/5)*5/252
  S <- fun.symuluj.1dim.1skok(skok,S,mean,sd,rnorm(1))
  
  inwestycja <- inwestycja*exp(r*skok)
  zwrot <- -VCall(S,2300,r,sd,0) + K*(VBinPut(S,E1,r,sd,0)+VBinCall(S,E2,r,sd,0)) - delta*S - inwestycja
  portfel <- rbind(portfel,c(S,-VCall(S,2300,r,sd,0),0,zwrot))
  return (zwrot)
}

##### wywo?anie pojedyncze oraz histogram #####
gamma.hedging.2(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20,2300,3100)

zwroty <- c()
for(i in 1:10000) {
  zwroty <- c(zwroty,gamma.hedging.2(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20,2300,3100))
}
zwroty <- sort(zwroty)
min(zwroty)
max(zwroty)
zwroty <- zwroty[1000:9500]
rysuj.histogram(zwroty)
mean(zwroty)
sd(zwroty)
 

######################################################################
# Podejscie III - 2 opcje binarne w r??nej ilo?ci
######################################################################
##### wykres delty #####
p <- optymalizuj.parametry.3(2700,param.T,dane.sd.WIG20,param.r)
d <- p[[1]][1]*DBinPut(2700,2300,param.r,dane.sd.WIG20,param.T)+p[[1]][2]*DBinCall(2700,3100,param.r,dane.sd.WIG20,param.T)-DCall(2700,2300,param.r,dane.sd.WIG20,param.T)
plot(S,-DCall(S,2300,param.r,dane.sd.WIG20,param.T)+p[[1]][1]*DBinPut(S,2300,param.r,dane.sd.WIG20,param.T)+p[[1]][2]*DBinCall(S,3100,param.r,dane.sd.WIG20,param.T)-d,xlab="kurs",ylab="delta", type='l')

##### funkcje optymalizuj?ce #####

amplituda.delty.3 <- function(S,t,sd,r,K1,L1) {
  min <- optimize(delta.portfela.3, interval=c(S*0.9,S*1.1),t,sd,r,K1,L1, maximum=FALSE)
  max <- optimize(delta.portfela.3, interval=c(S*0.9,S*1.1),t,sd,r,K1,L1, maximum=TRUE)
  return(max[[2]]-min[[2]])
}

amplituda.delty.wektor.3 <- function(x,S,t,sd,r) {
  return (amplituda.delty.3(S,t,sd,r,x[1],x[2]))
}

optymalizuj.parametry.3 <- function(S,t,sd,r) {
  p <- optim(c(10,10),amplituda.delty.wektor.3,S=S,t=t,sd=sd,r=r)
  return (p)
}

##### portfel #####

wartosc.portfela.3 <- function(S,t,sd,r,K1,L1) {
  wynik = -VCall(S,2300,r,sd,t)
  wynik = wynik + K1*VBinPut(S,2300,r,sd,t)
  wynik = wynik + L1*VBinCall(S,3100,r,sd,t)
  return (wynik)
}

delta.portfela.3 <- function(S,t,sd,r,K1,L1) {
  wynik = -DCall(S,2300,r,sd,t)
  wynik = wynik + K1*DBinPut(S,2300,r,sd,t)
  wynik = wynik + L1*DBinCall(S,3100,r,sd,t)
  return (wynik)
}

##### funkcje symuluj?ce portfel #####

licz.prowizje.3 <- function(p,pp,S,r,sd,t) {
  wynik = abs(p[[1]][1]-pp[[1]][1])*VBinPut(S,2300,r,sd,t)
  wynik = wynik + abs(p[[1]][2]-pp[[1]][2])*VBinCall(S,3100,r,sd,t)
  return (0.004*wynik)
}

licz.inwestycje.opcje.3 <- function(p,pp,S,r,sd,t) {
  wynik <- (pp[[1]][1]-p[[1]][1])*VBinPut(S,2300,r,sd,t)
  wynik <- wynik + (pp[[1]][2]-p[[1]][2])*VBinCall(S,3100,r,sd,t)
  return (wynik)
}

gamma.hedging.3 <- function(S0,r,sd,T,mean) {
  
  p <- optymalizuj.parametry.3(S0,T,sd,r)
  wartosc.portfela <- wartosc.portfela.3(S0,T,sd,r,p[[1]][1],p[[1]][2])
  delta <- delta.portfela.3(S0,T,sd,r,p[[1]][1],p[[1]][2])
  prowizje <- licz.prowizje.3(data.frame(c(0,0),c(0,0)),p,S0,r,sd,T) + 0.004*abs(delta*S0)
  inwestycja <- wartosc.portfela - delta*S0 + prowizje
  
  S=S0
  for(i in 1:floor(T*252/30)) {
    t <- i*30/252
    S <- fun.symuluj.1dim.1skok(30/252,S,mean,sd,rnorm(1))
    pp <- optymalizuj.parametry.3(S,T-t,sd,r)
    delta2 <- delta.portfela.3(S,T-t,sd,r,pp[[1]][1],pp[[1]][2])
    prowizje <- licz.prowizje.3(p,pp,S,r,sd,T-t)
    prowizje <- prowizje + 0.004*(delta2-delta)*S
    inwestycja <- inwestycja*exp(r*30/252)
    inwestycja <- inwestycja - (delta2-delta)*S
    inwestycja <- inwestycja + licz.inwestycje.opcje.3(p,pp,S,r,sd,T-t)
    inwestycja <- inwestycja + prowizje
    delta = delta2   
    p <- pp
  }
  
  skok <- T-floor(T*252/30)*30/252
  S <- fun.symuluj.1dim.1skok(skok,S,mean,sd,rnorm(1))
  inwestycja <- inwestycja*exp(r*skok)
  zwrot <- wartosc.portfela.3(S,0,sd,r,p[[1]][1],p[[1]][2]) - inwestycja - delta*S
  return (zwrot)
}

##### wywo?anie pojedyncze oraz histogram #####

gamma.hedging.3(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20)

zwroty <- c()
for(i in 1:1000) {
  zwroty <- c(zwroty,gamma.hedging.3(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20))
  plot(i,i)
}
rysuj.histogram(zwroty)
mean(zwroty)
sd(zwroty)
min(zwroty)
max(zwroty)
aaa <- sort(zwroty)
aaa
######################################################################
# Podejscie IV - 10 opcji binarnych w r??nej ilo?ci
######################################################################
##### funkcje optymalizuj?ce #####

amplituda.delty.4 <- function(S,t,sd,r,K1,K2,K3,K4,K5,L1,L2,L3,L4,L5) {
  min <- optimize(delta.portfela.4, interval=c(S*0.9,S*1.1),t,sd,r,K1,K2,K3,K4,K5,L1,L2,L3,L4,L5, maximum=FALSE)
  max <- optimize(delta.portfela.4, interval=c(S*0.9,S*1.1),t,sd,r,K1,K2,K3,K4,K5,L1,L2,L3,L4,L5, maximum=TRUE)
  return(max[[2]]-min[[2]])
}

amplituda.delty.wektor.4 <- function(x,S,t,sd,r) {
  return (amplituda.delty.4(S,t,sd,r,x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10]))
}

optymalizuj.parametry.4 <- function(S,t,sd,r) {
  p <- optim(c(10,10,10,10,10,10,10,10,10,10),amplituda.delty.wektor.4,S=S,t=t,sd=sd,r=r)
  return (p)
}

##### portfel #####
 
wartosc.portfela.4 <- function(S,t,sd,r,K1,K2,K3,K4,K5,L1,L2,L3,L4,L5) {
  wynik = -VCall(S,2300,r,sd,t)
  wynik = wynik + K1*VBinPut(S,2300,r,sd,t)
  wynik = wynik + K2*VBinPut(S,2400,r,sd,t)
  wynik = wynik + K3*VBinPut(S,2500,r,sd,t)
  wynik = wynik + K4*VBinPut(S,2600,r,sd,t)
  wynik = wynik + K5*VBinPut(S,2700,r,sd,t)
  wynik = wynik + L1*VBinCall(S,2800,r,sd,t)
  wynik = wynik + L2*VBinCall(S,2900,r,sd,t)
  wynik = wynik + L3*VBinCall(S,3000,r,sd,t)
  wynik = wynik + L4*VBinCall(S,3100,r,sd,t)
  wynik = wynik + L5*VBinCall(S,3200,r,sd,t)
  return (wynik)
}

delta.portfela.4 <- function(S,t,sd,r,K1,K2,K3,K4,K5,L1,L2,L3,L4,L5) {
   wynik = -DCall(S,2300,r,sd,t)
   wynik = wynik + K1*DBinPut(S,2300,r,sd,t)
   wynik = wynik + K2*DBinPut(S,2400,r,sd,t)
   wynik = wynik + K3*DBinPut(S,2500,r,sd,t)
   wynik = wynik + K4*DBinPut(S,2600,r,sd,t)
   wynik = wynik + K5*DBinPut(S,2700,r,sd,t)
   wynik = wynik + L1*DBinCall(S,2800,r,sd,t)
   wynik = wynik + L2*DBinCall(S,2900,r,sd,t)
   wynik = wynik + L3*DBinCall(S,3000,r,sd,t)
   wynik = wynik + L4*DBinCall(S,3100,r,sd,t)
   wynik = wynik + L5*DBinCall(S,3200,r,sd,t)
   return (wynik)
}
 
##### funkcja symuluj?ca portfel #####

licz.prowizje.4 <- function(p,pp,S,r,sd,t) {
  wynik = abs(p[[1]][1]-pp[[1]][1])*VBinPut(S,2300,r,sd,t)
  wynik = wynik + abs(p[[1]][2]-pp[[1]][2])*VBinPut(S,2400,r,sd,t)
  wynik = wynik + abs(p[[1]][3]-pp[[1]][3])*VBinPut(S,2500,r,sd,t)
  wynik = wynik + abs(p[[1]][4]-pp[[1]][4])*VBinPut(S,2600,r,sd,t)
  wynik = wynik + abs(p[[1]][5]-pp[[1]][5])*VBinPut(S,2700,r,sd,t)
  wynik = wynik + abs(p[[1]][6]-pp[[1]][6])*VBinCall(S,2800,r,sd,t)
  wynik = wynik + abs(p[[1]][7]-pp[[1]][7])*VBinCall(S,2900,r,sd,t)
  wynik = wynik + abs(p[[1]][8]-pp[[1]][8])*VBinCall(S,3000,r,sd,t)
  wynik = wynik + abs(p[[1]][9]-pp[[1]][8])*VBinCall(S,3100,r,sd,t)
  wynik = wynik + abs(p[[1]][10]-pp[[1]][10])*VBinCall(S,3200,r,sd,t)
  return (0.04*wynik)
}

licz.inwestycje.opcje.4 <- function(p,pp,S,r,sd,t) {
  wynik <- (pp[[1]][1]-p[[1]][1])*VBinPut(S,2300,r,sd,t)
  wynik <- wynik + (pp[[1]][2]-p[[1]][2])*VBinPut(S,2400,r,sd,t)
  wynik <- wynik + (pp[[1]][3]-p[[1]][3])*VBinPut(S,2500,r,sd,t)
  wynik <- wynik + (pp[[1]][4]-p[[1]][4])*VBinPut(S,2600,r,sd,t)
  wynik <- wynik + (pp[[1]][5]-p[[1]][5])*VBinPut(S,2700,r,sd,t)
  wynik <- wynik + (pp[[1]][6]-p[[1]][6])*VBinCall(S,2800,r,sd,t)
  wynik <- wynik + (pp[[1]][7]-p[[1]][7])*VBinCall(S,2900,r,sd,t)
  wynik <- wynik + (pp[[1]][8]-p[[1]][8])*VBinCall(S,3000,r,sd,t)
  wynik <- wynik + (pp[[1]][9]-p[[1]][9])*VBinCall(S,3100,r,sd,t)
  wynik <- wynik + (pp[[1]][10]-p[[1]][10])*VBinCall(S,3200,r,sd,t)
  return (wynik)
}

gamma.hedging.4 <- function(S0,r,sd,T,mean) {
  
  p <- optymalizuj.parametry.4(S0,T,sd,r)
  wartosc.portfela <- wartosc.portfela.4(S0,T,sd,r,p[[1]][1],p[[1]][2],p[[1]][3],p[[1]][4],p[[1]][5],p[[1]][6],p[[1]][7],p[[1]][8],p[[1]][9],p[[1]][10])
  delta <- delta.portfela.4(S0,T,sd,r,p[[1]][1],p[[1]][2],p[[1]][3],p[[1]][4],p[[1]][5],p[[1]][6],p[[1]][7],p[[1]][8],p[[1]][9],p[[1]][10])
  prowizje <- licz.prowizje.4(data.frame(c(0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0)),p,S0,r,sd,T) + 0.04*abs(delta*S0)
  inwestycja <- wartosc.portfela -delta*S0 + prowizje
  
  S=S0
  for(i in 1:floor(T*252/5)) {
    t <- i*5/252
    S <- fun.symuluj.1dim.1skok(5/252,S,mean,sd,rnorm(1))
    pp <- optymalizuj.parametry.4(S,T-t,sd,r)
    delta2 <- delta.portfela.4(S,T-t,sd,r,pp[[1]][1],pp[[1]][2],pp[[1]][3],pp[[1]][4],pp[[1]][5],pp[[1]][6],pp[[1]][7],pp[[1]][8],pp[[1]][9],pp[[1]][10])
    prowizje <- licz.prowizje.4(p,pp,S,r,sd,T-t)
    prowizje <- prowizje + 0.04*(delta2-delta)*S
    inwestycja <- inwestycja*exp(r*5/252)
    inwestycja <- inwestycja - (delta2-delta)*S
    inwestycja <- inwestycja + licz.inwestycje.opcje.4(p,pp,S,r,sd,T-t)
    inwestycja <- inwestycja + prowizje
    delta = delta2   
    p <- pp
  }
  
  skok <- T-floor(T*252/5)*5/252
  S <- fun.symuluj.1dim.1skok(skok,S,mean,sd,rnorm(1))
  inwestycja <- inwestycja*exp(r*skok)
  zwrot <- wartosc.portfela.4(S,0,sd,r,p[[1]][1],p[[1]][2],p[[1]][3],p[[1]][4],p[[1]][5],p[[1]][6],p[[1]][7],p[[1]][8],p[[1]][9],p[[1]][10]) - inwestycja - delta*S
  return (zwrot)
}

gamma.hedging.4(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20)
 
zwroty <- c()
for(i in 1:200) {
  zwroty <- c(zwroty,gamma.hedging.4(dane.s0.WIG20,param.r,dane.sd.WIG20,param.T,dane.mean.WIG20))
  plot(i,i)
}
rysuj.histogram(zwroty)
hist(zwroty)
 