N <- function(x) {
  return (pnorm(x))
}

d1 <- function(S,E,r,si,T) {
  licznik <- log(S/E)+T*(r+0.5*si^2)
  mianownik <- si*sqrt(T)
  return (licznik / mianownik)
}

d2 <- function(d1,si,T) {
  return (d1-si*sqrt(T))
}

VCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (S*N(d1)-E*exp(-r*T)*N(d2))
}

VPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (-S*N(-d1)+E*exp(-r*T)*N(-d2))
}

VBinCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (exp(-r*T)*N(d2))
}

VBinPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (exp(-r*T)*(1-N(d2)))
}

DCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  return (N(d1))
}

DPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  return (N(d1)-1)
}

DBinCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (exp(-r*T)*dnorm(d2)/(si*S*sqrt(T)))
}

DBinPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (-exp(-r*T)*dnorm(d2)/(si*S*sqrt(T)))
}

GCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  return (dnorm(d1)/(si*S*sqrt(T)))
}

GPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  return (dnorm(d1)/(si*S*sqrt(T)))
}

GBinCall <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (-exp(-r*T)*d1*dnorm(d2)/(si^2*S^2*T))
}

GBinPut <- function(S,E,r,si,T) {
  d1 <- d1(S,E,r,si,T)
  d2 <- d2(d1,si,T)
  return (exp(-r*T)*d1*dnorm(d2)/(si^2*S^2*T))
}