## taki tam funkcyjny smaczek, funkcja sluzaca do skladania funkcji
##przyklad:
#f <- function(x) 2*x
#g <- function(x) 3*x
#> composite(f,g)(3)
#[1] 18
#> h <- function(x) composite(f,g)(x)
#> h(3)
#[1] 18
## fajne nie?
composite<-function(f,g) function(...) f(g(...))

## funkcja generujaca wektor zmiennych, bedacych standardowym procesem Wienera
## T - przedzial czasu
## n - ile krokow wykonujemy
generate_Wiener <- function(T, n){
  step <- T/n
  result <- sqrt(step) * rnorm(n)
  result <- cumsum(result)
  result <- c(0, result)
  return(result)
}
## rozwiazanie geometrycznego ruchu browna
expl_solution <- function(s_0, lambda, mi, brown_motion, T, n){
  s_0*exp((lambda-0.5*mi^2)*seq(0,T,T/n)+mi*brown_motion)
}
##generate_Wiener(1, 1000)

##dX(t) = f(t,X(t))dt + g(t,X(t))dW(t)
##X(0)=X_0
## teraz uwaga programowanie funkcyjne, sweet
## nasza cudna i fufasna funkcja Euler Maruyama bierze dwie inne funkcje jako argumenty
## zgodnie z rownaniem powyzej
## T - przedzial czasu
## n - liczba krokow
##TODO mozna troche inaczej, ale teraz sie tak nie bawie
Euler_Maruyama <- function(T, n, f, g, X_0, brown_motion, s_vec){
  step <- T/n
  X_old <- X_0
  Xs <- rep(0,n)
  for(i in 1:n){
    Xs[i] <- X_old + step*f(s_vec[i]) + g(s_vec[i])*(brown_motion[i+1] - brown_motion[i])
    X_old <- Xs[i]
  }
  return(c(X_0, Xs))
}

## funkcja pomocnicza, bierze 4 funkcje, zwraca 2 ;)
## f i g sa funkcjami z rownania na S
## dv i d2v sa odpowiednio piewsza(dV/dS) i druga (d^2V/dS^2) pochodna ze wzoru Ito
ito_formula <- function(f, g, dv,d2v){
  f_dt <- function(x) f(x)*dv(x) + 0.5*(g(x)^2)*d2v(x) ## funkcja stojaca przy dt
  f_dw <- function(x) g(x)*dv(x) ## funkcja stojaca przy dW
  return(list(f_dt, f_dw))
}
## maly tescik
f <- function(x)2*x;
g <- function(x)x;
brown_motion <- generate_Wiener(1,1000)
X_0 = 1
test <- Euler_Maruyama(1, 1000, f, g, X_0, brown_motion)
plot(test, type="l")
explicit_solution <- expl_solution(1,2,1,brown_motion, 1, 1000)
lines(explicit_solution, type = "l", col = "red")
## dziala

##dS(t) = lambda*S(t)dt + mi*S(t)dW (t)
lambda <- 3
mi <- 2
S_f <- function(s) lambda*s;
S_g <- function(s) mi*s;
S_0 <- 1
b_m <- generate_Wiener(1,1000)

##TODO naklepane na szybko, jak bedzie czas to to zmienie ;)
## To nizej brzydko jest, ale i tak musze wpisac te hermity recznie, poniewaz nie chce mi sie bawic w obliczenia symboliczne w R
## to samo sie tyczy pochodnych
## Hermit 1
dv1 <- function(x)2;
d2v1 <- function(x)0;
fun_list1 <- ito_formula(S_f, S_g, dv1, d2v1)
f_dt1 <- fun_list1[[1]]
f_dw1 <- fun_list1[[2]] ## hyhyhy dziala, tylko ze te funkcje operuja w jezyku s, nie v


#f1 <- function(x)3*x;
#g1 <- function(x)2*x;
#hermit1 <- Euler_Maruyama(1, 1000, f1, g1, 1, b_m)
hermit1 <- Euler_Maruyama(1, 1000, f_dt1, f_dw1, 1, b_m, sol)
sol <- expl_solution(1,3,2,b_m, 1, 1000)
plot(hermit1, type="l")
lines(sol*2, type = "l", col = "red")

## Hermit 2
#f2 <- function(x)10*x+10;
#g2 <- function(x)4*x+8;
dv2 <- function(x)8*x;
d2v2 <- function(x)8;
fun_list2 <- ito_formula(S_f, S_g, dv2, d2v2)
f_dt2 <- fun_list2[[1]]
f_dw2 <- fun_list2[[2]]

hermit2 <- Euler_Maruyama(1, 1000, f_dt2, f_dw2, 1, b_m, sol)
plot(hermit2, type="l")
lines(4*(sol^2)-2, type = "l", col = "red")

##Hermit 3
#f3 <- function(x)36*x+360 +156*sqrt(x+6);
#g3 <- function(x)8*x+48 + 24*sqrt(x+6);
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm3 <- expression(8*x^3 - 12*x)
a <- D(herm3, 'x')
a
D(a,'x')
##
dv3 <- function(x)8 * (3 * x^2) - 12;
d2v3 <- function(x)8 * (3 * (2 * x));
fun_list3 <- ito_formula(S_f, S_g, dv3, d2v3)
f_dt3 <- fun_list3[[1]]
f_dw3 <- fun_list3[[2]]
hermit3 <- Euler_Maruyama(1, 1000, f_dt3, f_dw3, 1, b_m, sol)
plot(hermit3, type="l")
lines((8*sol^3)-12*sol, type = "l", col = "red")

## Hermit 4
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm4 <- expression(16*x^4-48*x^2+12)
a <- D(herm4, 'x')
a
D(a,'x')
##
dv4 <- function(x)16 * (4 * x^3) - 48 * (2 * x);
d2v4 <- function(x)16 * (4 * (3 * x^2)) - 48 * 2;
fun_list4 <- ito_formula(S_f, S_g, dv4, d2v4)
f_dt4 <- fun_list4[[1]]
f_dw4 <- fun_list4[[2]]
hermit4 <- Euler_Maruyama(1, 1000, f_dt4, f_dw4, 1, b_m, sol)
plot(hermit4, type="l")
lines(16*sol^4-48*sol^2+12, type = "l", col = "red")

## Hermit 5
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm5 <- expression(32*x^5-160*x^3+120*x)
a <- D(herm5, 'x')
a
D(a,'x')
##
dv5 <- function(x)32 * (5 * x^4) - 160 * (3 * x^2) + 120;
d2v5 <- function(x)32 * (5 * (4 * x^3)) - 160 * (3 * (2 * x));
fun_list5 <- ito_formula(S_f, S_g, dv5, d2v5)
f_dt5 <- fun_list5[[1]]
f_dw5 <- fun_list5[[2]]
hermit5 <- Euler_Maruyama(1, 1000, f_dt5, f_dw5, 1, b_m, sol)
plot(hermit5, type="l")
lines(32*sol^5-160*sol^3+120*sol, type = "l", col = "red")

## Hermit 6
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm6 <- expression(64*x^6-480*x^4+720*x^2-120)
a <- D(herm6, 'x')
a
D(a,'x')
##
dv6 <- function(x)64 * (6 * x^5) - 480 * (4 * x^3) + 720 * (2 * x);
d2v6 <- function(x)64 * (6 * (5 * x^4)) - 480 * (4 * (3 * x^2)) + 720 * 2;
fun_list6 <- ito_formula(S_f, S_g, dv6, d2v6)
f_dt6 <- fun_list6[[1]]
f_dw6 <- fun_list6[[2]]
hermit6 <- Euler_Maruyama(1, 1000, f_dt6, f_dw6, 1, b_m, sol)
plot(hermit6, type="l")
lines(64*sol^6-480*sol^4+720*sol^2-120, type = "l", col = "red")

## Hermit 7
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm7 <- expression(128*x^7-1344*x^5+3360*x^3-1680*x)
a <- D(herm7, 'x')
a
D(a,'x')
##
dv7 <- function(x)128 * (7 * x^6) - 1344 * (5 * x^4) + 3360 * (3 * x^2) - 1680;
d2v7 <- function(x)128 * (7 * (6 * x^5)) - 1344 * (5 * (4 * x^3)) + 3360 * (3 * (2 * x));
fun_list7 <- ito_formula(S_f, S_g, dv7, d2v7)
f_dt7 <- fun_list7[[1]]
f_dw7 <- fun_list7[[2]]
hermit7 <- Euler_Maruyama(1, 1000, f_dt7, f_dw7, 1, b_m, sol)
plot(hermit7, type="l")
lines(128*sol^7-1344*sol^5+3360*sol^3-1680*sol, type = "l", col = "red")

## Hermit 8
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm8 <- expression(265*x^8-3584*x^6+13440*x^4-13440*x^2+1680)
a <- D(herm8, 'x')
a
D(a,'x')
##
dv8 <- function(x)265 * (8 * x^7) - 3584 * (6 * x^5) + 13440 * (4 * x^3) - 13440 * (2 * x);
d2v8 <- function(x)265 * (8 * (7 * x^6)) - 3584 * (6 * (5 * x^4)) + 13440 * (4 *(3 * x^2)) - 13440 * 2;
fun_list8 <- ito_formula(S_f, S_g, dv8, d2v8)
f_dt8 <- fun_list8[[1]]
f_dw8 <- fun_list8[[2]]
hermit8 <- Euler_Maruyama(1, 1000, f_dt8, f_dw8, 1, b_m, sol)
plot(hermit8, type="l")
lines(265*sol^8-3584*sol^6+13440*sol^4-13440*sol^2+1680, type = "l", col = "red")

## Hermit 9
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm9 <- expression(512*x^9-9216*x^7+48384*x^5-80640*x^3+30240*x)
a <- D(herm9, 'x')
a
D(a,'x')
##
dv9 <- function(x)512 * (9 * x^8) - 9216 * (7 * x^6) + 48384 * (5 * x^4) - 80640 * (3 * x^2) + 30240;
d2v9 <- function(x)512 * (9 * (8 * x^7)) - 9216 * (7 * (6 * x^5)) + 48384 * (5 *(4 * x^3)) - 80640 * (3 * (2 * x));
fun_list9 <- ito_formula(S_f, S_g, dv9, d2v9)
f_dt9 <- fun_list9[[1]]
f_dw9 <- fun_list9[[2]]
hermit9 <- Euler_Maruyama(1, 1000, f_dt9, f_dw9, 1, b_m, sol)
plot(hermit9, type="l")
lines(512*sol^9-9216*sol^7+48384*sol^5-80640*sol^3+30240*sol, type = "l", col = "red")

## Hermit 10
## pomocnicze liczenie pochodnych, zebym sie nie pomylila
herm10 <- expression(1024*x^10-23040*x^8+161280*x^6-403200*x^4+302400*x^2-30240)
a <- D(herm10, 'x')
a
D(a,'x')
##
dv10 <- function(x)1024 * (10 * x^9) - 23040 * (8 * x^7) + 161280 * (6 * x^5) - 403200 * (4 * x^3) + 302400 * (2 * x);
d2v10 <- function(x)1024 * (10 * (9 * x^8)) - 23040 * (8 * (7 * x^6)) + 161280 * (6 * (5 * x^4)) - 403200 * (4 * (3 * x^2)) + 302400 * 2;
fun_list10 <- ito_formula(S_f, S_g, dv10, d2v10)
f_dt10 <- fun_list10[[1]]
f_dw10 <- fun_list10[[2]]
hermit10 <- Euler_Maruyama(1, 1000, f_dt10, f_dw10, 1, b_m, sol)
plot(hermit10, type="l")
lines(1024*sol^10-23040*sol^8+161280*sol^6-403200*sol^4+302400*sol^2-30240, type = "l", col = "red")##TODO