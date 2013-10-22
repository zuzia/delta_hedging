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
