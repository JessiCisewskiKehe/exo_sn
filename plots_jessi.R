

library(sn)



x <- seq(-3.25, 3.25, length.out = 1000)
xi <- 0
omega <- 1
alpha <- c(-3,0, 2,6,10)
dx <- lapply(1:length(alpha), function(ii) dsn(x, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))


par.index <- c(2,1,3,4,5)
par(mar = c(4,4,1,1), lwd = 8, cex = 2)
plot(x, dx[[1]], "n", xlab = "Y", ylab = "Density", ylim = c(0,.8))
temp <- sapply(1:length(alpha), function(ii) lines(x, dx[[ii]], col = par.index[ii], lty = par.index[ii]))
legend("topleft", legend = alpha, col = par.index, lty = par.index, lwd = 5, title = expression(alpha), cex = 1.75)


fbeta <- function(a){sqrt(2/pi)*a/sqrt(1+a^2)}
fgamma <- function(b){0.5*(4 - pi)*b^2*(1-b^2)^(-3/2)}
beta <- fbeta(alpha)
gamma <- fgamma(beta)
mu <- xi + omega*beta
sig2 <- omega^2*(1-beta^2)

round(cbind(alpha, mu, sig2, gamma),3)



