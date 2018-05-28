

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
fgamma <- function(b){0.5*(4 - pi)*b^3*(1-b^2)^(-3/2)}
beta <- fbeta(alpha)
gamma <- fgamma(beta)
mu <- xi + omega*beta
sig2 <- omega^2*(1-beta^2)


median_alpha <- sapply(1:length(alpha), function(ii) qsn(.5, xi = 0, omega = 1, alpha = alpha[ii]))

round(cbind(alpha, mu, sig2, gamma, median_alpha),3)

dp <- cbind(rep(0, length(alpha)), rep(1, length(alpha)), alpha)
cp0 <- round(sapply(1:length(alpha), function(ii) dp2cp(dp[ii,], "SN")), 3)
cp[2,] <-(cp0[2,])^2
cp

median_alpha_cp <- sapply(1:length(alpha), function(ii) qsn(.5, xi = 0, omega = 1, alpha = alpha[ii]))

################  Exploration of robust (wrt skewness) parameters
x <- seq(-3.25, 3.25, length.out = 1000)
xi <- 0
omega <- 1
alpha <- seq(0, 5, by = 0.25)
dx <- lapply(1:length(alpha), function(ii) dsn(x, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))
medx <- sapply(1:length(alpha), function(ii) qsn(.5, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))
fbeta <- function(a){sqrt(2/pi)*a/sqrt(1+a^2)}
fgamma <- function(b){0.5*(4 - pi)*b^2*(1-b^2)^(-3/2)}
beta <- fbeta(alpha)
gamma <- fgamma(beta)
meanx <- xi + omega*beta

par(mfrow = c(1,2), mar = c(4,4,1,1), lwd = 3, cex = 2)
#Median
plot(x, dx[[1]], "n", xlab = "Y", ylab = "Density", ylim = c(0,.8), main = "Median = vertical lines")
temp <- sapply(1:length(alpha), function(ii) lines(x, dx[[ii]], col = ii, lty = ii))
abline(v = medx, col = 1:length(alpha), lwd = 3, lty = 1)
legend("topleft", legend = alpha, col = 1:length(alpha), lty = 1:length(alpha), lwd = 2, title = expression(alpha), cex = .5)
#Mean
plot(x, dx[[1]], "n", xlab = "Y", ylab = "Density", ylim = c(0,.8), main = "Mean = vertical lines")
temp <- sapply(1:length(alpha), function(ii) lines(x, dx[[ii]], col = ii, lty = ii))
abline(v = meanx, col = 1:length(alpha), lwd = 3, lty = 1)

plot(meanx, medx)
abline(a = 0, b = 1)





################  Stable
x <- seq(-3.25, 3.25, length.out = 1000)
xi <- 0
omega <- 1
alpha <- seq(0, 5, by = 1)
dx <- lapply(1:length(alpha), function(ii) dsn(x, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))
medx <- sapply(1:length(alpha), function(ii) qsn(.5, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))
fbeta <- function(a){sqrt(2/pi)*a/sqrt(1+a^2)}
fgamma <- function(b){0.5*(4 - pi)*b^2*(1-b^2)^(-3/2)}
beta <- fbeta(alpha)
gamma <- fgamma(beta)
meanx <- xi + omega*beta
ffwhm <- function(sig){2*sqrt(2*log(2))*sig}
sig2 <- omega^2*(1-beta^2)
ffwhm(sqrt(sig2))

dx_rm <- lapply(1:length(alpha), function(ii) pnorm(alpha[ii]*(x-xi)/omega,0,1))

par(mfrow = c(1,1), mar = c(4,4,1,1), lwd = 3, cex = 1)
plot(x, dx[[1]], "n", xlab = "Y", ylab = "Density", ylim = c(0,.8), main = "")
temp <- sapply(1:length(alpha), function(ii) lines(x, dx[[ii]], col = ii, lty = ii))

# Remove skew portion
library(sfsmisc)
norm1 <- integrate.xy(x, dx[[1]]/dx_rm[[1]])
temp <- sapply(1:length(alpha), function(ii) lines(x, dx[[ii]]/dx_rm[[ii]]/2, col = ii, lty = ii))

lines(x, dnorm(x, xi, omega), col = "yellow")




### Remove skew portion

x <- seq(0, 8, length.out = 1000)
xi <- 4
omega <- 1
alpha <- 8
dx <- dsn(x, xi, omega, alpha, tau=0, dp=NULL, log=FALSE)
dx_rm <- pnorm(alpha*(x-xi)/omega,0,1)
medx <- sapply(1:length(alpha), function(ii) qsn(.5, xi, omega, alpha[ii], tau=0, dp=NULL, log=FALSE))
fbeta <- function(a){sqrt(2/pi)*a/sqrt(1+a^2)}
fgamma <- function(b){0.5*(4 - pi)*b^2*(1-b^2)^(-3/2)}
beta <- fbeta(alpha)
gamma <- fgamma(beta)
meanx <- xi + omega*beta
sig2 <- omega^2*(1-beta^2)



plot(x, dx, "l", col = 1, lwd = 3, xlab = "Y", ylab = "Density", ylim = range(dx, dx/dx_rm))
lines(x, dx/dx_rm, col = 2, lty = 2, lwd = 3)
abline(v = 0, lty = 2, col = "gray")
lines(x, dnorm(x, 0, 1), col = "green", lty = 3)
meanx
sum((dx/dx_rm)*x)/sum(dx/dx_rm)









