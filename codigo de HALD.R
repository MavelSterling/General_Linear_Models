install.packages("nlme")
install.packages("nls")
install.packages("lattice")
install.packages("grid")
install.packages("car")
install.packages("hier.part")
install.packages("Hmisc")
install.packages("format.pval")
install.packages("units")
library(MASS)
library(Hmisc)
library(hier.part)
library(car)



par(mfrow= c(2,2))
plot(paso1BIC, type = "b", pch= 21, xlab= "BIC del paso 1")
plot(paso2BIC, type = "b",pch=21,xlab= "BIC del paso 2")
plot(paso3BIC, type = "b",pch=21 ,xlab= "BIC del paso 3" )
plot(paso4BIC, type = "b", pch=21, xlab= "BIC del paso 4")


setwd("")#Colocar la direccion donde esta el excel.csv con los datos
Datos<-read.csv("Datos1.csv",header = T,";")
Datoss<-cbind(as.numeric(Datos[,1]),as.numeric(Datos[,2]),as.numeric(Datos[,3]),as.numeric(Datos[,4]),as.numeric(Datos[,5]))
Datos<-read.csv("Datos1.csv",header = T,";")

summary(Datos)
#Datoss<-cbind(as.numeric(Datos[,1]),as.numeric(Datos[,2]),as.numeric(Datos[,3]),as.numeric(Datos[,4]),as.numeric(Datos[,5]))

attach(Datos)
srt(Datos)

y <-c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2,102.7, 72.5,93.1,115.9, 83.8, 113.3, 109.4)
x1<-c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2<-c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3<-c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4<-c(60,52,20,47,33,22,6,44,22,26,34,12,12)

### AIC
step(lm(y ~ 1), y ~ x1 + x2 + x3 +x4, direction = "forward")



install.packages("nlme")
install.packages("nls")
install.packages("lattice")
install.packages("grid")
install.packages("car")
install.packages("hier.part")
install.packages("Hmisc")
install.packages("format.pval")
install.packages("units")
library(MASS)
library(Hmisc)
library(hier.part)
library(car)
setwd("/Volumes/GoogleDrive/Mi unidad/ESTAD??STICA/SEMESTRE 2018-2/MODELOS LINEALES I/BIC")#Colocar la direccion donde esta el excel.csv con los datos

#######################################


y <-c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2,102.7, 72.5,93.1,115.9, 83.8, 113.3, 109.4)
x1<-c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2<-c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3<-c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4<-c(60,52,20,47,33,22,6,44,22,26,34,12,12)

datosHald<- data.frame(x1,x2,x3,x4,y)
model.forward<-lm(y~1,data=datosHald)
summary(model.forward)
# con k=log(n) es por el criterio BIC
n<- nobs(model.forward)
step(model.forward,direction = "forward",
     k=log(nrow(datosHald)), scope = list(lower= ~1, upper= ~x1+x2+x3+x4),
     data=datosHald)

########################################

#  Copyright (C) 1997-2009, 2017 The R Core Team

### Helical Valley Function
### Page 362 Dennis + Schnabel

require(stats); require(graphics); require(utils)

theta <- function(x1,x2) (atan(x2/x1) + (if(x1 <= 0) pi else 0))/ (2*pi)
## but this is easier :
theta <- function(x1,x2) atan2(x2, x1)/(2*pi)

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2+x[2]^2)-1)
  f3 <- x[3]
  return(f1^2 + f2^2 + f3^2)
}

## explore surface {at x3 = 0}
x <- seq(-1, 2, length.out=50)
y <- seq(-1, 1, length.out=50)
z <- apply(as.matrix(expand.grid(x, y)), 1, function(x) f(c(x, 0)))
contour(x, y, matrix(log10(z), 50, 50))
str(nlm.f <- nlm(f, c(-1,0,0), hessian = TRUE))
points(rbind(nlm.f$estim[1:2]), col = "red", pch = 20)
stopifnot(all.equal(nlm.f$estimate, c(1, 0, 0)))

### the Rosenbrock banana valley function

fR <- function(x)
{
  x1 <- x[1]; x2 <- x[2]
  100*(x2 - x1*x1)^2 + (1-x1)^2
}

## explore surface
fx <- function(x)
{   ## `vectorized' version of fR()
  x1 <- x[,1]; x2 <- x[,2]
  100*(x2 - x1*x1)^2 + (1-x1)^2
}
x <- seq(-2, 2, length.out=100)
y <- seq(-0.5, 1.5, length.out=100)
z <- fx(expand.grid(x, y))
op <- par(mfrow = c(2,1), mar = 0.1 + c(3,3,0,0))
contour(x, y, matrix(log10(z), length(x)))

str(nlm.f2 <- nlm(fR, c(-1.2, 1), hessian = TRUE))
points(rbind(nlm.f2$estim[1:2]), col = "red", pch = 20)

## Zoom in :
rect(0.9, 0.9, 1.1, 1.1, border = "orange", lwd = 2)
x <- y <- seq(0.9, 1.1, length.out=100)
z <- fx(expand.grid(x, y))
contour(x, y, matrix(log10(z), length(x)))
mtext("zoomed in");box(col = "orange")
points(rbind(nlm.f2$estim[1:2]), col = "red", pch = 20)
par(op)
with(nlm.f2,
     stopifnot(all.equal(estimate, c(1,1), tol = 1e-5),
               minimum < 1e-11, abs(gradient) < 1e-6, code %in% 1:2))

fg <- function(x)
{
  gr <- function(x1, x2)
    c(-400*x1*(x2 - x1*x1)-2*(1-x1), 200*(x2 - x1*x1))
  x1 <- x[1]; x2 <- x[2]
  structure(100*(x2 - x1*x1)^2 + (1-x1)^2,
            gradient = gr(x1, x2))
}

nfg <- nlm(fg, c(-1.2, 1), hessian = TRUE)
str(nfg)
with(nfg,
     stopifnot(minimum < 1e-17, all.equal(estimate, c(1,1)),
               abs(gradient) < 1e-7, code %in% 1:2))

## or use deriv to find the derivatives

fd <- deriv(~ 100*(x2 - x1*x1)^2 + (1-x1)^2, c("x1", "x2"))
fdd <- function(x1, x2) {}
body(fdd) <- fd
nlfd <- nlm(function(x) fdd(x[1], x[2]), c(-1.2,1), hessian = TRUE)
str(nlfd)
with(nlfd,
     stopifnot(minimum < 1e-17, all.equal(estimate, c(1,1)),
               abs(gradient) < 1e-7, code %in% 1:2))

fgh <- function(x)
{
  gr <- function(x1, x2)
    c(-400*x1*(x2 - x1*x1) - 2*(1-x1), 200*(x2 - x1*x1))
  h <- function(x1, x2) {
    a11 <- 2 - 400*x2 + 1200*x1*x1
    a21 <- -400*x1
    matrix(c(a11, a21, a21, 200), 2, 2)
  }
  x1 <- x[1]; x2 <- x[2]
  structure(100*(x2 - x1*x1)^2 + (1-x1)^2,
            gradient = gr(x1, x2),
            hessian  =  h(x1, x2))
}

nlfgh <- nlm(fgh, c(-1.2,1), hessian = TRUE)
str(nlfgh)
## NB: This did _NOT_ converge for R version <= 3.4.0
with(nlfgh,
     stopifnot(minimum < 1e-15, # see 1.13e-17 .. slightly worse than above
               all.equal(estimate, c(1,1), tol=9e-9), # see 1.236e-9
               abs(gradient) < 7e-7, code %in% 1:2)) # g[1] = 1.3e-7
step(lm(y ~ 1), y ~ x1 + x2 + x3 +x4, direction = "forward", k = log(n))
