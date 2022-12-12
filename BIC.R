
### Julieth Salazar
### Mavelyn Sterling

####### FORWARD SELECTION USING BIC

# Datos de Hald

# y = calor producido
y <-c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2,
  102.7, 72.5,93.1,115.9, 83.8, 113.3, 109.4)

# x1 = aluminato tricalcico
x1<-c(7,1,11,11,7,11,3,1,2,21,1,11,10)

#x2 = silicato tricalcico
x2<-c(26,29,56,31,52,55,71,31,54,47,40,66,68)

#x3 = aluminoferrito tetracalcico
x3<-c(6,15,8,8,6,9,17,22,18,4,23,9,8)

#x4 = silicato dicalcico
x4<-c(60,52,20,47,33,22,6,44,22,26,34,12,12)

# total de datos por variables
n =13

#################################### Paso 1

# se calcula el modelo nulo
modelo0<- lm(y ~ 1)
summary(modelo0)
rss0<- sum(residuals(modelo0)^2)
BIC0=  n *log(rss0/n)+(log(n))*(0+1)

p1= 1 # numero de betas del modelo

#se calcula el modelo de y con x4
modelo4<-lm(y~x4)
summary(modelo4)
rss4<-sum(residuals(modelo4)^2)
BIC4= n *log(rss4/n)+(log(n))*(p1+1)

#se calcula el modelo de y con x3
modelo3 <-lm(y~x3)
summary(modelo3)
rss3<-sum(residuals(modelo3)^2)
BIC3= n *log(rss3/n)+(log(n))*(p1+1)

#se calcula el modelo de y con x2
modelo2 <-lm(y~x2)
summary(modelo2)
rss2<-sum(residuals(modelo2)^2)
BIC2= n *log(rss2/n)+(log(n))*(p1+1)

#se calcula el modelo de y con x1
modelo1<-lm(y~x1)
summary(modelo1)
rss1<-sum(residuals(modelo1)^2)
BIC1= n *log(rss1/n)+(log(n))*(p1+1)

rss0;BIC0
rss1;BIC1
rss2;BIC2
rss3;BIC3
rss4;BIC4

# vector de los BIC calculados
paso1BIC <- c(BIC0,BIC1,BIC2,BIC3, BIC4)

#se ordena el BIC de menor a mayor
sort(paso1BIC)


# Y lineas de ajustes
dx1<-seq(min(x1),max(x1),length=13)
dx2<- seq(min(x2), max(x2), length=13)
dx3<- seq(min(x3), max(x3),length=13)
dx4 <- seq(min(x4), max(x4), length=13)
dhald<-seq(min(datosHald),max(datosHald),length=13)
newdata<-data.frame(Dx1=dx1, Dx2=dx2,Dx3=dx3, Dx4=dx4,Dhald=dhald )


plot(modelo0, predict(modelo1,newdata), which = 1:6)
lines(datosHald, predict(modelo2,newdata), col="red")
lines(datosHald, predict(modelo3, newdata), col="blue")

################################# paso 2

## BIC = 59.98 de x4
## none
rss4;BIC4

p2 =2 #numero de betas del nuevo modelo


#se calcula el modelo lineal de y con x4 + x1
modelo1_2<-lm(y~x4+x1)
summary(modelo1_2)
rss1_2<- sum(residuals(modelo1_2)^2)
BIC1_2= n*log(rss1_2/n)+(log(n))*(p2 + 1)

#se calcula el modelo lineal de y con x4 + x2
modelo2_2<-lm(y~x4+x2)
summary(modelo2_2)
rss2_2<- sum(residuals(modelo2_2)^2)
BIC2_2= n*log(rss2_2/n)+(log(n))*(p2 + 1)

#se calcula el modelo lineal de y con x4 + x3
modelo3_2<-lm(y~x4+x3)
summary(modelo3_2)
rss3_2<- sum(residuals(modelo3_2)^2)
BIC3_2= n*log(rss3_2/n)+(log(n))*(p2 +1)

#vector de BIC calculados
paso2BIC <- c(BIC1_2,BIC2_2,BIC3_2)

#se ordena el BIC de menor a mayor
sort(paso2BIC)

################################## Paso 3

## BIC = 30.44
## none
rss1_2;BIC1_2

p3= 3 # numero de betas del nuevo modelo


#se calcula el modelo lineal de y con x4 + x1 + x2
modelo2_3<-lm(y~x4+x1+x2)
summary(modelo2_3)
rss2_3<- sum(residuals(modelo2_3)^2)
BIC2_3= n*log(rss2_3/n)+(log(n))*(4)

#se calcula el modelo lineal de y con x4 + x1 +x2
modelo3_3<-lm(y~x4+x1+x3)
summary(modelo3_3)
rss3_3<- sum(residuals(modelo3_3)^2)
BIC3_3= n*log(rss3_3/n)+(log(n))*(4)

#vector de BIC calculados
paso3BIC <- c(BIC2_3,BIC3_3)

#se ordena el BIC de menor a mayor
sort(paso3BIC)

#################################### Paso 4

##BIC = 27.23
## none
rss2_3;BIC2_3

#se calcula el modelo lineal de y con x4 + x1 +x2 +x3
modelo3_4<-lm(y~x4+x1+x2+x3)
summary(modelo3_4)
rss3_4<- sum(residuals(modelo3_4)^2)
BIC3_4= n*log(rss3_4/n)+(log(n))*(5)

#vector de BIC calculados
paso4BIC <- c(BIC2_3,BIC3_4)

#se ordena el BIC de menor a mayor
sort(paso4BIC) 
BIC3_4<BIC2_3 

plot(y,c(x1,x2,x4))



######### Comprobando el procedimiento 

datosHald<- data.frame(y,x1,x2,x3,x4)
model.forward<-lm(y~1,data=datosHald)
summary(model.forward)

# con k=log(n) es por el criterio BIC
n<- nobs(model.forward) # numero de observaciones

#comando para calcular BIC
step(model.forward,direction = "forward",
     k=log(nrow(datosHald)), scope = list(lower= ~1, upper= ~x1+x2+x3+x4),
     data=datosHald)


#require(leaps)
#require(faraway)
#library("leaps")
#library("faraway")

#a = regsubsets( y~., data=datosHald,
#                   nvmax=13,method= "forward")
#b= summary(a)
#plot(b$bic, xlab="Parameter", ylab="BIC") 



library(GGally)
grafico <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p}


p <- ggpairs( datosHald, lower = list(continuous = wrap(grafico, method = "lm")),
              diag = list(continuous = wrap("barDiag", colour = "blue")),
              upper = list(continuous = wrap("cor", size = 5)))

x11()
p











install.packages("GGally")
library(GGally)
x11()
ggpairs(datosHald, lower = list(continuous = wrap(lowerFn, method = "lm")),
        diag = list(continuous = wrap("barDiag", colour = "blue")),
        upper = list(continuous = wrap("cor", size = 5)))

ggpairs(datosHald, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")




lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") +
    geom_smooth(method = method, color = "red", ...)
  p
}
lowerFn(datosHald, )

windows()
p <- ggpairs(
  datosHald, lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 5))
)
p

install.packages("GGally")
library(GGally)
x11()
ggpairs(datosHald, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"),
        axisLabels = "none", mapping = aes(color = "red"))

