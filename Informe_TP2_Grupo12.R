library(Rglpk)
library(quadprog)
library(Rsolnp)
library(DEoptim)
library(robustbase)
library(quantmod) 
library(ggplot2)
library(tidyverse)
library(corrplot)
 options(scipen = 99)


#Programacion Lineal

LP_solver <- function(c, cstr = list(), trace = FALSE) {
  Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
  aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
  A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
  a <- Reduce(c, cstr[names(cstr) %in% "a"])
  sol <- Rglpk_solve_LP(obj = c,
                        mat = rbind(Aeq, A),
                        dir = c(rep("==", nrow(Aeq)),
                                rep(">=", nrow(A))),
                        rhs = c(aeq, a),
                        verbose = trace)
  status <- sol$status
  solution <- if (status) rep(NA, length(c)) else sol$solution
  list(solution = solution, status = status)
}

#Programacion Cuadratica

QP_solver <- function(c, Q, cstr = list(), trace = FALSE) {
  
  Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
  aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
  A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
  a <- Reduce(c, cstr[names(cstr) %in% "a"])
  sol <- try(solve.QP(Dmat = Q,
                      dvec = -2 * c,
                      Amat = t(rbind(Aeq, A)),
                      bvec = c(aeq, a),
                      meq = nrow(Aeq)),
             silent = TRUE)
  if (trace) cat(sol)
  if (inherits(sol, "try-error"))
    list(solution = rep(NA, length(c)), status = 1)
  else
    list(solution = sol$solution, status = 0)
}

#Programacion No Lineal

NLP_solver <- function(par, f, cstr = list(), trace = FALSE) {Aeq <- Reduce(rbind, cstr[names(cstr) %in% "Aeq"])
aeq <- Reduce(c, cstr[names(cstr) %in% "aeq"])
A <- Reduce(rbind, cstr[names(cstr) %in% "A"])
a <- Reduce(c, cstr[names(cstr) %in% "a"])
heq <- Reduce(c, cstr[names(cstr) %in% "heq"])
h <- Reduce(c, cstr[names(cstr) %in% "h"])
leqfun <- c(function(par) c(Aeq %*% par), heq)
eqfun <- function(par)
  unlist(lapply(leqfun, do.call, args = list(par)))
eqB <- c(aeq, rep(0, length(heq)))

lineqfun <- c(function(par) c(A %*% par), h)
ineqfun <- function(par)
  unlist(lapply(lineqfun, do.call, args = list(par)))
ineqLB <- c(a, rep(0, length(h)))
ineqUB <- rep(Inf, length(ineqLB))

sol <- solnp(par = par,
             fun = f,
             eqfun = eqfun,
             eqB = eqB,
             ineqfun = ineqfun,
             ineqLB = ineqLB,
             ineqUB = ineqUB,
             control = list(trace = trace))

status <- sol$convergence
solution <- if (status) rep(NA, length(par)) else sol$pars
list(solution= solution, status = status)
}

#Busco los Datos 
#Hago un vector con el id de las accione sque quiero en mi cartera y descargo los datos por cada vlaor en el vector.

tick <-c( "AMD", "NVDA", "BA", "AAPL", "SBUX", "MCD") 
  for(i in  tick) {
    getSymbols(i, src = "yahoo", from = "2019-01-01", to = "2020-11-30", periodicity = "daily")
  }

#Armo un dataset con todos los precios de cierre
prices <- merge.xts( AMD$AMD.Adjusted, NVDA$NVDA.Adjusted, BA$BA.Adjusted, AAPL$AAPL.Adjusted, SBUX$SBUX.Adjusted, MCD$MCD.Adjusted)

#Calculo los retornos discretos
ret.d <- na.omit(data.frame(apply( prices, 2, function(x) Delt(x))))
rownames(ret.d) <- index(prices[-1])
ts.ret.d = as.xts(ret.d) 

#Calculo los retornos continuos
ret.c <- na.omit(data.frame(apply( prices, 2, function(x) Delt(x, type = "log"))))
rownames(ret.c) <- index(prices[-1])
colnames(ret.c) <- tick
ts.ret.c = as.xts(ret.c) 
x <- as.matrix(ret.c)

#Analisis
summary(prices)
promedio.diario=apply(ret.d,2,mean)
volatilidad=apply(ret.d,2,sd)

par(mar=c(1,1,1,1))
par(mfrow=c(2,1))
plot(AMD$AMD.Adjusted, main = "Precio de accion AMD")
plot(BA$BA.Adjusted, main = "Precio de accion BA")
plot(SBUX$SBUX.Adjusted, main = "Precio de accion SBUX")
plot(AAPL$AAPL.Adjusted, main = "Precio de accion AAPL ")
plot(MCD$MCD.Adjusted, main = "Precio de accion MCD")
plot(NVDA$NVDA.Adjusted, main = "Precio de accion NVDA")
par(mfrow=c(1,1))

#Retorno promdedio diario y volatilidad.
ggplot()+
  geom_point(aes(volatilidad,promedio.diario),
             size=5,
             color="blue")+
  geom_text(aes(label=tick,volatilidad,promedio.diario),vjust=2)

#Correlacion entre las acciones
correlacion=cor(prices)
corrplot(correlacion, type = "upper",tl.col = "black", tl.srt = 45)
correlacion



#Optimizacion de cartera
#Tipos de restricciones

#Retorno esperado (tiene pesos para cada accion)
targetReturn <- function(x, target) {
    list(Aeq = rbind(colMeans(x)), aeq = target)
}

#Full Investment (los pesos tienen que sumar 1)
fullInvest <- function(x) {
  list(Aeq = matrix(1, nrow = 1, ncol = ncol(x)), aeq = 1)
}

#Posiciones largas unicamente (los pesos e basan en que solo compramos acciones)
longOnly <- function(x) {
    list(A = diag(1, ncol(x)), a = rep(0, ncol(x)))
}

#Modelo de optimizacion Media-Varianza --> Markowitz 1953

MV_QP <- function(x, target, Sigma = cov(x), ..., cstr = c(fullInvest(x), targetReturn(x, target), longOnly(x), ...), trace = FALSE) { 
                  size <- ncol(x)
                  c <- rep(0, size)
                  Q <- Sigma
                  
                  # optimization
                  sol <- QP_solver(c, Q, cstr, trace)
                  
                  # extract weights
                  weights <- sol$solution
                  names(weights) <- colnames(x)
                  weights
}

#x son los retornos, target es el retorno objetivo de la cartera y sigma es la covarianza estimada
#Modelo clasico
w <- MV_QP(x,mean(x))
sum(w) 

#Veo los retornos de la cartera (Al final los grafico todos juntos)
MVC <- data.frame(x %*% w)
rownames(MVC) <- index(prices[-1])
colnames(MVC) <- "MVC"
#MVC <- as.matrix(MVC) #Algunas funciones solo aceptan matrices, usar si hay error


#Grafico de proporciones
graph <- data.frame(w,tick)
colnames(graph) <- c("Peso", "Acciones")
ggplot(data = graph, aes( x = Acciones, y = Peso, color = Acciones))+
  geom_bar(stat = "identity", fill = "white")+
  ggtitle("Pesos de Modelo Mean-Variance")+
  theme_minimal()

#Modelo de optimizacion de cartera con de varianza robusta
#El problema del modelo clasico es que es muy sensible a los valores outliers
wr <- MV_QP(x, mean(x), Sigma = covMcd(x)$cov)
sum(wr)

#Veo los retornos
MVR <- data.frame(x %*% wr)
rownames(MVR) <- index(prices[-1])
colnames(MVR) <- "MVR"
#MVR <- as.matrix(MVR) #Lo paso como matriz para que funcione las otras funciones que hice

graph <- data.frame(wr,tick)
colnames(graph) <- c("Peso", "Acciones")
ggplot(data = graph, aes( x = Acciones, y = Peso, color = Acciones))+
  geom_bar(stat = "identity", fill = "white")+
  ggtitle("Pesos de Modelo Mean-Variance Robusto")+
  theme_minimal()


#Modelo de la Minima Varianza
wmv <- MV_QP(x, cstr = c(fullInvest(x), longOnly(x)))
sum(wmv)

#Veo los retornos 
MVM <- data.frame(x %*% wmv)
rownames(MVM) <- index(prices[-1])
colnames(MVM) <- "MVM"
#MVM <- as.matrix(MVM) #Lo paso como matriz para que funcione las otras funciones que hice

graph <- data.frame(wmv,tick)
colnames(graph) <- c("Peso", "Acciones")
ggplot(data = graph, aes( x = Acciones, y = Peso, color = Acciones))+
  geom_bar(stat = "identity", fill = "white")+
  ggtitle("Pesos de Modelo de Minima Varianza")+
  theme_minimal()

#Verifico el modelo de minima varianza buscando el retorno correspondiente al riesgo minimo
# Tambien verifico si hay un trade-off entre riesgo y rendimiento alterando proporciones de las acciones en distintos portfolios eficientes.
PMV <- MV_QP(x,0.001024032)
barplot(PMV)
sum(PMV)
port.2 <- MV_QP(x,0.001500141)
barplot(port.2)
port.3 <- MV_QP(x,0.00180167)
barplot(port.3)
port.4 <- MV_QP(x,0.002103211)
barplot(port.4)
port.5 <- MV_QP(x,0.002301590)
barplot(port.5)

combinaciones=cbind(round(PMV,4),round(port.2,4),round(port.3,4),round(port.4,4),round(port.5,4))
suma=round(c(sum(PMV),sum(port.2),sum(port.3),sum(port.4),sum(port.5)),0)
Rendimiento=c(0.00102,0.0015,0.0018,0.0021,0.0022)
riesgo=c(0.01850217,0.01924410,0.02037356,0.02192678,0.02313204)
portafolios=rbind(Rendimiento,combinaciones,suma,riesgo)
colnames(portafolios)=c("PMV","Port.2","Port.3","Port.4","Port.5")

view(portafolios)

ggplot(mapping = aes(riesgo,Rendimiento))+
  geom_point(size=4,
             color=c("blue","green","violet","black","red"))+
  geom_text(aes(label=c("PMV","Port.2","Port.3","Port.4","Port.5"),riesgo,Rendimiento),vjust=-1)+
  labs(title = "Portafolios eficientes")+
  ylim(0.001,0.00235)+
  xlim(0.0181,0.0234)




#Grafico de los retornos de todos los portfolios
#Calculo los retornos para las carteras con un capital inicial de 1000 dolares 
pftPerf <- function(x, w, W0 = 1000) {
  W0 * cumprod(c(1, 1 + x %*% w))
}

#Grafico los retornos de cada cartera que calcule
colors <- c("Mean-Variance" = "red", "MV robusto" = "blue", "Minimal Variance" = "brown")
ggplot(data = prices, aes(x = index(prices), y = "Wealth"))+
  geom_line(data = as.data.frame(pftPerf(x,w)),aes(y = pftPerf(x,w), color = "Mean-Variance"), size = 0.7)+
  geom_line(data = as.data.frame(pftPerf(x,wr)),aes(y = pftPerf(x,wr), color = "MV robusto"), size = 0.7)+
  geom_line(data = as.data.frame(pftPerf(x,wmv)),aes(y = pftPerf(x,wmv), color = "Minimal Variance"), size = 0.7)+ 
  #  geom_line(data = as.data.frame(pftPerf(x,wcvar)),aes(y = pftPerf(x,wcvar)), color = "violet", size = 0.7)+
  labs(
    title    = "Retornos acumulados de las carteras propuestas",
    x = "Tiempo",
    y = "Capital",
    color = "Leyenda"
  )+
  scale_color_manual(values = colors)+
  theme_minimal()


#Frontera Eficiente 
#Media de los retornos para cada activo
mu_1 <- mean(ret.d$AMD.Adjusted) 
mu_2 <- mean(ret.d$NVDA.Adjusted)
mu_3 <- mean(ret.d$BA.Adjusted) 
mu_4 <- mean(ret.d$AAPL.Adjusted)
mu_5 <- mean(ret.d$SBUX.Adjusted)
mu_6 <- mean(ret.d$MCD.Adjusted)


#Desvio estandar de los retornos para cada activo
sigma_1 <- sd(ret.d$AMD.Adjusted)
sigma_2 <- sd(ret.d$NVDA.Adjusted)
sigma_3 <- sd(ret.d$BA.Adjusted)
sigma_4 <- sd(ret.d$AAPL.Adjusted)
sigma_5 <- sd(ret.d$SBUX.Adjusted)
sigma_6 <- sd(ret.d$MCD.Adjusted)


#Covarianzas entre los activos
cov_12 <- cov(ret.d$AMD.Adjusted, ret.d$NVDA.Adjusted)
cov_13 <- cov(ret.d$AMD.Adjusted, ret.d$BA.Adjusted)
cov_14 <- cov(ret.d$AMD.Adjusted, ret.d$AAPL.Adjusted)
cov_15 <- cov(ret.d$AMD.Adjusted, ret.d$SBUX.Adjusted)
cov_16 <- cov(ret.d$AMD.Adjusted, ret.d$MCD.Adjusted)

cov_23 <- cov(ret.d$NVDA.Adjusted, ret.d$BA.Adjusted)
cov_24 <- cov(ret.d$NVDA.Adjusted, ret.d$AAPL.Adjusted)
cov_25 <- cov(ret.d$NVDA.Adjusted, ret.d$SBUX.Adjusted)
cov_26 <- cov(ret.d$NVDA.Adjusted, ret.d$MCD.Adjusted)

cov_34 <- cov(ret.d$BA.Adjusted, ret.d$AAPL.Adjusted)
cov_35 <- cov(ret.d$BA.Adjusted, ret.d$SBUX.Adjusted)
cov_36 <- cov(ret.d$BA.Adjusted, ret.d$MCD.Adjusted)

cov_45 <- cov(ret.d$AAPL.Adjusted, ret.d$SBUX.Adjusted)
cov_46 <- cov(ret.d$AAPL.Adjusted, ret.d$MCD.Adjusted)

cov_56 <- cov(ret.d$SBUX.Adjusted, ret.d$MCD.Adjusted)



#Todas las posibles carteras a formar utilizando solo tres activos
#No trabajamos con 6 ya que al ser tantas conmbinaciones pierde el sentido el grafico

mu_p <- rep(0,100*10)
var_p <- rep(0,100*10)

i <- 1
for(t in -25:75){
  t <- 0.02*t
  for(s in -25:75){
    s <- 0.02*s 
    mu_p[i] <- t*mu_1 + s*mu_2 + (1-t-s)*mu_3
    var_p[i] <- (t^2*sigma_1^2+s^2*sigma_2^2+(1-t-s)^2*sigma_3^2
                 +2*t*s*cov_12+2*t*(1-t-s)*cov_13+
                   +2*s*(1-t-s)*cov_23)        
    i <- i+1
  }
}

sigma_p <- sqrt(var_p) 
plot(sigma_p,mu_p,pch=".", 
     xlab = "Riesgo", ylab = "Rendimiento", main = "Combinaciones posibles de carteras con 3 activos")


#El procedimento para usar los 6 activos
#Tarda mucho en procesar y el grafico resultante no aporta mas informacion que el de 3 activos por lo que no se utiliza.

#i <- 1
#for(t in -25:75){
#  t <- 0.02*t
#  for(s in -25:75){
#   s <- 0.02*s 
#    for(r in -25:75){
#      r <- 0.02*r
#      for(o in -25:75){
#        o <- 0.02*o
#       for(p in -25:75){
#          p <- 0.02*p
#          mu_p[i] <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
#          var_p[i] <- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)        
#          i <- i+1
#        }
#      }
#    }
#  }
#}

#A partir de esto es que buscamos una frontera eficiente

mu <- apply(x, 2, mean)
reward <- seq(from = min(mu), to = max(mu), length.out = 1000)
sigma <- apply(x, 2, sd)

 Sigma <- cov(x)
 riskCov <- sapply(reward, function(targetReturn) {
    w <- MV_QP(x, targetReturn, Sigma)
      sd(c(x %*% w))
 })

#Grafico:
ggplot()+  
   geom_point(
     mapping= aes(x=riskCov, y=reward, color=riskCov)
   ) +
   geom_point(
     mapping = aes(x=sigma, y = mu),
     size    = 3,
     color   = "black",
   )+geom_text(aes(label= tick,sigma,mu),vjust=2)+
   labs(
     title    = "Frontera eficiente",
     subtitle = "6 activos de riesgo",
     x = "Riesgo",
     y = "Rendimiento"
   )+
   theme_minimal()+
   geom_text(aes(label="PMV",min(riskCov,na.rm = T),0.001024032),vjust=0.3,hjust=-0.3)+
   geom_point( 
    mapping = aes(min(riskCov,na.rm = T),0.001024032),
    size=4,
    color="brown")+
  geom_text(aes(label="MVClasic",x = mean(sd(x %*% w)),y = mean(Delt(pftPerf(x,w))[-1])),vjust=-0.4,hjust=-0.2)+
  geom_point( 
    mapping = aes(mean(sd(x %*% w)),mean(Delt(pftPerf(x,w))[-1])),
    size=4,
    color="red")+
  geom_text(aes(label="MVRobust",x = mean(sd(x %*% wr)),y = mean(Delt(pftPerf(x,wr))[-1])),vjust=1,hjust=-0.1)+
  geom_point( 
    mapping = aes(mean(sd(x %*% wr)),mean(Delt(pftPerf(x,wr))[-1])),
    size=4,
    color="blue")





## Muestro lo que pasa al diversificar una cartera

# 2 acitvos
t=0.5
s=0.5
r=0
p=0
o=0
Rend.p.2 <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
var.p.2<- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)

# 3 activos
t=0.33
s=0.33
r=0.34
p=0
o=0
Rend.p.3 <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
var.p.3<- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)

# 4 activos
t=0.25
s=0.25
r=0.25
p=0.25
o=0
Rend.p.4 <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
var.p.4<- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)

# 5 activos
t=0.2
s=0.2
r=0.2
p=0.2
o=0.2
Rend.p.5 <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
var.p.5<- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)

# 6 activos
t=0.166
s=0.166
r=0.166
p=0.166
o=0.166
Rend.p.6 <- t*mu_1 + s*mu_2 + r*mu_3 + o*mu_4 + p*mu_5 + (1-t-s-r-o-p)*mu_6
var.p.6<- (t^2*sigma_1^2+s^2*sigma_2^2+r^2*sigma_3^2+o^2*sigma_4^2+p^2*sigma_5^2+(1-t-s-r-o-p)^2*sigma_6^2+2*t*s*cov_12+2*t*r*cov_13+2*t*o*cov_14+2*t*p*cov_15+2*t*(1-t-s-r-o-p)*cov_16+2*s*r*cov_23+2*s*o*cov_24+2*s*p*cov_25+2*s*(1-t-s-r-o-p)*cov_26+2*r*o*cov_34+2*r*p*cov_35+2*r*(1-t-s-r-o-p)*cov_36+2*o*p*cov_45+2*o*(1-t-s-r-o-p)*cov_46+2*p*(1-t-s-r-o-p)*cov_56)

matriz.diversifiacion=matrix(c(Rend.p.2,var.p.2,
                               Rend.p.3,var.p.3,
                               Rend.p.4,var.p.4,
                               Rend.p.5,var.p.5,
                               Rend.p.6,var.p.6),5,byrow = T)
rownames(matriz.diversifiacion)=c("2 activos","3 activos","4 activos","5 activos","6 activos")
colnames(matriz.diversifiacion)=c("Rendimiento", "Varianza")
matriz.diversifiacion

ggplot()+
  geom_point(aes(x=c(2,3,4,5,6),matriz.diversifiacion[,2]),
             size=4,
             color=c("navyblue","forestgreen","coral","brown","orange"))+
  geom_text(aes(label=rownames(matriz.diversifiacion),x=c(2,3,4,5,6),matriz.diversifiacion[,2]),vjust=-0.9)+
  labs(title = "Criterio de diversificacion",
       y="Riesgo",
       x="Activos")+
  ylim(0.00049,0.0011)+
  xlim(1.7,6.3)
  

