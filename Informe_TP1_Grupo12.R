library(readxl)
library(tidyverse)
library(ggthemes)
library("Hmisc")
library(corrplot)
library(moments)
library(PASWR)

#Importamos los datos.

datos <- read_excel("C:/Users/Alex/Desktop/Computacion Actuarial/flavors_of_cacao.xlsx", 
                    col_types = c("text", "text", "numeric", 
                                  "numeric", "numeric", "text", "numeric", 
                                  "text", "text"))


View(datos)
datos = as.data.frame(datos)
attach(datos)

datos=datos %>%
  rename(pais=`Company Location`,
         compañia=`Company (Maker-if known)`,
         año=`Review Date`,
         cocoa=`Cocoa Percent`,
         bean.type=`Bean Type`,
         specific.bean.origin=`Specific Bean Origin or Bar Name`,
         broad.bean.origin=`Broad Bean Origin`)

str(datos)

#Identificamos la cantidad de datos faltantes para cada columna.

#Convierto los espacios en blanco en valores no asignados para ver cuantos hay
datos[datos == as.character(datos[2,8])] <- NA
attach(datos)


matriz.na=matrix(c(sum(is.na(compañia)),
                   sum(is.na(specific.bean.origin)),
                   sum(is.na(REF)),
                   sum(is.na(año)),
                   sum(is.na(cocoa)),
                   sum(is.na(pais)),
                   sum(is.na(Rating)),
                   sum(is.na(bean.type)),
                   sum(is.na(broad.bean.origin))),9)

rownames(matriz.na)=c("compañia","specific.bean.origin","ref","año","cocoa","pais","rating","bean.type" ,"broad.bean.origin")
colnames(matriz.na)=c("sum.NA")
matriz.na 


#Comenzamos con la visualizacion de los datos.

#1 Suma de rating por pais.

pais.freq <- datos %>%
  group_by(pais) %>%
  summarise(suma = sum(Rating)) %>%
  arrange(desc(suma))
ggplot(data=pais.freq,aes(x=reorder(pais.freq$pais,pais.freq$suma),y=pais.freq$suma)) + 
  geom_bar(stat ='identity',aes(fill=pais.freq$suma))+coord_flip()+xlab("Paises")+ylab("Suma")+ggtitle("Suma de rating por pais")

#1.1 Frecuencia de pais
pais.freq2 <- datos %>%
  group_by(pais) %>%
  summarise(count = n()) %>%
  arrange(count)
ggplot(data=pais.freq2,aes(x=reorder(pais.freq2$pais,pais.freq2$count),y=pais.freq2$count)) + 
  geom_bar(stat ='identity',aes(fill=pais.freq2$count))+coord_flip()+xlab("Paises")+ylab("Frecuencia")+ggtitle("Tabla de freciencia de los paises")

#1.2 Frecuencia de compañias
compañia.freq <- datos %>%
  group_by(compañia) %>%
  summarise(count = n()) %>%
  arrange(count)
ggplot(data=compañia.freq,aes(x=reorder(compañia.freq$compañia,compañia.freq$count),y=compañia.freq$count)) + 
  geom_bar(stat ='identity',aes(fill=compañia.freq$count))+coord_flip()+xlab("Compañias")+ylab("Frecuencia")+ggtitle("Tabla de frecuencia de las compañias")

#1.3 Frecuencia del origen especifico de granos
bean.freq <- datos %>%
  group_by(specific.bean.origin) %>%
  summarise(count = n()) %>%
  arrange(count)
ggplot(data=bean.freq,aes(x=reorder(bean.freq$specific.bean.origin,bean.freq$count),y=bean.freq$count)) + 
  geom_bar(stat ='identity',aes(fill=bean.freq$count))+coord_flip()+xlab("Compañias")+ylab("Frecuencia")+ggtitle("Tabla de freciencia del origen especifico de granos")

#1.4 Frecuencia del tipo de grano
  #1.4.1 Limpio un poco la variable
  datos$bean.type[grepl(",",datos$bean.type)] <- "Blend"
  datos$bean.type[grepl("Criollo",datos$bean.type)] <- "Criollo"
  datos$bean.type[grepl("Amazon",datos$bean.type)] <- "Amazon"
  datos$bean.type[grepl("Forastero",datos$bean.type)] <- "Forastero"
  datos$bean.type[grepl("Trinitario",datos$bean.type)] <- "Trinitario"
  datos$bean.type[grepl("Nacional",datos$bean.type)] <- "Naional"
  attach(datos)
  
  datos1 = datos[!is.na(datos$bean.type),]
  
bean.freq2 <- datos1 %>%
  group_by(datos$bean.type) %>%
  summarise(count = n()) %>%
  arrange(count)
ggplot(data=bean.freq2,aes(x=reorder(bean.freq2$`datos$bean.type`,bean.freq2$count),y=bean.freq2$count)) + 
  geom_bar(stat ='identity',aes(fill=bean.freq2$count))+coord_flip()+xlab("Compañias")+ylab("Frecuencia")+ggtitle("Tabla de freciencias del tipo de granos")

#1.5 Frecuencia del origen amplio de los granos
  #1.5.1 Limpio un poco la variable
  datos2 = datos[!is.na(datos$broad.bean.origin),]

bean.freq3 <- datos2 %>%
  group_by(broad.bean.origin) %>%
  summarise(count = n()) %>%
  arrange(count)
ggplot(data=bean.freq3,aes(x=reorder(bean.freq3$broad.bean.origin,bean.freq3$count),y=bean.freq3$count)) + 
  geom_bar(stat ='identity',aes(fill=bean.freq3$count))+coord_flip()+xlab("Compañias")+ylab("Frecuencia")+ggtitle("Tabla de freciencia del origen amplio de granos")

#1.6 Histograma de los años
hist(año, xlab = "Años", ylab = "Frecuencia", main = "Histograma de la variable año")

#1.7 Histograma de REF
hist(REF, xlab = "REF", ylab = "Frecuencia", main = "Histograma de la variable REF")

#2  Promedio de rating por pais.

pais.promedio<-datos%>%
  group_by(pais) %>%
  summarise(promedio = mean(Rating)) %>%
  arrange(desc(promedio))
ggplot(data=pais.promedio,aes(x=reorder(pais.promedio$pais,pais.promedio$promedio),y=pais.promedio$promedio)) + 
  geom_bar(stat ='identity',aes(fill=pais.promedio$promedio))+coord_flip()+xlab("Paises")+ylab("Promedio")+ggtitle("Promedio de rating por pais")
mean(Rating)


#3 Promedio de cocoa por pais.

pais.cocoa<-datos %>%
  group_by(pais) %>%
  summarise(promedio = mean(cocoa)) %>%
  arrange(desc(promedio))
ggplot(data=pais.cocoa,aes(x=reorder(pais.cocoa$pais,pais.cocoa$promedio),y=pais.cocoa$promedio,fill=pais.cocoa$promedio)) + 
  geom_bar(stat ='identity')+coord_flip()+xlab("Paises")+ylab("Promedio")+ggtitle("Promedio de rating por pais")+theme_minimal()


#4 Compañias con mayor rating.

cocoa.m=c()
origen=c()
mejores.comp=c()
for (i in 1:nrow(datos)) {
  if(Rating[i]>=4){origen=c(origen,specific.bean.origin[i]) 
  mejores.comp=c(mejores.comp,compañia[i])
  cocoa.m=c(cocoa.m,cocoa[i])
  }
}

view(matrix(c(origen,mejores.comp,cocoa.m),100,3))

mejores.datos=matrix(c(origen,mejores.comp,cocoa.m),100,3)

colnames(mejores.datos)=c("Specific bean origin","Compañia","Cocoa")
view(mejores.datos)

mean(cocoa.m)


#5 Compañias con peor rating.

cocoa.p=c()
origen.2=c()
peores.comp=c()
for (i in 1:nrow(datos)) {
  if(Rating[i]<=2){origen.2=c(origen.2,specific.bean.origin[i]) 
  peores.comp=c(peores.comp,compañia[i])
  cocoa.p=c(cocoa.p,cocoa[i])
  }
}

view(matrix(c(origen.2,peores.comp,cocoa.p),length(origen.2),3))

peores.datos=matrix(c(origen,mejores.comp,cocoa.m),100,3)
colnames(peores.datos)=c("Origen","Compañia","Cocoa")

view(peores.datos)

mean(cocoa.p)

cbind(mean(cocoa.m),mean(cocoa.p)) 


#6 Rating=5

rating.max=c()
origen.max=c()
for (i in 1:length(Rating)) {
  if(Rating[i]==5){
    rating.max=c(rating.max,pais[i])
    origen.max=c(origen.max,specific.bean.origin[i])
  }
}

matriz.max.rating=matrix(c(rating.max,origen.max),2)
colnames(matriz.max.rating)=c("Pais","Specific bean origin") 

matriz.max.rating

#7 Promedio de rating por origen especifico.
origen.especifico.promedio<-datos %>%
  group_by(specific.bean.origin) %>%
  summarise(promedio = mean(Rating)) %>%
  arrange(desc(promedio))
view(origen.especifico.promedio)
head(origen.especifico.promedio)

#Promedio de Rating por origen amplio del grano
#Limpio el dataset
datos3 = datos[!is.na(datos$broad.bean.origin),]

bean.promedio<-datos3%>%
  group_by(broad.bean.origin) %>%
  summarise(promedio = mean(Rating)) %>%
  arrange(desc(promedio))
ggplot(data=bean.promedio,aes(x=reorder(bean.promedio$broad.bean.origin,bean.promedio$promedio),y=bean.promedio$promedio)) + 
  geom_bar(stat ='identity',aes(fill=bean.promedio$promedio))+coord_flip()+xlab("Origenes de los Granos")+ylab("Promedio")+ggtitle("Promedio de rating por origenes de granos")

#Analisis.

#1
summary(datos)

#Momentos centrados
matriz.dispersion=matrix(c(sd(Rating),sd(cocoa), var(Rating), var(cocoa),skewness(Rating),skewness(cocoa),kurtosis(Rating),kurtosis(cocoa)),2) 
colnames(matriz.dispersion)=c("Desvio","Varianza","Asimetria","curtosis")
rownames(matriz.dispersion)=c("Rating","Cocoa")
matriz.dispersion

#Momentos Absolutos

matriz.dispersion2=matrix(c(mean(Rating), mean(cocoa), mean(Rating**2), mean(cocoa**2), mean(Rating**3), mean(cocoa**3), mean(Rating**4), mean(cocoa**4)),2) 
colnames(matriz.dispersion2)=c("Momento Absoluto 1","Momento Absoluto 2","Momento Absoluto 3","Momento Absoluto 4")
rownames(matriz.dispersion2)=c("Rating","Cocoa")

matriz.dispersion2


#2 EDA

EDA(Rating)
EDA(cocoa)

#Shapiro test - Analizar normalidad.
shapiro.test(Rating) # Se rechaza h0, no tiene una distribucion normal.
shapiro.test(cocoa)  # Se rechaza h0, no tiene una distribucion normal.

par(mfrow=c(1,2))
hist(cocoa,col="light blue",labels = T)
hist(Rating,col="red",labels=T)


#Identificacion de outliers.

par(mfrow=c(2,1)) 
boxplot(cocoa,horizontal = T,col="light blue",main="Boxplot Cocoa",xlab="Cocoa")
boxplot(Rating,horizontal = T,col="red",main="Boxplot rating",xlab="Rating")
outliers1=identify(Rating,rep(1,length(Rating))) 
outliers2=identify(cocoa,rep(1,length(cocoa)))   

valores.outliers1=c()
for (i in 1:length(outliers1)) {
  valores.outliers1=c(valores.outliers1,Rating[outliers1[i]])
  
}
valores.outliers1 #Valores de rating atipicos. 

valores.outliers2=c()
for (i in 1:length(outliers2)) {
  valores.outliers2=c(valores.outliers2,cocoa[outliers2[i]])
  
}
round(valores.outliers2,2) #Valores de cocoa atipicos. 

colours()
par(mfrow=c(2,1))
boxplot(cocoa.centralizado,horizontal = T,col="salmon",main="Boxplot Cocoa",xlab="Cocoa")
boxplot(Rating.centralizado,horizontal = T,col="wheat",main="Boxplot rating",xlab="Rating")


#3 Relaciones entre variables.

rcorr(as.matrix(datos[,c(3,4,5,7)]))
correlacion=cor(datos[,c(5,7,3,4)])
corrplot(correlacion, type = "upper",tl.col = "black", tl.srt = 45)

chisq.test(table(cocoa,Rating))

cor.test(REF,Rating) 
tapply(REF,año,summary) #Correlacion alta, hay mayor cantidad de entradas año tras año.
cor.test(REF,año)

tapply(Rating,año,summary) #Muestra que no hay una gran mejora en rating con el transcurrir de los años.
cor.test(Rating,año)

tapply(cocoa,round(Rating),summary) #Muestra que hay una correlacion negativa entre cocoa y rating.
cor.test(cocoa,Rating)
ggplot(data = datos) +
  geom_point(mapping = aes(x = Rating, y = cocoa, color=cocoa))



#Ejercicio 2

#Punto 1. Convierto las variables en 1 o 0 si su rating es 5 o distinto de 5 respectivamente

datos.copy = datos
for(i in 1:length(datos.copy$Rating)){
  if(datos.copy[i,7] < 5){
    datos.copy[i,7] = 0
  } else {
    datos.copy[i,7] = 1
  }
}


#Armo una lista con el dataset original y el nuevo dataset con rating modificado

list1 <- list(datos,datos.copy)


#Punto 2.A Armo un dataset con variables dummies y transformo la variable cocoa a binaria.


library(tidytable)

datos.copy2 = datos.copy 

#Cocoa

attach(datos)

datos.copy[datos.copy >= 0.4 & datos.copy < 0.60] = "40% a 60%"
datos.copy[datos.copy >= 0.60 & datos.copy < 0.70] = "60% a 70%"
datos.copy[datos.copy >= 0.70 & datos.copy < 0.75] = "70% a 75%"
datos.copy[datos.copy >= 0.75 & datos.copy < 0.80] = "75% a 80%"
datos.copy[datos.copy >= 0.80 & datos.copy < 0.85] = "80% a 85%"
datos.copy[datos.copy >= 0.85 & datos.copy < 1] = "85% a 100%"
datos.copy$cocoa[datos.copy$cocoa==1] = "85% a 100%"
datos.copy = datos.copy[!is.na(datos.copy$cocoa),]



datos.dummies1 = cbind(datos.copy2["Rating"],datos.copy2[,c(5,6)])

datos.dummies2 = cbind(datos.copy["Rating"],get_dummies.(datos.copy[,c(5,6)]))
datos.dummies2 = datos.dummies2[,-c(2,3)]#Saco las columnas repetidas


str(datos.dummies2)


#Punto 2.B Armo un dataset con variables agrupadas segun promedio de rating por pais

pais.promedio<-datos %>%
  group_by(pais) %>%
  summarise(promedio = mean(Rating)) %>%
  arrange(desc(promedio))


grupo=c()
for (i in 1:length(pais.promedio$promedio)) {
  if(pais.promedio$promedio[i]>3.25){
    grupo=c(grupo,1)}else{
      if(pais.promedio$promedio[i]<2.75){
        grupo=c(grupo,3)}else{
          grupo=c(grupo,2)
        }
    }
}


pais.promedio = cbind(pais.promedio, grupo)

attach(datos)

columna=c()
columna2=c()
for (i in 1:length(pais)) {
  for (j in 1:length(pais.promedio$pais)) {
    if(pais[i]==pais.promedio$pais[j]){
      columna=c(columna,pais.promedio$grupo[j])
      columna2=c(columna2,pais[i])
    }
  }
}

view(cbind(columna,columna2))


datos.con.grupos = datos

datos.con.grupos <- cbind(datos.con.grupos, grupo = columna)

#Armo una lista con el dataset con las variables dummies junto al dataset con los grupos

list2 <- list(datos.con.grupos,datos.dummies1,datos.dummies2)



#Punto 3. Separo los datasets en dos partes (70/30). Una parte de entrenamiento y una de validacion
#Primero lo hago para el dataset con las variables dummies

#Cocoa continua
datos.dummies1.train = datos.dummies1[1:round(nrow(datos.dummies1)*0.7),]
datos.dummies1.test = datos.dummies1[round(nrow(datos.dummies1)*0.7):nrow(datos.dummies1),]

#Cocoa binaria
datos.dummies2.train = datos.dummies2[1:round(nrow(datos.dummies2)*0.7),]
datos.dummies2.test = datos.dummies2[round(nrow(datos.dummies2)*0.7):nrow(datos.dummies2),]


#Ahora lo hago para el dataset con los grupos

datos.grupos.train = datos.con.grupos[1:round(nrow(datos.con.grupos)*0.7),]
datos.grupos.test = datos.con.grupos[round(nrow(datos.con.grupos)*0.7):nrow(datos.con.grupos),]


#Armo una lista compuesta de tres listas, dos con ambas poblaciones para los dataset con las variables dummies y otra con ambas poblaciones para el dataset con los grupos

list3 <- list( list(datos.dummies1.train,datos.dummies1.test),list(datos.dummies2.train,datos.dummies2.test),list(datos.grupos.train,datos.grupos.test))

#Punto 4.A Realizar un modelo para el dataset con las variables dummies, para la poblacion de entrenamiento

#cocoa continua
model_d1 = glm(Rating ~cocoa+as.factor(pais), data = datos.dummies1.train, family = binomial(link = "logit") )
summary(model_d1) 

#cocoa binaria
model_d2 = glm(Rating ~., data = datos.dummies2.train, family = binomial(link = "logit") )
summary(model_d2)

#Veo como predicen los modelos

#cocoa continua
predict_d1 = cbind(predict.glm(model_d1,datos.dummies1.test),datos.dummies1.test$Rating)

#cocoa bianria
predict_d2 = cbind(predict.glm(model_d2,datos.dummies2.test),datos.dummies2.test$Rating)

#Punto 4.B Realizar un modelo para el dataset con los grupos, para la poblacion de entrenamiento
model_c = lm(Rating~cocoa+grupo,datos.grupos.train)
summary(model_c)

#Comparo con test
predict_c = cbind(predict.lm(model_c,datos.grupos.test[,c("cocoa", "grupo")]), datos.grupos.test$Rating)

#Analizamos algunas metricas para los modelos

anova(model_c)
plot(model_c)

library(car)

# Test de Durbin Watson

durbinWatsonTest(model_c)

# Hay evidencia para rechazar H0, por lo que los residuos estan correlacionados.

# Test de Jarque Bera

jarque.test(model_c$residuals)

#el P-value es menor al alpha, hay evidencia para rechazar la h0 por lo que los residuos no distribuyen normal.

 

library(performance)

rmse = c(rmse(model_d1),rmse(model_d2), rmse(model_c))

R2 = c(r2(model_d1), r2(model_d2), r2(model_c))




