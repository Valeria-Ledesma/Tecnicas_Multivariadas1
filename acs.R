######################################
# Análisis de Correspondencia Simple #                  
######################################
# Para limpiar el workspace
rm(list=ls())
dev.off()
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#-------------------------------------------------
# Otras opciones
options(scipen=999)
options(digits = 3)
#-------------------------------------------------
# Paquetes
library(pacman)
p_load(MASS, ca, anacor,FactoMineR,vegan,
       gplots,vcd,graphics, factoextra,foreign)

#######################
# 1. Ingreso de Datos #                  
#######################

#                 Opinión
# Renta      Bueno Malo Regular
# Bajo        75   40      35
# Medio       60   50      70
# Alto        20   40      30
# Muy Alto    15   40      25
datos.acs <- matrix(c(75,40,35,
                      60,50,70,
                      20,40,30,
                      15,40,25),nrow=4,byrow=T) #ncol=3
datos.acs 
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos.acs)<- list(renta=c("Bajo","Medio","Alto",
                                   "Muy Alto"),
                           opinion=c("Bueno","Malo","Regular"))
datos.acs
datos.acs[2,1]
# Añadir totales
addmargins(datos.acs)
##################################################
# 3. Visualización de una Tabla de Contingecia   #
#            usando una Matriz Gráfica           #
##################################################

# Primera forma - Balloomplots
library(gplots)

# Convertir los datos en una tabla
dt <- as.table(datos.acs)
dt
str(dt)

# Para graficarlo con % fila (perifles fila)
dt <- prop.table(dt,margin=1) 
#si no se coloca margin, se gráfican % de tabla o fr
dt

balloonplot(t(dt), 
            main ="Gráfico Opinión Renta", 
            xlab ="Opinión", 
            ylab="Renta",
            label = F, cum.margins=F, 
            label.lines=F, show.margins = FALSE)
#con label=T se indican los valores
#---------------------------------------------------
# Segunda forma - Mosaicos
library(graphics)
mosaicplot(t(dt),shade=F)

###########################################
# 4. Prueba de Independencia Chi-Cuadrado # 
###########################################
prueba <- chisq.test(datos.acs)
prueba
# Ho: las variables son independientes
# H1: las variables son dependientes
# la prueba debe rechazarse, ser significativa
#Valor p???alfa: Las variables tienen una asociación estadísticamente
#significativa (Rechazar H0)
#Valor p>alfa: No se puede concluir que las variables están 
#asociadas (No se puede rechazar H0)
prueba$observed
prueba$expected

# Frecuencia Relativa (fij)
#dato/total
prop.table(datos.acs)

# Perfiles Fila
#dato/sumfila
prop.table(datos.acs, 1) 

# Perfiles Columna
prop.table(datos.acs, 2) 

# Tabla con el paquete gmodels y función CrossTable()
library(gmodels)
CrossTable(datos.acs,
           prop.t=F, #Frecuencia Relatica
           prop.r=F, #Perfil Fila
           prop.c=F, #Perfil Columna
           prop.chisq=FALSE)
#en el gráfico 1°Perfil fila, 2°Perfil colummna, 3°Fr

###########################################
# 5.Análisis de Correspondencias Simple   #
#         con el paquete FactoMiner       #
###########################################
library(FactoMineR) 
#3 filas y 4 columnas min(3,4)-1 -> ncp=2
res.ca <- CA(datos.acs,ncp=2,graph=F)
res.ca 

#--------------------------------------------------------------
# Scree Plot de los Autovalores
res.ca$eig
# Otra forma
library(factoextra)
eig.val <- get_eigenvalue(res.ca)
eig.val

fviz_screeplot(res.ca)
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 90))

# Con la funcion plot() sobre un objeto de clase ca se obtiene el 
# biplot. 

# Gráficos- Biplot

# Primera forma - usando plot.CA de FactoMineR

plot.CA(res.ca) # Mapa Simétrico
plot.CA(res.ca, axes = c(1,2), col.row = "blue", col.col = "red")
plot.CA(res.ca,mass=c(T,T))

# Segunda forma - usando fviz_ca_biplot de factoextra
fviz_ca_biplot(res.ca, repel = T)
# Elegir distintos temas
library(ggthemes)
library(tvthemes)
fviz_ca_biplot(res.ca, repel = T) + theme_minimal()
fviz_ca_biplot(res.ca, repel = T) + theme_light()
fviz_ca_biplot(res.ca, repel = T) + theme_void()
fviz_ca_biplot(res.ca, repel = T) + theme_gray()
fviz_ca_biplot(res.ca, repel = T) + theme_bw()
fviz_ca_biplot(res.ca, repel = T) + theme_stata()
fviz_ca_biplot(res.ca, repel = T) + theme_simpsons()

#---------------------------------------------
# Interpretación de los Indicadores del ACS

summary(res.ca,nb.dec = 3, ncp = 2) 
#nb.dec= # de decimales

#Call:
#CA(X = datos.acs, ncp = 2, graph = F) 

#The chi square of independence between the two variables is equal
#to 40 (p-value =  0.000000445 ).

#Eigenvalues
#                       Dim.1   Dim.2
#Variance               0.065   0.015
#% of var.             81.279  18.721
#Cumulative % of var.  81.279 100.000
#
# Rows
#            Iner*1000    Dim.1    ctr   cos2    Dim.2    ctr   cos2  
# Bajo     |    34.375 | -0.322 47.655  0.903 | -0.106 22.341  0.097
# Medio    |     9.485 | -0.027  0.391  0.027 |  0.160 61.558  0.973
# Alto     |    13.219 |  0.268 19.803  0.975 | -0.043  2.178  0.025
# Muy Alto |    23.019 |  0.362 32.151  0.909 | -0.114 13.923  0.091
#
# Columns
#            Iner*1000    Dim.1    ctr   cos2    Dim.2    ctr   cos2  
# Bueno    |    40.923 | -0.344 61.919  0.985 | -0.042  4.081  0.015
# Malo     |    26.667 |  0.253 33.467  0.817 | -0.120 32.533  0.183
# Regular  |    12.509 |  0.097  4.614  0.240 |  0.172 63.386  0.760

# Resultados extendidos, tomar las coordenadas, contribuciones 
# absolutas y relativas.
# En summary se tienen dos indicadores importantes: 
# Contribución Absoluta (ctr)
# Contribución Relativa (cos2)

# Interpretación de la Contribución Absoluta (ctr)
# la columna ctr siempre suma 1
# 47.655+0.391+19.803+32.151
# 61.919+33.467+4.614
# Por ejemplo: para la fila BAJO y la dimensión 1 se tiene 
# una ctr = 47.655
# El 47,65% de la inercia de la dimensión 1 es explicada 
# por la fila BAJO
# 
# Interpretación de la Contribución Relativa (cos2)
# Por ejemplo: para la fila BAJO y la dimensión 1 se tiene 
# una cos2 = 0.903
# El 90,3% de la inercia de la fila BAJO es explicada 
# por la dimensión 1
#
# Interpretación de Iner*1000
# Por ejemplo:El 34.375% de la inercia de la fila BAJO es 
# explicado para el total

## PROPIEDAD ##
# Inercia Total = (Chi cuadrado)/(Total de la Tabla)
40/500
0.0651+0.015

##################################################################
# Coordenadas de las Dimensiones para filas y columnas 

row <- get_ca_row(res.ca)
col <- get_ca_col(res.ca)

head(row$coord)
head(col$coord)

# Gráficos de las contribuciones absolutas de las filas y 
# columnas a cada dimensión

head(row$contrib)
head(col$contrib)

fviz_contrib(res.ca, choice = "row", axes = 1)
fviz_contrib(res.ca, choice = "row", axes = 2)

fviz_contrib(res.ca, choice = "col", axes = 1)
fviz_contrib(res.ca, choice = "col", axes = 2)


# Gráficos de las contribuciones relativas de cada dimensión

head(row$cos2)
head(col$cos2)

fviz_cos2(res.ca, choice = "row", axes = 1)
fviz_cos2(res.ca, choice = "row", axes = 2)

fviz_cos2(res.ca, choice = "col", axes = 1)

###########################################
# 6.Análisis de Correspondencias Simple   #
#         de una base de datos            #
###########################################

library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                   use.value.labels = T,  
                   to.data.frame=TRUE)

attach(datos)   

table(nrodepen)
table(dpto)

addmargins(table(dpto,nrodepen))
datos.acs1 <- as.matrix(table(dpto,nrodepen))
datos.acs1

library(FactoMineR) 
res.ca1 <- CA(datos.acs1,ncp=5,graph=FALSE)
res.ca1$eig

fviz_screeplot(res.ca1)
#según el gráfico 2 variables son suficientes
res.ca1 <- CA(datos.acs,ncp=2,graph=FALSE)
summary(res.ca1,nb.dec=3,ncp=2)
# Analizar las que estan por encima del promedio
100/6

fviz_ca_biplot(res.ca1, repel = T)

#################################
# 7.ACS con el paquete anacor   #
#################################
library(anacor)
fit2 <- anacor(datos.acs)
str(fit2)

summary(fit2)

plot(fit2,plot.type="jointplot")
plot(fit2)

################################
# 8.ACS con el paquete vegan   #
################################
library(vegan)
corres2 <- cca(datos.acs)
summary(corres2)
fviz_cos2(res.ca, choice = "col", axes = 2)

###############################################
#  9.ACS con filas y columnas suplementarias  #
###############################################
#Nueva fila, nueva columna
#             E1	E2	E3	E4	E5	E6	E7	E8	E9	Ideal
# Precios	    16	17	18	19	16	45	15	19	18	45
# Variedad	   8	15	18	17	27	20	 2	14	53	53
# Rapidez	    20	20	23	21	29	20	18	19	25	29
# Información	11	13	12	17	20	16	15	10	44	44
# Trato	      28	25	25	22	30	26	24	22	26	30
# Condiciones	21	21	20	24	27	22	18	21	24	27
# Acceso	    21	21	21	23	26	15	16	18	21	26

datos_s.acs <- matrix(c(16,17,18,19,16,45,15,19,18,45,
                        8,15,18,17,27,20, 2,14,53,53,
                        20,20,23,21,29,20,18,19,25,29,
                        11,13,12,17,20,16,15,10,44,44,
                        28,25,25,22,30,26,24,22,26,30,
                        21,21,20,24,27,22,18,21,24,27,
                        21,21,21,23,26,15,16,18,21,26),
                      nrow=7,byrow=T)
datos_s.acs
# Asignación de nombres a las filas y columnas de la tabla
dimnames(datos_s.acs)<-list(Atributos=c("Precios", "Variedad", 
                                        "Rapidez", "Información",
                                        "Trato","Condiciones",
                                        "Acceso")
                            ,Empresa=c("Empresa 1","Empresa 2",
                                       "Empresa 3","Empresa 4",
                                       "Empresa 5","Empresa 6",
                                       "Empresa 7","Empresa 8",
                                       "Empresa 9","Ideal"))
datos_s.acs
addmargins(datos_s.acs)
#-----------------------------------------------------------------
# Prueba de Independencia Chi-Cuadrado
prueba <- chisq.test(datos_s.acs[,-10])
prueba
#se rechaza Ho pvalor<
#¿si no hay dependencia? 
#se puede realizar el análisis de correspondencia
#el perfil medio no depende del análisis de fila o columna 
#todas las variables se comportan igual que el promedio

#-----------------------------------------------------------------

# ACS con el paquete FactoMiner
library(FactoMineR)
res.ca.s <- CA(datos_s.acs,
               ncp=2,
               graph=FALSE,
               col.sup = 10) #Indica la columna suplementaria [10]
#-----------------------------------------------------------------
# Scree Plot de los Autovalores
library(ggplot2)
library(factoextra)
get_eigenvalue(res.ca.s)

fviz_screeplot(res.ca.s, addlabels = TRUE, ylim = c(0, 80))

# Interpretación de los Indicadores del ACS
summary(res.ca.s,nb.dec = 3, ncp = 2) 
#empresas 100/9
#atributos 100/7
# La empresa 9 esta asociada con la variedad y la información
# Empresa 3 y Empresa 4 su perfil columna es similar a su perfil medio

# Biplot filas, columnas y columna suplementaria
fviz_ca_biplot(res.ca.s, repel = T) + theme_light()

############################
# 10. Contraejemplo de ACS #
############################
datos.acs.c <- matrix(c(25,30,10,
                        30,30,5,
                        35,20,15,
                        40,15,20,
                        25,10,15),nrow=5,byrow=T)
datos.acs.c
dimnames(datos.acs.c) <- list(renta=c("Marca 1", "Marca 2", 
                                      "Marca 3", "Marca 4", 
                                      "Marca 5")
                              ,opinion=c("Adulto","NSE A/B",
                                         "Auto"))
datos.acs.c
prueba <- chisq.test(datos.acs.c)
prueba


res.ca.c <- CA(datos.acs.c,ncp=2,graph=FALSE)

eig.val <- get_eigenvalue(res.ca.c)
eig.val


fviz_screeplot(res.ca)

# Gráficos- Biplot
# Usando fviz_ca_biplot de factoextra
fviz_ca_biplot(res.ca.c, repel = T)

#--------------------------------------------------------------
# Interpretación de los Indicadores del ACS
summary(res.ca.c,nb.dec = 3, ncp = 2) 

