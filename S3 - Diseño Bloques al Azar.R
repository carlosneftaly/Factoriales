#' ---
#' title: "S3 - Diseño de Bloques Al Azar (DBA)"
#' author: "Nombre y apellido"
#' date: "Mes día,  año"
#' ---


# Nota: recuerde registrar sus datos con las variables en cada columna y las
# observaciones en las filas. La primera fila debe contener los nombres de las
# variables y se debe comenzar en la primera celda (A1)

# Cargar paquetes para análisis
library(lattice)
library(car)
library(multcomp)
library(agricolae)
# Colocar otros paquetes

# Selección directorio de trabajo y carga de datos: 
# La ubicación cambia según la carpeta donde se fijo el directorio de trabajo. 


# Carga de archivo .csv: 
datos<-read.csv('Datos/DBA.csv',header=T,sep=';')
attach(datos)
# Para verificar la correcta importación de los datos se llama por el nuevo nombre:datos


# Este comando reorganiza los tratamientos, con el fin de que cuando se hace la prueba de Dunett se comparen
# los tratamientos contra el control
datos$Tratamiento<-relevel(datos$Tratamiento, ref='M1') #VERIFICAR CUAL ES EL CONTROL

###############################ANÁLISIS EXPLORATORIOS#####################################################
# Análisis exploratorio: Boxplot superpuesto con stripchart
# Genera un gráfico de bigotes buscando tendecias (tendecia central, dispersión y simetría)
boxplot(Esporas~Tratamiento+Bloque,las=2)
stripchart(Esporas~Tratamiento+Bloque,vert=T,pch=21,col='red',add=T)

# Genera un histograma de la variable respuesta
hist(Esporas,prob=T)
lines(density(Esporas))


# Ajuste de modelo lineal general
# Este modelo relaciona la variable respuesta en función de un solo factor
# Pueden existir dos formas de hacerlo: con el comando lm o aov
modelo1<-lm(Esporas~Tratamiento+Bloque)
modelo2<-aov(Esporas~Tratamiento+Bloque)



################################EVALUACIÓN DE SUPUESTOS#############################################
##EVALUACIÓN DE NORMALIDAD##
#Se continuará trabajando con modelo1 (lm)
shapiro.test(modelo1$residuals)
# Análisis gráfico del modelo
# El comando "par" genera panales 2*2
par(mfrow=c(2,2))
# Gráfico de cuantiles
plot(modelo1)

# Con el comando "par" se vuelve a generar panel de 1*1
par(mfrow=c(1,1))


##EVALUACIÓN DE HOMOGENEIDAD DE VARIANZAS##
##Extraccion de los residuales
residuales<-resid(modelo1)
#Gráfico de cajas y bigotes
boxplot(residuales~Tratamiento, data=datos, ylab="Residuales", xlab="Tratamiento", main="Homogeneidad de residuales") 

#Gráfico de residuales dispersos por tratammiento
#Nota: Aunque se requiere el paquete lattice, este ya se encuentra activo por un paso anterior

stripplot(residuales~Tratamiento, data=datos, ylab="Residuales", xlab="Tratamiento", main="Homogeneidad de residuales")

# Homocedasticidad prueba de Levene 

leveneTest(Esporas~interaction(Tratamiento,Bloque))

#NOTA: Recuerde que los residuales deben cumplir con los supuestos necesarios
#para que el ANOVA sea válido. En caso de que el análisis de residuales muestre
#problemas a este respecto, la variable respuesta debe ser transformada hasta
#que los residuales del modelo ajusten a los supuestos. 



# Tabla ANOVA
anova(modelo1)

summary(modelo1)
summary(modelo2)

################################COMPARACIONES MÚLTIPLES##################################################

#  Dunett
summary(glht(modelo1,linfct=mcp(Tratamiento="Dunnett")))

# Tukey HSD
HSD.test(modelo1,"Tratamiento", group=TRUE, console = T)


# LSD
LSD.test(modelo1,"Tratamiento",p.adj="bon",console=TRUE)

# Cambiar 'Tratamiento' por el nombre del factor, tal cual como se definió en el modelo.
