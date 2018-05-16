#' ---
#' title: "S4-Diseño Factorial (DF)"
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
datos<-read.csv('Datos/DF.csv',header=T,sep=';')
attach(datos)
# Para verificar la correcta importación de los datos se llama por el nuevo nombre:datos





###############################ANÁLISIS EXPLORATORIOS#####################################################
# Análisis exploratorio: Boxplot superpuesto con stripchart
# Genera un gráfico de bigotes buscando tendecias (tendecia central, dispersión y simetría)
boxplot(Peso~Dosis+Supp,las=2, xlab='Tratamiento',ylab='Peso')
stripchart(Peso~Dosis+Supp,vert=T,pch=21,col='red',add=T)

# Genera un histograma de la variable respuesta
hist(Peso,prob=T)
lines(density(Peso))

# Antes de ajustar el modelo lineal, se debe escoger el factor, ya que la variable aparece como numérica. 
Dosis<-as.factor(Dosis)

# Ajuste de modelo lineal general
# Este modelo relaciona la variable respuesta en función de un solo factor
# Pueden existir dos formas de hacerlo: con el comando lm o aov
modelo1<-lm(Peso~Supp+Dosis)
modelo2<-aov(Peso~Supp+Dosis)
# Para la visualización de los parámetros etimados con le modelo:
summary(modelo1)
summary(modelo2)


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
boxplot(residuales~Supp+Dosis, data=datos, ylab="Residuales", xlab="Tratamiento", main="Homogeneidad de residuales") 

#Gráfico de residuales dispersos por tratammiento
#Nota: Aunque se requiere el paquete lattice, este ya se encuentra activo por un paso anterior

stripplot(residuales~Supp+Dosis, data=datos, ylab="Residuales", xlab="Tratamiento", main="Homogeneidad de residuales")

# Homocedasticidad prueba de Levene 

leveneTest(Peso~interaction(Supp,Dosis))

#NOTA: Recuerde que los residuales deben cumplir con los supuestos necesarios
#para que el ANOVA sea válido. En caso de que el análisis de residuales muestre
#problemas a este respecto, la variable respuesta debe ser transformada hasta
#que los residuales del modelo ajusten a los supuestos. 


# Tabla ANOVA
anova(modelo1)


################################COMPARACIONES MÚLTIPLES##################################################


# Tukey HSD
HSD.test(modelo1,"Supp", group=TRUE,console = T)


HSD.test(modelo1,"Dosis", group=TRUE, console = T)




########################## Ajuste modelo interactivo###########################

modelo2<-lm(Peso~Supp*Dosis)

      # Supuestos
        # Normalidad
        shapiro.test(modelo2$residuals)  
      # Homogeneidad de varianzas
      
        leveneTest(Peso~interaction(Supp,Dosis))


# ANOVA
anova(modelo2)
HSD.test(modelo2,'Supp',console = T)

# Efectos simples
# Determinaremos el efecto de la dosis a cada nivel de 'Supp'

datosOJ<-subset(datos,Supp=='OJ')

modeloOJ<-lm(Peso~as.factor(Dosis),datosOJ)
shapiro.test(modeloOJ$residuals)
leveneTest(Peso~as.factor(Dosis),datosOJ)

anova(modeloOJ)
HSD.test(modeloOJ,'as.factor(Dosis)',console = T)




datosVC<-subset(datos,Supp=='VC')

modeloVC<-lm(Peso~as.factor(Dosis),datosVC)
shapiro.test(modeloVC$residuals)
leveneTest(Peso~as.factor(Dosis),datosVC)

anova(modeloVC)
HSD.test(modeloVC,'as.factor(Dosis)',console = T)

