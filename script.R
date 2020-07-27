

# Cargo librerias e instalamos paquetes
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)   
library(tidytext)
library(metR)
library(magrittr)
install.packages("tidyverse")
install.packages("metR")
install.packages("megrittr")

# Cargo los datos a la variable peliculas
peliculas <- as.data.frame(read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv"))

#Generalidades de los datos cargados
str(peliculas)       # Se muestra un paneo general de lo que contiene la variable peliculas
                     # como cantidad de observaciones y variables, asi como tipo de variables
summary(peliculas)   # Resumen de todas las variables del data.frame.
View(peliculas)      


# ALGORTIMO GENEROS: 
# Se detallan, de manera separada, cada uno de los generos que aparecen en el data.frame
# debido a que hay peliculas que pertenecen a mas de un genero
res <- NULL                             # Se incializa el arreglo de caracteres resultado
ngen <- strsplit(peliculas$genero, ", ")# Se guarda en ngen todos los generos que contiene cada pelicula
for (i in 1:length(ngen)){              # Se recorre ngen fila por fila
  for (j in 1:length(ngen[[i]])) {      # Luego de entrar en la fila, se recorre cada genero de la fila
    if (!ngen[[i]][j]%in%res){          # Si ya no se agrego al conjunto resultado, se agrega
      gen <- as.character(ngen[[i]][j]) # Primero se convierte a caracter
      res <- c(res,gen)                 # Se agrega el genero al resultado
    }
  }
}
res
 

# A continuacion se realiza una descripción de las variables a tomar en cuenta para 
# el estudio, usando las medidas de resumen que se creen convenientes

#------------------------------- ANALISIS UNIVARIADO -----------------------------------#
# VARIANZA
varianzaAnio <-var(peliculas$anio)
varianzaAnio

varianzaDuracion <-var(peliculas$duracion)
varianzaDuracion

varianzaGanancias <-var(peliculas$ganancias[!is.na(peliculas$ganancias)])
varianzaGanancias


# DESVIACION ESTANDAR
desvAnio <- sd(peliculas$anio)
desvAnio

desvDuracion <- sd(peliculas$duracion)
desvDuracion 

desvGanancias <- sd(peliculas$ganancias[!is.na(peliculas$ganancias)])
desvGanancias


# COEFICIENE DE VARIACION
# Por definicion el coeficiente de variacion es el desvio estandar dividido el valor absoluto de la media 
mediaA <- mean(peliculas$anio)
mediaD <- mean(peliculas$duracion)
mediaG <- mean(peliculas$ganancias[!is.na(peliculas$ganancias)])

coefvarA <- desvAnio/mediaA
coefvarA

coefvarD <- desvDuracion/mediaD
coefvarD

coefvarG <- desvGanancias/mediaG
coefvarG


# COEFICIENTE DE SIMETRIA/ASIMETRIA
# Para este coeficiente se que carga la libreria moments
library(moments)
a <- skewness(peliculas$anio) 
a
 
d <- skewness(peliculas$duracion) 
d

g <- skewness(peliculas$ganancias[!is.na(peliculas$ganancias)]) 
g


# COEFICIENTE DE CURTOSIS
kurtosisA <- kurtosis(peliculas$anio)
kurtosisA

kurtosisD <- kurtosis(peliculas$duracion)
kurtosisD

kurtosisG <- kurtosis(peliculas$ganancias[!is.na(peliculas$ganancias)]) 
kurtosisG


#----------------------------------- ALGUNOS GRAFICOS -----------------------------------#
# DIAGRAMA DE BARRAS: AÑO

ggplot(peliculas, aes(x=anio))+
  geom_point(stat="count", color="#005824")+
  geom_line(stat="count", width=0.2,color="#005824")+
  labs(title= "Diagrama de dispersión conectado: Año", x="Año", y="Cantidad de peliculas")


# DIAGRAMA DE BARRAS: DURACIÓN
ggplot(peliculas) + 
   geom_bar(aes(x=duracion), fill= "#005824", color="#005824") + 
   labs(title= "Diagrama de barras: Duración", x= "Duración", y="Cantidad de películas")


# DIAGRAMA DE BARRAS: GANANCIA
ggplot(peliculas)+ 
  geom_bar(aes(x=ganancias), color="#005824")+ labs (title= "Diagrama de barras: Ganancia", x= "Ganancia", y="Cantidad de peliculas" ) +
   scale_x_log10()


# DIAGRAMA DE BARRAS: CANTIDAD DE PELÍCULAS POR GÉNERO
# Se creara una tabla con dos columnas, la primera que contenga los generos y la segunda la
# cantidad de peliculas que hay en cada uno de ellos. Luego se realizara el diagrama de barras
# correspondiente
#Este algoritmo es necesario ya que cada pelicula contiene MAS DE UN GENERO asociado y vimos necesario SEPARARLOS.
tabla=tibble("genero"=character(),"cantPelis"=integer()) #creo tabla tipo tibble, que va a tener 2 columnas
cont <- 0
for(i in 1:length(res)){               # Este for, recorre los 21 generos
  genero <- as.character(res[i])       # Se guarda el primer genero
  for(j in 1:nrow(peliculas)){         # Este for, recorre las peliculas
    genPrimPel <- peliculas$genero[j]  # Se guarda lista de generos por pelicula
    ngen <- strsplit(genPrimPel, ", ") # Se separa de la coma y se queda con los generos sueltos
    for(k in 1:length(ngen[[1]])){     # Se recorre esos generos de la pelicula
      if (ngen[[1]][k]==genero){       # Se compara si el genero de la lista, es igual al primero elegido
        cont <- cont+1                 # Si es igual entonces sumo uno al contador de pelis de ese genero
      }
    }
  }
  tabla <- tabla%>%add_row(genero=genero, cantPelis= cont) # Se agrega a la tabla final, la fila con
                                                           # genero y cantidad de pelis de ese genero
  cont <- 0                                                # Se pone el contador en 0 porque se va a 
                                                           # arrancar con otro genero
}

data <- as.data.frame(tabla) #se convierte el tibble en data frame
data

#se crea grafico de cantidad de peliculas por genero
ggplot(data) + 
    geom_col(aes(x=(reorder(genero, cantPelis)), y=cantPelis), fill="#005824") +
    theme(axis.text.x= element_text(angle=50, face="bold"), text = element_text(size=10)) +
    labs(title= "Cantidad de peliculas por genero", x= "Género", y="Cantidad de peliculas")


#--------------------------------------- ANALISIS BIVARIADO ---------------------------------------#
# Grafico anio y duracion
ggplot(peliculas) + geom_point(aes(x=anio, y =duracion))

pairs(peliculas$anio~peliculas$duracion)
# Observando los graficos se aprecia que no hay correlacion y si la hay es muy baja
# pero para estar seguros se a analiza el coeficiente de correlacion

cor(peliculas$anio, peliculas$duracion)
# La correlacion al dar muy cercano a cero, indica que no hay correclacion entre 
# dichas variables, como  supusimos


#anio ganancias
pairs(peliculas$anio~peliculas$ganancias)
# Analizando este otro grafico se puede decir que si hay relacion pero que la misma no 
# parece ser lineal, si no mas bien logaritmica o exponencial.
# Nuevamente se comprueba con el coef de correlacion
# Anteriormente se noto que habian NAs en las ganancias, asi que primero los elimino

cor(peliculas$anio, peliculas$ganancias, method = "pearson", use = "pairwise.complete.obs")
# Nuevamente da un numero muy cercano al cero asi que se puede decir que no hay correacion lineal



#--------------------------------------- PREGUNTAS PROBLEMAS ---------------------------------------#
################ PREGUNTA 1 (categorica-categorica) ################
# ¿Generalmente los directores se inclinan por realizar siempre el mismo género de películas? 
# Ordenamos las peliculas de forma decreciente por la frecuencia absoluta de directores

pelisOrdenadas <- data.frame(sort(table(peliculas$direccion), decreasing=TRUE)) 

#10 directores con mas peliculas
primeros <- head(pelisOrdenadas[1], 10) 

generos <- peliculas%>%               # En genero, se guarda una fila por pelicula de esos 10 directores
  select(direccion, genero)%>%        # Interesa guardar director y genero
  filter(direccion%in%primeros[,1])   # Se filtran las peliculas de los 10 directores a estudiar

generosOrdenados<-data[order(-data$cantPelis),] #Se obtiene la lista de generos ordenados segun cantidad de peliculas por genero

# Se crea tabla tipo tibble, que va a tener 3 columnas
tabla=tibble("director"=character(), "genero"=character(),"cantPelis"=integer()) #creo tabla tipo tibble, que va a tener 3 columnas
cont <- 0
#Este algoritmo es necesario ya que en la misma fila de la pelicula se presenta MAS DE UN GENERO, y aqui se separan.
#El algoritmo retorna en tabla, tres columnas, una con el director de la pelicula, otro con
#el genero de la pelicula y otro con la cantidad de peliculas en ese genero para ese director
for(i in 1:nrow(primeros)){                                 # este for recorre los directores elegidos
  director <- as.character(primeros[i,1])                   # se guarda el primer director
  for(j in 1:length(generosOrdenados[,1])){                 # este for, recorre los generos
    primGen <- generosOrdenados[j,1]                        # se guarda el primer genero
    for(k in 1:length(generos[[1]])){                       # ahora que tenemos director y genero, recorremos cada fila que contiene mas de un genero por pelicula
      ngen <- strsplit(generos[k,2], ", ")                  # guardamos en ngen, todos los generos de la pelicula de la fila actual
      for (m in 1:length(ngen[[1]])) {                      # recorremos cada genero suelto
        if (generos[k,1]==director&&ngen[[1]][m]==primGen){ # comparamos esos generos, con el primer genero guardado y comparamos el director actual con el director guardado
          cont <- cont+1                                    # si se cumplen las condiciones, entonces se encontro una pelicula del director elegido y del genero elegido
        }
      }
    }
    tabla <- tabla%>%add_row(director=director,genero=primGen, cantPelis= cont) # agregamos a la tabla final, la fila con director, genero y cantidad de pelis de ese genero
    cont <- 0                                                                   # ponemos el contador en 0 para volver a arrancar con otro genero
  }
}

data1 <- as.data.frame(tabla) #se convierte el tibble en data frame

# Se realiza el MAPA DE CALOR correspondiente con los generos ordenados en orden decreciente
#de cantidad de peliculas
ggplot(data1) + 
  geom_tile(aes(director, reorder(genero, cantPelis), fill = cantPelis))+
  scale_fill_gradient(low= 'white', high = '#005824') +
  theme(axis.text.x = element_text(angle=30)) +
  labs(x="Directores", y= "Géneros", title= "Relación entre directores y género")


################ PREGUNTA 2 (Categorica-numerica) ################
#¿El género incide en las ganancias de las películas?


tabla=tibble("genero"=character(), "ganancias"=double()) # Se crea tabla tipo tibble, que va a tener 2 columnas

#Para esta parte tambien se utilizo un algoritmo similar. El cual cuenta genero a genero recorriendo
#las peliculas, ya que en cada pelicula se presenta MAS DE UN GENERO asociado.
#El algoritmo retorna una tabla con dos columnas, en una el genero y en otra las ganancias de ese genero para las 10.000 peliculas
for(i in 1:length(res)){                                 # Este for, recorre los 21 generos
  genero <- as.character(res[i])                         # Se guarda el primer genero
  for(j in 1:nrow(peliculas)){                           # Este for, recorre las peliculas
    genPrimPel <- peliculas$genero[j]                    # Se guarda lista de generos por pelicula
    ngen <- strsplit(genPrimPel, ", ")                   # Se separa de la coma y se queda con los generos sueltos
    for(k in 1:length(ngen[[1]])){                       # Se recorre esos generos de la pelicula
      if (ngen[[1]][k]==genero){                         # Se compara si el genero de la lista, es igual al primero elegido
        if (!is.na(peliculas$ganancias[j])){
          tabla <- tabla%>%add_row(genero=genero, ganancias=peliculas$ganancias[j]) # Se agrega a la tabla final, la fila con genero y cantidad de pelis de ese genero
        }
      }
    }
    
  }
}

data2 <- as.data.frame(tabla) #se convierte el tibble en dataframe

ggplot(data2, aes(x=ganancias, y=reorder(genero, ganancias))) +     #se grafica ganancias y genero en diagrama de cajas
  geom_boxplot(color='#005824') +
  labs(y="Género", x= "Ganancias", title= "Diagrama de cajas: Ganancias según género")




################ PREGUNTA 3 (Numerica-numerica) ################
#¿El transcurso de los años modifica la duración de una película?

#PRIMER GRÁFICO QUE REALIZAMOS

ggplot(peliculas)+ geom_line(aes(x=anio, y=duracion), color="#005824")

df <- as.data.frame(tapply(peliculas$duracion, peliculas$anio, mean)) #se crea data drame con la media de la duracion de las peliculas agrupadas por año
anios <- c(1915,1916,1919:2018) #se genera el recorrido de los anios
media <- NULL
for(i in 1:length(anios)){ #se recorren los anios
   media <- c(media,df[[i,1]])  #se guardan las medias en un vector
}

data3 <- data.frame(anios,media) #se crea dataframe a graficar

ggplot(data3, aes(x=anios, y=media)) + #se crea grafico de lineas
  geom_line(color="darkolivegreen4")



#----------PARTE 4--------------#
woddy<-subset(peliculas, subset=direccion%in%c('Woody Allen')) #se crea subconjunto de peliculas de woddy allen

ggplot(woddy, aes(x=anio, y=duracion)) + #se crea grafico de anio y duracion con la recta de regresion correspondiente
  geom_point() + geom_smooth(method="lm",col="darkolivegreen4",size=0.8, se=FALSE)

ggplot(woddy, aes(x=anio, y=puntaje)) +  #se crea grafico de anio y puntaje con la recta de regresion correspondiente
  geom_point() + geom_smooth(method="lm",col="Red", size=0.8,se=FALSE)

cor(woddy$anio,woddy$duracion) #se halla el valor de correlacion de pearson





