library(readxl)
library(readr)
#install.packages("dplyr")
library(dplyr)        
library(tidyr)        
library(stringr) 
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggtext)  
library(gghighlight) 
library(showtext)
library(ggthemes)
#install.packages("datos")


datos <- read_csv("Datos/terremotos_sin_procesar.csv")



#geolocalizacion inversa---
#install.packages("maps")

library(maps)

startm <- Sys.time()

country<-map.where(database="world", 
                   datos$Longitude, datos$Latitude)

endm <- Sys.time()

country
my.na <- is.na(country)
table(my.na)
###################### otra forma de geolocalizacion inversa
install.packages("tidygeocoder")
library(tidygeocoder)
reverse <- datos %>%
  reverse_geocode(lat = Latitude, long = Longitude, method = 'osm',
                  address = address_found, full_results = TRUE) 




##################################################################
library(tidygeocoder)
reverse <- datos[1:10,] %>%
  reverse_geocode(lat = Latitude, long = Longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

reverse










######## se dejan las variables que se quieren ocupar, tiempo, latitud, longitud, magnitud y profundidad


v <- datos[1:9]
v <- v[,-8]
v <- v[,-5]
v <- v[,-6]
v <- cbind(v,country)
tipos <- datos$Type
v <- cbind(v,tipos)
####### se trasforma hora de terremoto a hora, para saber a que hora fue (como funcion parte entera)
v$Time <- format(as.POSIXct(v$Time), format = "%H")
v$Time <- as.numeric(v$Time)

####### tambien se trasforma las fechas a años, para asi saber cuantos terremotos hay a medida que pasa el tiempo
v$Date <- as.numeric(substring(v$Date,first = 7))
table(v$Date)


#terremotos en cantidad segun el tiempo
v |> 
  filter(tipos=="Earthquake") |> 
  ggplot(aes(x=Date))+
  geom_bar(color = "#79d0cc", size = 1) 

#a medida que pasa el tiempo han subido la cantidad de terremotos


  


#cantidad de terremotos con magnitud mayor a 8 durante el tiempo
v |> 
  filter(tipos=="Earthquake", Magnitude>8) |> 
  ggplot(aes(x=Date, y=Magnitude))+
  geom_point(color = "Black", size = 2) 

#como dato curioso entre 1971 hasta 1989 no hubieron terremotos con magnitud mayor a 8





# cantidad de ocurrencias de terremotos segun la hora
v |> 
  filter(tipos=="Earthquake") |> 
  ggplot(aes(x=Time))+
  geom_bar(color = "green") 

#se puede apreciar que no existe una hora especifica en la que sucedan mas terremotos




##### relacion entre tiempo y profundida
v |> 
  filter(tipos=="Earthquake") |> 
  ggplot(aes(x=Depth, y=Magnitude))+
  geom_point(color = "black", size=0.5) 

#los terremotos por sobre 8 grados son ocasionados a menos de 100km de profundidad



########################################################################################################
#tablas

#install.packages("gtsummary")
#install.packages("gt")

library(gtsummary)























