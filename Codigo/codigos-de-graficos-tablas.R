library(gtsummary)
library(gt)
library(readxl)
library(readr)
library(dplyr)        
library(tidyr)        
library(stringr) 
library(ggplot2)
library(forcats)
library(ggtext)  
library(gghighlight) 
library(showtext)
library(ggthemes)
library(plotrix)
library(zoom)
library(countrycode)
library(gcookbook)
remove.packages("vctrs")
install.packages("vctrs")
library(vctrs)
datos <- NULL
datos <- read_csv("/Users/joelf/OneDrive/Documentos/let/Trabajo-Let/Datos/terremotos_sin_procesar.csv", show_col_types = FALSE)

v <- data.frame(datos$Date,datos$Time,datos$Latitude,datos$Longitude,datos$Type,datos$Magnitude)



library(sf)
library(rnaturalearth)
library(mregions)
sf_use_s2(FALSE)

# Con st_join() con una capa de paises --------------------------------------

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% 
  st_make_valid()

join_pais <- df_sp %>% 
  st_join(world, join = st_nearest_feature)
show_col_types = FALSE

p <- join_pais["name"]
p <- data.frame(p$name)
setwd("/Users/joelf/OneDrive/Documentos/let/Trabajo-Let/Datos")
write.table(p,file="País.csv",sep=",",row.names = F)

q <- read_csv("/Users/joelf/OneDrive/Documentos/let/Trabajo-Let/Datos/País.csv", show_col_types = FALSE)

v <- cbind(v,q)
#cambiar nombre de variables
names(v)= c("Fecha", "Hora","Latitud","Longitud", "Tipo", "Magnitud", "País")

#Traducir nombres de paises----

v[v=="Fr. S. Antarctic Lands"] <- "Tierras Antárticas Francesas"
v[v=="S. Sudan"] <- "South Sudan"
v[v=="Somaliland"] <- "Somalia"
v <- v |> 
  mutate(País = countrycode(sourcevar = País,
                            origin = "country.name",
                            destination = "cldr.name.es"),
         .after = País) 


######## se dejan las variables que se quieren ocupar, tiempo, latitud, longitud, magnitud, tipo y pais de origen



####### se trasforma hora de terremoto a hora, para saber a que hora fue (como funcion parte entera)
v$Hora <- format(as.POSIXct(v$Hora), format = "%H")
v$Hora <- as.numeric(v$Hora)

####### tambien se trasforma las fechas a años, para asi saber cuantos terremotos hay a medida que pasa el tiempo
v$Fecha <- as.numeric(substring(v$Fecha,first = 7))

#terremotos en cantidad segun el tiempo
#grafico 1----
table(v$Fecha)

v |> 
  filter(Tipo=="Earthquake") |> 
  ggplot(aes(x=Fecha))+
  geom_bar(fill = "#79d0cc", size =1) +
  labs(title = "Cantidad de terremotos en el mundo por año",
       subtitle = "período entre 1965 y 2016",
       y ="Cantidad",
       x = "Años")+
  scale_x_continuous(breaks = seq(1965, 2016, by = 5))+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "Black",size=2)

#se puede apreciar que el 2010 fue el año con mas terremotos de grado mayor o igual a 5,5
#una respuesta para este fenomenos es que los sismos registrados corresponden a replicas originadas por
# el terremoto del 27 de febrero

which(is.na(v$Fecha))
v$Fecha[20651] <- 2011
v$Fecha[7513] <- 1985 
v$Fecha[3379] <- 1975
#grafico 2----
v |> 
  filter(Tipo=="Earthquake", País=="Chile") |> 
  ggplot(aes(x=Fecha))+
  geom_bar(fill = "#79d0cc", size = 1)+ 
labs(title = "Cantidad de terremotos que ocurren en Chile a medida que pasa el tiempo",
     subtitle = "período entre 1965 y 2016",
     y ="Cantidad",
     x = "Años")+
  scale_x_continuous(breaks = seq(1965, 2016, by = 5))+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "Black",size=2)
#nuevamente hubo mas terremotos en el 2010 debido a las replicas del 27f


v <- cbind(v,rep(1,length(v$Fecha)))
rep(1,length(v$Fecha))

names(v)[8]="y"
v <- v[,c(-8,-9)]
#cantidad de terremotos con magnitud mayor a 8 durante el tiempo
#grafico3----
#terremotos de magnitud mayor a 8 entre 1965 y 2016
v |> 
 filter(tipos=="Earthquake", Magnitud>8) |> 
  ggplot(aes(x=Fecha, y=y, size = Magnitud))+
  geom_point(shape = 21, colour = "black", fill = "white",stroke = 1, aes(size=Magnitud)) +
  labs(title = "Terremotos de magnitud mayor a 8 en todo el mundo",
       subtitle = "período entre 1965 y 2016",
       x = "Años")+
  geom_hline(yintercept = 1) + # agrego una línea
  labs(y = NULL) + # elimino el nombre del eje
  theme(axis.text.y = element_blank(), #elimino los números del eje
        axis.ticks.y = element_blank())+# elimino las marcas donde van los números
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# ver los años en que ocurrio entre 1970 y 1990, decir que entre esos años no hubieron de mayor a 8




#grafico4----
v |> 
  filter(Tipo=="Earthquake") |> 
  ggplot(aes(Magnitud)) +
  geom_histogram()+
  labs(title = "Número de sismos en el mundo según la magnitud",
       subtitle = "período entre 1965 y 2016", y="Cantidad")

datos[3379,]
v <- v[-7513,]
v <- v[-3379,]
v <- v[-20649,]
any(is.na(v$Hora))


#grafico 5----
# cantidad de ocurrencias de terremotos segun la hora
v |> 
  filter(Tipo=="Earthquake") |> 
  ggplot(aes(x=Hora))+
  geom_bar(color="black",fill="#FF7F50")+
  theme_bw()+
  labs(title = "Número de sismos en el mundo según la hora en que se origina",
       subtitle = "período entre 1965 y 2016", y="Cantidad", x= "Hora (0-24)")
  
#se puede apreciar que no existe una hora especifica en la que sucedan mas terremotos



table(v$Fecha)






########################################################################################################
#tablas

#install.packages("gtsummary")
#install.packages("gt")

library(gtsummary)
library(gt)


#encontrar maximos en magnitud por año

mayores <- v[order(v$Magnitud, decreasing = TRUE),]
mayores<- mayores[c(1,2,3,4,5),]
mayores <- mayores[,c(1,6,7)]


#tabla 5 mayores terremotos en el periodo estudiado
gt(mayores)

class(datos$Date)

#tabla presentacion de datos

nombres <- c("Fecha","Hora","Latitud","Longitud",
          "Magnitud","País") 
data.frame(Variables= nombres, 
           Tipo = c("Fecha",
                    "Tiempo",
                    "Numérico",
                    "Numérico",
                    "Numérico",
                    "Carácter"),
           Descripción = c(
             "Muestra la fecha en que ocurrio el sismo",
             "Entrega la hora en que se originó un terremoto",
             "Señala la latitud como cordenada geografica ",
             "Señala la longitud como cordenada geografica",
             "Es la magnitud en escala Richter de los terremotos",
             "Corresponde al país en el que sucedio el sismo") ) |> 
  gt() |> 
  tab_header(title = "Descripción de variables") |> 
  tab_source_note(source_note = "Fuente: Kaggle.com")




ggsave("Figuras/Tabla_01.png", width = 10, height = 7)  










