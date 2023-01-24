# Universidad Autónoma Metropolitana
# Unidad Azcapotzalco
# Licenciatura en Economía
# Econometría Espacial
# Prof. Jaime A. Prudencio Vázquez

## Análisis exploratorio de datos espaciales

## Dos paquetes para el ESDA: i)spdep y ii) rgeoda


### Ruta i: construcción de matrices con `spdep`

#### Paquetes
library(rgdal)
library(spdep)

#### Carga de la base: genera un objeto de tipo Spatial Polygon Dataframe
covid_zmvm <-rgdal::readOGR("bases/covid_zmvm.shp")

#### Matrices de pesos espaciales

#### Argumento queen=TRUE
mTRUE <- spdep::poly2nb(covid_zmvm)
mTRUE

#### Argumento queen=FALSE
mFALSE<- spdep::poly2nb(covid_zmvm, queen = FALSE)
mFALSE

#### Mapa de conectividad
plot(covid_zmvm, border = 'lightgrey')
plot(mTRUE, coordinates(covid_zmvm), add=TRUE, col='lightblue')

#### Mapa de conectividad con las dos estructuras espaciales

plot(covid_zmvm, border = 'lightgrey')
plot(mTRUE, coordinates(covid_zmvm), add=TRUE, col='blue')
plot(mFALSE, coordinates(covid_zmvm), add=TRUE, col='lightgreen')



### Ruta ii: construcción de matrices con `rgeoda`

#### Paquetes
library(sf)
library(rgeoda)

#### Carga de la base de datos: genera un objeto de tipo sf
covid_zmvm_sf <- sf::st_read("bases/covid_zmvm.shp")


#### Matrices de adyacencia
#### Matriz de tipo reina
queen_w <- rgeoda::queen_weights(covid_zmvm_sf)
summary(queen_w)

#### Matriz de tipo torre
rook_w <- rgeoda::rook_weights(covid_zmvm_sf)
summary(rook_w)

#### Matrices basadas en distancia
#### Definición de un umbral mínimo
umbral <- rgeoda::min_distthreshold(covid_zmvm_sf)

#### Matriz de distancia mínima (todas las observaciones tienen al menos un vecino)
dist_w <- rgeoda::distance_weights(covid_zmvm_sf, umbral)
summary(dist_w)

#### Matriz de k-vecinos más cercanos
k4_w <- rgeoda::knn_weights(covid_zmvm_sf, 4)
summary(k4_w)



### Rezagos espaciales con `spdep`

#### Estandarización de la matriz mTRUE
mTRUE.est <- spdep::nb2listw(mTRUE)
mTRUE.est

#### Estandarización de la matriz mFALSE
mFALSE.est <- spdep::nb2listw(mFALSE)
mFALSE.est

#### Construcción del rezago
lag_poshab <- spdep::lag.listw(mTRUE.est, covid_zmvm$pos_hab)
head(lag_poshab)



### Rezagos espaciales con `rgeoda`
lag <- rgeoda::spatial_lag(queen_w, covid_zmvm_sf['pos_hab'])
head(lag)

## Coeficiente de correlación espacial: la I de Moran con `spdep`
spdep::moran.test(covid_zmvm$pos_hab, mTRUE.est)

### Gráfico de Moran
spdep::moran.plot(((covid_zmvm$pos_hab)-mean(covid_zmvm$pos_hab))/(sd(covid_zmvm$pos_hab)),
                  listw = mTRUE.est, 
                  xlab="Casos positivos",
                  ylab="Rezago espacial de los casos positivos",
                  main="Diagrama de Moran para casos positivos",
                  col="lightblue")

## LISA con rgeoda

### Extracción de la variable para el análisis
pos_hab=covid_zmvm_sf["pos_hab"]
### Construcción del objeto que contiene el indicador LISA
lisa_poshab <- rgeoda::local_moran(w=queen_w, df=pos_hab)

### Mapa de cluster
#### Preliminares
##### Colores de cada unidad espacial según su categoría
lisa_colores <- rgeoda::lisa_colors(lisa_poshab)
##### Etiquetas para cada categoría
lisa_etiq <- c("No significativo", "Alto-Alto", "Bajo-Bajo", "Bajo-Alto", "Alto-Bajo", "No definido", "Aislado")
##### Extracción de los valores que definen cada cluster
lisa_clusters <- rgeoda::lisa_clusters(lisa_poshab)

#### Construcción mapa de cluster con sf
plot(sf::st_geometry(covid_zmvm_sf), 
     col=sapply(lisa_clusters, function(x){return(lisa_colores[[x+1]])}), 
     border = "#333333", lwd=0.2)
title(main = "Moran Local de pos_hab")
legend('bottomleft', legend = lisa_etiq, fill = lisa_colores, border = "#eeeeee")


### Mapa de significancia
#### Preliminares
#### Extracción de los valores p asociados a cada observación
lisa_p <- rgeoda::lisa_pvalues(lisa_poshab)
#### Definición de las etiquetas de cada grupo de valores p
p_etiq <- c("No significativo", "p <= 0.05", "p <= 0.01", "p <= 0.001")
#### Definición de los colores de cada grupo de valores p
p_colores <- c("#eeeeee", "#84f576", "#53c53c", "#348124")

# Mapa de significancia: construcción
plot(st_geometry(mun), 
     col=sapply(lisa_p, function(x){
       if (x <= 0.001) return(p_colors[4])
       else if (x <= 0.01) return(p_colors[3])
       else if (x <= 0.05) return (p_colors[2])
       else return(p_colors[1])
     }), 
     border = "#333333", lwd=0.2)
title(main = "Local Moran Map of Crm_prs")
legend('bottomleft', legend = p_labels, fill = p_colors, border = "#eeeeee")