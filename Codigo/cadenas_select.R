## Selección de las ramas por cadena productiva


### Agrupamiento 1

```{r}
cl1 <- cargasPCA %>% select(rama, nom_rama, RC1) %>% arrange(-abs(RC1))
cl1
```

### Agrupamiento 2

```{r}
cl2 <- cargasPCA %>% select(rama, nom_rama, RC2) %>% arrange(-abs(RC2))
cl2
```

### Agrupamiento 3

```{r}
cl3 <- cargasPCA %>% select(rama, nom_rama, RC3) %>% arrange(-abs(RC3))
cl3
```

### Agrupamiento 4

```{r}
cl4 <- cargasPCA %>% select(rama, nom_rama, RC4) %>% arrange(-abs(RC4))
cl4
```

### Agrupamiento 5

```{r}
cl5 <- cargasPCA %>% select(rama, nom_rama, RC5) %>% arrange(-abs(RC5))
cl5
```

### Agrupamiento 6

```{r}
cl6 <- cargasPCA %>% select(rama, nom_rama, RC6) %>% arrange(-abs(RC6))
```

### Agrupamiento 7

```{r}
cl7 <- cargasPCA %>% select(rama, nom_rama, RC7) %>% arrange(-abs(RC7))
```

### Agrupamiento 8

```{r}
cl8 <- cargasPCA %>% select(rama, nom_rama, RC8) %>% arrange(-abs(RC8))
```

### Agrupamiento 9

```{r}
cl9 <- cargasPCA %>% select(rama, nom_rama, RC9) %>% arrange(-abs(RC9))
```

### Agrupamiento 10

```{r}
cl10 <- cargasPCA %>% select(rama, nom_rama, RC10) %>% arrange(-abs(RC10))
```

### Agrupamiento 11

```{r}
cl11 <- cargasPCA %>% select(rama, nom_rama, RC11) %>% arrange(-abs(RC11))
```

### Agrupamiento 12

```{r}
cl12 <- cargasPCA %>% select(rama, nom_rama, RC12) %>% arrange(-abs(RC12))
```

### Agrupamiento 13

```{r}
cl13 <- cargasPCA %>% select(rama, nom_rama, RC13) %>% arrange(-abs(RC13))
```

### Agrupamiento 14

```{r}
cl14 <- cargasPCA %>% select(rama, nom_rama, RC14) %>% arrange(-abs(RC14))
```

### Agrupamiento 15

```{r}
cl15 <- cargasPCA %>% select(rama, nom_rama, RC15) %>% arrange(-abs(RC15))
```

### Agrupamiento 16

```{r}
cl16 <- cargasPCA %>% select(rama, nom_rama, RC16) %>% arrange(-abs(RC16))
```

### Agrupamiento 17

```{r}
cl17 <- cargasPCA %>% select(rama, nom_rama, RC17) %>% arrange(-abs(RC17))
```

### Agrupamiento 18

```{r}
cl18 <- cargasPCA %>% select(rama, nom_rama, RC18) %>% arrange(-abs(RC18))
```

### Agrupamiento 19

```{r}
cl19 <- cargasPCA %>% select(rama, nom_rama, RC19) %>% arrange(-abs(RC19))
```

### Agrupamiento 20

```{r}
cl20 <- cargasPCA %>% select(rama, nom_rama, RC20) %>% arrange(-abs(RC20))
```

### Agrupamiento 21

```{r}
cl21 <- cargasPCA %>% select(rama, nom_rama, RC21) %>% arrange(-abs(RC21))
```

### Agrupamiento 22

```{r}
cl22 <- cargasPCA %>% select(rama, nom_rama, RC22) %>% arrange(-abs(RC22))
```

### Agrupamiento 23

```{r}
cl23 <- cargasPCA %>% select(rama, nom_rama, RC23) %>% arrange(-abs(RC23))
```

### Agrupamiento 24

```{r}
cl24 <- cargasPCA %>% select(rama, nom_rama, RC24) %>% arrange(-abs(RC24))
```

### Agrupamiento 25

```{r}
cl25 <- cargasPCA %>% select(rama, nom_rama, RC25) %>% arrange(-abs(RC25))
```


## Número de municipios por cadena

library(sf)
library(rgeoda)
library(tidyverse)
mun <- sf::st_read("Bases de datos/shp_R/cluster_mun.shp")
q_1 <- rgeoda::queen_weights(mun)

## Cadena industria química
### Modelo cadena química
mcl6 <- lm(data = mun, formula = po ~ pocl6)
### Residuales modelo cadena química
res_cl6<- mcl6[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl6 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl6'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl6=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl6"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl6)
### Conteo de número de municipios por cadena
mun %>% filter(Gcl6>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl6) %>% count()



## Cadena industria Eléctrico-electrónica
### Modelo cadena Eléctrico-electrónica
mcl8 <- lm(data = mun, formula = po ~ pocl8)
### Residuales modelo cadena química
res_cl8<- mcl8[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl8 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl8'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl8=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl8"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl8)



## Cadena industria Textiles, confección y prendas de vestir
### Modelo cadena Eléctrico-electrónica
mcl10 <- lm(data = mun, formula = po ~ pocl10)
### Residuales modelo cadena química
res_cl10<- mcl10[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl10 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl10'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl10=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl10"])$lisa_vals))) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl10)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl10>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl10) %>% count()



## Cadena industria construcción
### Modelo cadena construcción
mcl12 <- lm(data = mun, formula = po ~ pocl12)
### Residuales modelo cadena construcción
res_cl12<- mcl12[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl12 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl12'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl12=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl12"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl12)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl12>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl12) %>% count()


## Cadena industria Autotransportes
### Modelo cadena Autotransportes
mcl13 <- lm(data = mun, formula = po ~ pocl13)
### Residuales modelo cadena Autotransportes
res_cl13<- mcl13[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl13 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl13'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl13=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl13"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl13)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl13>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl13) %>% count()


## Cadena industria Papel e impresión
### Modelo cadena Papel e impresión
mcl15 <- lm(data = mun, formula = po ~ pocl15)
### Residuales modelo cadena Autotransportes
res_cl15<- mcl15[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl15 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl15'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl15=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl15"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl15)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl15>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl15) %>% count()


## Cadena industria Plásticos y fibras sintéticas
### Modelo cadena Plásticos y fibras sintéticas
mcl17 <- lm(data = mun, formula = po ~ pocl17)
### Residuales modelo cadena Autotransportes
res_cl17<- mcl17[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl17 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl17'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl17=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl17"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl17)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl17>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl17) %>% count()


## Cadena industria Muebles y productos de madera
### Modelo cadena Muebles y productos de madera
mcl20 <- lm(data = mun, formula = po ~ pocl20)
### Residuales modelo cadena Muebles y productos de madera
res_cl20<- mcl20[["residuals"]]
### Añadir residuales a la base original
mun<-res_cl20 %>% cbind(mun)
### Añadir nombres a la columna con los residuales
colnames(mun)[1]<-'res_cl20'
### Estimación de índice Gi*: cambiar los nombres de res_clX y GclX
mun<-mun %>% 
  mutate(Gcl20=as.vector(scale(rgeoda::local_gstar(w = q_1,mun[,"res_cl20"])$lisa_vals)*-1)) %>%
  dplyr::select(CVEGEO,CVE_ENT,CVE_MUN,NOMGEO,Gcl20)
### Conteo de número de municipios por cadena: cambiar los nombres de res_clX y GclX
mun %>% filter(Gcl20>=1.96) %>% st_drop_geometry() %>% select(CVEGEO,NOMGEO,Gcl20) %>% count()
### Qué municipios
mun %>% filter(Gcl20>=1.96) %>% st_drop_geometry() %>% select(NOMGEO)

