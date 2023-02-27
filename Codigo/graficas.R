grafica_barras<-function(base,variable){

  base %>% 
    setNames(paste0('sec_', names(.)))%>% 
    filter({{variable}}!=0) %>%
    ggplot(aes(x=reorder(sec_cve_rama,{{variable}}),y={{variable}})) + geom_col() + 
    labs(x="",y="") + 
    theme_classic() + 
    coord_flip() #columnas invertidas  
  
}



custom_filtro<-function(data_set,umbral=.35){
  
  nom<-names(data_set)[3]
  names(data_set)[3]<-c('Carga')
  
  Resultados<- data_set %>% 
    filter(Carga>umbral) %>%
    mutate(cluster=nom) %>%
    dplyr::select(cluster,nom_rama,rama,Carga)
  Resultados
}


mapa_funcion<-function(data_set,indice){
  
  lisa_colores <- rgeoda::lisa_colors(indice)
  ##### Etiquetas para cada categoría
  lisa_etiq <- lisa_labels(indice)
  ##### Extracción de los valores que definen cada cluster con un 5% de significancia
  lisa_clusters <- rgeoda::lisa_clusters(indice,cutoff = 0.05)
  #### Construcción mapa de cluster con sf
  plot(sf::st_geometry(data_set), 
       col=sapply(lisa_clusters, function(x){return(lisa_colores[[x+1]])}), 
       border = "#333333", lwd=0.1)
  legend('bottomleft', legend = lisa_etiq, fill = lisa_colores, border = "#eeeeee")
  
}



mapa_sig<-function(data_set,indice){
  
  ### Mapa de significancia
  #### Preliminares
  #### Extracción de los valores p asociados a cada observación
  lisa_p <- lisa_pvalues(indice)
  #### Definición de las etiquetas de cada grupo de valores p
  p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
  #### Definición de los colores de cada grupo de valores p
  p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")
  # Mapa de significancia: construcción
  plot(st_geometry(data_set), 
       col=sapply(lisa_p, function(x){
         if (x <= 0.001) return(p_colors[4])
         else if (x <= 0.01) return(p_colors[3])
         else if (x <= 0.05) return (p_colors[2])
         else return(p_colors[1])
       }), 
       border = "#333333", lwd=0.2)
  legend('bottomleft', legend = p_labels, fill = p_colors, border = "#eeeeee")
}














