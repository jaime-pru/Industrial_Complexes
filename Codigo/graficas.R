grafica_barras<-function(base,variable){

  base %>% 
    setNames(paste0('sec_', names(.)))%>% 
    filter({{variable}}!=0) %>%
    ggplot(aes(x=reorder(sec_cve_rama,{{variable}}),y={{variable}})) + geom_col() + 
    labs(x="",y="") + 
    theme_classic() + 
    coord_flip() #columnas invertidas  
  
}