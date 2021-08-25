#Para ejecutar muchas librerias recomiendo pac-man.
#La base de datos se recoge de la pagina oficial del Ministerio de ciencias
#Ejecutar Linea a linea Para obtener resultados.


library(pacman)
p_load(readr,tidyverse,patchwork,reshape2,jsonlite,geojsonio,RColorBrewer,sf,
       geogrid,cartography)

#datos min ciencia -------------------------------------------------

datos<-read.csv2("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto25/CasosActualesPorComuna.csv",
                 sep = ",", encoding = "UTF-8")

# Limpieza datos y format  ----------------------------------------------------------
{
  datos<- datos %>% 
    gather(variable, valor, c(6:146), na.rm = T) %>% 
    na.omit(datos)
  
  datos<- datos %>% 
    mutate(variable=gsub('\\.', '-', datos$variable)) 
  datos<- datos %>% 
    mutate(variable= substring(variable, 2,11)) %>% 
    mutate(variable= as.Date.character(variable, "%Y-%m-%d"))
  
  datos<-datos %>% 
    mutate(Poblacion= as.numeric(datos$Poblacion)) %>% 
    mutate(valor= as.numeric(datos$valor))
}
# filtro ultima actualization ---------------------------------------------
{
df<-datos %>% 
  filter(variable=="2021-08-16") %>% 
  rename("codigo_comuna"=Codigo.comuna) 
df[,4]<- as.numeric(df$codigo_comuna)
}

# Carga masiva de topojson Regiones Chile------------------------------------------------

link<- "https://raw.githubusercontent.com/pachadotdev/chilemapas/master/data_topojson/comunas/"
regiones<- c("r01", "r02", "r03", "r04", "r05", "r06", "r07", "r08",
             "r09", "r10", "r11", "r12", "r13", "r14", "r15", "r16")
archivos<-{}

for (link_1 in regiones){
      regi<-paste0("https://raw.githubusercontent.com/pachadotdev/chilemapas/master/data_topojson/comunas/", regiones, ".topojson")
      lista<- regi
        for(i in lista){
        print(i)
        archivos[[i]] <- topojson_read((i))
        as.vector(archivos[[i]])
      }
    break
}

archivos<- map(archivos,. %>% mutate_at(2, as.numeric))
names(archivos)<- regiones

# un solo topojson gracias a reduce y left join ---------------------------------------

test<-reduce(archivos, rbind)

test[,2]<-as.numeric(test$codigo_comuna)

join<- df %>% 
  left_join(test, df, by="codigo_comuna")


# Para graficar solo aplico cambio de nombre en filtro y en labs.
#Para ejemplo creare 2 objetos. Maule y Biobio.

maule<-join %>% 
  filter(Region == "Maule") %>% 
  ggplot(aes(fill=valor, group=Comuna, geometry= geometry)) +
  geom_sf(color="black", size=0.1) +
  geom_sf_text(aes(label = Comuna, geometry= geometry), size=2.5, colour = "black") +
  scale_fill_gradientn(colours = brewer.pal(5,"Purples"), name = "Nº de Casos") +
  labs(title = "Región de Maule", subtitle = "Casos actuales al 16-08-2021",
       caption = "Fuente: Ministerio de Ciencias",
       x="", y="") +
  theme_minimal(base_size = 13)

maule

biobio <- join %>% 
  filter(Region == "Biobio") %>% 
  ggplot(aes(fill=valor, group=Comuna, geometry= geometry)) +
  geom_sf(color="black", size=0.1) +
  geom_sf_text(aes(label = Comuna, geometry= geometry), size=2.5, colour = "black") +
  scale_fill_gradientn(colours = brewer.pal(5,"Purples"), name = "Nº de Casos") +
  labs(title = "Región del Biobio", subtitle = "Casos actuales al 16-08-2021",
       caption = "Fuente: Ministerio de Ciencias",
       x="", y="") +
  theme_minimal(base_size = 13)



graficos <- maule + biobio # puedo unir imagenes gracias a patchwork

graficos






