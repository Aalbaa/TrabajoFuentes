#https://github.com/rOpenSpain/climaemet

library(remotes)
#install_github("ropenspain/climaemet")
library(climaemet)
#aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJubXExMDAxQGFsdS51YnUuZXMiLCJqdGkiOiJlOWRmMTc0OC05NjcxLTRiMjctODM2OS01NmYxNTZjNjNjYzIiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY2NjE2NzExNCwidXNlcklkIjoiZTlkZjE3NDgtOTY3MS00YjI3LTgzNjktNTZmMTU2YzYzY2MyIiwicm9sZSI6IiJ9.mIlOZxnREIqfi6hTeh37A_yMnryWyI0NPJzilMMAMmU", install = TRUE)

stations <- aemet_stations() # Need to have the API Key registered

knitr::kable(head(stations))


## Get last observation values 
data_observation <- aemet_last_obs("all")
data_observation


summary(data_observation)


## Get daily/annual climatology values, tabla completa
data_daily <- aemet_daily_period_all(start = 2020, end = 2021) #MIRAR FECHAS COMPARAR CON LO DE ALBA
data_daily
View(data_daily)


# Plot a climate stripes graph for a period of years for a station
library(ggplot2) 

climatestripes_station("2331", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))
climatestripes_station("1505", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))
climatestripes_station("1014", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))


# Plot a windrose showing the wind speed and direction for a station over a days period.
windrose_days(
  "2331",
  start = "2010-01-01",
  end = "2020-12-31",
  n_speeds = 5,
  speed_cuts = c(2.5, 5, 7.5, 10, 12.5, 15)
) +
  theme(plot.title = element_text(size = 10))


#mapa españa con temperaturas maximas la ultima hora del dia de hoy
library(ggplot2)
library(dplyr)

library(climaemet)
aemet_last_obs("9434")

all_stations <- aemet_last_obs(return_sf = TRUE)
# Last hour
all_last <-
  all_stations %>% filter(fint == all_stations[["fint"]][1])

last_hour <- max(all_last$fint)


ggplot(all_last) +
  geom_sf(aes(colour = ta),
          shape = 19,
          size = 2,
          alpha = 0.5
  ) +
  labs(
    title = "Temperature in Spain",
    subtitle = last_hour,
    color = "Max temp.\n(celsius)",
    caption = "Source: AEMET"
  ) +
  scale_colour_gradientn(
    colours = hcl.colors(10, "RdBu", rev = TRUE, alpha = 0.5),
    guide = "legend"
  ) +
  guides(colour = guide_legend(n.breaks = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )


#Para ver la tabla
stations
str(stations)
View(stations) #NO USAR
summary(stations)

#se crea una nueva columna denomicada ccaa a partir de la columna provincia
tablaComunidades <- data_daily %>% 
  mutate(ComunidadesAutónomas = 
           case_when(provincia %in% c("BARCELONA", "TARRAGONA", "GIRONA","LLEIDA") ~ 'CATALUÑA', 
                     provincia %in% c("BURGOS", "SORIA", "SALAMANCA", "LEON","AVILA","PALENCIA","SEGOVIA","VALLADOLID","ZAMORA") ~ 'CyL',
                     provincia %in% c("ASTURIAS") ~ 'ASTURIAS',
                     provincia %in% c("A CORUÑA","LUGO","OURENSE","PONTEVEDRA") ~ 'GALICIA',
                     provincia %in% c("BIZKAIA","GIPUZKOA","ARABA/ALAVA") ~ 'PAIS VASCO',
                     provincia %in% c("CANTABRIA") ~ 'CANTABRIA',
                     provincia %in% c("NAVARRA") ~ 'NAVARRA',
                     provincia %in% c("MADRID") ~ 'MADRID',
                     provincia %in% c("CACERES","BADAJOZ") ~ 'EXTREMADURA',
                     provincia %in% c("CUENCA","GUADALAJARA","TOLEDO","CIUDAD REAL","ALBACETE") ~ 'CLM',
                     provincia %in% c("CORDOBA","HUELVA","CADIZ","GRANADA","JAEN","SEVILLA","ALMERIA","MALAGA") ~ 'ANDALUCIA',
                     provincia %in% c("MELILLA") ~ 'MELILLA',
                     provincia %in% c("ALICANTE","CASTELLON","VALENCIA") ~ 'VALENCIA',
                     provincia %in% c("MURCIA") ~ 'MURCIA',
                     provincia %in% c("TERUEL","HUESCA","ZARAGOZA") ~ 'ARAGON',
                     provincia %in% c("LA RIOJA") ~ 'LA RIOJA',
                     provincia %in% c("ILLES BALEARS") ~ 'ISLAS BALEARES',
                     provincia %in% c("LAS PALMAS","STA. CRUZ DE TENERIFE") ~ 'ISLAS CANARIAS',
                     provincia %in% c("CEUTA") ~ 'CEUTA',
                     
                     
                     
                     ))

View(tablaComunidades)
  
#se agrupan los valores por comunidades autonomas (calculando el valor medio)
tablasAgrupamientos <- tablaComunidades %>%
  group_by(ComunidadesAutónomas) %>%
  summarise(across(c(tmax,tmin, tmed,altitud), ~ mean(.x, na.rm = TRUE)))

View(tablasAgrupamientos)

#grafica temperatura maxima por comunidad autonoma
ggplot(data = tablasAgrupamientos, aes(x =tmax , y = ComunidadesAutónomas))+
  geom_point(aes(colour = ComunidadesAutónomas))+
  labs(x = "Promedio Temperatura", y = "Promedio riqueza de especies",title = 'Temperatura maxima por comunidad autonoma')
  



