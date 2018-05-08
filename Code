library(rnaturalearth) # devtools::install_github('ropenscilabs/rnaturalearth')

countries <- ne_countries()

library(rgdal)

countries$longitude <- coordinates(countries)[,1]

countries$latitude <- coordinates(countries)[,2]

countries_xy <- countries@data %>%
  dplyr::select(admin, longitude, latitude)
setnames(countries_xy, "admin", "exporter")
setnames(countries_xy, "longitude", "longitude.x")
setnames(countries_xy, "latitude", "latitude.x")
df3= merge(dfg, unique(countries_xy), by ="exporter")
countries_xy <- countries@data %>%
  dplyr::select(admin, longitude, latitude)
setnames(countries_xy, "admin", "partner")
setnames(countries_xy, "longitude", "longitude.p")
setnames(countries_xy, "latitude", "latitude.p")

df3= merge(df3, unique(countries_xy), by ="partner")

setDT(df3)
df3=subset(df3, partner=="Indonesia"| partner=="Malaysia"| partner=="Singapore"| partner=="Vietnam"
           | partner=="Cambodia"| partner=="Philippines"| partner=="Thailand")
df3=subset(df3, exporter=="Indonesia"| exporter=="Malaysia"| exporter=="Singapore"| exporter=="Vietnam"
           | exporter=="Cambodia"| exporter=="Philippines"| exporter=="Thailand"| exporter=="China"| exporter=="India"|
           exporter=="Japan")

df3 = df3[, j = list(v=sum(v, na.rm=TRUE)), by = list(partner, exporter,longitude.x,longitude.p,latitude.x,latitude.p)]




df3=df3 %>%
  mutate(latitude.p=replace(latitude.p, partner=="Indonesia", -4)) %>%
  as.data.frame()
df3=df3 %>%
  mutate(latitude.x=replace(latitude.x, exporter=="Indonesia", -4)) %>%
  as.data.frame()


df3=df3 %>%
  mutate(latitude.p=replace(latitude.p, partner=="Malaysia", 3.22)) %>%
  as.data.frame()
df3=df3 %>%
  mutate(latitude.x=replace(latitude.x, exporter=="Malaysia", 3.22)) %>%
  as.data.frame()

df3=df3 %>%
  mutate(longitude.p=replace(longitude.p, partner=="Malaysia", 102.05)) %>%
  as.data.frame()
df3=df3 %>%
  mutate(longitude.x=replace(longitude.x, exporter=="Malaysia", 102.75)) %>%
  as.data.frame()




library(geosphere)

flows <- gcIntermediate(df3[,c(3,5)], df3[,c(4,6)], sp = TRUE, addStartEnd = TRUE)

flows$v <- df3$v

flows$exporter <- df3$exporter

flows$partner <- df3$partner


library(leaflet)
library(RColorBrewer)

hover <- paste0(df3$partner, "'s imports from ", 
                df3$exporter, ': ', 
                as.character(df3$v))

pal <- colorFactor(brewer.pal(5, 'Set3'), df3$partner)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~v/1900000, label = hover, 
               group = ~partner, color = ~pal(partner)) %>%
  addLayersControl(overlayGroups = unique(df3$partner), 
                   options = layersControlOptions(collapsed = FALSE))%>%
  addLegend("bottomright", pal = pal, values=flows$partner,
          title="Importers",
          opacity = 1)




