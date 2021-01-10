install.packages("spData")

install.packages("dplyr")

library(spatstat)

library(here)

library(sp)

library(rgeos)

library(maptools)

library(GISTools)

library(tmap)

library(sf)

library(geojson)

library(geojsonio)

library(tmaptools)

library(spData)

library(spdep)

library(readr)

library(dplyr)

library(sf)


wuhan <- st_read(here::here("wuhan4.shp"))

point<-st_read(here::here( "point9.shp"))

data<-read_csv('read.csv')


wuhanMerged <- wuhan %>% 

      left_join(data, 

                    by = c("NAME_2" = "district"))%>%

      distinct(OBJECTID,NAME_2,geometry)


 tm_shape(wuhanMerged) +

      tm_polygons(col = NA, alpha = 0.5) +

      tm_shape(point) +

      tm_dots(col = "blue")


pointSub <- point[wuhanMerged,]

tm_shape(wuhanMerged) +

  tm_polygons(col = NA, alpha = 0.5) +

tm_shape(pointSub) +

  tm_dots(col = "blue")


points_sf_joined <- wuhanMerged%>%

     st_join(pointSub)%>%

     add_count(NAME_2)%>%

     janitor::clean_names()%>%

     #calculate area

     mutate(area=st_area(.))%>%

     #then density of the points per ward

     mutate(density=n/area)


points_sf_joined<- points_sf_joined %>%    
                
     group_by(objectid) %>%         

     summarise(density = first(density),

               name= first(name_2),

               plaquecount= first(n))


#calculate the centroids of all points in Wuhan

coordsW <- points_sf_joined%>%

  st_centroid()

  st_geometry()


#create a neighbours list

point_nb <- points_sf_joined %>%

  poly2nb(., queen=T)


#create a spatial weights object from these weights

point.lw <- point_nb %>%

  nb2listw(., style="C")


#calculate Moran's I

moran <- points_sf_joined %>%

  pull(density) %>%

  as.vector()%>%

  moran.test(., point.lw)


moran


