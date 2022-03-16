library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(tmap)

Pais          <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Cusco         <- subset(Pais , NAME_1  == "Cusco")
Anta          <- subset(Cusco , NAME_3  == "Anta")
plot(Anta)

Anta_Elev    = get_elev_raster(Anta, z=12)

Anta_alt    <- crop(Anta_Elev, Anta)
Anta_alt    <-Anta_alt  <- mask(Anta_alt , Anta)

slope = terrain(Anta_alt  , opt = "slope") 
aspect = terrain(Anta_alt   , opt = "aspect")
hill = hillShade(slope , aspect, angle = 40, direction = 270)

slop = terrain(Anta_Elev  , opt = "slope") 
aspec = terrain(Anta_Elev   , opt = "aspect")
hil = hillShade(slop , aspec, angle = 40, direction = 270)

tm_shape(hill) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE)+
  tm_shape(Anta_alt) +
  tm_raster(alpha = 0.6, palette = colss ,n=30,
            legend.show = T, title="Elevacion \n(m.s.n.m)")


colss <-c("#4361ee" , "#38b000", "#38b000", "#fcf300", "#ff9100", "#ff0000", "#ff0000")


Mapa= tm_shape(hil) +
  tm_raster(palette = gray(0:10 / 10), n = 100, legend.show = FALSE)+
  tm_shape(Anta_Elev) +
  tm_raster(alpha = 0.6, palette = colss ,n=30,
            legend.show = T, title="Elevacion \n(m.s.n.m)")+
  tm_shape(Anta)+
  tm_borders("white",lwd=2)+
  tm_text("NAME_3",size = .8, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.35)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "black", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.86, 0.05), text.color = "black")+
  tm_layout(title = "",
            title.bg.color = "white",
            title.size = 1.5, 
            legend.position = c(0.85,0.50),
            legend.bg.color = "gray",
            legend.format = c(text.align = "right", 
                              text.separator = "-"))+
  tm_credits("Provincia de Anta \n     Elevacion y Relieve", position = c(.1, .8), col = "black", fontface="bold", size=2, fontfamily = "serif")+
  tm_credits("Data: https://www.https://code.earthengine.google.com/ \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo", position = c(0.1, .04), col = "black", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.60, 0.05))+
  tm_grid(col = 'gray', alpha = 0.5)

tmap_save(Mapa, "Mapa/Mapa elevacion de anta.png", dpi = 1200, width = 12 #Ancho
          ,height = 12) #Largo
