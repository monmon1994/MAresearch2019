library(sf)
library(tmap)
library(dplyr)
library(spData)
library(raster)
library(leaflet)
library(cartogram)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(stargazer)
library(wbstats)
library(dplyr)
library(tidyverse)

theme_set(
    theme_void()
)

africa = world %>% 
    filter(continent == "Africa", !is.na(iso_a2)) %>% 
    left_join(worldbank_df, by = "iso_a2") %>% 
    dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
    st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

plot(africa["HDI"])

central_africa = filter(africa, subregion == c("Middle Africa", "Southern Africa")) +
    tm_shape(central_africa) +
    tm_polygons(col = c("HDI")) +
    qtm(africa, fill = NULL)


### Building maps

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white")

africa <- map_data("africa")

afr_1 <- c("Equatorial Guinea", "Namibia")

afr_1_maps <- map_data("world", region = afr_1)

region.lab.data <- afr_1_maps  %>%
            group_by(region) %>%
            summarise(long = mean(long), lat = mean(lat))

ggplot(afr_1_maps, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = region))+
    geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
    scale_fill_viridis_d()+
    theme_void()+
    theme(legend.position = "none")

sov.expand("Equitorial Guinea")

iso.expand("GNQ")


#### Exploring countries

GNQplot <- wb(country = c("GNQ"), indicator = c("CC.EST", "GE.EST","RQ.EST", 
                                               "PV.EST", "RL.EST", "VA.EST"), 
                startdate = 2000, enddate = 2018, return_wide = TRUE)

NAMplot <- wb(country = c("NAM"), indicator = c("CC.EST", "GE.EST","RQ.EST", 
                                                "PV.EST", "RL.EST", "VA.EST"), 
              startdate = 2000, enddate = 2018, return_wide = TRUE)



#create data
GNQplot$date <- as.numeric(GNQplot$date)

year <- GNQplot$date

## NAM
NAMplot$date <- as.numeric(GNQplot$date)
year2 <- NAMplot$date

### GGPLOT FIGURE 3.1 

pdf(file = "GNQtimeseries.pdf")
ggplot(data = GNQplot, aes(x = year)) +
    geom_line(aes(y = GE.EST, color = "GE"), linetype = "solid") +
    geom_line(aes(y = RQ.EST, color = "RQ"), linetype="longdash") +
    geom_line(aes(y = CC.EST, color = "CC"), linetype="twodash") +
    geom_line(aes(y = VA.EST, color = "VA"), linetype="solid") +
    geom_line(aes(y = PV.EST, color = "PV"), linetype="dashed") +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(x = "year", y = "WGI score scale") +
    scale_color_discrete(name = "WGI", labels = c("GE", "RQ", "CC", "VA", "PV")) +
    theme_classic()
dev.off()

### GGPLOT FIGURE 3.2

pdf(file = "NAMtimeseries.pdf")
ggplot(data = NAMplot, aes(x = year)) +
    geom_line(aes(y = GE.EST, color = "GE"), linetype = "solid") +
    coord_cartesian(ylim = c(-2, 2)) +
    labs(x = "year", y = "WGI score scale") +
    geom_line(aes(y = CC.EST, color = "CC"), linetype="twodash") +
    geom_line(aes(y = VA.EST, color = "VA"), linetype="solid") +
    geom_line(aes(y = PV.EST, color = "PV"), linetype="dashed") +
    geom_line(aes(y = RQ.EST, color = "RQ"), linetype="longdash") +
    scale_color_discrete(name = "WGI", labels = c("GE", "CC", "VA", "PV",
                                                  "RQ")) +
    theme_classic()
dev.off()
