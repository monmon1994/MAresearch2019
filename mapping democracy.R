#### Mapping LEXICAL

library(maps)
library(sf)
library(ggplot2)
library(dplyr)

UK <- map_data("world") %>% filter(region=="UK")

world <- map_data("world") 

ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)


plot(wb_epi_df$lied_dummy)


sum(wb_epi_df$lied_dummy)/101
