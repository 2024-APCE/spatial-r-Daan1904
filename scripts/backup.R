woody_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Woody Biomass") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
woody_map_sa

# plot the rainfall map
rainfall_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(350,900),
                       oob=squish,
                       name="mm / year") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
rainfall_map_sa

# plot the elevation map
elevation_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(500,2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Elevation") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
elevation_map_sa

all_maps_sa <- woody_map_sa + elevation_map_sa + rainfall_map_sa + 
  patchwork::plot_layout(ncol=1)
all_maps_sa

# make maps also for the other layers that you found
burnfreq<-terra::rast("./my_data/BurnFreq.tif")
burnfreq_sa<-terra::crop(burnfreq_map,saExt)
CEC_map<-terra::rast("./my_data/CEC_5_15cm.tif")
CEC_sa<-terra::crop(CEC_map,saExt)
annual_rainfall_wet<-terra::rast("./my_data/ChirpsAnnualRainfall2001_2020.tif")
annual_rainfall_wet_sa<-terra::crop(annual_rainfall_wet_map,saExt)
annual_rainfall_dry<-terra::rast("./my_data/ChirpsAnnualRainfall2001_2020_(dry).tif")
annual_rainfall_dry_sa<-terra::crop(annual_rainfall_dry_map,saExt)
copernicus_tree_cover<-terra::rast("./my_data/copernicus_tree_cover.tif")
copernicus_tree_cover_sa<-terra::crop(copernicus_tree_cover_map,saExt)
distance2river<-terra::rast("./my_data/DistanceToRiver.tif")
distance2river_sa<-terra::crop(distance2river_map,saExt)
landform<-terra::rast("./my_data/landforms.tif")
landform_sa<-terra::crop(landform_map,saExt)
lastyear_burn<-terra::rast("./my_data/YearLastBurned.tif")
lastyear_burn_sa<-terra::crop(lastyear_burn_map,saExt)

# frequently burnt map
burnfreq_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=burnfreq_sa) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(n = 3, name = "Reds"),
                       limits=c(0,19),
                       oob=squish,
                       name="days") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Frequently burnt") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
burnfreq_map_sa

# CEC map
CEC_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=CEC_sa) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(n = 9, name = "RdYlGn"),
                       limits=c(0,300),
                       oob=squish,
                       name="mmol/kg") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Cation Exchange Capacity of the Soil") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
CEC_map_sa

# Copernicus tree cover map
copernicus_tree_cover_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=copernicus_tree_cover_sa) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(n = 9, name = "Greens"),
                       limits=c(0,60),
                       oob=squish,
                       name="procentage %") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Copernicus Tree Cover") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
copernicus_tree_cover_map_sa

# Distance 2 River map
distance2river_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=distance2river_sa) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(n = 11, name = "Spectral")),
                       limits=c(0,15000),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Distance to the River") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
distance2river_map_sa

# landform map
# Convert to factor
landform_sa <- terra::as.factor(landform_sa)
landform_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=landform_sa) +
  scale_fill_manual(
    values = c(
      "11" = "#141414", "12" = "#383838", "13" = "#808080", 
      "14" = "#ebeb8f", "15" = "#f7d311", "21" = "#aa0000", 
      "22" = "#d89382", "23" = "#ddc9c9", "24" = "#dccdce", 
      "31" = "#1c6330", "32" = "#68aa63", "33" = "#b5c98e", 
      "34" = "#e1f0e5", "41" = "#a975ba", "42" = "#6f198c"
    ),
    breaks = c("11", "12", "13", "14", "15", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42"),  # Define the range explicitly
    na.value = "grey",             # Set a color for NA values
    labels = c(
      "Peak/ridge (warm)", "Peak/ridge", "Peak/ridge (cool)", "Mountain/divide", "Cliff", "Upper slope (warm)", "Upper slope", "Upper slope (cool)", "Upper slope (flat)", "Lower slope (warm)", "Lower slope", "Lower slope (cool)", "Lower slope (flat)", "Valley", "Valley (narrow)"),
    name = "landform types"
  ) +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
landform_map_sa

# Last Year burn map
lastyear_burn_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data=lastyear_burn_sa) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(n = 9, name = "Greys")),
                       limits=c(0,1),
                       oob=squish,
                       name="yes or no") +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Last Year Burn") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5)  
lastyear_burn_map_sa


### put all maps together
all_maps_sa <- woody_map_sa + elevation_map_sa + rainfall_map_sa + 
  burnfreq_map_sa + CEC_map_sa + copernicus_tree_cover_map_sa + 
  distance2river_map_sa + landform_map_sa + lastyear_burn_map_sa + 
  patchwork::plot_layout(ncol=3)
all_maps_sa