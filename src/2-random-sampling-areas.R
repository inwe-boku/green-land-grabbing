source("src/functions.R")

crs_all <- crs(st_read("data/land-tenure/Landtenuremodel/landtenuremodel.shp"))

wind_parks <- load_single_final_wind_parks_from_shape(crs_all)

### this would mean it is the first on the project...
pv_parks <- load_single_final_pv_parks_from_shape(crs_all)

municipalities <- st_read("data/municipalities/BR_Municipios_2022.shp") %>% 
  st_transform(crs_all)

municipalities_intersecting_total <- get_intersecting_municipalities(municipalities, wind_parks, pv_parks) %>% 
  bind_rows(municipalities %>% filter(NM_MUN=="Xique-Xique"))


#wind_parks <- st_join(wind_parks, municipalities_intersecting_total %>% 
#                        dplyr::select(NM_MUN), largest = TRUE) %>% 
#  mutate(MUNICIP=NM_MUN)

### area wind parks
#municipalities_intersecting_total %>% 
#  mutate(area_km2 = as.numeric(st_area(.)) / 10^6) %>%
#  filter(NM_MUN %in% wind_parks$MUNICIP) %>% 
#  summarize(area_km2 = sum(area_km2))

#municipalities_intersecting_total %>% 
#  mutate(area_km2 = as.numeric(st_area(.)) / 10^6) %>%
 # filter(!(NM_MUN %in% wind_parks$MUNICIP)) %>% 
  #summarize(area_km2 = sum(area_km2))


###area municipalities wind parks
#st_join(municipalities_intersecting_total, wind_parks, largest = TRUE) %>% st_area() %>% sum()/10^6

###area municipalities solar parks
#st_join(municipalities_intersecting_total, pv_parks, largest = TRUE) %>% st_area() %>% sum()/10^6

#st_write(obj=municipalities_intersecting_total, 
 #        dsn=glue("intermediate/municipalities_intersecting_total.shp"),
  #       layer_options = "ENCODING=UTF-8", 
   #      delete_layer = TRUE)


#r <- raster("data/gwa/BRA_power-density_100m.tif")

#r_crop <- crop(r, municipalities_intersecting_total)

#plot(r_crop)

#writeRaster(r_crop, "intermediate/gwa-municipalities-crop.tif")

gwa <- raster("intermediate/gwa-municipalities-crop.tif")
#res(gwa)
gwa_downsampled <- aggregate(gwa, fact = 10)
#res(gwa_downsampled)

#wind_parks_power_density <- extract(gwa_downsampled, wind_parks)
#save(wind_parks_power_density, file = "intermediate/wind-parks-power-density.RData")
#load(file = "intermediate/wind-parks-power-density.RData")

#power_densities <- lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)}) %>% 
#  unlist()

#wind_parks$power_density <- power_densities

#st_write(obj = wind_parks,
#         dsn = "intermediate/wind_parks_power_density.shp")

wind_parks <- st_read("intermediate/wind_parks_power_density.shp")

#gwa_summary_mean_windparks <- summary(unlist(lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)})))
#gwa_summary_min_windparks <- summary(unlist(lapply(wind_parks_power_density, function(x){min(x, na.rm=TRUE)})))
#e<-ecdf(unlist(lapply(wind_parks_power_density, function(x){min(x, na.rm=TRUE)})))
cutoff <- 260
#e(260)
#mean(unlist(lapply(wind_parks_power_density, function(x){max(x, na.rm=TRUE)})))

#municipality_power_density <- extract(gwa_downsampled, municipalities_intersecting_total)
#save(municipality_power_density, file = "intermediate/municipality-power-density.RData")
#load(file = "intermediate/municipality-power-density.RData")

#summary(unlist(lapply(municipality_power_density, function(x){mean(x, na.rm=TRUE)})))
#summary(unlist(lapply(municipality_power_density, function(x){min(x, na.rm=TRUE)})))
#summary(unlist(lapply(municipality_power_density, function(x){max(x, na.rm=TRUE)})))

#gwa_polygon <- rasterToPolygons(gwa_downsampled)


#gwa_polygon_sf <- st_as_sf(gwa_polygon)

#head(gwa_polygon_sf)
#st_write(obj=gwa_polygon_sf, 
#         dsn=glue("intermediate/gwa_polygon_full.shp"),
#         layer_options = "ENCODING=UTF-8", 
#         delete_layer = TRUE)

#gwa_polygon_sf <- st_read("intermediate/gwa_polygon_full.shp")

#gwa_polygon_sf_reduced <- gwa_polygon_sf %>% filter(gwa.municipalities.crop > cutoff)
#st_write(obj=gwa_polygon_sf_reduced, 
#         dsn=glue("intermediate/gwa_polygon_reduced.shp"),
#         layer_options = "ENCODING=UTF-8", 
#         delete_layer = TRUE)

#gwa_polygon_reduced_union <- st_union(gwa_polygon_sf_reduced)

#plot(gwa_polygon_reduced_union)

#municipalities_intersecting_total_intersect_gwa <- st_intersection(municipalities_intersecting_total,
#                                                     gwa_polygon_reduced_union)

#plot(st_geometry(municipalities_intersecting_total_intersect_gwa))

#st_write(obj=municipalities_intersecting_total_intersect_gwa, 
#         dsn=glue("intermediate/municipalities_intersecting_total_gwa.shp"),
#         layer_options = "ENCODING=UTF-8", 
#        delete_layer = TRUE)

municipalities_intersecting_total_intersect_gwa <- st_read("intermediate/municipalities_intersecting_total_gwa.shp")

municipalities_intersecting_total_intersect_gwa %>% 
  mutate(area_km2 = as.numeric(st_area(.)) / 10^6) %>%
  filter(NM_MUN %in% wind_parks$MUNICIP) %>% 
  summarize(area_km2 = sum(area_km2))

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

random_park_shape_extraction <- function(i, parks, municipalities, technology, PLOT_MAP = FALSE){

  if(i %% 100 == 0 | i == 1){
    print(glue("Starting to randomize park: {i}"))
  }
  
  #if(technology == "wind" & (i %in% c(183, 45, 85, 120, 160, 183, 297, 316, 317, 345, 569, 46, 71, 298, 387, 315, 340, 385, 578, 579, 299, 314, 384, 433))){
  #  municipalities <- municipalities_intersecting_total
  #}
  
  park <- parks[i, ]
  municipality <- park$MUNICIP %>% unlist()
  UF <- park$UF %>% unlist()
  
  double_municipalities <- c(
    "João Câmara e Parazinho",
    "Parazinho e Touros",
    "Touros e São Miguel do Gostoso",
    "Chuí e Santa Vitória do Palmar",
    "Cafarnaum e Mulungu do Morro",
    "Bodó e Lagoa Nova",
    "Bodó, Cerro Corá e Lagoa Nova",
    "Marcolândia e Caldeirão Grande do Piauí"
  )

  mun <- municipalities %>% 
    filter(str_detect(tolower(municipality), tolower(NM_MUN)) & 
             SIGLA_UF == UF & 
             !NM_MUN %in% c("São Miguel", 
                            "Macaúbas", 
                            "Angicos",
                            "Emas"))

  if(nrow(mun) == 0){
    print(i)
    print("No municipality found")

  }
  
  if(nrow(mun) > 1 & !(municipality %in% double_municipalities)){
    print(i)
    print("More than 1 municipality found")
  }
  
  if(municipality %in% double_municipalities){
    mun <- st_union(mun)
  }
  
  
  pv_parks_here <- pv_parks %>% 
    filter(MUNICIP == municipality)
  
  wind_parks_here <- wind_parks %>% 
    filter(MUNICIP == municipality)

  
 
  
  samples <- 100

  
  if(i %in% c(42, 49, 58, 62, 94, 96, 160, 162, 170, 173, 195, 317, 403, 405, 406, 407, 408, 454, 500, 547)){
    samples <- 600
  }
  #if(municipality %in% c("Rio do Fogo", 
  #                       "Barra dos Coqueiros", 
  #                       "Parazinho", 
  #                       "Paranatama", 
  #                       "Tenente Laurentino Cruz",
  #                       "Marcolândia",
  #                       "Caetés",
  #                       "São Bento do Norte",
  #                       "Santa Luzia") & technology == "wind") {
  #  samples <- 500
 #
    
  #  if(i %in% c(551, 133)){
  #    samples <- 1000
  #  }

  #}
  random_rotation_1 <- runif(1, 0, pi/4)
  random_rotation_2 <- runif(1, pi/4, pi/2)
  random_rotation_3 <- runif(1, pi/2, pi * 3/4)
  random_rotation_4 <- runif(1, pi * 3/4, pi)
  
  
  
  points <- st_sample(mun, samples)
  centroid_park <- st_centroid(st_geometry(park))
  park_new <- park
  
  parks_new_1 <- (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_1) + st_geometry(centroid_park)
  parks_new_2 <- (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_2) + st_geometry(centroid_park)
  parks_new_3 <- (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_3) + st_geometry(centroid_park)
  parks_new_4 <- (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_4) + st_geometry(centroid_park)

  parks_new <- bind_rows(st_as_sf(parks_new_1), 
                         st_as_sf(parks_new_2),
                         st_as_sf(parks_new_3),
                         st_as_sf(parks_new_4))
  
  parks_new_shifted <-  (st_geometry(parks_new) + st_geometry(points) - st_geometry(centroid_park))
  
  #for(i in 1:samples){
  #  parks_new[i] <- (st_geometry(parks_new[i]) - st_geometry(centroid_park))  * rot(random_rotation[i]) + st_geometry(centroid_park)
  #}
  parks_new_shifted <- st_set_crs(parks_new_shifted, crs(park))

  if(PLOT_MAP == TRUE) {  
    plot(st_geometry(mun))
    plot(st_geometry(pv_parks_here), add=TRUE, col="yellow")
    plot(st_geometry(wind_parks_here), add=TRUE, col="green")
    plot(st_geometry(park), add=TRUE, col="red")
    plot(points, add=TRUE)
    
  }
  if(!((technology == "wind-control-wind"|technology == "wind-random" ) & i %in% c(551))){
    parks_new_shifted <- parks_new_shifted[st_contains(mun, parks_new_shifted, sparse = FALSE)]
  }
    
  if(length(parks_new_shifted) == 0){
    print(glue("No park found for id {i} in {municipality} in {UF}"))

  }
  index1 <- rowSums(st_intersects(parks_new_shifted, wind_parks_here, sparse = FALSE))
  index2 <- rowSums(st_intersects(parks_new_shifted, pv_parks_here, sparse = FALSE))
  if(!((technology == "wind-control-wind"|technology == "wind-random" )& i %in% c(133, 346))){
    
    parks_new_shifted <- parks_new_shifted[!index1 & !index2]
  }
  
  if(length(parks_new_shifted) == 0){
    print(glue("No park found for id {i} in {municipality} in {UF}"))

  }
  park_new_final <- NULL
  if(technology == "wind-control-wind"){
    library(exactextractr)
    wind_parks_power_density <- exact_extract(gwa_downsampled, parks_new_shifted, "mean")
    
    parks_new_shifted_temp <- parks_new_shifted
    parks_new_shifted_temp$power_density <- wind_parks_power_density
    
    park_density <- exact_extract(gwa_downsampled, parks[i, ], "mean")
    
  
    park_new_final <- st_as_sf(parks_new_shifted_temp[which.min(abs(parks_new_shifted_temp$power_density - park_density))])
    if((length(parks_new_shifted) == 0)){
      wind_parks_power_density <- exact_extract(gwa_downsampled, parks[i, ], "mean")
      parks_new_shifted_temp <- st_as_sf(st_geometry(park))
      
      
      parks_new_shifted_temp$power_density <- wind_parks_power_density
      park_new_final <- st_as_sf(parks_new_shifted_temp[which.max(parks_new_shifted_temp$power_density)])
      
    }

  }else{
    park_new_final <- st_as_sf(parks_new_shifted[runif(1, 1, length(parks_new_shifted))])
    if((length(parks_new_shifted) == 0)){
      parks_new_shifted_temp <- st_as_sf(st_geometry(park))
      park_new_final <- st_as_sf(parks_new_shifted_temp[runif(1, 1, length(parks_new_shifted_temp))])
      
     
    }
    
  }
  if(PLOT_MAP == TRUE) {  
    plot(st_geometry(parks_new_shifted), add=TRUE)
    plot(st_geometry(park_new_final), add=TRUE, col="orange")
  }
  
  return(park_new_final)
}

#### special test cases

municipalities_intersecting_total %>% 
  bind_rows(municipalities %>% filter(NM_MUN=="Xique-Xique"))

random_park_shape_extraction(548, wind_parks, municipalities_intersecting_total, "wind-control-wind", PLOT_MAP = TRUE)
random_park_shape_extraction(575, wind_parks, municipalities_intersecting_total, "wind-control-wind", PLOT_MAP = TRUE)
random_park_shape_extraction(133, wind_parks, municipalities_intersecting_total, "wind", PLOT_MAP = TRUE)

#x<-random_park_shape_extraction(62, wind_parks, PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(133, wind_parks, PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(551, wind_parks, PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(346, wind_parks, PLOT_MAP = TRUE)

#random_park_shape_extraction(1, wind_parks, PLOT_MAP = TRUE)
#random_park_shape_extraction(1, pv_parks, PLOT_MAP = TRUE)

write_several_parks <- function(n, parks, municipalities, technology, identifier){
  counter<-1
  #full extraction
  for(j in 1:n){
    parks_final_random <- NULL
    for(i in 1:nrow(parks)) {
      random_park <- random_park_shape_extraction(i, 
                                   parks, 
                                   municipalities,
                                   technology,
                                   PLOT_MAP = FALSE)
      if(is.null(parks_final_random)){
        parks_final_random<-random_park
      }else{
        parks_final_random <- bind_rows(parks_final_random, random_park)
      }
        
    }
    
    j <- 1
    parks_new <- parks
    print(nrow(parks))
    print(nrow(parks_final_random))
    if(nrow(parks) != nrow(parks_final_random)){
      print("no park written.")
    }
    else{
      #st_geometry(parks_new) <- st_geometry(parks_final_random)
      #st_geometry(parks_new[345, ]) <- st_geometry(wind_parks[345, ])
      #parks_new <- st_set_crs(parks_new, crs(parks))
      
      #wind_parks_power_density <- extract(gwa_downsampled, parks_new)
      
      #power_densities <- lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)}) %>% 
      #  unlist()
      
      #parks_new$power_density <- power_densities
      st_geometry(parks_new) <- st_geometry(parks_final_random)
      parks_power_density <- exact_extract(gwa_downsampled, parks_new, "mean")
      
      parks_new$power_density <- parks_power_density
      
      #park_new_final <- st_as_sf(parks_new_shifted[which.max(parks_new_shifted$power_density)])
      
      
      st_write(obj=parks_new, 
             dsn=glue("intermediate/random-shapes/{identifier}-{technology}-{counter}.shp"),
             layer_options = "ENCODING=UTF-8", 
             delete_layer = TRUE)
      counter<-counter+1
    }
  }
  
}

municipalities_intersecting_total_intersect_gwa <- st_transform(municipalities_intersecting_total_intersect_gwa, 
                                                                crs(municipalities_intersecting_total))



suppressMessages(write_several_parks(1, wind_parks, municipalities_intersecting_total, "wind-control-wind", "whole-area-control-wind"))
suppressMessages(write_several_parks(1, wind_parks, municipalities_intersecting_total, "wind-random", "whole-area"))
suppressMessages(write_several_parks(1, pv_parks, municipalities_intersecting_total, "pv", "whole-area"))
#suppressMessages(write_several_parks(200, wind_parks, municipalities_intersecting_total_intersect_gwa, "wind", "area-5p"))

st_area(p)
parks1<-st_read("intermediate/random-shapes/whole-area-control-wind-wind-control-wind-1.shp")
parks2<-st_read("intermediate/random-shapes/whole-area-wind-random-1.shp")
parks3<-st_read("intermediate/random-shapes/whole-area-pv-1.shp")

plot(wind_parks)
parks_power_density <- exact_extract(gwa_downsampled, wind_parks, "mean")
mean(parks_power_density)

plot(parks1)
parks_power_density <- exact_extract(gwa_downsampled, parks1, "mean")
mean(parks_power_density)

plot(parks2)
parks_power_density <- exact_extract(gwa_downsampled, parks2, "mean")
mean(parks_power_density, na.rm=TRUE)



#parks2<-st_read("intermediate/random-shapes/whole-area-wind-1.shp")

#wind_parks_power_density <- extract(gwa_downsampled, wind_parks)
#summary(unlist(lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)})))

#wind_parks_power_density <- extract(gwa_downsampled, parks1)
#summary(unlist(lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)})))

#wind_parks_power_density <- extract(gwa_downsampled, parks2)
#summary(unlist(lapply(wind_parks_power_density, function(x){mean(x, na.rm=TRUE)})))

#x<-random_park_shape_extraction(45, wind_parks, municipalities_intersecting_total_intersect_gwa, "wind", PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(120, wind_parks, municipalities_intersecting_total_intersect_gwa,"wind",  PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(317, wind_parks, municipalities_intersecting_total_intersect_gwa, "wind", PLOT_MAP = TRUE)
#x<-random_park_shape_extraction(569, wind_parks, municipalities_intersecting_total_intersect_gwa, "wind", PLOT_MAP = TRUE)


#### legacy code ####
#municipalities %>% filter(startsWith(NM_MUN, "Tenente"))

#wind_parks %>% filter(startsWith(MUNICIP, "Emas"))
#pv_parks %>% filter(startsWith(MUNICIP, "Emas"))


