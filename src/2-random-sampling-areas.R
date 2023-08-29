source("src/functions.R")

crs_all <-
  crs(st_read("data/land-tenure/Landtenuremodel/landtenuremodel.shp"))

wind_parks <- load_single_final_wind_parks_from_shape(crs_all)

pv_parks <- load_single_final_pv_parks_from_shape(crs_all)

municipalities <-
  st_read("data/municipalities/BR_Municipios_2022.shp") %>%
  st_transform(crs_all)

municipalities_intersecting_total <-
  get_intersecting_municipalities(municipalities, wind_parks, pv_parks) %>%
  bind_rows(municipalities %>% filter(NM_MUN == "Xique-Xique"))

gwa <- raster("intermediate/gwa-municipalities-crop.tif")
gwa_downsampled <- aggregate(gwa, fact = 10)
gwa_downsampled <- projectRaster(gwa_downsampled, crs = crs_all)
wind_parks <- st_read("intermediate/wind_parks_power_density.shp")


municipalities_intersecting_total_intersect_gwa <-
  st_read("intermediate/municipalities_intersecting_total_gwa.shp")

municipalities_intersecting_total_intersect_gwa %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 10 ^ 6) %>%
  filter(NM_MUN %in% wind_parks$MUNICIP) %>%
  summarize(area_km2 = sum(area_km2))

rot = function(a)
  matrix(c(cos(a), sin(a),-sin(a), cos(a)), 2, 2)

random_park_shape_extraction <-
  function(i,
           parks,
           municipalities,
           technology,
           PLOT_MAP = FALSE) {
    if (i %% 100 == 0 | i == 1) {
      print(glue("Starting to randomize park: {i}"))
    }
    
    park <- parks[i,]
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
      filter(
        str_detect(tolower(municipality), tolower(NM_MUN)) &
          SIGLA_UF == UF &
          !NM_MUN %in% c("São Miguel",
                         "Macaúbas",
                         "Angicos",
                         "Emas")
      )
    
    if (nrow(mun) == 0) {
      print(i)
      print("No municipality found")
      
    }
    
    if (nrow(mun) > 1 & !(municipality %in% double_municipalities)) {
      print(i)
      print("More than 1 municipality found")
    }
    
    if (municipality %in% double_municipalities) {
      mun <- st_union(mun)
    }
    
    
    pv_parks_here <- pv_parks %>%
      filter(MUNICIP == municipality)
    
    wind_parks_here <- wind_parks %>%
      filter(MUNICIP == municipality)
    
    
    
    
    samples <- 100
    
    
    if (i %in% c(42,
                 49,
                 58,
                 62,
                 94,
                 96,
                 160,
                 162,
                 170,
                 173,
                 195,
                 317,
                 403,
                 405,
                 406,
                 407,
                 408,
                 454,
                 500,
                 547)) {
      samples <- 600
    }
    
    random_rotation_1 <- runif(1, 0, pi / 4)
    random_rotation_2 <- runif(1, pi / 4, pi / 2)
    random_rotation_3 <- runif(1, pi / 2, pi * 3 / 4)
    random_rotation_4 <- runif(1, pi * 3 / 4, pi)
    
    
    
    points <- st_sample(mun, samples)
    centroid_park <- st_centroid(st_geometry(park))
    park_new <- park
    
    parks_new_1 <-
      (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_1) + st_geometry(centroid_park)
    parks_new_2 <-
      (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_2) + st_geometry(centroid_park)
    parks_new_3 <-
      (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_3) + st_geometry(centroid_park)
    parks_new_4 <-
      (st_geometry(park) - st_geometry(centroid_park))  * rot(random_rotation_4) + st_geometry(centroid_park)
    
    parks_new <- bind_rows(
      st_as_sf(parks_new_1),
      st_as_sf(parks_new_2),
      st_as_sf(parks_new_3),
      st_as_sf(parks_new_4)
    )
    
    parks_new_shifted <-
      (st_geometry(parks_new) + st_geometry(points) - st_geometry(centroid_park))
    
    parks_new_shifted <- st_set_crs(parks_new_shifted, crs(park))
    
    if (PLOT_MAP == TRUE) {
      plot(st_geometry(mun))
      plot(st_geometry(pv_parks_here),
           add = TRUE,
           col = "yellow")
      plot(st_geometry(wind_parks_here),
           add = TRUE,
           col = "green")
      plot(st_geometry(park), add = TRUE, col = "red")
      plot(points, add = TRUE)
      
    }
    if (!((
      technology == "wind-control-wind" |
      technology == "wind-random"
    ) & i %in% c(551))) {
      parks_new_shifted <-
        parks_new_shifted[st_contains(mun, parks_new_shifted, sparse = FALSE)]
    }
    
    if (length(parks_new_shifted) == 0) {
      print(glue("No park found for id {i} in {municipality} in {UF}"))
      
    }
    index1 <-
      rowSums(st_intersects(parks_new_shifted, wind_parks_here, sparse = FALSE))
    index2 <-
      rowSums(st_intersects(parks_new_shifted, pv_parks_here, sparse = FALSE))
    if (!((
      technology == "wind-control-wind" |
      technology == "wind-random"
    ) & i %in% c(133, 346))) {
      parks_new_shifted <- parks_new_shifted[!index1 & !index2]
    }
    
    if (length(parks_new_shifted) == 0) {
      print(glue("No park found for id {i} in {municipality} in {UF}"))
      
    }
    park_new_final <- NULL
    if (technology == "wind-control-wind") {
      library(exactextractr)
      wind_parks_power_density <-
        exact_extract(gwa_downsampled, parks_new_shifted, "mean")
      
      parks_new_shifted_temp <- parks_new_shifted
      parks_new_shifted_temp$power_density <- wind_parks_power_density
      
      park_density <-
        exact_extract(gwa_downsampled, parks[i,], "mean")
      
      
      park_new_final <-
        st_as_sf(parks_new_shifted_temp[which.min(abs(parks_new_shifted_temp$power_density - park_density))])
      if ((length(parks_new_shifted) == 0)) {
        wind_parks_power_density <-
          exact_extract(gwa_downsampled, parks[i,], "mean")
        parks_new_shifted_temp <- st_as_sf(st_geometry(park))
        
        
        parks_new_shifted_temp$power_density <-
          wind_parks_power_density
        park_new_final <-
          st_as_sf(parks_new_shifted_temp[which.max(parks_new_shifted_temp$power_density)])
        
      }
      
    } else{
      park_new_final <-
        st_as_sf(parks_new_shifted[runif(1, 1, length(parks_new_shifted))])
      if ((length(parks_new_shifted) == 0)) {
        parks_new_shifted_temp <- st_as_sf(st_geometry(park))
        park_new_final <-
          st_as_sf(parks_new_shifted_temp[runif(1, 1, length(parks_new_shifted_temp))])
        
        
      }
      
    }
    if (PLOT_MAP == TRUE) {
      plot(st_geometry(parks_new_shifted), add = TRUE)
      plot(st_geometry(park_new_final),
           add = TRUE,
           col = "orange")
    }
    
    return(park_new_final)
  }

#### special test cases
random_park_shape_extraction(2,
                             wind_parks,
                             municipalities_intersecting_total,
                             "wind-control-wind",
                             PLOT_MAP = TRUE)
random_park_shape_extraction(575,
                             wind_parks,
                             municipalities_intersecting_total,
                             "wind-control-wind",
                             PLOT_MAP = TRUE)
random_park_shape_extraction(133,
                             wind_parks,
                             municipalities_intersecting_total,
                             "wind",
                             PLOT_MAP = TRUE)

write_several_parks <-
  function(n,
           parks,
           municipalities,
           technology,
           identifier) {
    counter <- 1
    #full extraction
    for (j in 1:n) {
      parks_final_random <- NULL
      for (i in 1:nrow(parks)) {
        random_park <- random_park_shape_extraction(i,
                                                    parks,
                                                    municipalities,
                                                    technology,
                                                    PLOT_MAP = FALSE)
        if (is.null(parks_final_random)) {
          parks_final_random <- random_park
        } else{
          parks_final_random <- bind_rows(parks_final_random, random_park)
        }
        
      }
      
      j <- 1
      parks_new <- parks
      print(nrow(parks))
      print(nrow(parks_final_random))
      if (nrow(parks) != nrow(parks_final_random)) {
        print("no park written.")
      }
      else{
        st_geometry(parks_new) <- st_geometry(parks_final_random)
        parks_power_density <-
          exact_extract(gwa_downsampled, parks_new, "mean")
        
        parks_new$pwr_dns <- parks_power_density
        
        st_write(
          obj = parks_new,
          dsn = glue(
            "intermediate/random-shapes/{identifier}-{technology}-{counter}.shp"
          ),
          layer_options = "ENCODING=UTF-8",
          delete_layer = TRUE
        )
        counter <- counter + 1
      }
    }
    
  }

municipalities_intersecting_total_intersect_gwa <-
  st_transform(
    municipalities_intersecting_total_intersect_gwa,
    crs(municipalities_intersecting_total)
  )



write_several_parks(
  1,
  wind_parks,
  municipalities_intersecting_total,
  "wind-control-wind",
  "whole-area-control-wind"
)
write_several_parks(1,
                    wind_parks,
                    municipalities_intersecting_total,
                    "wind-random",
                    "whole-area")
write_several_parks(1,
                    pv_parks,
                    municipalities_intersecting_total,
                    "pv",
                    "whole-area")

parks_wind_control <-
  st_read("intermediate/random-shapes/whole-area-control-wind-wind-control-wind-1.shp")
parks_random_control <-
  st_read("intermediate/random-shapes/whole-area-wind-random-1.shp")
parks_pv <- st_read("intermediate/random-shapes/whole-area-pv-1.shp")

plot(wind_parks)
parks_power_density <-
  exact_extract(gwa_downsampled, wind_parks, "mean")
mean(parks_power_density)

plot(parks_wind_control)
parks_power_density <-
  exact_extract(gwa_downsampled, parks_wind_control, "mean")
mean(parks_power_density)

plot(parks_random_control)
parks_power_density <-
  exact_extract(gwa_downsampled, parks_random_control, "mean")
mean(parks_power_density, na.rm = TRUE)
