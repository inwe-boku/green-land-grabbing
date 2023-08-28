source("src/functions.R")

###shit rework

sf_use_s2(TRUE)


land_tenure <- st_read("data/land-tenure/Landtenuremodel/landtenuremodel.shp")
land_tenure <- st_snap_to_grid(land_tenure, 0.00001)
land_tenure <- st_make_valid(land_tenure)

crs_all <- crs(land_tenure)


#land_tenure <- land_tenure %>% 
#  st_transform(crs_all)

make_intersection_aggregate <- function(parks, land_tenure){
  intersect_parks %>% 
    mutate(area_km2 = as.numeric(st_area(.))/10^6) %>% 
    dplyr::select(Key, 
                  category, runid, run, Start_yr, INICIO, dat_clos, date_privatization=date, area_km2, number_parks) %>% 
    as_tibble() 
}

wind_parks <-st_read("intermediate/wind_parks_power_density.shp") %>% 
  st_transform(crs_all)

p_random <- st_read("intermediate/random-shapes/whole-area-wind-1.shp") %>% 
  st_transform(crs_all)

p_random_wind <- st_read("intermediate/random-shapes/whole-area-control-wind-wind-control-wind-1.shp") %>% 
  st_transform(crs_all)


all_parks <- bind_rows(wind_parks %>% 
                         mutate(runid = "observed") %>% 
                         mutate(number_parks = 1) ,
                       p_random %>% 
                         mutate(runid = "Control random") %>% 
                         mutate(number_parks = 1),
                       p_random_wind %>% 
                         mutate(runid = "Control match wind resource") %>% 
                         mutate(number_parks = 1))

intersect_wind_parks <- st_intersection(all_parks, land_tenure) %>% 
  mutate(area_km2 = as.numeric(st_area(.))/10^6) %>% 
  as_tibble() %>% 
  dplyr::select(Key, 
                category, 
                runid, 
                UF, 
                MUNICIP, 
                power_density = pwr_dns, 
                Strt_yr, 
                INICIO, 
                dat_clos, 
                date_privatization=date, 
                area_km2, 
                number_parks) 
  
intersect_wind_parks %>% 
  write_csv("intermediate/all-intersections-wind.csv")




pv_parks <- load_single_final_pv_parks_from_shape(crs_all)
p <- st_read("intermediate/random-shapes/whole-area-pv-1.shp") %>% 
  st_transform(crs_all)

all_parks <- bind_rows(pv_parks %>% 
                         mutate(runid = "observed") %>% 
                         mutate(number_parks = 1) ,
                       p %>% 
                         mutate(runid = "random-whole") %>% 
                         mutate(number_parks = 1))

intersect_pv_parks <- st_intersection(all_parks, land_tenure) %>% 
  mutate(area_km2 = as.numeric(st_area(.))/10^6) %>% 
  as_tibble() %>% 
  dplyr::select(Key, 
                category, 
                runid, 
                UF, 
                MUNICIP, 
                Start_yr, 
                INICIO, 
                dat_clos, 
                date_privatization=date, 
                area_km2, 
                number_parks) 


intersect_pv_parks %>% 
  write_csv("intermediate/all-intersections-pv.csv")



