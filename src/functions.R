if(!require(gg.layers)) remotes::install_github("rpkgs/gg.layers")

if (!require("pacman")) install.packages("pacman")
require(pacman)
p_load(fs, stringi, writexl, tidyverse, readxl, sf, fuzzyjoin, raster, rgeos,
       irenabpdata, rgdal, lubridate, units, rmapshaper, tidytext, glue, msir, lwgeom,
       gg.layers, sandwich, lmtest, estimatr, networkD3, webshot, ggsankey, viridis, did)

theme_set(theme_bw())
theme_update(text = element_text(size = 10))

sf_use_s2(TRUE)

load_single_final_wind_parks_from_shape <- function(crs_all){
  st_read("intermediate/wind_bloom.shp") %>% 
    mutate(dat_clos = as.Date(dat_cls)) %>% 
    group_by(Key) %>% 
    filter(dat_cls == min(dat_cls)) %>%
    filter(Type == first(Type)) %>% 
    filter(cmpny_nm == first(cmpny_nm)|is.na(cmpny_nm)) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "\\r\\n", "")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "TrairiCE", "Trairi")) %>%  
    mutate(MUNICIP=trimws(MUNICIP)) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Paranatanama", "Paranatama")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Parnaíba, Ilha Grande", "Parnaíba")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Morro do Chpaéu", "Morro do Chapéu")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Santana dos Matos", "Santana do Matos")) %>% 
   st_transform(crs_all) %>% 
    dplyr::select(Key, Start_yr, INICIO, UF, MUNICIP, dat_cls, dat_clos) %>% 
    unique() %>% 
    mutate(unique_id = c(1:n())) 
    
    
  
}

load_single_final_pv_parks_from_shape <- function(crs_all){
  
  st_read("intermediate/pv_bloom.shp") %>% 
    mutate(dat_clos = as.Date(dat_cls)) %>% 
    group_by(AREA_ID)  %>% 
    filter(dat_cls == min(dat_cls)) %>%
    filter(Type == first(Type)) %>% 
    filter(cmpny_nm == first(cmpny_nm)|is.na(cmpny_nm)) %>% 
    filter(POT_MW == max(POT_MW)) %>% 
    filter(INICIO == min(INICIO)) %>% 
    filter(Key == first(Key)) %>%
    unique() %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "\\r\\n", "")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "TrairiCE", "Trairi")) %>% 
    st_transform(crs_all) %>%  
    mutate(MUNICIP=trimws(MUNICIP)) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Paranatanama", "Paranatama")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Parnaíba, Ilha Grande", "Parnaíba")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Morro do Chpaéu", "Morro do Chapéu")) %>% 
    mutate(MUNICIP=str_replace_all(MUNICIP, "Santana dos Matos", "Santana do Matos")) %>% 
    mutate(unique_id = c(1:n()))
  
  }

get_intersecting_municipalities <- function(municipalities, wind_parks, pv_parks){
  
  intersecting_municipalities_wind <- st_intersects(municipalities, wind_parks, sparse = FALSE)
  intersecting_municipalities_pv <- st_intersects(municipalities, pv_parks, sparse = FALSE)
  intersecting_wind_municipalities <- which(rowSums(intersecting_municipalities_wind) > 0)
  intersecting_pv_municipalities <- which(rowSums(intersecting_municipalities_pv) > 0)
  
  municipalities_intersecting_total <- municipalities[union(intersecting_wind_municipalities,
                                                            intersecting_pv_municipalities), ]
  return(municipalities_intersecting_total)
}

merge_all <- function() {
  files <- list.files("intermediate/random-shapes/", pattern = "*.shp")
  f_union <- st_read(glue("intermediate/random-shapes/{files[1]}")) %>% 
    st_union()
  
  
  for(f in files[2:length(files)]){
    f <- st_read(glue("intermediate/random-shapes/{f}")) %>% 
      st_union()
    if(st_crs(f) != st_crs(f_union)){
      
      f <- st_transform(f, st_crs(f_union))
      print("reprojecting")
    }
    f_union <- st_union(f_union, f)
    
    st_write(obj=f_union, 
             dsn="intermediate/union.shp",
             layer_options = "ENCODING=UTF-8", 
             delete_layer = TRUE)
  }
  
  
}

load_wind_data <- function() {
  
  join_bloomberg_aneel_owners_investors <-  get_windparks_aneel_bloomberg_owners_investors()
  
  join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
  join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]
  
  organizations_unique <- get_organizations_windparks()
  
  get_full_owners_investors_aneel_windparks(join_bloomberg_aneel_owners,
                                                           join_bloomberg_aneel_investors,
                                                           organizations_unique)
  

}

load_pv_data <- function(){
  join_bloomberg_aneel_owners_investors <-  get_solar_aneel_bloomberg_owners_investors()
  
  join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
  join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]
  
  organizations_unique <- get_organizations_solarparks()
  
  #### join with bloomberg organizations data to check direct owner area
  
  joined_data <- get_full_owners_investors_aneel_solarparks(join_bloomberg_aneel_owners,
                                                            join_bloomberg_aneel_investors,
                                                            organizations_unique)
  
  return(joined_data)
  
}

figure_investments_by_company <- function(joined_data, file_name){

  tot_area <- joined_data %>% 
    group_by(Type) %>% 
    summarize(area = sum(area)) %>% 
    dplyr::select(area) %>% 
    unlist() %>% 
    .[1]
  
  
  p <- joined_data %>% 
    filter(!is.na(company_name)) %>% 
    mutate(company_name = shorten_company_name(company_info_act, Country)) %>% 
    group_by(company_name, Type) %>% 
    summarize(area = 100 * sum(area) / tot_area) %>% 
    ungroup() %>% 
    group_by(Type) %>% 
    arrange(desc(area)) %>%
    mutate(i = 1:n()) %>% 
    ungroup() %>% 
    filter(i <= 10) %>% 
    ggplot(aes(y = reorder_within(company_name, area, Type), x = area)) +
    geom_bar(stat = "identity") + 
    scale_y_reordered() +
    facet_wrap(.~Type, scale = "free") +
    xlab("Share of total area (%)") +
    ylab("") 
  
  plot(p)
  
  ggsave(file_name, p, width = 10, height = 7)
}

aggregate_data_by_region <- function(joined_data){
  
  joined_data %>%   
    group_by(Country_Category, Start_year, Type) %>% 
    summarize(POT_MW=sum(POT_MW),
              area=sum(area)) %>% 
    dplyr::select(Start_year, Country_Category, Type, POT_MW, area) %>% 
    filter(!is.na(Start_year)) 
}

aggregate_data_by_region_inc_na <- function(joined_data){
  
  joined_data %>%   
    group_by(Country_Category, Start_year, Type) %>% 
    summarize(POT_MW=sum(POT_MW),
              area=sum(area)) %>% 
    dplyr::select(Start_year, Country_Category, Type, POT_MW, area) 
}

aggregate_data_by_region_full <- function(joined_data_region){
  
  full_data_set <- tibble(Start_year=c(2005:2021)) %>% 
    crossing(tibble(Country_Category=joined_data_region$Country_Category %>% unique())) %>% 
    crossing(tibble(Type=joined_data_region$Type %>% unique())) 
  
  full_data_set %>% 
    left_join(joined_data_region) %>% 
    mutate(POT_MW=ifelse(is.na(POT_MW),0,POT_MW))  %>% 
    mutate(area=ifelse(is.na(area),0,area))  
  
}


figure_map_investors <- function(joined_data, file_name) {
  tot_area <- joined_data %>% 
    group_by(Type) %>% 
    summarize(area = sum(area)) %>% 
    dplyr::select(area) %>% 
    unlist() %>% 
    .[1]
  
  
  map.world <- map_data("world")
  
  country_grouped <- joined_data %>% 
    group_by(Country, Type) %>% 
    summarize(area_share = 100 * sum(area) / tot_area) 
  
  p <- map.world %>% 
    full_join(country_grouped, by = c("region" = "Country")) %>% 
    filter(!is.na(Type)) %>% 
    #filter(region != "Brazil") %>% 
    ggplot() +
    geom_polygon(data = map.world, (aes(x= long, y = lat, group = group)), alpha = 0.3) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = area_share)) +
    facet_wrap(.~Type, scale = "free")
  
  plot(p)
  
  ggsave(file_name, p, width = 10, height = 6)
  
  
}

figure_capacity_by_region_timeseries <- function(joined_data_region_full,
                                             file_name,
                                             start_year){
  
  p <- joined_data_region_full %>% 
    filter(Start_year>start_year) %>% 
    ggplot(aes(x=Start_year,y=POT_MW)) +
    geom_area(aes(fill=Country_Category),size=0.5) +
    xlab("Year") +
    ylab("Capacity (MW)") +
    theme_bw() +
    scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
    facet_wrap(.~Type)
  
  plot(p)
  
  ggsave(file_name, p, width=8, height=4)
  
}

figure_area_by_region_timeseries <- function(joined_data_region_full,
                                                 file_name,
                                                 start_year){
  
  p <- joined_data_region_full %>% 
    filter(Start_year>start_year) %>% 
    ggplot(aes(x=Start_year,y=area)) +
    geom_area(aes(fill=Country_Category),size=0.5) +
    xlab("Year") +
    ylab("Area (km2)") +
    theme_bw() +
    scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
    facet_wrap(.~Type)
  
  plot(p)
  
  ggsave(file_name, p, width=8, height=4)
  
}

load_areas_pv <- function(){
  
  pv_areas<-st_read("data/solar-pv_area-manual-area_BR_14-06-2022.shp")
  
  st_make_valid(pv_areas)

}

load_aneel_pv <- function(){
  
  pv_aneel <- st_read("data/solar-pv_operating_min-5MW_UFV_BR_edit-aneel_04-02-2022.shp") 
  
  clean_aneel_names(pv_aneel %>% mutate(NOME_EOL=NOME))

}

derive_shape_file_pv <- function(areas_pv, aneel_pv, joined_data_pv){

  areas_pv %>% 
    dplyr::select(AREA_ID) %>%
    full_join(aneel_pv %>% as_tibble() %>% mutate(AREA.ID=as.numeric(AREA.ID)) , 
              by=c("AREA_ID"="AREA.ID")) %>% 
    dplyr::select(Key, AREA_ID) %>% 
    right_join(joined_data_pv,by=c("Key"="Key")) 

}

find_threshold_overlap <- function(areas,
                                   threshold_overlap,
                                   up){
  
  overlap <- 1000
  j <- 0
  while(overlap > threshold_overlap){
    st_rand <- st_make_valid(areas)
    j <- j + 1
    print(glue("Iteration: {j}"))
    
    x <- runif(1, -up, 0)
    y <- runif(1, -up, 0)
    red_point <- data.frame(x=x, y=y) %>% 
      st_as_sf(coords = c("x", "y"))
    
    # add the two geometries together (just the geometry columns!)
    st_geometry(st_rand) <- st_geometry(st_rand) + red_point$geometry
    
    st_rand <- st_set_crs(st_rand, st_crs(areas))
    
    intersect <- st_intersection(areas, st_rand)
    overlap <- as.numeric(st_area(intersect) %>% sum()/10^6)
  }
  print(glue("Overlap: {overlap}"))
  return(st_rand)
  
}

print_largest_companies <- function(joined_data, investor_type){
  joined_data %>% 
    group_by(Type) %>% 
    mutate(area_sum=sum(area)) %>% 
    ungroup() %>% 
    filter(Type==investor_type) %>% 
    group_by(Country,Type) %>% 
    summarize(a=100*sum(area)/max(area_sum)) %>% 
    arrange(desc(a)) 
}

filter_country <- function(joined_data, country){
  
  joined_data %>% 
    group_by(Type) %>% 
    mutate(area_sum=sum(area)) %>% 
    ungroup() %>% 
    group_by(Country,Type) %>% summarize(a=100*sum(area)/max(area_sum)) %>% 
    arrange(desc(a)) %>% 
    filter(Country == country) %>% 
    print()
  
  
}

write_ownership_investors_by_country_region <- function(joined_data,
                                                        joined_data_region,
                                                 file_name_1,
                                                 file_name_2){
  joined_data_region %>% 
    group_by(Type) %>% 
    mutate(area_sum=sum(area)) %>% 
    ungroup() %>% 
    group_by(Country_Category,Type) %>% summarize(a=100*sum(area)/max(area_sum)) %>% 
    write_csv(file_name_1)
  
  joined_data %>% 
    group_by(Type) %>% 
    mutate(area_sum=sum(area)) %>% 
    ungroup() %>% 
    group_by(Country,Type) %>% summarize(a=100*sum(area)/max(area_sum)) %>% 
    arrange(desc(a)) %>% 
    write_csv(file_name_2)

}


shorten_company_name <- function(company, country, length_tot = 40, linebreak = 20){
  
  ctry_length <- str_length(country)
  
  company_short <- substr(company, 1, length_tot - ctry_length)
  
  #full_name <- company_short
  
  #if(country[1] !=""){
  full_name <- glue("{trimws(company_short)} ({trimws(country)})")
    
  #}
  positions <- str_locate_all(substr(full_name, 1, linebreak), " ")
  index <- mapply(function(i){return(i[length(i)])}, positions)
  
  full_name <- mapply(function(i, name){
    sub(paste0("(.{", i-1, "})."), "\\1\\\n", name)},
                      index,
                      full_name)    
  return(full_name)
  
}

# A connection data frame is a list of flows with intensity for each flow
do_sankey <- function(joined_data, type, nmb_companies, colormap = "D", title = "", direction=-1, alpha = 1, begin = 0, end = 1){
  
  
  joined_data <- joined_data %>% 
    filter(Type %in% c(type))
  
  joined_data <- joined_data %>% 
    mutate(Country_Category = ifelse(Country_Category == "Other", "Other regions", Country_Category)) %>% 
    mutate(parent_Country_Category = ifelse(parent_Country_Category == "Other", "Other regions", parent_Country_Category)) 
  
  joined_data_region <- aggregate_data_by_region_inc_na(joined_data)
  
    
  
  joined_data_cat <- joined_data %>% 
    #filter(!is.na(company_name)) %>% 
    mutate(company_name = company_info_act) %>% 
    group_by(company_name, Type, Country_Category, company_info_act_parent, parent_Country_Category) %>% 
    summarize(area = sum(area)) %>% 
    ungroup() %>% 
    group_by(Type) %>% 
    arrange(desc(area)) %>%
    mutate(i = 1:n()) %>% 
    ungroup() %>% 
    mutate(single = ifelse(i < nmb_companies, company_name, "Other regions")) %>% 
    group_by(single, Country_Category, Type, company_info_act_parent, parent_Country_Category) %>% 
    summarize(area = sum(area)) %>% 
    arrange(area) %>% 
    mutate(parent_Country_Category = ifelse(is.na(parent_Country_Category), "No information", parent_Country_Category))
  
  level2 <- joined_data_cat %>% 
    dplyr::select(source=Country_Category, target=single, value=area) %>% 
    mutate(source = ifelse(is.na(source), "No information", source))
  
  level3 <- joined_data_cat %>% 
    mutate(company_info_act_parent = ifelse(single =="Other regions", "Other regions", company_info_act_parent)) %>% 
    mutate(company_info_act_parent = ifelse(single == company_info_act_parent, glue("{single}"), company_info_act_parent)) %>% 
    dplyr::select(source=single, target=company_info_act_parent, value=area)  %>% 
    mutate(target = ifelse(is.na(target), "Other regions", target)) %>% 
    mutate(source = ifelse(is.na(source), "Other regions", source))
  
  
  level4 <- joined_data_cat %>% 
    mutate(company_info_act_parent = ifelse(single =="Other regions", "Other regions", company_info_act_parent)) %>% 
    mutate(company_info_act_parent = ifelse(single == company_info_act_parent, glue("{single}"), company_info_act_parent)) %>% 
    mutate(parent_Country_Category = glue("{parent_Country_Category}")) %>% 
    dplyr::select(source=company_info_act_parent, target=parent_Country_Category, value=area)  %>% 
    mutate(source = ifelse(is.na(source), "Other regions", source)) %>%  
    mutate(target = ifelse(is.na(target), "No information", target)) 

      
  
  source_data <- joined_data_region %>% 
    group_by(Country_Category) %>% 
    summarize(area = sum(area)) %>% 
    mutate(source = "Total park area") %>% 
    #na.omit() %>%
    dplyr::select(`Total park area`=source, `Direct country`=Country_Category, value=area) %>% 
    mutate(`Direct country` = ifelse(is.na(`Direct country`), "No information", `Direct country`))

    
  source_data_long <- source_data %>% 
    make_long(`Total park area`, `Direct country`, value = value) %>% 
    na.omit()
  
  cname_direct<-paste0("Direct ", tolower(title))
  level2_long <- level2 %>% 
    ungroup() %>% 
    dplyr::select(`Direct country`=source, !!cname_direct:=target, value)%>% 
    make_long(`Direct country`, !!cname_direct, value = value) %>% 
    na.omit() 
  
  cname_parent<-paste0("Parent ", tolower(title))
  level3_long <- level3 %>% 
    ungroup() %>% 
    dplyr::select(!!cname_direct:=source, !!cname_parent:=target, value) %>%  
    make_long(!!cname_direct, !!cname_parent, value = value) %>% 
    na.omit()
  
  level4_long <- level4 %>% 
    ungroup() %>% 
    dplyr::select(!!cname_parent:=source, `Parent country`=target, value) %>%  
    make_long(!!cname_parent, `Parent country`, value = value)
  
  full_data <-  source_data_long %>%
    bind_rows(level2_long) %>% 
    bind_rows(level3_long) %>% 
    bind_rows(level4_long) 
  
  #full_data$x <- recode_factor(full_data$x, Direct = paste("Direct ", tolower(title)), 
  #                                Parent = paste("Parent ", tolower(title)))
  
#  full_data$next_x <- recode_factor(full_data$next_x, Direct = paste("Direct ", tolower(title)), 
  #                             Parent = paste("Parent ", tolower(title)))
  
  
  
    
                      
                      
  
  #full_data$node <- factor(full_data$node, levels=unique(full_data$node[sort(full_data$value)]))
    
  
  p <-   full_data %>% 
    ggplot( aes(x = x,
                next_x = next_x,
                node = node,
                next_node = next_node,
                fill = factor(node),
                value = value,
                label = node)) +
    geom_sankey() +
    geom_sankey_label() +
    theme_sankey(base_size = 20) + 
    theme(legend.position = "none") +
    xlab("") +
    #scale_fill_manual(values = COLORS10)
    scale_fill_viridis(discrete = TRUE, option = colormap, direction = direction, alpha = alpha, begin = begin, end = end) +
    ggtitle(title)
  
  p
  
  
  #nodes <- data.frame(
  #  name=c(as.character(source_data$source), 
  #         as.character(source_data$target)) %>% unique()
  #)
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  #source_data$IDsource <- match(source_data$source, nodes$name)-1 
  #source_data$IDtarget <- match(source_data$target, nodes$name)-1
  
  # Make the Network
  #p <- sankeyNetwork(Links = source_data, Nodes = nodes,
  #                   Source = "IDsource", Target = "IDtarget",
  #                   Value = "value", NodeID = "name", 
  #                   sinksRight=FALSE,
  #                   fontSize=15,
  #                   iterations=iterations)
  
  #saveNetwork(p, filename)
  
  
}


R_PV <- 0.58
R_WIND <- 0.7

get_full_owners_investors_aneel_solarparks<-function(join_bloomberg_aneel_owners,
                                                     join_bloomberg_aneel_investors,
                                                     organizations_unique){
  
  direct_owner <- join_bloomberg_aneel_owners %>%  
    left_join(organizations_unique,by=c("Direct Owner Tickers"="Bloomberg Ticker")) %>%  
    mutate(Type="Direct owner")   %>% 
    mutate(company_info=paste0("Direct owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    dplyr::select(Start_year, UF, INICIO_OPE=INIC_OPER, MUNIC, POT_MW=POT_MW_SHARE, area=area_share_km2, Country, Type, share, Key, company_info, company_name=`Organization Name Matched`, ownership_p, date_close, company_info_act) 
  
  direct_owner %>% summarize(area=sum(area,na.rm=TRUE))
  direct_owner %>% filter(is.na(Country)) %>% nrow()
  
  #### join with bloomberg data to check parent owner area
  parent_owner <- join_bloomberg_aneel_owners %>% 
    left_join(organizations_unique,by=c("Parent Owner Tickers"="Bloomberg Ticker")) %>% 
    mutate(Type="Parent owner") %>%
    mutate(company_info=paste0("Parent owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    dplyr::select(Start_year, UF, INICIO_OPE=INIC_OPER, MUNIC, POT_MW=POT_MW_SHARE,  area=area_share_km2, Country, Type, share, Key, company_info, company_name=`Organization Name Matched`, ownership_p, date_close, company_info_act)
  
  parent_owner %>% summarize(area=sum(area,na.rm=TRUE))
  
  parent_owner %>% filter(is.na(Country)) %>% nrow()
  
  
  #### join with bloomberg data to check investor area, assuming that investors are equally invested
  
  direct_investor <- join_bloomberg_aneel_investors %>% 
    left_join(organizations_unique,by=c("org_name"="Organization Name Matched")) %>% 
    mutate(Type="Direct investor") %>% 
    mutate(company_info=paste0("Direct investor: ",org_name)) %>% 
    mutate(company_info_act=paste0(org_name)) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE=INIC_OPER, MUNIC, POT_MW=POT_MW_SHARE, area=area_share_km2, Country, Type, share, company_info, company_name=org_name, ownership_p, date_close, company_info_act)
  
  nrow(direct_investor)
  
  direct_investor %>% summarize(POT_MW=sum(POT_MW))
  
  direct_investor %>% filter(is.na(Country)) %>% nrow()
  
  xx<-direct_investor %>% filter(is.na(Country))
  
  
  #### join with bloomberg data to check parent of investor area, assuming that investors are equally invested
  parent_investor <- join_bloomberg_aneel_investors %>% 
    #  dplyr::select(`Key`, UF, MUNIC, , POT_MW, POT_MW_SHARE, org_name, Start_year, area_share_km2) %>% 
    left_join(organizations_unique,by=c("org_name"="Organization Name Matched")) %>%
    left_join(organizations_unique,by=c("Parent/Operational Investor"="Organization Name Matched")) %>% 
    mutate(Type="Parent investor") %>% 
    mutate(company_info=paste0("Parent investor: ", org_name, " parent: ", `Parent/Operational Investor`)) %>% 
    mutate(company_info_act=paste0(`Parent/Operational Investor`)) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE=INIC_OPER, MUNIC, POT_MW=POT_MW_SHARE, area=area_share_km2, Country=Country.y, Type, share, company_info, company_name=org_name, ownership_p, date_close, company_info_act)
  
  
  nrow(parent_investor)
  
  #parent_investor %>% summarize(a=sum(area)/100)
  
  parent_investor %>% filter(is.na(Country)) %>% nrow()
  
  joined_data <- bind_rows(direct_owner, 
                           parent_owner,
                           direct_investor,
                           parent_investor) %>%
    mutate(Country_Category=ifelse(Country=="Brazil","Brazil",Country)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Norway", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Canada", "United States"),"North America",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("China","Qatar", "Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia"),"Other",Country_Category))  
  
  
   return(joined_data)
  
}



get_full_owners_investors_aneel_solarparks_including_parents<-function(join_bloomberg_aneel_owners,
                                                     join_bloomberg_aneel_investors,
                                                     organizations_unique){
  
  direct_owner <- join_bloomberg_aneel_owners %>%  
    left_join(organizations_unique,by=c("Direct Owner Tickers"="Bloomberg Ticker")) %>%  
    mutate(Type="Direct owner")   %>% 
    mutate(company_info=paste0("Direct owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    left_join(organizations_unique %>% dplyr::select(`Bloomberg Ticker`, 
                                                     `Organization Name Matched`, 
                                                     parent_country = Country),
              by=c("Parent Owner Tickers"="Bloomberg Ticker"))  %>% 
    mutate(company_info_parent=paste0("Parent owner: ",`Organization Name Matched.y`)) %>% 
    mutate(company_info_act_parent=`Organization Name Matched.y`) %>% 
    dplyr::select(Key, Start_year, UF, INIC_OPER, MUNIC, area=area_share_km2, POT_MW=POT_MW_SHARE, Country, Type, share, Key, company_info, company_name = `Organization Name Matched.x`, ownership_p, date_close, company_info_act, company_info_parent, company_info_act_parent, parent_country) 
  
    
  #### join with bloomberg data to check investor area, assuming that investors are equally invested
  
  direct_investor <- join_bloomberg_aneel_investors %>% 
    left_join(organizations_unique,by=c("org_name"="Organization Name Matched")) %>% 
    mutate(Type="Direct investor") %>% 
    mutate(company_info=paste0("Direct investor: ",org_name)) %>% 
    mutate(company_info_act=paste0(org_name)) %>% 
    left_join(organizations_unique %>% 
                dplyr::select(`Bloomberg Ticker`, 
                              `Organization Name Matched`, 
                              parent_country = Country),
              by=c("Parent/Operational Investor"="Organization Name Matched")) %>%
    mutate(company_info_parent=paste0("Parent investor: ", org_name, " parent: ", `Parent/Operational Investor`)) %>% 
    mutate(company_info_act_parent=`Parent/Operational Investor`) %>% 
    dplyr::select(Key, Start_year, UF, INIC_OPER, MUNIC, area=area_share_km2, POT_MW=POT_MW_SHARE, Country, Type, share, Key, company_info, company_name = org_name, ownership_p, date_close, company_info_act, company_info_parent, company_info_act_parent, parent_country) 
  
    
    
    
    
  joined_data <- bind_rows(direct_owner, 
                           direct_investor) %>%
    mutate(Country_Category=ifelse(Country=="Brazil","Brazil",Country)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Norway", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Canada", "United States"),"North America",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("China","Qatar", "Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia"),"Other",Country_Category))  %>% 
    mutate(parent_Country_Category=ifelse(parent_country=="Brazil","Brazil",parent_country)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Norway", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",parent_Country_Category)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("Canada", "United States"),"North America", parent_Country_Category)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("China","Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia", "Qatar"),"Other", parent_Country_Category))  
  
  
  
  return(joined_data)
  
}

load_aneel_wind <- function(){
  wind_parks_aneel_shape <- st_read("data/wind-parks_JOIN_turbines_power-plants_EOL_BR_aneel_10_05_2023_collected_encoded_fin.shp")  %>% 
    filter(FASE %in% c("Operação", "Construção", "Construção não iniciada"))
  
  clean_aneel_names(wind_parks_aneel_shape) 
  

}

derive_shape_file_wind <- function(aneel_areas_wind, joined_data_wind){
  aneel_areas_wind %>% 
    dplyr::select(Key) %>% 
    full_join(joined_data_wind,by=c("Key"="Key")) %>% 
    filter(!is.na(Start_year))
  
}

get_full_owners_investors_aneel_windparks <- function(join_bloomberg_aneel_owners,
                                                       join_bloomberg_aneel_investors,
                                                       organizations_unique){
  
  direct_owner <- join_bloomberg_aneel_owners %>%  
    left_join(organizations_unique,by=c("Direct Owner Tickers"="Bloomberg Ticker")) %>%  
    mutate(Type="Direct owner")   %>% 
    mutate(company_info=paste0("Direct owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country, Type, share, Key, company_info, company_name = `Organization Name Matched`, ownership_p, date_close, company_info_act) 
  
  direct_owner %>% summarize(area=sum(area)/100)
  
  direct_owner %>% filter(is.na(Country)) %>% nrow()
  
  #### join with bloomberg data to check parent owner area
  parent_owner <- join_bloomberg_aneel_owners %>% 
    left_join(organizations_unique,by=c("Parent Owner Tickers"="Bloomberg Ticker")) %>% 
    mutate(Type="Parent owner") %>%
    mutate(company_info=paste0("Parent owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country, Type, share, company_info, company_name = `Organization Name Matched`, ownership_p, date_close, company_info_act)
  
  parent_owner %>% summarize(area=sum(area,na.rm=TRUE)/100)
  
  parent_owner %>% filter(is.na(Country)) %>% nrow()
  
  
  #### join with bloomberg data to check investor area, assuming that investors are equally invested
  direct_investor <- join_bloomberg_aneel_investors %>% 
    dplyr::select(-Country) %>% 
    #dplyr::select(Key, UF, INICIO_OPE, MUNIC1, POT_MW, POT_MW_SHARE, AREA_M2, AREA_M2_SHARE, org_name, Start_year, ownership_p, date_close)  %>% 
    left_join(organizations_unique,by=c("org_name"="Organization Name Matched"))  %>% 
    mutate(Type="Direct investor") %>% 
    mutate(company_info=paste0("Direct investor: ",org_name)) %>% 
    mutate(company_info_act=org_name) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country, Type, share, company_info, company_name = org_name, ownership_p, date_close, company_info_act)
  
  nrow(direct_investor)
  
  direct_investor %>% summarize(area=sum(area)/100)
  
  direct_investor %>% filter(is.na(Country)) %>% nrow()
  
  #### join with bloomberg data to check parent of investor area, assuming that investors are equally invested
  parent_investor <- join_bloomberg_aneel_investors %>% 
    left_join(organizations_unique,by=c("org_name"="Organization Name Matched")) %>%
    left_join(organizations_unique,by=c("Parent/Operational Investor"="Organization Name Matched")) %>% 
    mutate(Type="Parent investor") %>% 
    mutate(company_info=paste0("Parent investor: ", org_name, " parent: ", `Parent/Operational Investor`)) %>% 
    mutate(company_info_act=`Parent/Operational Investor`) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country=Country, Type, share, company_info, company_name = org_name, date_close, company_info_act)
  
  
  nrow(parent_investor)
  
  parent_investor %>% summarize(a=sum(area)/100)
  
  parent_investor %>% filter(is.na(Country)) %>% nrow()
  
  
  #join all datasets together
  joined_data <- bind_rows(direct_owner, 
                           parent_owner,
                           direct_investor,
                           parent_investor) %>%
    mutate(Country_Category=ifelse(Country=="Brazil","Brazil",Country)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Canada", "United States"),"North America",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("China","Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia"),"Other",Country_Category))  
  
  
  lacking_information <- joined_data %>% 
    filter(is.na(Country_Category)) %>% 
    dplyr::select(company_info) %>% 
    unique() %>% 
    arrange(company_info)
  
  lacking_information %>% 
    write_csv("intermediate/lacking-information-wind.csv")
  
  return(joined_data)
  
}


get_full_owners_investors_aneel_windparks_including_parents <- function(join_bloomberg_aneel_owners,
                                                      join_bloomberg_aneel_investors,
                                                      organizations_unique){
  
  direct_owner <- join_bloomberg_aneel_owners %>%  
    left_join(organizations_unique,by=c("Direct Owner Tickers"="Bloomberg Ticker")) %>%  
    mutate(Type="Direct owner")   %>% 
    mutate(company_info=paste0("Direct owner: ",`Organization Name Matched`)) %>% 
    mutate(company_info_act=`Organization Name Matched`) %>% 
    left_join(organizations_unique %>% dplyr::select(`Bloomberg Ticker`, 
                                                     `Organization Name Matched`, 
                                                     parent_country = Country),
              by=c("Parent Owner Tickers"="Bloomberg Ticker"))  %>% 
    mutate(company_info_parent=paste0("Parent owner: ",`Organization Name Matched.y`)) %>% 
    mutate(company_info_act_parent=`Organization Name Matched.y`) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country, Type, share, Key, company_info, company_name = `Organization Name Matched.x`, ownership_p, date_close, company_info_act, company_info_parent, company_info_act_parent, parent_country) 
  
  #### join with bloomberg data to check investor area, assuming that investors are equally invested
  direct_investor <- join_bloomberg_aneel_investors %>% 
    dplyr::select(-Country) %>% 
    #dplyr::select(Key, UF, INICIO_OPE, MUNIC1, POT_MW, POT_MW_SHARE, AREA_M2, AREA_M2_SHARE, org_name, Start_year, ownership_p, date_close)  %>% 
    left_join(organizations_unique, by=c("org_name"="Organization Name Matched"))  %>% 
    mutate(Type="Direct investor") %>% 
    mutate(company_info=paste0("Direct investor: ",org_name)) %>% 
    mutate(company_info_act=org_name) %>% 
    left_join(organizations_unique %>% 
                dplyr::select(`Bloomberg Ticker`, 
                              `Organization Name Matched`, 
                              parent_country = Country),
              by=c("Parent/Operational Investor"="Organization Name Matched")) %>%
    mutate(company_info_parent=paste0("Parent investor: ", org_name, " parent: ", `Parent/Operational Investor`)) %>% 
    mutate(company_info_act_parent=`Parent/Operational Investor`) %>% 
    dplyr::select(Key, Start_year, UF, INICIO_OPE, MUNIC1, area=AREA_M2_SHARE, POT_MW=POT_MW_SHARE, Country, Type, share, Key, company_info, company_name = org_name, ownership_p, date_close, company_info_act, company_info_parent, company_info_act_parent, parent_country) 

 
  #join all datasets together
  joined_data <- bind_rows(direct_owner, 
                           direct_investor
                           ) %>%
    mutate(Country_Category=ifelse(Country=="Brazil","Brazil",Country)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("Canada", "United States"),"North America",Country_Category)) %>% 
    mutate(Country_Category=ifelse(Country_Category %in% c("China","Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia"),"Other",Country_Category)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country=="Brazil","Brazil",parent_country)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("Denmark", "United Kingdom", "Italy", "Spain", "France", "Germany", "Luxembourg", "Portugal", "Sweden", "Virgin Islands, British", "Isle of Man"),"Europe",parent_Country_Category)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("Canada", "United States"),"North America", parent_Country_Category)) %>% 
    mutate(parent_Country_Category=ifelse(parent_country %in% c("China","Hong Kong", "Bahrain", "Australia", "Argentina", "Colombia"),"Other", parent_Country_Category))  
  
  
  
  lacking_information <- joined_data %>% 
    filter(is.na(Country_Category)) %>% 
    dplyr::select(company_info) %>% 
    unique() %>% 
    arrange(company_info)
  
  lacking_information %>% 
    write_csv("intermediate/lacking-information-wind.csv")
  
  return(joined_data)
  
}


get_organizations_windparks <- function(){
  ####check if anything strange is happening during the merge
  organizations <- read_xlsx("data/organizations_A-Z_2022_matched.xlsx") %>% 
    bind_rows(tibble(`Organization Name Matched`=c("Forca Eolica Do Brasil", 
                                                   "MSU Energy SA",
                                                   "Kingdom of Denmark",
                                                   "Government of Brazil"),
                     `Bloomberg Ticker`=c("x11", "x12","x13","x14"),
                     `Country`=c("Brazil", "Argentina","Denmark","Brazil"),
                     `Parent/Operational Investor`=c("Neoenergia Renovaveis SA", 
                                                     "MSU Energy SA", 
                                                     "Kingdom of Denmark",
                                                     "Government of Brazil"))) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Cia Paranaense de Energia","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Petroleo Brasileiro SA","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Superintendencia do Desenvolvimento do Nordeste","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(startsWith(`Organization Name Matched`,"Fundacao Ele"),"Engie Brasil Energia SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Neoenergia Renovaveis SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Neoenergia SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="PEC Energia SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Servtec Energia Ltd","Grupo HLC Sgps",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Horizonte Energias Renovaveis Ltda","Cemig Geracao e Transmissao SA",`Parent/Operational Investor`)) 
  
  organizations %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Gestamp Eolica Brasil SA",1,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="KfW",2,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Sowitec do Brasil Energias Alternativas Ltda",3,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Fundacao Eletrosul de Previdencia e Assistencia Social",4,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Horizonte Energias Renovaveis Ltda",5,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Renobrax Energias Renovaveis",6,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Sequoia Capital Ltda",7,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Servtec Energia Ltd",8,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Superintendencia do Desenvolvimento do Nordeste",9,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Ventos Brasil Comercio e Representacoes Ltda",10,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Martifer Renewables SGPS SA",11,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Grupo HLC Sgps",12,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Rio Sul 1 Energia Ltda",13,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="State Grid Brazil Power Participacoes SA",14,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="SoWiTec International GmbH",15,`Bloomberg Ticker`)) %>% 
    mutate(id=1:n()) %>% 
    group_by(`Bloomberg Ticker`) %>% 
    filter(id==min(id)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`%in% c("Actis LLP",
                                                                                  "Banco Santander SA",
                                                                                  "BNP Paribas SA",
                                                                                  "Brazil Energy SA",
                                                                                  "Casa dos Ventos Energias Renovaveis S/A",
                                                                                  "CER-Cia de Energias Renovaveis",
                                                                                  "Citigroup Inc",
                                                                                  "Energisa SA",
                                                                                  "Inveravante Inversiones Universales SL",
                                                                                  "J Malucelli Energia SA",
                                                                                  "Omega Energia Renovavel S/A",
                                                                                  "Petra Asset Gestao de Investimentos Ltda",
                                                                                  "Renova Energia SA",
                                                                                  "Renobrax Energias Renovaveis",
                                                                                  "Sequoia Capital Ltda",
                                                                                  "STOA Infra & Energy SAS",
                                                                                  "V2i Transmissao de Energia Eletrica SA",
                                                                                  "Vale SA",
                                                                                  "Ventos Brasil Comercio e Representacoes Ltda",
                                                                                  "Bioenergy Geradora de Energia Ltda",
                                                                                  "Ecopart Investimentos SA"),
                                                `Organization Name Matched`,
                                                `Parent/Operational Investor`))  %>% 
    return()
}


get_windparks_aneel_bloomberg_owners_investors <- function(){
  sf_use_s2(TRUE)
  
  full_data_bloomberg<-load_bloomberg_data()
  
  wind_parks_bloomberg_all_transactions <- clean_bloomberg_names(full_data_bloomberg)
  
  wind_parks_bloomberg <- wind_parks_bloomberg_all_transactions %>% 
    dplyr::select(Name,Key) %>%   
    unique()

  #wind_parks_aneel <- st_read("data/wind-parks_JOIN_turbines_power-plants_EOL_BR_aneel_04-02-2022.shp")  %>% 
  #  filter(FASE %in% c("Operação", "Construção", "Construção não iniciada"))
  
    
  wind_parks_aneel <- st_read("data/wind-parks_JOIN_turbines_power-plants_EOL_BR_aneel_10_05_2023_collected_encoded_fin.shp") %>% 
    filter(FASE %in% c("Operação", "Construção", "Construção não iniciada")) %>% 
    mutate(area_check = as.numeric(st_area(.))/10^6) %>% 
    mutate(AREA_M2 = area_check) %>% 
    mutate(POT_MW = as.numeric(POT_MW))
  
  #st_geometry(wind_parks_aneel) <- st_geometry(wind_parks_aneel_geometry)
  
  ###area bahia
  wind_parks_aneel<-clean_aneel_names(wind_parks_aneel) %>% 
    as_tibble()
  
  wind_parks_aneel_shape<-clean_aneel_names(wind_parks_aneel) 
  
  wind_parks_bloomberg_all_transactions_owners<-wind_parks_bloomberg_all_transactions %>% mutate(n=1:n()) %>%  group_by(Key,`Direct Owner Tickers`) %>% 
    filter(n==min(n))
  
  owners <- wind_parks_aneel %>% 
    right_join(wind_parks_bloomberg_all_transactions_owners,
               by=c('Key'='Key'))  %>%
    mutate(Start_year=year(ymd(INICIO_OPE))) %>% 
    dplyr::select(Key, UF, FASE, MUNIC1, POT_MW, INICIO_OPE, AREA_M2, `Direct Owner Tickers`, `Parent Owner Tickers`, Start_year,`% of ownership`, org_name, date_close) %>%   
    unique() %>% 
    group_by(Key) %>% 
    mutate(share=`% of ownership`/sum(`% of ownership`)) %>% 
    mutate(AREA_M2_SHARE=AREA_M2*share) %>% 
    mutate(POT_MW_SHARE=POT_MW*share) %>% 
    mutate(ownership_p=`% of ownership`) %>% 
    ungroup()  %>% 
    filter(!is.na(Start_year)) 
  
  investors <- wind_parks_aneel %>% 
    right_join(wind_parks_bloomberg_all_transactions,
               by=c('Key'='Key'))  %>%
    mutate(AREA_M2=area_check)  %>%
    mutate(Start_year=year(ymd(INICIO_OPE))) %>% 
    group_by(Key) %>% 
    mutate(debt = max(deb_provid, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(Key, eq_provid, debt) %>% 
    mutate(share_int = 1/n()) %>% 
    dplyr::select(Key, eq_provid, deb_provid, Country, AREA_M2, share_int, POT_MW, UF, INICIO_OPE, MUNIC1, Start_year,`% of ownership`, org_name, date_close) %>% 
    mutate(share = 0) %>% 
    mutate(share = ifelse(eq_provid & debt, (1 - R_PV) * share_int, ifelse(eq_provid, share_int, share))) %>% 
    mutate(share = ifelse(deb_provid, R_PV * share_int, share)) %>% 
    ungroup() %>%     
    group_by(Key) %>% 
    mutate(POT_MW_SHARE = POT_MW * share) %>% 
    mutate(AREA_M2_SHARE = AREA_M2 * share) %>% 
    mutate(ownership_p=NA) %>% 
    ####debt or equity
    ####
    ungroup() %>% 
    filter(!is.na(Start_year)) %>% 
    mutate(org_name=ifelse(org_name=="Echoenergia e Participacoes SA", "Echoenergia Participacoes SA", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Enernova-Novas Energias SA", "Enernova Novas Energias SA", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Kirton Bank SA Banco Multiplo", "Kirton Bank SA", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Petra Asset Gestao de Investimentos Ltda/Brazil", "Petra Asset Gestao de Investimentos Ltda", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Total Eren SA", "Total Eren", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Ventos do Sul Energia SA", "Ventos do Sul Energia S/A", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Neoenergia SA", "Neoenergia Renovaveis SA", org_name)) 
  
  return(list(owners,investors))
  
}



get_solar_aneel_bloomberg_owners_investors<-function(){
  
  
  
  full_data_bloomberg<-load_bloomberg_data()
  
  solar_parks_bloomberg_all_transactions<-clean_bloomberg_names(full_data_bloomberg, "Solar")
  
  solar_parks_bloomberg <- solar_parks_bloomberg_all_transactions %>% 
    filter(`Capacity (MW)`>5) %>% 
    dplyr::select(Name,Key,Bloomberg_Name=`Project name`,geometry) %>%   
    unique() 
  
  solar_parks_bloomberg_full <- solar_parks_bloomberg_all_transactions %>% 
    filter(`Capacity (MW)`>5) %>% 
    dplyr::select(Name,Key, Bloomberg_Name=`Project name`, abstract, descriptio, prj_abstra, prj_descri, trans_name) %>%   
    group_by(Name, Key) %>% 
    summarize(abstract=paste0(abstract, collapse=''),
              descriptio=paste0(descriptio, collapse=''),
              prj_abstra=paste0(prj_abstra, collapse=''),
              prj_descri=paste0(prj_descri, collapse=''),
              trans_name=paste0(trans_name, collapse='')) %>% 
    unique()
  
  solar_parks_aneel <- st_read("data/solar-pv_operating_min-5MW_UFV_BR_edit-aneel_04-02-2022.shp") 
  
  solar_parks_aneel<-clean_aneel_names(solar_parks_aneel %>% mutate(NOME_EOL=NOME))
  
  solar_parks_aneel_areas<-st_read("data/solar-pv_area-manual-area_BR_14-06-2022.shp")
  
  solar_parks_aneel_areas<-st_make_valid(solar_parks_aneel_areas)
  
  
  #####Better do according to capacity!
  solar_parks_aneel_area_join <- solar_parks_aneel %>% 
    as_tibble() %>% 
    mutate(AREA.ID=as.numeric(AREA.ID)) %>% 
    full_join(solar_parks_aneel_areas %>% as_tibble(),
              by=c("AREA.ID"="AREA_ID")) %>% 
    group_by(AREA.ID) %>% 
    mutate(area_km2=area_calcu/n()/10^6)
  
  
  solar_parks_bloomberg_all_transactions_owners<-solar_parks_bloomberg_all_transactions %>% 
    ungroup() %>% 
    mutate(n=1:n()) %>%  
    group_by(Key,`Direct Owner Tickers`) %>% 
    filter(n==min(n)) %>% 
    as_tibble()
  
  join_bloomberg_aneel_owners <-  solar_parks_aneel_area_join %>% 
    right_join(solar_parks_bloomberg_all_transactions_owners,
               by=c('Key'='Key')) %>% 
    mutate(Start_year=year(ymd(INIC_OPER))) %>% 
    mutate(POT_MW=POT_KW/1000) %>% 
    dplyr::select(Key, UF, INIC_OPER, MUNIC, POT_MW, area_km2,`Direct Owner Tickers`, `Parent Owner Tickers`, Start_year,`% of ownership`, org_name, date_close) %>%   
    unique() %>% 
    group_by(Key) %>% 
    mutate(share=`% of ownership`/sum(`% of ownership`)) %>% 
    mutate(POT_MW_SHARE=POT_MW*share) %>% 
    mutate(area_share_km2=area_km2*share) %>%
    mutate(ownership_p=`% of ownership`) %>% 
    ungroup() %>% 
    filter(!is.na(Start_year))
  
  
  
  join_bloomberg_aneel_investors <- solar_parks_aneel_area_join %>% 
    right_join(solar_parks_bloomberg_all_transactions %>% as_tibble(),
               by=c('Key'='Key'))  %>%
    mutate(Start_year=year(ymd(INIC_OPER))) %>% 
    mutate(POT_MW=POT_KW/1000) %>% 
    group_by(Key) %>% 
    mutate(debt = max(deb_provid, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(Key, eq_provid, debt) %>% 
    mutate(share_int = 1/n()) %>% 
    dplyr::select(Key, eq_provid, deb_provid, area_km2, share_int, POT_MW, UF, INIC_OPER, MUNIC, Start_year,`% of ownership`, org_name, date_close) %>% 
    mutate(share = 0) %>% 
    mutate(share = ifelse(eq_provid & debt, (1 - R_PV) * share_int, ifelse(eq_provid, share_int, share))) %>% 
    mutate(share = ifelse(deb_provid, R_PV * share_int, share)) %>% 
    mutate(POT_MW_SHARE=POT_MW * share) %>% 
    mutate(area_share_km2=area_km2 * share) %>% 
    mutate(ownership_p=NA) %>% 
    ungroup() %>%  
    filter(!is.na(Start_year)) %>% 
    dplyr::select(Key, UF, INIC_OPER, MUNIC, POT_MW, POT_MW_SHARE, area_share_km2, Start_year,`% of ownership`, org_name, ownership_p, date_close, share)  %>% 
    mutate(org_name=ifelse(org_name=="Canadian Solar Brasil Comercializacao Importacao e Exportacao de Paineis Solare","Canadian Solar Brasil Comercializacao Importacao e Exportacao de Paineis Solares", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="KfW", "KFW Development Bank", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Solatio Energia","Solatio Brasil Gestao de Projetos Solares Ltda", org_name)) %>% 
    mutate(org_name=ifelse(org_name=="Sunedison Brasil","SunEdison Brasil - Projetos Montagem e Instalacao de Empreendimentos de Energia", org_name))
  
  
  return(list(join_bloomberg_aneel_owners, join_bloomberg_aneel_investors))
}

get_organizations_solarparks<-function(){
  organizations <- read_xlsx("data/organizations_A-Z_2022_matched.xlsx") %>% 
    bind_rows(tibble(`Organization Name Matched`=c("Forca Eolica Do Brasil", 
                                                   "MSU Energy SA",
                                                   "Kingdom of Denmark",
                                                   "Government of Brazil",
                                                   "Government of Minas Gerais",
                                                   "Grupo Perfin",
                                                   "Grupo Rio Alto",
                                                   "Solatio"),
                     `Bloomberg Ticker`=c("x11", "x12","x13","x14","x15","x16","x17","x18"),
                     `Country`=c("Brazil", "Argentina","Denmark","Brazil","Brazil","Brazil","Brazil","Spain"),
                     `Parent/Operational Investor`=c("Neoenergia Renovaveis SA", 
                                                     "MSU Energy SA", 
                                                     "Kingdom of Denmark",
                                                     "Government of Brazil",
                                                     "Government of Minas Gerais",
                                                     "Grupo Perfin",
                                                     "Grupo Rio Alto",
                                                     "Solatio"))) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Cia Paranaense de Energia","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Petroleo Brasileiro SA","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Superintendencia do Desenvolvimento do Nordeste","Government of Brazil",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(startsWith(`Organization Name Matched`,"Fundacao Ele"),"Engie Brasil Energia SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Neoenergia Renovaveis SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Neoenergia SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="PEC Energia SA","Iberdrola SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Servtec Energia Ltd","Grupo HLC Sgps",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Horizonte Energias Renovaveis Ltda","Cemig Geracao e Transmissao SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Cia Energetica de Minas Gerais","Government of Minas Gerais",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="EDP - Energias do Brasil SA","EDP - Energias de Portugal SA",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Inter-American Investment Corp","Inter-American Development Bank",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Mori Energia","Grupo Perfin",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Rio Alto Energia Empreendimentos e Participacoes Ltda","Grupo Rio Alto",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Solatio Energia","Solatio",`Parent/Operational Investor`)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`=="Solatio Brasil Gestao de Projetos Solares Ltda","Solatio",`Parent/Operational Investor`)) 
  
  
  organizations_unique <- organizations %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Canadian Solar Brasil Comercializacao Importacao e Exportacao de Paineis Solares",1,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Fram Capital Distribuidora de Titulos e Valores Mobiliarios SA",2,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="KFW Development Bank",3,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Sertao Brasil Energia Solar Eireli - ME",4,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Sindustrial Engenharia Ltda",5,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Solatio Brasil Gestao de Projetos Solares Ltda",6,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Steelcons Empreiteira Construcao Civil Ltda",7,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="SunEdison Brasil - Projetos Montagem e Instalacao de Empreendimentos de Energia",8,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Total Eren SA",9,`Bloomberg Ticker`)) %>% 
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Verde Vale Energia Ltda",10,`Bloomberg Ticker`)) %>%
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Vila Energia Renovavel SS Ltda",11,`Bloomberg Ticker`)) %>%
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="SESA Bidco Ltd",12,`Bloomberg Ticker`)) %>%
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Martifer Renewables SGPS SA",13,`Bloomberg Ticker`)) %>%
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="Mori Energia Solar",14,`Bloomberg Ticker`)) %>%
    mutate(`Bloomberg Ticker`=ifelse(`Organization Name Matched`=="D2C Energias Renovavies e Participacoes Ltda",15,`Bloomberg Ticker`)) %>%
    mutate(id=1:n()) %>% 
    group_by(`Bloomberg Ticker`) %>% 
    filter(id==min(id)) %>% 
    mutate(`Parent/Operational Investor`=ifelse(`Organization Name Matched`%in% c("Actis LLP",
                                                                                  "Banco Santander SA",
                                                                                  "BNP Paribas SA",
                                                                                  "Brazil Energy SA",
                                                                                  "Casa dos Ventos Energias Renovaveis S/A",
                                                                                  "China Development Bank",
                                                                                  "CER-Cia de Energias Renovaveis",
                                                                                  "Citigroup Inc",
                                                                                  "DNB Bank ASA",
                                                                                  "Energisa SA",
                                                                                  "Fram Capital Distribuidora de Titulos e Valores Mobiliarios SA",
                                                                                  "Inveravante Inversiones Universales SL",
                                                                                  "J Malucelli Energia SA",
                                                                                  "Kroma Comercializadora de Energia Ltda",
                                                                                  "Omega Energia Renovavel S/A",
                                                                                  "Petra Asset Gestao de Investimentos Ltda",
                                                                                  "Quaatro Participacoes",
                                                                                  "Renova Energia SA",
                                                                                  "Renobrax Energias Renovaveis",
                                                                                  "Sequoia Capital Ltda",
                                                                                  "Sertao Brasil Energia Solar Eireli - ME",
                                                                                  "Sindustrial Engenharia Ltda",
                                                                                  "Steelcons Empreiteira Construcao Civil Ltda",
                                                                                  "STOA Infra & Energy SAS",
                                                                                  "V2i Transmissao de Energia Eletrica SA",
                                                                                  "Vale SA",
                                                                                  "Ventos Brasil Comercio e Representacoes Ltda",
                                                                                  "Vila Energia Renovavel SS Ltda",
                                                                                  "Bioenergy Geradora de Energia Ltda",
                                                                                  "Ecopart Investimentos SA",
                                                                                  "Verde Vale Energia Ltda"),
                                                `Organization Name Matched`,
                                                `Parent/Operational Investor`)) 
  
  return(organizations_unique)
}

#is broken....

spatial_join_date<-function(shape_file, area_land_tenure){
  
  shape_file <- shape_file %>% 
    filter(Type=="Direct investor")
  
  area_land_tenure <- area_land_tenure %>% dplyr::select(date_subm,date_appr) 
  area_land_tenure <- st_transform(area_land_tenure, crs(shape_file))
  
  join_land_tenure_bloomberg <- st_join(area_land_tenure, 
          shape_file,
          left=FALSE)
  
  private_land_bloomberg <- join_land_tenure_bloomberg %>% 
    as_tibble() %>% 
    dplyr::select(Key,date_subm,date_appr,date_close) %>% 
    mutate(date_close=mdy(date_close)) %>% 
    mutate(diff_subm=date_close-date_subm) %>% 
    mutate(diff_appr=date_close-date_appr)
  
  return(private_land_bloomberg)
    
}


clean_bloomberg_names<-function(full_data, Technology="Wind"){
  full_data_selection<-full_data %>% 
    filter(Type==Technology) %>%
    mutate(Name=`Project name`) %>% 
    mutate(Key=str_replace(Name, " Wind Farm","")) %>% 
    mutate(Key=str_replace(Key, "IMPSA ","")) %>% 
    mutate(Key=str_replace(Key, "CPFL ","")) %>% 
    mutate(Key=str_replace(Key, "Actis ","")) %>% 
    mutate(Key=str_replace(Key, "AES ","")) %>% 
    mutate(Key=str_replace(Key, "Omega ","")) %>% 
    mutate(Key=str_replace(Key, "Enel ","")) %>% 
    mutate(Key=str_replace(Key, "Engie ","")) %>% 
    mutate(Key=str_replace(Key, "SIIF ","")) %>% 
    mutate(Key=str_replace(Key, "DESA ","")) %>% 
    mutate(Key=str_replace(Key, "Chesf ","")) %>% 
    mutate(Key=str_replace(Key, "CHESF ","")) %>% 
    mutate(Key=str_replace(Key, "Copel ","")) %>% 
    mutate(Key=str_replace(Key, "EDP ","")) %>% 
    mutate(Key=str_replace(Key, "Dobreve ","")) %>% 
    mutate(Key=str_replace(Key, "EDF ","")) %>% 
    mutate(Key=str_replace(Key, "CER ","")) %>% 
    mutate(Key=str_replace(Key, "Casa dos Ventos ","")) %>% 
    mutate(Key=str_replace(Key, "Casa Dos Ventos ","")) %>% 
    mutate(Key=str_replace(Key, "Desenvix ","")) %>% 
    mutate(Key=str_replace(Key, "Voltalia ","")) %>% 
    mutate(Key=str_replace(Key, "Ventos Tecnologia ","")) %>% 
    mutate(Key=str_replace(Key, " Parazinho","")) %>% 
    mutate(Key=str_replace(Key, "Wobben ","")) %>% 
    mutate(Key=str_replace(Key, "Gestamp ","")) %>% 
    mutate(Key=str_replace(Key, "Brasventos ","")) %>% 
    mutate(Key=str_replace(Key, "Tiete ","")) %>% 
    mutate(Key=str_replace(Key, "NEO ","")) %>% 
    mutate(Key=str_replace(Key, "Odebrecht ","")) %>% 
    mutate(Key=str_replace(Key, "Neoenergia ","")) %>% 
    mutate(Key=str_replace(Key, "Rio Energy ","")) %>% 
    mutate(Key=str_replace(Key, "Tractebel ","")) %>% 
    mutate(Key=str_replace(Key, "Queiroz Galvao ","")) %>% 
    mutate(Key=str_replace(Key, "Pacific Hydro ","")) %>% 
    mutate(Key=str_replace(Key, "Sequoia Capital ","")) %>% 
    mutate(Key=str_replace(Key, "Renova Energia ","")) %>% 
    mutate(Key=str_replace(Key, "Atlantic Energias Renovaveis ","")) %>% 
    mutate(Key=str_replace(Key, "Darby and Servtec ","")) %>% 
    mutate(Key=str_replace(Key, "Eletrosul ","")) %>% 
    mutate(Key=str_replace(Key, "Galvao Energia ","")) %>% 
    mutate(Key=str_replace(Key, "Renovaveis ","")) %>% 
    mutate(Key=str_replace(Key, "Energias Renovaveis ","")) %>% 
    mutate(Key=str_replace(Key, "Bioenergy ","")) %>% 
    mutate(Key=str_replace(Key, "Furnas ","")) %>% 
    mutate(Key=str_replace(Key, "Petrobras ","")) %>% 
    mutate(Key=str_replace(Key, "Total Eren ","")) %>% 
    mutate(Key=str_replace(Key, "V2i Transmissao ","")) %>% 
    mutate(Key=str_replace(Key, "Energisa ","")) %>% 
    mutate(Key=str_replace(Key, "Atlantic Energias ","")) %>% 
    mutate(Key=str_replace(Key, "Enerbrasil ","")) %>% 
    mutate(Key=str_replace(Key, "Tramandai ","")) %>% 
    mutate(Key=str_replace(Key, "Servtec ","")) %>% 
    mutate(Key=str_replace(Key, "Pec Energia ","")) %>% 
    mutate(Key=str_replace(Key, "PEC Energia ","")) %>% 
    mutate(Key=str_replace(Key, "Votorantim ","")) %>% 
    mutate(Key=str_replace(Key, "Atlantic ","")) %>% 
    mutate(Key=str_replace(Key, "Energias ","")) %>% 
    mutate(Key=str_replace(Key, "ENERGEN ","")) %>% 
    mutate(Key=str_replace(Key, "Energia ","")) %>% 
    mutate(Key=str_replace(Key, "Eolica Energia ","")) %>% 
    mutate(Key=str_replace(Key, "Vale ","")) %>% 
    mutate(Key=str_replace(Key, "Rosa dos Ventos ","")) %>% 
    mutate(Key=str_replace(Key, "Rio Energy ","")) %>% 
    mutate(Key=str_replace(Key, "Brasil ","")) %>% 
    mutate(Key=str_replace(Key, "PVPlant","")) %>% 
    arrange(Name) %>% 
    mutate(Key=str_replace_all(Key, " 0","")) %>% 
    mutate(Key=str_replace_all(Key, " ","")) %>% 
    mutate(Key=str_replace(Key, "CalangoIV","Calango4")) %>% 
    mutate(Key=str_replace(Key, "CalangoII","Calango2")) %>% 
    mutate(Key=str_replace(Key, "CalangoIII","Calango3")) %>% 
    mutate(Key=str_replace(Key, "CalangoI","Calango1")) %>% 
    mutate(Key=str_replace(Key, "CalangoV","Calango5")) %>% 
    mutate(Key=str_replace(Key, "ReiDosVentosI","ReiDosVentos1")) %>% 
    mutate(Key=str_replace(Key, "MangueSecoV","MangueSeco5")) %>% 
    mutate(Key=str_replace(Key, "CaetiteII","Caetite2")) %>% 
    mutate(Key=str_replace(Key, "SantaJoana","VentosdeSantaJoana")) %>% 
    mutate(Key=str_replace(Key, "ReiDosVentosIII","ReiDosVentos3")) %>% 
    mutate(Key=str_replace(Key, "MangueSecoIII","MangueSeco3")) %>% 
    mutate(Key=str_replace(Key, "MelII","Mel2")) %>% 
    mutate(Key=str_replace(Key, "Carcara1","CarcaraI")) %>% 
    mutate(Key=str_replace(Key, "Carcara1","CarcaraII")) %>% 
    mutate(Key=str_replace(Key, "SaoClemente","VentosdeSaoClemente")) %>% 
    mutate(Key=str_replace(Key, "Carcara2","CarcaraII")) %>% 
    mutate(Key=str_replace(Key, "SantaBrigidaI","VentosdeSantaBrigidaI")) %>% 
    mutate(Key=str_replace(Key, "SaoJoaoEOL","SaoJoao")) %>% 
    mutate(Key=str_replace(Key, "SantoEstevao","VentosdeSantoEstevao")) %>%
    mutate(Key=str_replace(Key, "AlubarMangueSecoI","MangueSeco1")) %>% 
    mutate(Key=str_replace(Key, "RiodoFogo","RN15-RiodoFogo")) %>% 
    mutate(Key=str_replace(Key, "PVPlant","")) %>% 
    mutate(Key=str_replace(Key, "GPG","")) %>% 
    mutate(Key=str_replace(Key, "SER","")) %>% 
    mutate(Key=str_replace(Key, "DG","")) %>% 

    mutate(Key=str_replace(Key, "Brookfield","")) %>% 
    mutate(Key=str_replace(Key, "Eletronorte","")) %>% 
    mutate(Key=str_replace(Key, "EuropeanEnergy","")) %>% 
    mutate(Key=str_replace(Key, "MPX","")) %>% 
    mutate(Key=str_replace(Key, "CanadianSolar&","")) %>% 
    mutate(Key=str_replace(Key, "SunEdison","")) %>% 
    mutate(Key=str_replace(Key, "NebrasPower","")) %>% 
    mutate(Key=str_replace(Key, "Steelcons","")) %>% 
    mutate(Key=str_replace(Key, "Multiplan","")) %>% 
    mutate(Key=str_replace(Key, "PajeudoVentos","PajeudoVento")) %>% 
    mutate(Key=str_replace(Key, "VentosdeSaoAbraao","VentosdeSantoAbraao")) %>% 
    mutate(Key=str_replace(Key, "DunasdoParacuru","DunasdeParacuru")) %>% 
    mutate(Key=ifelse(Key=="ArizonaI","Arizona1",Key)) %>% 
    mutate(Key=str_replace(Key, "Solatio","")) %>% 
    mutate(Key=str_replace(Key, "SolarTechnology","")) %>% 
    mutate(Key=str_replace(Key, "Martifer","")) %>% 
    mutate(Key=str_replace(Key, "SolarTecnologia","")) %>% 
    mutate(Key=str_replace(Key, "Sindustrialand","")) %>% 
    mutate(Key=str_replace(Key, "Martifer",""))  %>% 
    mutate(Key=ifelse(str_detect(Key, "SerraDaBabiloniaE"),"SerradaBabiloniaE", Key)) %>% 
    mutate(Key=ifelse(Key=="Cacimbas","Cacimbas1",Key)) %>% 
    mutate(Key=ifelse(Key=="Caetite2I","Caetite3",Key)) %>%  
    mutate(Key=ifelse(Key=="CaetiteI","Caetite1",Key)) %>% 
    mutate(Key=ifelse(Key=="Calango2I","Calango3",Key)) %>% 
    mutate(Key=ifelse(Key=="CidreiraI","ElebrasCidreiraI",Key)) %>% 
    mutate(Key=ifelse(Key=="CopelGuajiru","DreenGuajiru",Key)) %>% 
    mutate(Key=ifelse(Key=="Cutia","DreenCutia",Key)) %>% 
    mutate(Key=ifelse(Key=="EngieGuajiru","Guajiru",Key)) %>% 
    mutate(Key=ifelse(Key=="EolicaPedradoReino","PedradoReinoIII",Key)) %>% 
    mutate(Key=ifelse(Key=="Horizonte","AguaDoce",Key)) %>% 
    mutate(Key=ifelse(Key=="IbirapuitaI","Ibirapuita1",Key)) %>% 
    mutate(Key=ifelse(Key=="IMPSABomJardim","BomJardim",Key)) %>% 
    mutate(Key=ifelse(Key=="MiassabaIII","Miassaba3",Key)) %>% 
    mutate(Key=ifelse(Key=="Parazinho","VentosdoParazinho",Key)) %>% 
    mutate(Key=ifelse(Key=="PedraCheirosaI","PedraCheirosa",Key)) %>% 
    mutate(Key=ifelse(Key=="PedradoReinoIV","PedradoReinoIII",Key)) %>% 
    mutate(Key=ifelse(Key=="ReiDosVentos1II","ReiDosVentos3",Key)) %>% 
    mutate(Key=ifelse(Key=="RioEnergy SerraDaBabiloniaE","SerradaBabiloniaE",Key)) %>% 
    mutate(Key=ifelse(Key=="RosaDosVentosCanoaQuebrada","CanoaQuebradaII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantaBrigidaV","VentosdeSantaBrigidaV",Key)) %>% 
    mutate(Key=ifelse(Key=="SantaBrigidaVI","VentosdeSantaBrigidaVI",Key)) %>% 
    mutate(Key=ifelse(Key=="SantaBrigidaVII","VentosdeSantaBrigidaVII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantanderCassinoI","REBCassinoI",Key)) %>% 
    mutate(Key=ifelse(Key=="SantanderCassinoII","REBCassinoII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantanderCassinoIII","REBCassinoIII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoI","VentosdeSantoAugustoI",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoII","VentosdeSantoAugustoII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoIV","VentosdeSantoAugustoIV",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoVI","VentosdeSantoAugustoVI",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoVII","VentosdeSantoAugustoVII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoAugustoVIII","VentosdeSantoAugustoVIII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoOnofreI","VentosdeSantoOnofreI",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoOnofreII","VentosdeSantoOnofreII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoOnofreIII","VentosdeSantoOnofreIII",Key)) %>% 
    mutate(Key=ifelse(Key=="SantoOnofreIV","VentosdeSantoOnofreIV",Key)) %>% 
    mutate(Key=ifelse(Key=="SaoVirgilio1","VentosdeSaoVirgilio1",Key)) %>% 
    mutate(Key=ifelse(Key=="SaoVirgilio2","VentosdeSaoVirgilio2",Key)) %>% 
    mutate(Key=ifelse(Key=="SaoVirgilio3","VentosdeSaoVirgilio3",Key)) %>% 
    mutate(Key=ifelse(Key=="ServtecCanoaQuebrada","BonsVentos",Key)) %>% 
    mutate(Key=ifelse(Key=="VentosDeSantaMonica","SantaMonica",Key)) %>% 
    mutate(Key=ifelse(Key=="VentosDeSantaUrsula","SantaUrsula",Key)) %>% 
    mutate(Key=ifelse(Key=="VentosdeSaoClemente","SaoClemente",Key)) %>% 
    mutate(Key=ifelse(Key=="VentosdeSaoMario","VentosdoSaoMario",Key)) %>% 
    mutate(Key=ifelse(Key=="VentosdoSulOsorio","Osorio",Key)) 
  
    if(Technology=="Wind"){
    full_data_selection <- full_data_selection %>% 
    filter(!(Key %in%c("CaetiteC", 
                       "ItaremaVI", 
                       "Macau", 
                       "Farol", 
                       "MorrodosVentosII", 
                       "OlhodAgua", 
                       "OuroVerde",
                       "PilotodeRioGrande",
                       "SantaMaria",
                       "SantaMonica",
                       "SantoUriel",
                       "SaoBentodoNorte",
                       "Tubarao",
                       "VentosdaBahiaIII",
                       "VentosdeSaoJanuario10",
                       "VilaRioGrandedoNorteI",
                       "WobbenBomJardim",
                       "Delta5I","Delta5II","Delta6I","Delta6II","AratuaPhaseI", "PedraPreta", "BoaVista")))
    }else{
      
      full_data_selection <- full_data_selection %>% 
        mutate(Key=ifelse(Key=="Brigida1","Brigida",Key)) %>% 
        mutate(Key=ifelse(Key=="EquinorandScatecApodiI","ApodiI",Key)) %>% 
        mutate(Key=ifelse(Key=="EquinorandScatecApodiII","ApodiII",Key)) %>% 
        mutate(Key=ifelse(Key=="EquinorandScatecApodiIII","ApodiIII",Key)) %>% 
        mutate(Key=ifelse(Key=="EquinorandScatecApodiIV","ApodiIV",Key)) %>% 
        mutate(Key=ifelse(Key=="Salgueiro","SolarSalgueiro",Key)) %>% 
        mutate(Key=ifelse(Key=="SalgueiroII","SolarSalgueiroII",Key)) %>% 
        
        mutate(Key=ifelse(Key=="SaoGoncaloVI","SaoGoncalo6",Key)) %>% 
        mutate(Key=ifelse(Key=="SoldoFuturoI","SteelconsSoldoFuturoI()",Key)) %>% 
        mutate(Key=ifelse(Key=="SoldoFuturoII","SteelconsSoldoFuturoII(AntigaSteelconsMiracema2)",Key)) %>% 
        mutate(Key=ifelse(Key=="SoldoFuturoIII","SteelconsSoldoFuturoIII(AntigaSteelconsMiracema3)",Key)) %>% 
        mutate(Key=ifelse(Key=="VerdeIII","VerdeValeIII",Key)) %>% 
        mutate(Key=ifelse(Key=="VilaBJL11","BJL11",Key)) %>% 
        mutate(Key=ifelse(Key=="VilaBJL4","BJL4",Key)) %>% 
        filter(!(Key %in% c("AlsolUberlandiaIII",
                          "FontesSolarI",
                          "FontesSolarII",
                          "BoaHora1",
                          "BoaHora2",
                          "BoaHora3")))
        
     }
  
  
  #full_data_selection <- full_data_selection %>% 
  #  mutate(Key=ifelse(Key=="BomJardim", ifelse(`Project name`=="IMPSA Bom Jardim Wind Farm","IMPSABomJardim","WobbenBomJardim"),Key)) %>% 
  #  mutate(Key=ifelse(Key=="CanoaQuebrada", ifelse(`Project name`=="Rosa dos Ventos Canoa Quebrada Wind Farm","RosaDosVentosCanoaQuebrada","ServtecCanoaQuebrada"),Key)) %>% 
  #  mutate(Key=ifelse(Key=="Guajiru", ifelse(`Project name`=="Copel Guajiru Wind Farm","CopelGuajiru","EngieGuajiru"),Key)) 
    
  return(full_data_selection %>% mutate(Key=tolower(Key)))
}

clean_aneel_names<-function(power_plants_aneel){
  
  power_plants_aneel<-power_plants_aneel %>% 
    mutate(Key=str_replace_all(NOME_EOL, " 0","")) %>% 
    mutate(Key=str_replace_all(Key, " ","")) %>% 
    mutate(Key=stri_trans_general(Key,"latin-ascii")) %>% 
    mutate(Key=str_replace_all(Key,"(AntigaSteelconsMiracema1)","")) 
    #mutate(Key=str_replace_all(Key,"(AntigaSteelconsMiracema1)","")) %>% 
    #mutate(Key=str_replace_all(Key,"(AntigaSteelconsMiracema1)","")) %>% 
    #mutate(Key=str_replace_all(Key,"(AntigaSteelconsMiracema1)","")) %>% 
    #mutate(Key=str_replace_all(Key,"(AntigaSteelconsMiracema1)","")) %>% 
    
  
  
  
  return(power_plants_aneel %>% mutate(Key=tolower(Key)))
  
}

load_bloomberg_data<-function(){
  
  wind_solar<-st_read("data/wind_solar_BR_bloomberg_2000-2021_31-01-2022.shp")
  wind_solar1<-read_xlsx("data/wind_solar_2000_2021_updated.xlsx", sheet=1)
  wind_solar$project_status<-wind_solar1$`Project >> Status`
  wind_owners<-read_xlsx("data/wind_solar_2000_2021_updated.xlsx",sheet=3) %>% 
    mutate(Type="Wind")
  solar_owners<-read_xlsx("data/wind_solar_2000_2021_updated.xlsx",sheet=4) %>% 
    mutate(Type="Solar")
  
  wind_solar <- wind_solar %>% 
    filter(!(project_status %in% c("Decommissioned","Project abandoned")))
  
  #####Filter here: "abandoned" "decommissioned" in column "Project >> Status"
  
  owners<-bind_rows(wind_owners,solar_owners) %>% 
    mutate(re_prj_ID=`Renewable Project ID`)
  
  
  full_data_bloomberg<-wind_solar %>% 
    merge(owners,by='re_prj_ID') %>% 
    na.omit()
  
  return(full_data_bloomberg)
}
