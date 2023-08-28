source("src/functions.R")

joined_data_wind <- load_wind_data()

figure_investments_by_company(joined_data_wind, 
                              "figures/company-shares-wind.png")

joined_data_region_wind <- aggregate_data_by_region(joined_data_wind)

figure_map_investors(joined_data_wind, "figures/world-map-wind.png")

print_largest_companies(joined_data_wind, "Parent owner")

print_largest_companies(joined_data_wind, "Parent investor")

filter_country(joined_data_wind, "China")

filter_country(joined_data_wind, "Spain")

filter_country(joined_data_wind, "Italy")

write_ownership_investors_by_country_region(joined_data_wind, 
                                            joined_data_region_wind,
                                            "intermediate/land-ownership-by-region-wind.csv",
                                            "intermediate/land-ownership-by-country-wind.csv")


joined_data_region_full_wind <- aggregate_data_by_region_full(joined_data_region_wind)

figure_capacity_by_region_timeseries(joined_data_region_full_wind,
                                     "figures/owners-investors-capacity-wind.png",
                                     2005)

figure_area_by_region_timeseries(joined_data_region_full_wind,
                                 "figures/owners-investors-area-wind.png",
                                 2005)


joined_data_wind %>% write_excel_csv("intermediate/investment-data-capacity-area-wind.csv")

aneel_areas_wind <- load_aneel_wind()

library(ggsankey)

join_bloomberg_aneel_owners_investors <-  get_windparks_aneel_bloomberg_owners_investors()
join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]
organizations_unique <- get_organizations_windparks()
joined_data_wind <- get_full_owners_investors_aneel_windparks_including_parents(join_bloomberg_aneel_owners,
                                                                                join_bloomberg_aneel_investors,
                                                                                organizations_unique)



joined_data_wind_ <- joined_data_wind %>% 
  mutate(company_info_act = abbreviate(company_info_act, minlength=30)) %>% 
  mutate(company_info_act_parent = abbreviate(company_info_act_parent, minlength=30)) %>% 
  mutate(area=area/100)


p <- do_sankey(title="Owner", joined_data_wind_, "Direct owner", 6, "I", -1, 0.7, 0.3, 1)
p
ggsave("figures/sankey-wind-owner.eps", p, width = 12, height = 6)
ggsave("figures/sankey-wind-owner.pdf", p, width = 12, height = 6)
ggsave("figures/sankey-wind-owner.png", p, width = 12, height = 6)


p <- do_sankey(title="Investor", joined_data_wind_, "Direct investor", 6, "I", -1, 0.7, 0.3, 1)
p
ggsave("figures/sankey-wind-investor.eps", p, width = 12, height = 6)
ggsave("figures/sankey-wind-investor.pdf", p, width = 12, height = 6)
ggsave("figures/sankey-wind-investor.png", p, width = 12, height = 6)


has_international_involvment <- joined_data_wind_ %>% 
  mutate(international_involvment = ifelse(Country_Category !="Brazil" | parent_Country_Category !="Brazil", 1, 0)) %>% 
  group_by(Key) %>% 
  summarize(international_involvment = max(international_involvment,na.rm=TRUE)) 

full_join(joined_data_wind_, has_international_involvment) %>% 
  group_by(Type, international_involvment) %>% 
  summarize(area=sum(area, na.rm=TRUE)) 


full_join(joined_data_wind_, has_international_involvment) %>% 
  filter(company_name == "Banco Nacional de Desenvolvimento Economico e Social") %>% 
  group_by(Type, international_involvment) %>% 
  summarize(area=sum(area, na.rm=TRUE)) 
joined_data %>%   
  group_by(Country_Category, Start_year, Type) %>% 
  summarize(POT_MW=sum(POT_MW),
            area=sum(area)) %>% 
  dplyr::select(Start_year, Country_Category, Type, POT_MW, area) %>% 
  filter(!is.na(Start_year)) 


country_join <- joined_data_wind %>% 
  group_by(Country) %>% 
  summarize(area = sum(area))


shape_file_wind <- derive_shape_file_wind(aneel_areas_wind,
                                          joined_data_wind) 

shape_file_wind_index <- shape_file_wind %>% 
  as_tibble() %>% 
  dplyr::select(Key) %>% 
  unique() %>% 
  mutate(AREA_ID = 1:n())

shape_file_wind <- shape_file_wind %>% 
  full_join(shape_file_wind_index, by = c("Key" = "Key")) %>% 
  mutate(date_close=mdy(date_close)) %>% 
  dplyr::select(Key, 
                AREA_ID, 
                Start_yr = Start_year, 
                UF, 
                INICIO=INICIO_OPE,
                MUNICIP=MUNIC1,
                area,
                POT_MW,
                Country,
                Type,
                share,
                cmpny_nf=company_info,
                cmpny_nm=company_name,
                ownrsh_p=ownership_p,
                dat_cls=date_close,
                cmp_act=company_info_act,
                Cnty_Cat=Country_Category,
                geometry)

st_write(obj=shape_file_wind, 
         dsn="intermediate/wind_bloom.shp",
         layer_options = "ENCODING=UTF-8", 
         delete_layer = TRUE)
