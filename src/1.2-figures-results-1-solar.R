source("src/functions.R")

joined_data_pv <- load_pv_data()

figure_investments_by_company(joined_data_pv, "figures/company-shares-pv.png")

joined_data_region_pv <- aggregate_data_by_region(joined_data_pv)

figure_map_investors(joined_data_pv, "figures/world-map-pv.png")

print_largest_companies(joined_data_pv, "Parent owner")

print_largest_companies(joined_data_pv, "Parent investor")

filter_country(joined_data_pv, "China")

filter_country(joined_data_pv, "Spain")

filter_country(joined_data_pv, "Italy")



write_ownership_investors_by_country_region(joined_data_pv, 
                                            joined_data_region_pv,
                                            "intermediate/land-ownership-by-region-pv.csv",
                                            "intermediate/land-ownership-by-country-pv.csv")


joined_data_region_full_pv <- aggregate_data_by_region_full(joined_data_region_pv)

figure_capacity_by_region_timeseries(joined_data_region_full_pv,
                                 "figures/owners-investors-capacity-pv.png",
                                 2014)

figure_area_by_region_timeseries(joined_data_region_full_pv,
                                     "figures/owners-investors-area-pv.png",
                                     2014)


joined_data_pv %>% write_excel_csv("intermediate/investment-data-capacity-area-pv.csv")

areas_pv <- load_areas_pv()

aneel_pv <- load_aneel_pv()

shape_file_pv <- derive_shape_file_pv(areas_pv, aneel_pv, joined_data_pv)

shape_file_pv_index <- shape_file_pv %>% 
  as_tibble() %>% 
  dplyr::select(AREA_ID) %>% 
  unique() %>% 
  mutate(AREA_ID_NEW = 1:n())

shape_file_pv <- shape_file_pv %>% 
  full_join(shape_file_pv_index, by = c("AREA_ID" = "AREA_ID")) %>% 
  mutate(AREA_ID = AREA_ID_NEW) %>% 
  mutate(date_close=mdy(date_close)) %>% 
  dplyr::select(Key,
                AREA_ID, 
                Start_yr = Start_year, 
                UF, 
                INICIO=INICIO_OPE,
                MUNICIP=MUNIC,
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
                geometry=geometry.x)

st_write(obj=shape_file_pv, 
         dsn="intermediate/pv_bloom.shp",
         layer_options = "ENCODING=UTF-8", 
         delete_layer = TRUE)





join_bloomberg_aneel_owners_investors <-  get_solar_aneel_bloomberg_owners_investors()
join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]
organizations_unique <- get_organizations_windparks()
joined_data_pv <- get_full_owners_investors_aneel_solarparks_including_parents(join_bloomberg_aneel_owners,
                                                                                join_bloomberg_aneel_investors,
                                                                                organizations_unique)



joined_data_pv_ <- joined_data_pv %>% 
  mutate(company_info_act = abbreviate(company_info_act, minlength=30)) %>% 
  mutate(company_info_act_parent = abbreviate(company_info_act_parent, minlength=30)) %>% 
  mutate()


p <- do_sankey(title="Owner", joined_data_pv_, "Direct owner", 6, "I", -1, 0.7, 0.3, 1)
p
ggsave("figures/sankey-pv-owner.eps", p, width = 12, height = 6)
ggsave("figures/sankey-pv-owner.pdf", p, width = 12, height = 6)
ggsave("figures/sankey-pv-owner.png", p, width = 12, height = 6)


p <- do_sankey(title="Investor", joined_data_pv_, "Direct investor", 6, "I", -1, 0.7, 0.3, 1)
p
ggsave("figures/sankey-pv-investor.eps", p, width = 12, height = 6)
ggsave("figures/sankey-pv-investor.pdf", p, width = 12, height = 6)
ggsave("figures/sankey-pv-investor.png", p, width = 12, height = 6)


has_international_involvment <- joined_data_pv_ %>% 
  mutate(international_involvment = ifelse(Country_Category !="Brazil" | parent_Country_Category !="Brazil", 1, 0)) %>% 
  group_by(Key) %>% 
  summarize(international_involvment = max(international_involvment,na.rm=TRUE)) 

full_join(joined_data_pv_, has_international_involvment) %>% 
  group_by(Type, international_involvment) %>% 
  summarize(area=sum(area))


full_join(joined_data_pv_, has_international_involvment) %>% 
  filter(company_name == "Banco do Nordeste do Brasil SA") %>% 
  group_by(Type, international_involvment) %>% 
  summarize(area=sum(area, na.rm=TRUE)) 



