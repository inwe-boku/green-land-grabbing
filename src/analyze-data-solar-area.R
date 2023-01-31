source("src/functions.R")

join_bloomberg_aneel_owners_investors <-  get_solar_aneel_bloomberg_owners_investors()

join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]

organizations_unique <- get_organizations_solarparks()

# only 4 countries of organizations unknown
sum(is.na(organizations_unique$Country))


#### join with bloomberg organizations data to check direct owner area

joined_data <- get_full_owners_investors_aneel_solarparks()

joined_data %>% 
  group_by(Key) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

joined_data_pv <- joined_data

x <- joined_data_pv %>% 
  mutate(date_close = mdy(date_close)) %>% 
  group_by(Key, Type) %>% 
  summarize(max = max(date_close, na.rm = TRUE),
            min = min(date_close, na.rm = TRUE)) %>% 
  mutate(diff = as.numeric(max - min))

x %>% ggplot(aes(x = diff)) +
  geom_histogram(aes(fill = Type), position = "dodge")

x %>% 
  filter(diff > 0)

x %>% 
  filter(Type == "Direct investor") %>% 
  filter(diff > 0)



joined_data_country <- joined_data %>%   
  group_by(Country_Category, Start_year, Type) %>% 
  summarize(POT_MW=sum(POT_MW),
            area=sum(area)) %>% 
  dplyr::select(Start_year, Country_Category, Type, POT_MW, area) %>% 
  filter(!is.na(Start_year)) 

joined_data_country %>% 
  group_by(Type) %>% 
  mutate(area_sum=sum(area)) %>% 
  ungroup() %>% 
  group_by(Country_Category,Type) %>% summarize(a=100*sum(area)/max(area_sum))

joined_data %>% 
  group_by(Type) %>% 
  mutate(area_sum=sum(area)) %>% 
  ungroup() %>% 
  filter(Type=="Parent owner") %>% 
  group_by(Country,Type) %>% summarize(a=100*sum(area)/max(area_sum)) %>% 
  arrange(desc(a))



full_data_set <- tibble(Start_year=c(2005:2021)) %>% 
  crossing(tibble(Country_Category=joined_data_country$Country_Category %>% unique())) %>% 
  crossing(tibble(Type=joined_data_country$Type %>% unique())) %>% 
  filter(Start_year>2014)

joined_data_country<-full_data_set %>% 
  left_join(joined_data_country) %>% 
  mutate(POT_MW=ifelse(is.na(POT_MW),0,POT_MW))  %>% 
  mutate(area=ifelse(is.na(area),0,area))  

joined_data_country %>% group_by(Type) %>% 
  summarize(p=sum(POT_MW),
            a=sum(area))

joined_data_country %>% 
  filter(Start_year>2014) %>% 
  ggplot(aes(x=Start_year,y=POT_MW)) +
  geom_area(aes(fill=Country_Category),size=0.5) +
  xlab("Year") +
  ylab("Capacity (MW)") +
  theme_bw() +
  scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
  facet_wrap(.~Type)


ggsave("figures/solar-owners-investor-capacity.png", width=8, height=4)
#geom_bar(stat="identity",position="dodge")

joined_data_country %>% 
  filter(Start_year>2014) %>% 
  ggplot(aes(x=Start_year,y=area)) +
  geom_area(aes(fill=Country_Category),size=0.5) +
  xlab("Year") +
  ylab("Area (km2)") +
  theme_bw() +
  scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
  facet_wrap(.~Type)


ggsave("figures/solar-owners-investor-area.png", width=8, height=4)


joined_data_solar <- joined_data


joined_data_solar %>% write_excel_csv("intermediate/solar-investment-data-capacity-area.csv")

solar_parks_aneel_areas<-st_read("data/solar-pv_area-manual-area_BR_14-06-2022.shp")

solar_parks_aneel_areas<-st_make_valid(solar_parks_aneel_areas)

solar_parks_aneel <- st_read("data/solar-pv_operating_min-5MW_UFV_BR_edit-aneel_04-02-2022.shp") 

solar_parks_aneel<-clean_aneel_names(solar_parks_aneel %>% mutate(NOME_EOL=NOME))


shape_file <- solar_parks_aneel_areas %>% 
  dplyr::select(AREA_ID) %>%
  full_join(solar_parks_aneel %>% as_tibble() %>% mutate(AREA.ID=as.numeric(AREA.ID)) , 
            by=c("AREA_ID"="AREA.ID")) %>% 
  dplyr::select(Key, AREA_ID) %>% 
    right_join(joined_data_solar,by=c("Key"="Key")) 


shape_file_short <- shape_file %>% 
  dplyr::select(AREA_ID) %>% 
  unique()

st_write(obj=shape_file, 
         dsn="intermediate/pv_bloom.shp",
         layer_options = "ENCODING=UTF-8", 
         delete_layer = TRUE)




