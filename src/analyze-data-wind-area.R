source("src/functions.R")

join_bloomberg_aneel_owners_investors <-  get_windparks_aneel_bloomberg_owners_investors()

join_bloomberg_aneel_owners <- join_bloomberg_aneel_owners_investors[[1]]
join_bloomberg_aneel_investors <- join_bloomberg_aneel_owners_investors[[2]]

nrow(join_bloomberg_aneel_owners)

join_bloomberg_aneel_owners %>% summarize(AREA_M2_SHARE=sum(AREA_M2_SHARE)/100)


organizations_unique <- get_organizations_windparks()
  
# only 4 countries of organizations unknown
sum(is.na(organizations_unique$Country))

#direct owner calculation
joined_data <- get_full_owners_investors_aneel_windparks()

joined_data_wind <- joined_data

x <- joined_data_wind %>% 
  mutate(date_close = mdy(date_close)) %>% 
  group_by(Key, Type) %>% 
  summarize(max = max(date_close, na.rm = TRUE),
            min = min(date_close, na.rm = TRUE)) %>% 
  mutate(diff = as.numeric(max - min))

x %>% ggplot(aes(x = diff)) +
  geom_histogram(aes(fill = Type), position = "dodge")

x %>% 
  filter(Type == "Direct investor") %>% 
  filter(diff > 0)
  
joined_data_country <- joined_data %>%   
  group_by(Country_Category, Start_year, Type) %>% 
  summarize(area=sum(area,na.rm=TRUE)/100,
            POT_MW=sum(POT_MW)) %>% 
  dplyr::select(Start_year, area, POT_MW, Country_Category, Type) %>% 
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
  crossing(tibble(Type=joined_data_country$Type %>% unique()))

joined_data_country<-full_data_set %>% 
  left_join(joined_data_country) %>% 
  mutate(area=ifelse(is.na(area),0,area)) %>% 
  mutate(POT_MW=ifelse(is.na(POT_MW),0,POT_MW))  
  

joined_data_country %>% group_by(Type) %>% 
  summarize(a=sum(area),
            p=sum(POT_MW))
  
  
joined_data_country %>% 
    ggplot(aes(x=Start_year,y=area)) +
    geom_area(aes(fill=Country_Category),size=0.5) +
    xlab("Year") +
    ylab("Area (km2)") +
    theme_bw() +
    scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
    facet_wrap(.~Type)
  
ggsave("figures/wind-owners-investor-area.png", width=8, height=4)
  
  
joined_data_country %>% 
    ggplot(aes(x=Start_year,y=POT_MW)) +
    geom_area(aes(fill=Country_Category),size=0.5) +
    xlab("Year") +
    ylab("Capacity (MW)") +
    theme_bw() +
    scale_fill_manual(values=COLORS10[c(1,3,7,9,4)])+
    facet_wrap(.~Type)
  
    
ggsave("figures/wind-owners-investor-capacity.png", width=8, height=4)
#geom_bar(stat="identity",position="dodge")

joined_data_wind <- joined_data

joined_data_wind %>% write_excel_csv("intermediate/wind-investment-data-capacity-area.csv")

wind_parks_aneel_shape <- st_read("data/wind-parks_JOIN_turbines_power-plants_EOL_BR_aneel_04-02-2022.shp")  %>% 
  filter(FASE %in% c("Operação", "Construção", "Construção não iniciada"))

wind_parks_aneel_shape <- clean_aneel_names(wind_parks_aneel_shape) 

shape_file <- wind_parks_aneel_shape %>% 
  dplyr::select(Key) %>% 
  full_join(joined_data_wind,by=c("Key"="Key")) %>% 
  filter(!is.na(Start_year))

st_write(obj=shape_file, 
         dsn="intermediate/wind_bloom.shp")

