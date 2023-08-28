source("src/functions.R")

theme_set(theme_bw())
theme_update(text = element_text(size = 16))

wind_turbines_analysis <- read_csv("intermediate/wind_turbines_analysis.csv")

wind_turbines_analysis %>% 
  mutate(count_share = count/sum(count))

all_intersections_wind <- read_csv("intermediate/all-intersections-wind.csv") %>% 
  mutate(technology = "wind")%>%  
  mutate(category = ifelse(category %in% c("CAR_poor", "CAR_preem"), "CAR", category)) %>% 
  mutate(Area = ifelse(runid == "observed", "Park", runid)) %>% 
  mutate(category = ifelse(category == "Public", "Public land", category)) %>% 
  mutate(category = ifelse(category == "UPL", "Undesignated \npublic land", category)) %>% 
  mutate(category = ifelse(category == "CAR", "Private land with \nCAR title", category)) %>% 
  mutate(category = ifelse(category == "SIGEF_SNCI", " Private land with\nlegal property title", category)) 

all_intersections_pv <- read_csv("intermediate/all-intersections-pv.csv") %>% 
  mutate(technology = "pv")%>%  
  mutate(category = ifelse(category %in% c("CAR_poor", "CAR_preem"), "CAR", category)) %>% 
  mutate(Area = ifelse(runid == "observed", "Park", runid)) %>% 
  mutate(Area = ifelse(runid == "random-whole", "Control random", Area)) %>% 
  mutate(category = ifelse(category == "Public", "Public land", category)) %>% 
  mutate(category = ifelse(category == "UPL", "Undesignated \npublic land", category)) %>% 
  mutate(category = ifelse(category == "CAR", "Private land with \nCAR title", category)) %>% 
  mutate(category = ifelse(category == "SIGEF_SNCI", " Private land with\nlegal property title", category)) 

all_intersections <- bind_rows(all_intersections_wind %>% mutate(tech = " Wind power"),
                               all_intersections_pv %>% mutate(tech = "Solar PV")) 


all_intersections_wind %>% 
  group_by(Strt_yr,category, runid) %>% 
  summarize(area = sum(area_km2)) %>% 
  ggplot(aes(x=Strt_yr, y=area)) + 
  geom_area(aes(fill=category)) +
  facet_wrap(.~runid)

all_intersections_pv %>% 
  group_by(Strt_yr=Start_yr,category, runid) %>% 
  summarize(area = sum(area_km2)) %>% 
  ggplot(aes(x=Strt_yr, y=area)) + 
  geom_area(aes(fill=category)) +
  facet_wrap(.~runid)

  
plot_intersections_panel <- function(all_intersections, add_string){
  
  dat_mean <- all_intersections %>% 
    group_by(Area, category, tech) %>% 
    summarize(area_km2 = sum(area_km2)) %>%
    ungroup() %>% 
    group_by(Area, tech) %>% 
    mutate(area_share = 100 * area_km2 / sum(area_km2)) 
  
  dat_mean$Area = factor(dat_mean$Area, levels=c("Park",
                                                 "Control random",
                                                 "Control match wind resource"))
  
  print(dat_mean)
  
  
  p <- dat_mean %>% 
    ggplot(aes(y = reorder(category, area_share), x = area_share)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = Area)) +
    xlab("Share of area (%)") +
    scale_fill_manual(values = COLORS3) +
    ylab("Types of land tenure regimes") +
    theme(legend.position="bottom") +
    facet_wrap(.~tech)
  
  plot(p)
  
  ggsave("figures/land-tenure-panel-mean.pdf", p, width=10, height = 6)
  
}

plot_intersections_panel(all_intersections)


plot_intersections_complete <- function(all_intersections, add_string){
  
  ### add land share
  all_intersections_area_share <- all_intersections %>% 
    as_tibble() %>% 
    group_by(Key, UF, category, Area) %>% 
    summarize(area_km2 = sum(area_km2)) %>% 
    group_by(Key,  Area) %>% 
    mutate(area_share = 100 * area_km2/sum(area_km2)) %>% 
    dplyr::select(category, area_share) %>% 
    ungroup() %>% 
    spread(category, area_share) %>% 
    mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    gather(category, area_share, -Key, -Area)
  
  
  ### land share by land tenure 
  
  dat_mean <- all_intersections %>% 
    group_by(Area, category) %>% 
    summarize(area_km2 = sum(area_km2)) %>%
    ungroup() %>% 
    group_by(Area) %>% 
    mutate(area_share = 100 * area_km2 / sum(area_km2)) 
  
  dat_mean$Area = factor(dat_mean$Area, levels=c("Park",
                                                 "Control random",
                                                 "Control match wind resource"))
  
  print(dat_mean)
  
  
  p <- dat_mean %>% 
    ggplot(aes(y = category, x = area_share)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = Area)) +
    xlab("Share of area (%)") +
    scale_fill_manual(values = COLORS3) +
    ylab("Types of land tenure regimes") +
    theme(legend.position="bottom") 
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-mean.png"), p, width=10, height = 6)
  
  all_intersections_area_share$Area = factor(all_intersections_area_share$Area, levels=c("Park", 
                                                                     "Control random",
                                                                     "Control match wind resource"))
  
  
  p <- all_intersections_area_share %>%
    ggplot(aes(y = Area, x = area_share)) +
    geom_boxplot(aes(fill = Area)) +
    ylab("Area (%)") +
    scale_fill_manual(values = COLORS3) +
    xlab("Share of park area (%)") +
    facet_wrap(.~category, ncol = 2) + 
    theme(legend.position="bottom") +
    stat_summary(fun.y=mean, geom="point", shape=20, size=6, color=COLORS10[8], fill="red") +
    theme(legend.position="bottom")
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-boxplots.png"), p,  width=10, height = 6)
  
  if(add_string == "wind"){
  return(all_intersections_area_share %>%
           ungroup() %>% 
           spread(Area, area_share) %>%    
           group_by(category) %>% 
           summarize(p.value.observed.match.wind.5p = t.test(`Park`,  `Control match wind resource`)$p.value,
                  p.value.observed.random.5p = t.test(`Park`,  `Control random`)$p.value,
                  p.value.random.match.wind.5p = t.test(`Control random`,  `Control match wind resource`)$p.value))
  }else{
    return(all_intersections_area_share %>%
             ungroup() %>% 
             spread(Area, area_share) %>%    
             group_by(category) %>% 
             summarize(p.value.observed.random.5p = t.test(`Park`,  `Control random`)$p.value))
    
  }
  
}

res <- plot_intersections_complete(all_intersections_wind, "wind")

res1 <- plot_intersections_complete(all_intersections_pv, "pv")

privatizations_park <- all_intersections_wind %>%
  group_by(Key, Area, power_density, runid, category) %>% 
  summarize(area_km2 = sum(area_km2)) %>% 
  ungroup() %>% 
  group_by(Key, Area, power_density) %>% 
  mutate(area_km2_share = 100 * area_km2/sum(area_km2)) %>% 
  ungroup()

summary(lm(area_km2_share ~ power_density, data = privatizations_park %>% 
             filter(runid=="observed")))
summary(lm(area_km2_share ~ power_density, data = privatizations_park %>% 
             filter(runid!="observed")))

privatizations_park %>% 
  ggplot(aes(x = power_density, y = area_km2_share)) +
  geom_point() +
  facet_wrap(.~runid)

plot_cumulative_privatizations <- function(all_intersections){
  
  privatizations <- all_intersections %>% 
    #filter(year(date_privatization) != 2006) %>%
    group_by(Key, runid, Area, tech) %>% 
    mutate(area_share = area_km2/sum(area_km2)) %>% 
    ungroup() %>% 
    filter(category == "SIGEF_SNCI") %>% 
    mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
    mutate(diff_inicio_aproval = as.numeric(date_privatization - INICIO)) 
  
  privatizations_complete <- privatizations %>% 
    mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
    mutate(diff_inicio_aproval = as.numeric(date_privatization - INICIO)) 
  
  privatizations_small <- privatizations_complete %>% 
    as_tibble() %>% 
    dplyr::select(Key,
                  runid, 
                  INICIO, 
                  dat_clos, 
                  date_privatization, 
                  area_km2, 
                  area_share,
                  diff_closure_aproval,
                  Area, 
                  tech) %>% 
    group_by(Key, runid, tech) %>% 
    arrange(date_privatization) %>% 
    mutate(area_km2_cum = cumsum(area_km2),
           area_share_cum = cumsum(area_share)) %>% 
    ungroup()
  
  privatizations_small$Area = factor(privatizations_small$Area, levels=c("Park",
                                                                         "Control random",
                                                                         "Control match wind resource"))
  
  p <- privatizations_small %>% 
    group_by(runid, tech) %>% 
    arrange(diff_closure_aproval) %>% 
    mutate(area_km2_cum = cumsum(area_km2)) %>% 
    ggplot(aes(x = diff_closure_aproval/365, y = area_km2_cum)) +
    geom_line(aes(col = Area), linewidth = 1) +
    scale_color_manual(values=COLORS3) +
    xlab("Difference between land privatization and first investment (Years)") +
    ylab("Privatized land (km2)") +
    theme(legend.position="bottom") +
    facet_wrap(.~tech, scale="free")
  
  plot(p)
  
  ggsave(paste0("figures/cumulative-privatizations-over-time.png"), width = 10, height = 6)
  
}

plot_cumulative_privatizations(all_intersections)
