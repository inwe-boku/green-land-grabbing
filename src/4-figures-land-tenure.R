source("src/functions.R")

theme_set(theme_bw())
theme_update(text = element_text(size = 16))

wind_turbines_analysis <- read_csv("intermediate/wind_turbines_analysis.csv")

wind_turbines_analysis %>% 
  mutate(count_share = count/sum(count))

all_intersections_wind <- read_csv("intermediate/all-intersections-wind.csv") %>% 
  mutate(technology = "wind")%>%  
  mutate(category = ifelse(category %in% c("CAR_poor", "CAR_preem"), "CAR", category)) %>% 
  mutate(`Type of land` = ifelse(runid == "observed", "Park areas", runid)) %>% 
  mutate(`Type of land` = ifelse(runid == "random-whole", "Random area", `Type of land`))


all_intersections_pv <- read_csv("intermediate/all-intersections-pv.csv") %>% 
  mutate(technology = "pv")%>%  
  mutate(category = ifelse(category %in% c("CAR_poor", "CAR_preem"), "CAR", category)) %>% 
  mutate(`Type of land` = ifelse(runid == "observed", "Park areas", runid)) %>% 
  mutate(`Type of land` = ifelse(runid == "random-whole", "Random area", `Type of land`))


plot_intersections <- function(all_intersections, add_string){
  
  ### add land share
  all_intersections_area_share <- all_intersections %>% 
    as_tibble() %>% 
    group_by(Key, UF, category, `Type of land`) %>% 
    summarize(area_km2 = sum(area_km2)) %>% 
    group_by(Key,  `Type of land`) %>% 
    mutate(area_share = 100 * area_km2/sum(area_km2)) %>% 
    dplyr::select(category, area_share) %>% 
    ungroup() %>% 
    spread(category, area_share) %>% 
    mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    gather(category, area_share, -Key, -`Type of land`)
  
  ### land share by land tenure 
  
  dat_mean <- all_intersections %>% 
    group_by(`Type of land`, category) %>% 
    summarize(area_km2 = sum(area_km2)) %>%
    ungroup() %>% 
    group_by(`Type of land`) %>% 
    mutate(area_share = 100 * area_km2 / sum(area_km2)) 
  
  print(dat_mean)
  
  p <- dat_mean %>% 
    ggplot(aes(y = category, x = area_share)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = `Type of land`)) +
    xlab("Share of area (%)") +
    scale_fill_manual(values = COLORS3) +
    ylab("Type of land regularization") +
    theme(legend.position="bottom") 
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-mean.png"), p, width=10, height = 6)
  
  p <- all_intersections_area_share %>%
    ggplot(aes(y = `Type of land`, x = area_share)) +
    geom_boxplot(aes(fill = `Type of land`)) +
    ylab("Area (%)") +
    scale_fill_manual(values = COLORS3) +
    xlab("Share of park area (%)") +
    facet_wrap(.~category, ncol = 2) + 
    theme(legend.position="bottom") +
    stat_summary(fun.y=mean, geom="point", shape=20, size=6, color=COLORS10[8], fill="red") +
    theme(legend.position="bottom")
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-boxplots.png"), p,  width=10, height = 6)
  
  return(all_intersections_area_share %>%
           ungroup() %>% 
           spread(`Type of land`, area_share) %>%    
           group_by(category) %>% 
           summarize(p.value.observed.5p = t.test(`Park areas`,  `Random area`)$p.value))
  
}

res<-plot_intersections(all_intersections_wind, "wind")

res1<-plot_intersections(all_intersections_pv, "pv")



plot_intersections_uf <- function(all_intersections, add_string){
  
  ### add land share
  all_intersections_area_share <- all_intersections %>% 
    as_tibble() %>% 
    group_by(Key, UF, category, `Type of land`) %>% 
    summarize(area_km2 = sum(area_km2)) %>% 
    group_by(Key,  `Type of land`, UF) %>% 
    mutate(area_share = 100 * area_km2/sum(area_km2)) %>% 
    dplyr::select(category, area_share) %>% 
    ungroup() %>% 
    spread(category, area_share) %>% 
    mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    gather(category, area_share, -Key, -`Type of land`, -UF)
  
  ### land share by land tenure 
  
  dat_mean <- all_intersections %>% 
    group_by(`Type of land`, category, UF) %>% 
    summarize(area_km2 = sum(area_km2)) %>%
    ungroup() %>% 
    group_by(`Type of land`, UF) %>% 
    mutate(area_share = 100 * area_km2 / sum(area_km2)) 
  
  print(dat_mean)
  
  p <- dat_mean %>% 
    ggplot(aes(y = category, x = area_share)) +
    geom_bar(stat = "identity", position = "dodge", aes(fill = `Type of land`)) +
    xlab("Share of area (%)") +
    scale_fill_manual(values = COLORS3) +
    ylab("Type of land regularization") +
    theme(legend.position="bottom") +
    facet_wrap(.~UF)
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-mean.png"), p, width=10, height = 6)
  
  p <- all_intersections_area_share %>%
    ggplot(aes(y = `Type of land`, x = area_share)) +
    geom_boxplot(aes(fill = `Type of land`)) +
    ylab("Area (%)") +
    scale_fill_manual(values = COLORS3) +
    xlab("Share of park area (%)") +
    facet_wrap(.~category, ncol = 2) + 
    theme(legend.position="bottom") +
    stat_summary(fun.y=mean, geom="point", shape=20, size=6, color=COLORS10[8], fill="red") +
    theme(legend.position="bottom")
  
  plot(p)
  
  ggsave(glue("figures/land-tenure-{add_string}-boxplots.png"), p,  width=10, height = 6)
  
  return(all_intersections_area_share %>%
           ungroup() %>% 
           spread(`Type of land`, area_share) %>%    
           group_by(category) %>% 
           summarize(p.value.observed.5p = t.test(`Park areas`,  `Random area`)$p.value))
  
}


#xx<-all_intersections_wind %>% 
#  filter(year(date_privatization) == 2006) %>% 
#  dplyr::select(Key, date_privatization, INICIO, dat_clos, area_km2) %>% 
#  arrange(date_privatization)

#st_read("intermediate/wind_bloom.shp") %>% 
#  as_tibble() %>% 
#  filter(startsWith(Key, "campolargo")) %>% 
#  arrange(dat_cls)

# maybe remove 2006 from data set? research campo largo windpark?

privatizations_park <- all_intersections_wind %>%
  group_by(Key, `Type of land`, power_density, runid, category) %>% 
  summarize(area_km2 = sum(area_km2)) %>% 
  ungroup() %>% 
  group_by(Key, `Type of land`, power_density) %>% 
  mutate(area_km2_share = 100 * area_km2/sum(area_km2)) %>% 
  ungroup()

####### Finding 1
####### no relation between wind speed density and privatizations

summary(lm(area_km2_share ~ power_density, data = privatizations_park %>% 
             filter(runid=="observed")))
summary(lm(area_km2_share ~ power_density, data = privatizations_park %>% 
             filter(runid!="observed")))

privatizations_park %>% 
  ggplot(aes(x = power_density, y = area_km2_share)) +
  geom_point() +
  facet_wrap(.~runid)

privatizations <- all_intersections_wind %>% 
  #filter(year(date_privatization) != 2006) %>%
  group_by(Key, runid, `Type of land`) %>% 
  mutate(area_share = area_km2/sum(area_km2)) %>% 
  ungroup() %>% 
  filter(category == "SIGEF_SNCI") %>% 
  mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
  mutate(diff_inicio_aproval = as.numeric(date_privatization - INICIO)) 






##### privatizations in wind park areas in general EARLIER than in control areas.
privatizations %>% 
  dplyr::select(category, runid, diff_closure_aproval,diff_inicio_aproval, area_km2, Key, `Type of land`) %>% 
  gather(variable, value, -category, -runid, -Key, -`Type of land`, -area_km2) %>% 
  mutate(variable = ifelse(variable == "diff_closure_aproval", 
                           "Difference date between date of privatization \nand first investment", 
                           "Difference date of privatization \nand start of park operation")) %>% 
  ggplot(aes(x=variable, y=value/365)) +
  geom_boxplot(aes(fill=`Type of land`, weight=area_km2)) +
  scale_fill_manual(values=COLORS3) +
    ylab("Years")+
    xlab("")

ggsave("figures/temporal-difference-years-privatizations.png", 
       width=10, 
       height = 6)

all_intersections_wind %>% 
  filter(category == "SIGEF_SNCI") %>% 
  #filter(runid == "observed") %>% 
  dplyr::select(runid, date_privatization, dat_clos) %>% 
  gather(variable, value, -runid) %>% 
  ggplot(aes(y=value)) +
  geom_histogram(aes(fill=variable), alpha=1) +
  facet_grid(runid~variable)
  
all_intersections_wind %>% 
  filter(category == "SIGEF_SNCI") %>%
  dplyr::select(date_privatization, INICIO, dat_clos, area_km2, runid) %>% 
  gather(date_type, date, -area_km2, -runid) %>% 
  filter(date_type == "date_privatization") %>% 
  filter(year(date) != 2006) %>%
  ggplot(aes(x = date, y = area_km2)) +
  geom_point(aes(col = runid, alpha = runid)) +
  geom_smooth(aes(col = runid), method = "gam") +
  scale_alpha_manual(values = c(0.4, 0.4, 0.4)) +
  facet_wrap(.~date_type)

all_intersections_wind %>% 
  filter(category == "SIGEF_SNCI") %>%
  group_by(year(date_privatization)) %>% 
  summarize(area =sum(area_km2))

privatizations %>% 
  filter(category == "SIGEF_SNCI") %>%
  mutate(year = round(diff_closure_aproval/365)) %>% 
#  group_by(Key, runid) %>% 
 # summarize(area_km2 = sum(area_km2),
  #          diff_closure_aproval = mean(diff_closure_aproval)/365) %>% 
  #filter(diff_closure_aproval > -2000) %>% 
  #filter(date_type == "date_privatization") %>% 
  group_by(year, runid) %>% 
  summarize(area_km2 = sum(area_km2)) %>% 
  ggplot(aes(x = year, y = area_km2)) +
  geom_point(aes(col = runid, alpha = runid)) +
  geom_smooth(aes(col = runid), level = 0.95) +
  scale_alpha_manual(values = c(0.8, 0.4, 0.4)) 


privatizations %>% 
  group_by(Key, runid) %>% 
  summarize(n = 1) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(n = n())

privatizations_complete <- privatizations

#####add lacking dummy privatizations to make all combinations available in data set
for(i in 1:nrow(privatizations_complete)) {
  row <- privatizations_complete[i, ]
  if((row$Key %in% observed_only$Key) & !(row$Key %in% random_only$Key)){
    add <- row
    add$date_privatization <- as.Date("2022-01-01")
    add$area_km2 <- 0
    add$area_share <- 0
    add$runid <- "random-whole"
    add$power_density <- all_intersections_wind %>% 
      filter(Key == row$Key & runid=="random-whole") %>% 
      dplyr::select(power_density) %>% 
      unique() %>% unlist()
    privatizations_complete <- privatizations_complete %>% 
      bind_rows(add)
  }
  if(!(row$Key %in% observed_only$Key) & (row$Key %in% random_only$Key)){
    add <- row
    add$date_privatization <- as.Date("2022-01-01")
    add$area_km2 <- 0
    add$area_share <- 0
    add$runid = "observed"
    add$power_density <- all_intersections_wind %>% 
      filter(Key == row$Key & runid=="observed") %>% 
      dplyr::select(power_density) %>% 
      unique() %>% unlist()
    
    privatizations_complete <- privatizations_complete %>% 
      bind_rows(add)
  }
  observed_only <- privatizations_complete %>% 
    filter(runid == "observed")
  
  random_only<- privatizations_complete %>% 
    filter(runid == "random-whole")
  
}

privatizations_complete <- privatizations_complete %>% 
  mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
  mutate(diff_inicio_aproval = as.numeric(date_privatization - INICIO)) %>% 
  mutate(`Type of land` = ifelse(runid == "observed", "Park areas", runid)) %>% 
  mutate(`Type of land` = ifelse(runid == "random-whole", "Random area", `Type of land`))



privatizations_complete %>% 
  group_by(Key, runid) %>% 
  summarize(n = 1) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(n = n())

privatizations %>% 
  group_by(Key, runid) %>% 
  summarize(n = 1) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(n = n())

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
                power_density,
                `Type of land`) %>% 
  group_by(Key, runid) %>% 
  arrange(date_privatization) %>% 
  mutate(area_km2_cum = cumsum(area_km2),
         area_share_cum = cumsum(area_share)) %>% 
  ungroup()

privatizations_small %>% 
  filter(Key == "ventosdetiangua") %>% dplyr::select(date_privatization, area_share_cum)


privatizations_small %>% 
  arrange(Key, runid)

min_date <- floor_date(min(privatizations$dat_clos), "month")
max_date <- ceiling_date(max(privatizations$date_privatization), "month")

timeseries <- tibble(t=seq(min_date, 
                           max_date, 
                           by = "m"))

timeseries_d <- tibble(t=seq(min_date, 
                           max_date, 
                           by = "d"))


privatizations_small %>% 
  group_by(runid) %>% 
  arrange(diff_closure_aproval) %>% 
  mutate(area_km2_cum = cumsum(area_km2)) %>% 
  ggplot(aes(x = diff_closure_aproval/365, y = area_km2_cum)) +
  geom_line(aes(col = `Type of land`), linewidth = 0.5) +
  scale_color_manual(values=COLORS3) +
  xlab("Difference between land privatization and first investment (Years)") +
  ylab("Privatized land (km2)")

ggsave("figures/cumulative-differences-privatization-wind.png", width = 10, height = 6)



####### randomsimulations
privatizations_small %>% 
  mutate(date_privatization = sample(timeseries_d$t, nrow(.))) %>% 
  mutate(dat_clos = sample(timeseries_d$t, nrow(.))) %>% 
  mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
  ungroup() %>% 
  mutate(tt="simulation") %>% 
  group_by(runid) %>% 
  arrange(diff_closure_aproval) %>% 
  mutate(area_km2_cum = cumsum(area_km2)) %>% 
  bind_rows(privatizations_small %>% group_by(runid) %>% 
              arrange(diff_closure_aproval) %>% 
              mutate(area_km2_cum = cumsum(area_km2)) %>% ungroup() %>%  mutate(tt="actual")) %>% 
    ggplot(aes(x = diff_closure_aproval/365, y = area_km2_cum)) +
    geom_line(aes(col = `Type of land`, linetype = tt), linewidth = 0.5) +
    scale_color_manual(values=COLORS3) +
    xlab("Difference between land privatization and first investment (Years)") +
    ylab("Privatized land (km2)")



privatizations_small %>% 
  group_by(runid) %>% 
  arrange(diff_closure_aproval) %>% 
  mutate(area_km2_cum = cumsum(area_km2)) %>% 
  mutate(share = area_km2_cum/max(area_km2_cum)) %>% 
  ggplot(aes(x = diff_closure_aproval/365, y = share)) +
  geom_line(aes(col = `Type of land`), linewidth = 0.5) +
  scale_color_manual(values=COLORS3) +
  xlab("Difference between land privatization and first investment (Years)") +
  ylab("Privatized land (Share of total privatized land)")
  

privatizations_small$Key %>% unique() %>% sort()

### extend timeseries

part1 <- privatizations_small %>% 
  dplyr::select(Key, dat_clos) %>% 
  unique() %>% 
  mutate(runid = "observed")

part2 <- privatizations_small %>% 
  dplyr::select(Key, dat_clos) %>% 
  unique() %>% 
  mutate(runid = "random-whole")


#part2 <-  tibble(runid = privatizations_small$runid %>% unique(),
#                 run = c(1),
#                 date_privatization = min_date,
#                 area_km2 = 0,
#                 area_share = 0,
#                 diff_closure_aproval = 0,
#                 area_km2_cum = 0,
#                 area_share_cum = 0)

#privatizations_small_full <- bind_rows(privatizations_small, merge(part1, part2))

privatizations_monthly <- privatizations_small %>%
  mutate(date_privatization = floor_date(date_privatization, "month")) %>% 
  group_by(Key, runid, date_privatization, INICIO, dat_clos, power_density) %>% 
  summarize(area_km2_cum = max(area_km2_cum),
            area_share_cum = max(area_share_cum),
            area_km2 = sum(area_km2)) %>% 
  ungroup()

privatizations_monthly %>% filter(Key == "ventosdetiangua") 

timeseries_full <- merge(timeseries, bind_rows(part1, part2), all = TRUE)
#timeseries_full <- merge(timeseries_full, part2 %>% dplyr::select(runid), all = TRUE)

privatizations_timeseries <- left_join(timeseries_full, 
                                       privatizations_monthly %>% 
                                         dplyr::select(runid, Key, date_privatization, area_km2, area_km2_cum, area_share_cum, power_density), by = c("runid" = "runid", "Key" = "Key", "t" = "date_privatization")) %>% 
  as_tibble() %>% 
  group_by(Key, runid) %>% 
  arrange(t) %>% 
  fill(area_km2_cum) %>% 
  fill(area_share_cum) %>%
  mutate(area_km2_cum = ifelse(is.na(area_km2_cum), 0, area_km2_cum)) %>% 
  mutate(area_share_cum = ifelse(is.na(area_share_cum), 0, area_share_cum)) %>% 
  mutate(area_km2 = ifelse(is.na(area_km2), 0, area_km2)) %>% 
  group_by(Key, runid) %>% 
  fill(power_density, .direction = "down") %>% 
  fill(power_density, .direction = "up")


#privatizations_timeseries <- merge(timeseries_full, privatizations_monthly, all = TRUE)

privatizations_timeseries$Key %>% unique() %>% sort()

privatizations_monthly %>% 
  filter(Key == "zeusii") %>% 
  dplyr::select(runid, date_privatization, area_share_cum)

privatizations_timeseries %>% 
  filter(Key == "zeusii") %>%  
  filter(t>=as.Date("2014-08-01"))
  

privatizations_timeseries_treated %>% 
  filter(Key == "galpoes") %>%  
  ggplot(aes(x = t, y =area_share_cum)) +
  geom_line(aes(col=runid, linetype=as.character(time))) 


privatizations_timeseries %>% 
  filter(Key == "ubatuba") %>%  
  ggplot(aes(x = t, y =area_share_cum)) +
  geom_line(aes(col=runid)) 


privatizations_timeseries %>% 
  filter(Key == "ubatuba") %>%  
  ggplot(aes(x = t, y =area_km2)) +
  geom_line(aes(col=runid)) 

#privatizations_timeseries %>% 
  #mutate(dat_clos = floor_date(dat_clos, "month")) %>% 
#  mutate(dat_clos = year(dat_clos)) %>% 
#  mutate(closures = ifelse(dat_clos == t, 1, 0))  %>% 
#  filter(dat_clos == 2020) %>% 
#  group_by(t, runid) %>% 
#  summarize(area_km2_cum = sum(area_km2_cum),
#            closures = sum(closures)) %>%
#  ungroup() %>% 
#  group_by(runid) %>% 
#  arrange(t) %>% 
#  mutate(closures = cumsum(closures)) %>% 
#  ggplot(aes(x = t, y = area_km2_cum)) +
#  geom_line(aes(col=runid), linewidth = 0.5) +
  #geom_line(aes(x = t, y = closures), linewidth = 0.5) +
#  scale_color_manual(values=COLORS3) +
#  xlab("Time") +
#  ylab("Area privatized (km2)")

  
### randomization
t_ <- seq(min_date, max_date, by = "d")
t_ <- seq(max_date - 5 *365, max_date, by = "d")

p1 <- privatizations_timeseries %>% 
  filter(runid == "observed") %>% 
  mutate(dat_clos = sample(t_, replace = TRUE, nrow(.)))

p2 <- privatizations_timeseries %>% 
  filter(runid == "random-whole") %>% 
  mutate(dat_clos = p1$dat_clos)

privatizations_timeseries_random <- bind_rows(p1, p2)

parks_20 <- privatizations_timeseries$Key %>% unique() %>% .[1:20]

privatizations_timeseries %>% 
  filter(Key %in% c("alegriaii")) %>% 
  ggplot(aes(x = t, y =area_km2_cum)) +
  geom_line(aes(col = runid)) +
  facet_wrap(.~Key, scale = "free")

privatizations_timeseries %>%
  group_by(runid, Key) %>% 
  arrange(t) %>% 
  mutate(area_km2_change = area_km2_cum - lag(area_km2_cum)) %>% 
  filter(area_km2_change == max(area_km2_change, na.rm =TRUE)) %>% 
  arrange(Key, runid, t) %>% 
  summarize(t = mean(t),
            dat_clos = mean(dat_clos)) %>% 
  summarize(c = cor(as.numeric(dat_clos), as.numeric(t)))

do_did_cum <- function(years_before, privatizations_timeseries){
  
  privatizations_timeseries_treated <- privatizations_timeseries %>%
    #mutate(area_km2 = 1) %>% 
     filter(runid != "random-area-5p") %>% 
    mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
    #group_by(year=year(t), treated,Key,power_density,year_close=year(dat_clos)) %>% 
    #summarize(area_km2_cum = max(area_km2_cum)) %>% 
    mutate(time = ifelse(t > (dat_clos), 1, 0)) %>% 
    mutate(did = time*treated) 
  #%>% 
  #  mutate(time = ifelse(year > year_close, 1, 0))
  
  
  #l=unique(glue("{privatizations_timeseries_treated$Key}{privatizations_timeseries_treated$treated}"))
  
  #privatizations_timeseries_treated <- privatizations_timeseries_treated %>% 
   # mutate(id_name_string = glue("{Key}{treated}")) %>% 
    #mutate(id_name=as.numeric(factor(id_name_string, levels=l)))
 
  
  #out <- att_gt(yname = "area_km2_cum",
  #              gname = "treated",
  #              idname = "Key",
  #              tname = "year",
  #              xformla = ~power_density,
  #              data = privatizations_timeseries_treated,
  #              est_method = "reg"
  #)
  
  
  
  didreg =  lm_robust(area_km2_cum ~ treated + time + did + power_density, 
              data = privatizations_timeseries_treated, se_type = "stata")
  return(didreg)
  
}

privatizations_timeseries_treated_cum <- privatizations_timeseries %>%
  group_by(year=year(t), Key, runid, year_close=year(dat_clos), power_density) %>% 
  summarize(area_km2_cum = max(area_km2_cum, na.rm=TRUE)) %>% 
  dplyr::select(year, Key, runid, power_density, year_close, area_km2_cum) %>%
  mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
  mutate(treatment_timing = ifelse(year > (year_close), 1, 0) * treated) %>%
  group_by(Key, runid) %>%
  arrange(year) %>% 
  mutate(period = year) %>% 
  mutate(d = treatment_timing - lag(treatment_timing)) %>%
  na.omit() %>% 
  mutate(G = 0) %>% 
  mutate(G = ifelse(d==1 & treated, period, 0)) %>% 
  mutate(G = max(G, na.rm=TRUE)) 

privatizations_timeseries_treated <- privatizations_timeseries %>%
  group_by(year=year(t), Key, runid, year_close=year(dat_clos), power_density) %>% 
  summarize(area_km2 = sum(area_km2, na.rm=TRUE)) %>% 
  dplyr::select(year, Key, runid, power_density, year_close, area_km2) %>%
  mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
  mutate(treatment_timing = ifelse(year > (year_close), 1, 0) * treated) %>%
  group_by(Key, runid) %>%
  arrange(year) %>% 
  mutate(period = year) %>% 
  mutate(d = treatment_timing - lag(treatment_timing)) %>%
  na.omit() %>% 
  mutate(G = 0) %>% 
  mutate(G = ifelse(d==1 & treated, period, 0)) %>% 
  mutate(G = max(G, na.rm=TRUE)) 

  
l=unique(glue("{privatizations_timeseries_treated$Key}{privatizations_timeseries_treated$treated}"))

privatizations_timeseries_treated <- privatizations_timeseries_treated %>% 
  mutate(id_name_string = glue("{Key}{treated}")) %>%
  mutate(id_name=as.numeric(factor(id_name_string, levels=l)))

privatizations_timeseries_treated <- privatizations_timeseries_treated %>% 
  mutate(id_name=1)

privatizations_timeseries_treated_short_cum <- privatizations_timeseries_treated_cum %>% 
  filter((G > 2000 & G < 2022) | G ==0)

privatizations_timeseries_treated_short <- privatizations_timeseries_treated %>% 
  filter((G > 2000 & G < 2022) | G ==0) %>% 
  mutate(G = ifelse(G>0, 2010, 0))

out_cum <- att_gt(yname="area_km2_cum", 
              tname="period", 
              gname="G", 
              idname = "id_name", 
              xformla=~power_density, 
              data=privatizations_timeseries_treated_short_cum,
              est_method = "reg",
              panel = FALSE)

ggdid(out_cum)

es_cum <- aggte(out_cum, type = "dynamic")
summary(es_cum)

ggdid(es_cum)

privatizations_timeseries_treated_short %>% 
  group_by(year, runid) %>% 
  summarize(area_km2 = sum(area_km2)) %>% 
  ggplot(aes(x=year, y=area_km2)) +
  geom_line(aes(col=runid))

privatizations_timeseries_treated_short <- privatizations_timeseries_treated %>% 
  filter((G > 2000 & G < 2022) | G ==0) %>% 
  mutate(G = ifelse(G>0, 2005, 0))

  
out <- att_gt(yname="area_km2", 
                  tname="period", 
                  gname="G", 
#                  idname = "id_name", 
                  xformla=~power_density, 
                  data=privatizations_timeseries_treated_short,
                  est_method = "reg",
                  panel = FALSE)

ggdid(out)  + xlab("Year") + ylab("Land privatization (km2/park)")

group_effects <- aggte(out, type = "group")
summary(group_effects)

es <- aggte(out, type = "dynamic")
summary(es)

ggdid(es) + xlab("Year before/after investment") + ylab("Land privatization (km2/park)")

sprivatizations_timeseries_treated_short %>% 
  filter(Key == "itaremavii") %>%  
  ggplot(aes(x = year, y =area_km2)) +
  geom_line(aes(col=runid)) 


l <- reset.sim()
data <- build_sim_dataset(l)

n<-200
years <- rep(c(1980:2030), n)
treated <- rep(c(rep(1, 30), rep(1.5, 21)) + runif(51, -0.0, 0.4), n)
untreated <- rep(rep(1, 51) + runif(51, -0.4, 0.4), n)

data <- tibble(years, y=treated) %>%
  mutate(G = 2010) %>% 
  bind_rows(
  tibble(years, y=untreated) %>%
    mutate(G = 0)) %>% 
  mutate(idname=1)

out <- att_gt(yname="y", 
              tname="years", 
              gname="G", 
              data=data,
              est_method = "reg",
              panel = FALSE)

ggdid(out)

out

es <- aggte(out, type = "dynamic")  
ggdid(es)

#%>% 
#  mutate(time = ifelse(year > year_close, 1, 0))


privatizations_timeseries_treated %>% filter(Key == "ubatuba") %>% ggplot(aes(x=t, y=area_km2_cum)) + geom_line(aes(col=as.character(treated)))
privatizations_timeseries_treated$Key %>% unique() %>% .[2]
privatizations_timeseries_treated %>% filter(Key == "itaremavii") %>% ggplot(aes(x=t, y=area_km2_cum)) + geom_line(aes(col=as.character(treated)))
privatizations_timeseries_treated %>% filter(Key == "itaremavii") %>% dplyr::select(Key,treated,power_density) %>% unique()

privatizations_timeseries_treated %>% filter(Key == "ubatuba") %>% ggplot(aes(x=t, y=area_km2_cum)) + geom_line(aes(col=as.character(treated)))

range <- -5:5
models <- mapply(do_did_cum, range, list(privatizations_timeseries), SIMPLIFY = FALSE)


extract_values <- function(models, range) {
  summaries <- mapply(summary, models, SIMPLIFY = FALSE)
  
  did_cof <- mapply(function(x){x$coefficients[4, 1]}, summaries)
  trend_cof <- mapply(function(x){x$coefficients[3, 1]}, summaries)
  treatment_cof <- mapply(function(x){x$coefficients[2, 1]}, summaries)
  power_density_cof <- mapply(function(x){x$coefficients[5, 1]}, summaries)
  
  did_pvalue <- mapply(function(x){x$coefficients[4, 4]}, summaries)
  trend_pvalue <- mapply(function(x){x$coefficients[3, 4]}, summaries)
  treatment_pvalue <- mapply(function(x){x$coefficients[2, 4]}, summaries)
  power_density_pvalue <- mapply(function(x){x$coefficients[5, 4]}, summaries)
  
  r_squared <- mapply(function(x){x$r.squared}, summaries)
  
  years <- range
  
  p <- tibble(did_cof, trend_cof, treatment_cof, power_density_cof, years) %>% 
    gather(variable, value, -years) %>% 
    ggplot(aes(x=years, y=value)) + 
    geom_line(aes(col=variable))
  
  plot(p)
  
  p<-tibble(did_pvalue, trend_pvalue, treatment_pvalue, power_density_pvalue, years) %>% 
    gather(variable, value, -years) %>% 
    ggplot(aes(x=years, y=value)) + 
    geom_line(aes(col=variable))
  plot(p)
  
  plot(years, r_squared)
  
  #return(tibble(did_pvalue, trend_pvalue, treatment_pvalue, power_density_pvalue, years))
  return(tibble(did_cof, trend_cof, treatment_cof, power_density_cof) %>% 
           bind_cols(tibble(did_pvalue, trend_pvalue, treatment_pvalue, power_density_pvalue, years)))
  
}

xx<-extract_values(models, range)

do_did_single <- function(years_shift, privatizations_timeseries){
    
    privatizations_timeseries_treated <- privatizations_timeseries %>%
      group_by(Key, runid) %>% 
      arrange(t) %>% 
      mutate(area_km2 = area_km2_cum - lag(area_km2_cum)) %>% 
      na.omit() %>% 
      group_by(Key, runid, year_t=year(t), power_density) %>% 
      summarize(area_km2 = sum(area_km2),
                dat_used = min(year(INICIO))) %>% 
      ungroup() %>% 
      group_by(Key, runid, power_density) %>% 
      mutate(area_km2_share = area_km2/sum(area_km2)) %>% 
      #mutate(area_km2 = 1) %>% 
      filter(runid != "random-area-5p") %>% 
      mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
      #mutate(time = ifelse((t > (dat_clos - (years_before))) & (t <= (dat_clos)), 1, 0)) %>% 
      mutate(dat_clos = dat_clos) %>% 
      mutate(time = ifelse((year_t > (dat_used - (years_shift))) & (year_t < (dat_used + (3))), 1, 0)) %>% 
      mutate(did = time*treated) 
    
    
    didreg =  lm_robust(area_km2_share ~ treated + time + did + power_density*treated, 
                        data = privatizations_timeseries_treated, se_type = "stata")
    return(didreg)
    
}

do_did_single_share <- function(years_shift, privatizations_timeseries){
  
  privatizations_timeseries_treated <- privatizations_timeseries %>%
    group_by(Key, runid) %>% 
    arrange(t) %>% 
    mutate(area_share = area_share_cum - lag(area_share_cum)) %>% 
    na.omit() %>% 
    group_by(Key, runid, year_t=year(t), power_density) %>% 
    summarize(area_share = sum(area_share),
              dat_used = min(year(INICIO))) %>% 
    ungroup() %>% 
    group_by(Key, runid, power_density) %>% 
    #mutate(area_km2 = 1) %>% 
    filter(runid != "random-area-5p") %>% 
    mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
    #mutate(time = ifelse((t > (dat_clos - (years_before))) & (t <= (dat_clos)), 1, 0)) %>% 
    mutate(dat_clos = dat_clos) %>% 
    mutate(dat_used = dat_used + years_shift) %>% 
    mutate(time = ifelse((year_t > (dat_used - (5))) & (year_t < (dat_used + (5))), 1, 0)) %>% 
    mutate(did = time*treated) 
  
  
  didreg =  lm_robust(area_share ~ treated + time + did + power_density*treated, 
                      data = privatizations_timeseries_treated, se_type = "stata")
  return(didreg)
  
}


privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  arrange(t) %>% 
  mutate(area_km2 = area_km2_cum - lag(area_km2_cum)) %>% 
  na.omit() %>% 
  group_by(Key, runid, year_t=year(t)) %>% 
  summarize(area_km2 = sum(area_km2),
            dat_clos = min(year(dat_clos))) %>% 
  filter(Key %in% parks_20) %>% 
  ggplot(aes(x = year_t, y = area_km2)) +
  geom_line(aes(col=runid)) +
  facet_wrap(.~Key)

privatizations_timeseries_adjust <- privatizations_timeseries %>% 
  mutate(dat_clos = as.Date("2000-01-01"))

range <- -5:5
models <- mapply(do_did_single_share, range, list(privatizations_timeseries), SIMPLIFY = FALSE)

privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  summarize(a=max(area_km2_cum)) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(a=mean(a))

privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  summarize(a=min(area_km2_cum)) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(a=mean(a))

extract_values(models, range)

summaries <- mapply(summary, models, SIMPLIFY = FALSE)
summaries[[6]]



#######################PV

privatizations <- all_intersections_pv %>% 
  #filter(year(date_privatization) != 2006) %>%
  group_by(Key, runid, `Type of land`) %>% 
  mutate(area_share = area_km2/sum(area_km2)) %>% 
  ungroup() %>% 
  filter(category == "SIGEF_SNCI") %>% 
  mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
  mutate(diff_inicio_aproval = as.numeric(date_privatization - INICIO)) 


##### privatizations in wind park areas in general EARLIER than in control areas.
privatizations %>% 
  dplyr::select(category, runid, diff_closure_aproval,diff_inicio_aproval, area_km2, Key, `Type of land`) %>% 
  gather(variable, value, -category, -runid, -Key, -`Type of land`, -area_km2) %>% 
  mutate(variable = ifelse(variable == "diff_closure_aproval", 
                           "Difference date between date of privatization \nand first investment", 
                           "Difference date of privatization \nand start of park operation")) %>% 
  ggplot(aes(x=variable, y=value/365)) +
  geom_boxplot(aes(fill=`Type of land`, weight=area_km2)) +
  scale_fill_manual(values=COLORS3) +
  ylab("Years")+
  xlab("")

ggsave("figures/temporal-difference-years-privatizations-pv.png", 
       width=10, 
       height = 6)

all_intersections_pv %>% 
  filter(category == "SIGEF_SNCI") %>% 
  #filter(runid == "observed") %>% 
  dplyr::select(runid, date_privatization, dat_clos) %>% 
  gather(variable, value, -runid) %>% 
  ggplot(aes(y=value)) +
  geom_histogram(aes(fill=variable), alpha=1) +
  facet_grid(runid~variable)

privatizations %>% 
  filter(category == "SIGEF_SNCI") %>%
  mutate(year = round(diff_closure_aproval/365)) %>% 
  #  group_by(Key, runid) %>% 
  # summarize(area_km2 = sum(area_km2),
  #          diff_closure_aproval = mean(diff_closure_aproval)/365) %>% 
  #filter(diff_closure_aproval > -2000) %>% 
  #filter(date_type == "date_privatization") %>% 
  group_by(year, runid) %>% 
  summarize(area_km2 = sum(area_km2)) %>% 
  ggplot(aes(x = year, y = area_km2)) +
  geom_point(aes(col = runid, alpha = runid)) +
  geom_smooth(aes(col = runid), level = 0.95) +
  scale_alpha_manual(values = c(0.8, 0.4, 0.4)) 

privatizations_small <- privatizations %>% 
  as_tibble() %>% 
  dplyr::select(Key,
                runid, 
                INICIO, 
                dat_clos, 
                date_privatization, 
                area_km2, 
                area_share,
                diff_closure_aproval,
                `Type of land`) %>% 
  group_by(Key, runid) %>% 
  arrange(date_privatization) %>% 
  mutate(area_km2_cum = cumsum(area_km2),
         area_share_cum = cumsum(area_share)) %>% 
  ungroup()

min_date <- floor_date(min(privatizations$dat_clos), "month")
max_date <- ceiling_date(max(privatizations$date_privatization), "month")

timeseries <- tibble(t=seq(min_date, 
                           max_date, 
                           by = "m"))

timeseries_d <- tibble(t=seq(min_date, 
                             max_date, 
                             by = "d"))


privatizations_small %>% 
  group_by(runid) %>% 
  arrange(diff_closure_aproval) %>% 
  mutate(area_km2_cum = cumsum(area_km2)) %>% 
  ggplot(aes(x = diff_closure_aproval/365, y = area_km2_cum)) +
  geom_line(aes(col = `Type of land`), linewidth = 0.5) +
  scale_color_manual(values=COLORS3) +
  xlab("Difference between land privatization and first investment (Years)") +
  ylab("Privatized land (km2)")

ggsave("figures/cumulative-differences-privatization-pv.png", width = 10, height = 6)


privatizations_small %>% 
  mutate(date_privatization = sample(timeseries_d$t, nrow(.))) %>% 
  mutate(dat_clos = sample(timeseries_d$t, nrow(.))) %>% 
  mutate(diff_closure_aproval = as.numeric(date_privatization - dat_clos)) %>% 
  ungroup() %>% 
  mutate(tt="simulation") %>% 
  group_by(runid) %>% 
  arrange(diff_closure_aproval) %>% 
  mutate(area_km2_cum = cumsum(area_km2)) %>% 
  bind_rows(privatizations_small %>% group_by(runid) %>% 
              arrange(diff_closure_aproval) %>% 
              mutate(area_km2_cum = cumsum(area_km2)) %>% ungroup() %>%  mutate(tt="actual")) %>% 
  ggplot(aes(x = diff_closure_aproval/365, y = area_km2_cum)) +
  geom_line(aes(col = `Type of land`, linetype = tt), linewidth = 0.5) +
  scale_color_manual(values=COLORS3) +
  xlab("Difference between land privatization and first investment (Years)") +
  ylab("Privatized land (km2)")

### extend timeseries

part1 <- privatizations_small %>% 
  dplyr::select(Key, INICIO, dat_clos) %>% 
  unique()

part2 <-  tibble(runid = privatizations_small$runid %>% unique(),
                 run = c(1),
                 date_privatization = min_date,
                 area_km2 = 0,
                 area_share = 0,
                 diff_closure_aproval = 0,
                 area_km2_cum = 0,
                 area_share_cum = 0)

privatizations_small_full <- bind_rows(privatizations_small, merge(part1, part2))

privatizations_monthly <- privatizations_small_full %>%
  mutate(date_privatization = floor_date(date_privatization, "month")) %>% 
  group_by(Key, runid, date_privatization, INICIO, dat_clos) %>% 
  summarize(area_km2_cum = max(area_km2_cum),
            area_share_cum = max(area_share_cum)) %>% 
  ungroup()

timeseries_full <- merge(timeseries, part1, all = TRUE)
timeseries_full <- merge(timeseries_full, part2 %>% dplyr::select(runid), all = TRUE)

privatizations_timeseries <- left_join(timeseries_full, 
                                       privatizations_monthly %>% 
                                         dplyr::select(runid, Key, date_privatization, area_km2_cum, area_share_cum), by = c("runid" = "runid", "Key" = "Key", "t" = "date_privatization")) %>% 
  as_tibble() %>% 
  fill(area_km2_cum) %>% 
  fill(area_share_cum)

### randomization
t_ <- seq(min_date, max_date, by = "d")
t_ <- seq(max_date - 5 *365, max_date, by = "d")

p1 <- privatizations_timeseries %>% 
  filter(runid == "observed") %>% 
  mutate(dat_clos = sample(t_, replace = TRUE, nrow(.)))

p2 <- privatizations_timeseries %>% 
  filter(runid == "random-whole") %>% 
  mutate(dat_clos = p1$dat_clos)

privatizations_timeseries_random <- bind_rows(p1, p2)

parks_20 <- privatizations_timeseries$Key %>% unique() %>% .[1:20]

privatizations_timeseries %>%
  group_by(runid, Key) %>% 
  arrange(t) %>% 
  mutate(area_km2_change = area_km2_cum - lag(area_km2_cum)) %>% 
  filter(area_km2_change == max(area_km2_change, na.rm =TRUE)) %>% 
  arrange(Key, runid, t) %>% 
  summarize(t = mean(t),
            dat_clos = mean(dat_clos)) %>% 
  summarize(c = cor(as.numeric(dat_clos), as.numeric(t)))

do_did_cum <- function(years_before, privatization_timeseries){
  
  privatizations_timeseries_treated <- privatizations_timeseries %>%
    #mutate(area_km2 = 1) %>% 
    filter(runid != "random-area-5p") %>% 
    mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
    #mutate(time = ifelse((t > (dat_clos - (years_before))) & (t <= (dat_clos)), 1, 0)) %>% 
    mutate(time = ifelse((t > (dat_clos + (365 * years_before))), 1, 0)) %>% 
    mutate(did = time*treated) 
  
  
  didreg =  lm_robust(area_km2_cum ~ treated + time + did + power_density, 
                      data = privatizations_timeseries_treated, se_type = "stata")
  return(didreg)
  
}

range <- -5:5
models <- mapply(do_did_cum, range, list(privatizations_timeseries), SIMPLIFY = FALSE)


extract_values <- function(models, range) {
  summaries <- mapply(summary, models, SIMPLIFY = FALSE)
  
  did_cof <- mapply(function(x){x$coefficients[4, 1]}, summaries)
  trend_cof <- mapply(function(x){x$coefficients[3, 1]}, summaries)
  treatment_cof <- mapply(function(x){x$coefficients[2, 1]}, summaries)
  
  did_pvalue <- mapply(function(x){x$coefficients[4, 4]}, summaries)
  trend_pvalue <- mapply(function(x){x$coefficients[3, 4]}, summaries)
  treatment_pvalue <- mapply(function(x){x$coefficients[2, 4]}, summaries)
  
  r_squared <- mapply(function(x){x$r.squared}, summaries)
  
  years <- range
  
  p <- tibble(did_cof, trend_cof, treatment_cof, years) %>% 
    gather(variable, value, -years) %>% 
    ggplot(aes(x=years, y=value)) + 
    geom_line(aes(col=variable))
  
  plot(p)
  
  p<-tibble(did_pvalue, trend_pvalue, treatment_pvalue, years) %>% 
    gather(variable, value, -years) %>% 
    ggplot(aes(x=years, y=value)) + 
    geom_line(aes(col=variable))
  plot(p)
  
  plot(years, r_squared)
  
}

extract_values(models, range)

do_did_single <- function(years_shift, privatizations_timeseries){
  
  privatizations_timeseries_treated <- privatizations_timeseries %>%
    group_by(Key, runid) %>% 
    arrange(t) %>% 
    mutate(area_km2 = area_km2_cum - lag(area_km2_cum)) %>% 
    na.omit() %>% 
    group_by(Key, runid, year_t=year(t), power_density) %>% 
    summarize(area_km2 = sum(area_km2),
              dat_used = min(year(INICIO))) %>% 
    ungroup() %>% 
    group_by(Key, runid, power_density) %>% 
    mutate(area_km2_share = area_km2/sum(area_km2)) %>% 
    #mutate(area_km2 = 1) %>% 
    filter(runid != "random-area-5p") %>% 
    mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
    #mutate(time = ifelse((t > (dat_clos - (years_before))) & (t <= (dat_clos)), 1, 0)) %>% 
    mutate(dat_clos = dat_clos) %>% 
    mutate(time = ifelse((year_t > (dat_used - (years_shift))) & (year_t < (dat_used + (3))), 1, 0)) %>% 
    mutate(did = time*treated) 
  
  
  didreg =  lm_robust(area_km2_share ~ treated + time + did + power_density*treated, 
                      data = privatizations_timeseries_treated, se_type = "stata")
  return(didreg)
  
}

do_did_single_share <- function(years_shift, privatizations_timeseries){
  
  privatizations_timeseries_treated <- privatizations_timeseries %>%
    group_by(Key, runid) %>% 
    arrange(t) %>% 
    mutate(area_share = area_share_cum - lag(area_share_cum)) %>% 
    na.omit() %>% 
    group_by(Key, runid, year_t=year(t)) %>% 
    summarize(area_share = sum(area_share),
              dat_used = min(year(INICIO))) %>% 
    ungroup() %>% 
    group_by(Key, runid) %>% 
    #mutate(area_km2 = 1) %>% 
    filter(runid != "random-area-5p") %>% 
    mutate(treated = ifelse(runid == "observed", 1, 0)) %>% 
    #mutate(time = ifelse((t > (dat_clos - (years_before))) & (t <= (dat_clos)), 1, 0)) %>% 
    mutate(dat_clos = dat_clos) %>% 
    mutate(dat_used = dat_used + years_shift) %>% 
    mutate(time = ifelse((year_t > (dat_used - (5))) & (year_t < (dat_used + (5))), 1, 0)) %>% 
    mutate(did = time*treated) 
  
  
  didreg =  lm_robust(area_share ~ treated + time + did, 
                      data = privatizations_timeseries_treated, se_type = "stata")
  return(didreg)
  
}


privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  arrange(t) %>% 
  mutate(area_km2 = area_km2_cum - lag(area_km2_cum)) %>% 
  na.omit() %>% 
  group_by(Key, runid, year_t=year(t)) %>% 
  summarize(area_km2 = sum(area_km2),
            dat_clos = min(year(dat_clos))) %>% 
  filter(Key %in% parks_20) %>% 
  ggplot(aes(x = year_t, y = area_km2)) +
  geom_line(aes(col=runid)) +
  facet_wrap(.~Key)

privatizations_timeseries_adjust <- privatizations_timeseries %>% 
  mutate(dat_clos = as.Date("2000-01-01"))

range <- -5:5
models <- mapply(do_did_single_share, range, list(privatizations_timeseries), SIMPLIFY = FALSE)

privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  summarize(a=max(area_km2_cum)) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(a=mean(a))

privatizations_timeseries %>%
  group_by(Key, runid) %>% 
  summarize(a=min(area_km2_cum)) %>% 
  ungroup() %>% 
  group_by(runid) %>% 
  summarize(a=mean(a))

extract_values(models, range)

summaries <- mapply(summary, models, SIMPLIFY = FALSE)
summaries[[6]]








x <- 1:100 + 5 * runif(100)
y <- 1:100 + 5 * runif(100)
x[x>50] <- x[x>50]*4
plot(x)

xy <- tibble(t=1:100, x, y)
do_did <- function(years_before){
  
  xy <- xy %>% 
    gather(variable, value, -t)
  
  treated <- xy %>% 
    mutate(treated = ifelse(variable == "x", 1, 0)) %>% 
    mutate(time = ifelse((t > (50 + (years_before))), 1, 0)) %>% 
    mutate(did = time*treated)
  
  didreg =  lm_robust(value ~ treated + time + did, 
                      data = treated, se_type = "stata")
  return(didreg)
  
}

range <- -15:15
models <- mapply(do_did, range, SIMPLIFY = FALSE)
summaries <- mapply(summary, models, SIMPLIFY = FALSE)

did_cof <- mapply(function(x){x$coefficients[4, 1]}, summaries)
trend_cof <- mapply(function(x){x$coefficients[3, 1]}, summaries)
treatment_cof <- mapply(function(x){x$coefficients[2, 1]}, summaries)
did_pvalue <- mapply(function(x){x$coefficients[4, 4]}, summaries)
trend_pvalue <- mapply(function(x){x$coefficients[3, 4]}, summaries)
treatment_pvalue <- mapply(function(x){x$coefficients[2, 4]}, summaries)
r_squared <- mapply(function(x){x$r.squared}, summaries)

tibble(did_cof, trend_cof, treatment_cof, range) %>% 
  gather(variable, value, -range) %>% 
  ggplot(aes(x=range, y=value)) + 
  geom_line(aes(col=variable))

tibble(did_pvalue, trend_pvalue, treatment_pvalue, range) %>% 
  gather(variable, value, -range) %>% 
  ggplot(aes(x=range, y=value)) + 
  geom_line(aes(col=variable))

plot(range, r_squared)





model_best <- models[[5]]

model1_robust <- coeftest(model_best, 
                          vcov = vcovHAC)

tidy(model1_robust)

summaries[[6]]
###best model: 1 year before investment.
hist(summaries[[5]]$residuals)

summaries[[4]]


privatizations_small %>% 
  group_by(runid) %>% 
  summarize(area_km2_mean = mean(area_km2))

mult <- 1/365

privatizations_small %>% 
  mutate(diff_inicio_date_closure = mult * (INICIO - dat_clos),
         diff_inicio_date_privatization = mult * (INICIO - date_privatization),
         diff_date_closure_date_privatization = mult * (dat_clos - date_privatization)) %>%
  dplyr::select(runid, 
                area_km2, 
                diff_inicio_date_closure,
                diff_inicio_date_privatization,
                diff_date_closure_date_privatization) %>% 
  gather(variable, value, -runid) %>% 
  group_by(runid, variable) %>% 
  summarize(value_mean = mean(value))


privatizations %>% 
  filter(year(date_privatization) > 2007) %>% 
  group_by(Key, runid, run) %>% 
  filter(run == 1) %>% 
  summarize(area_km2 = sum(area_km2),
            diff_closure_aproval = mean(diff_closure_aproval)) %>% 
  ggplot(aes(x = diff_closure_aproval, y = area_km2)) +
  geom_point(aes(col = runid), alpha = 0.3) +
  geom_smooth(aes(col = runid), method = "gam")

privatizations %>% 
  filter(category == "SIGEF_SNCI") %>% 
  mutate(before_ = ifelse(diff_closure_aproval < 0, " before closure", "after closure")) %>% 
  filter(!is.na(before_)) %>%
  group_by(Key, runid, run) %>% 
  mutate(area_share = area_km2/sum(area_km2)) %>% 
  dplyr::select(before_, area_share) %>% 
  ggplot(aes(x = before_, y = area_share)) +
  geom_boxplot(aes(fill = runid))

privatizations_update <- privatizations %>% 
  filter(category == "SIGEF_SNCI") %>% 
  mutate(before_ = ifelse(diff_closure_aproval < 0, 
                          " before closure", 
                          "after closure")) %>% 
  filter(!is.na(before_)) %>%
  group_by(Key, runid, run) %>% 
  mutate(area_share = area_km2/sum(area_km2))

privatizations_update %>% 
  mutate(runid = ifelse(runid == "observed", "Wind park areas", runid)) %>% 
  mutate(runid = ifelse(runid == "random-area-5p", "Random area, high wind speeds", runid)) %>% 
  mutate(runid = ifelse(runid == "random-whole", "Random area", runid)) %>% 
  mutate(`Type of land` = runid) %>%
  ungroup() %>% 
  group_by(before_, `Type of land`, run) %>% 
  summarize(area_km2_by_period = sum(area_km2)) %>% 
  ungroup() %>% 
  group_by(`Type of land`, run) %>% 
  mutate(area_share = area_km2_by_period/sum(area_km2_by_period)) %>% 
  ggplot(aes(x = before_, y = area_share)) +
  geom_boxplot(aes(fill = `Type of land`)) +
  scale_fill_manual(values=COLORS3)
ggsave("figures/final-figure.png", width = 10, height = 6)  
  
  group_by(runid, before_) %>% 
  summarize(m = mean(area_share))
  ggplot(aes(x = before_, y=area_km2)) +
  geom_boxplot(aes(fill=runid))

privatizations %>% 
  mutate(before_ = ifelse(diff_closure_aproval < 0, " before closure", "after closure")) %>% 
  group_by(before_, runid) %>% 
  summarize(m = mean(area_share))

privatizations %>% 
  na.omit() %>% 
  mutate(before_ = ifelse(diff_closure_aproval < 0, " before closure", "after closure")) %>% 
  ggplot(aes(x = before_, y = area_share)) +
  geom_boxplot(aes(fill = runid))
  
  

privatizations %>% 
  ggplot(aes(x = year, y = area_km2)) +
  geom_line(aes(col = runid, linetype = as.character(run))) +
  scale_linetype(guide = NULL)

privatizations %>% 
  ggplot(aes(x = year, y = area_share)) +
  geom_line(aes(col = runid, linetype = as.character(run))) +
  scale_linetype(guide = NULL)

private_aggregate <- privatizations %>%
  na.omit() %>% 
  mutate(before_ = ifelse(year < 0, " before closure", "after closure"))  
group_by(before_, runid, run) %>%  
  summarize(area_share = sum(area_share)) 

privatizations %>%   
  na.omit() %>% 
  mutate(before_ = ifelse(year < 0, " before closure", "after closure")) %>% 
  ggplot(aes(x = before_, y = area_share)) +
  geom_boxplot(aes(fill = as.character(runid))) +
  scale_fill_manual(values = COLORS3)



#solar <- st_read("data/florian/S_solar_pri_int.shp") %>% 
#  mutate(INICIO_ = ymd(INICIO_)) %>% 
#  mutate(dat_cls = mdy(dat_cls))%>% 
#  mutate(d_aprov_n  = ymd(d_aprov_n)) %>% 
#  filter(year(d_aprov_n) > 2009)
  
#wind <- st_read("data/florian/wind_private.shp") %>% 
  
#  mutate(dat_cls = dat_cls_2) %>% 
#  mutate(INICIO_ = ymd(INICIO_)) %>% 
#  mutate(dat_cls = mdy(dat_cls)) %>% 
#  mutate(d_aprov_n  = ymd(d_aprov_n)) %>% 
#  filter(year(d_aprov_n) > 2009)
  

plot_regulated_share_over_time <- function(s, 
                                           x_annotation_1, 
                                           x_annotation_2, 
                                           y_annotation_1, 
                                           y_annotation_2, 
                                           period_length = 365,
                                           nmb_runs = 100,
                                           show_mean_execution_time = FALSE){
  
  privatization <- s %>% 
    as_tibble() %>% 
    mutate(diff_closure_aproval = as.numeric(d_aprov_n - dat_cls)/period_length) %>%
    mutate(a_int_share = area_int/sum(area_int)) %>%
    mutate(year = round(diff_closure_aproval)) %>% 
    group_by(year) %>% 
    summarize(a_int_share = sum(a_int_share)) %>% 
    mutate(variable = "Annual\nprivatization") %>% 
    dplyr::select(year, variable, value = a_int_share)
    
  
  simulations <- derive_simulation(s, nmb_runs, period_length)
    
  print("mean privatization")
  privatization %>% 
    mutate(rel = value * year) %>% 
    summarize(sum(rel)) %>% 
    print()

#  print("mean privatization simulate")
#  privatization_simulate %>% 
#    mutate(rel = value * year) %>% 
#    summarize(sum(rel)) %>% 
#    print()
  
  
  p <- privatization %>% 
    ggplot(aes(x = year, y = 100 * value)) +
    geom_ribbon(data = simulations, aes(x = x, y = y, ymin = lower, ymax = upper, fill = type), alpha = 0.1) +
    geom_line(data = simulations, aes(x = x, y = y, col = type)) +
    geom_line(size = 1, col = COLORS3[1]) +
    theme_bw() +
    xlab("Date of land privatization - date of financial closure (Years)") +
    ylab("% of land privatized") +
    geom_vline(xintercept = 0, col = "black", linetype = 2) +
    annotate("text", 
             x = x_annotation_1, 
             y = y_annotation_1, 
             label = "Earliest\nfinancial\ntransaction", 
             col = "black") +
    scale_alpha_manual(values = c(1, rep(5*1/nmb_runs, nmb_runs))) +
    theme(legend.position="none")
  
 return(p)

}

derive_simulation <- function(s, nmb_runs, period_length){
  privatization_simulate <- NULL
  ret_dat <- mapply(function(i){bind_rows(privatization_simulate, 
                              s %>%
                                as_tibble() %>%
                                mutate(d_aprov_n_simulate = sample(seq(min(d_aprov_n), max(d_aprov_n), by ="d"), n())) %>% 
                                mutate(dat_cls_simulate = sample(seq(min(dat_cls), max(dat_cls), by ="d"), n())) %>% 
                                mutate(diff_closure_aproval = as.numeric(d_aprov_n_simulate - dat_cls)/period_length) %>%
                                mutate(a_int_share = area_int/sum(area_int)) %>%
                                mutate(year = round(diff_closure_aproval)) %>% 
                                group_by(year) %>% 
                                summarize(a_int_share = sum(a_int_share)) %>% 
                                mutate(variable = glue("run {i} aproval simulate")) %>%
                                mutate(type = "aproval simulate") %>% 
                                dplyr::select(year, type, variable, value = a_int_share)
  )}, 1:nmb_runs, SIMPLIFY = FALSE) %>% 
    bind_rows()
  privatization_simulate <- NULL
  #ret_dat <- 
  bind_rows(ret_dat, mapply(function(i){bind_rows(privatization_simulate, 
                                          s %>%
                                            as_tibble() %>%
                                            mutate(d_aprov_n_simulate = sample(seq(min(d_aprov_n), max(d_aprov_n), by ="d"), n())) %>% 
                                            mutate(dat_cls_simulate = sample(seq(min(dat_cls), max(dat_cls), by ="d"), n())) %>% 
                                            mutate(diff_closure_aproval = as.numeric(d_aprov_n - dat_cls_simulate)/period_length) %>%
                                            mutate(a_int_share = area_int/sum(area_int)) %>%
                                            mutate(year = round(diff_closure_aproval)) %>% 
                                            group_by(year) %>% 
                                            summarize(a_int_share = sum(a_int_share)) %>% 
                                            mutate(variable = glue("run {i}")) %>%
                                            mutate(type = "closure simulate") %>% 
                                            dplyr::select(year, type, variable, value = a_int_share)
  )}, 1:nmb_runs, SIMPLIFY = FALSE) %>% 
    bind_rows())
  privatization_simulate <- NULL
  #ret_dat <- 
    
  bind_rows(ret_dat, mapply(function(i){bind_rows(privatization_simulate, 
                                                             s %>%
                                                               as_tibble() %>%
                                                               mutate(d_aprov_n_simulate = sample(seq(min(d_aprov_n), max(d_aprov_n), by ="d"), n())) %>% 
                                                               mutate(dat_cls_simulate = sample(seq(min(dat_cls), max(dat_cls), by ="d"), n())) %>% 
                                                               mutate(diff_closure_aproval = as.numeric(d_aprov_n_simulate - dat_cls_simulate)/period_length) %>%
                                                               mutate(a_int_share = area_int/sum(area_int)) %>%
                                                               mutate(year = round(diff_closure_aproval)) %>% 
                                                               group_by(year) %>% 
                                                               summarize(a_int_share = sum(a_int_share)) %>% 
                                                               mutate(variable = glue("run {i}")) %>%
                                                               mutate(type =  "closure aproval simulate") %>% 
                                                               dplyr::select(year, type, variable, value = a_int_share)
  )}, 1:nmb_runs, SIMPLIFY = FALSE) %>% 
    bind_rows())
  
  res <- ret_dat %>% 
    group_by(type) %>% 
    reframe(s = loess.sd(year, 100 * value, nsigma = 2.576))
  
  rows <- c(2, 3, 5, 6)
  res <- res[c(rows, rows + 6, rows + 12),]
  res$names <- rep(c("x", "y", "lower", "upper"), 3)
  
  
  
  res %>% 
    unnest(cols = c(s)) %>%
    group_by(type, names) %>% 
    mutate(n = 1:n()) %>% 
    unique() %>% 
    spread(names, s) %>% 
    return()
  
}


p <- plot_regulated_share_over_time(solar, -2, 1, 25, 100, 365, 100)
p
ggsave("figures/regulated-over-time-pv.png", width = 10, height = 6)

p <- plot_regulated_share_over_time(wind, 3, 1, 25, 50, 365, 100)
p
ggsave("figures/regulated-over-time-wind.png", width = 10, height = 6)

plot_regulated_operation_share_over_time <- function(s, x_annotation_1, y_annotation_1){
  
  p <- s %>% 
    as_tibble() %>% 
    mutate(d_aprov_n = ymd(d_aprov_n)) %>% 
    mutate(INICIO_ = ymd(INICIO_)) %>% 
    mutate(diff_inicio_aproval = as.numeric(d_aprov_n - INICIO_)/365) %>% 
    dplyr::select(diff_inicio_aproval, area_int) %>%
    arrange(diff_inicio_aproval) %>% 
    mutate(a_int_cum = cumsum(area_int)) %>% 
    ggplot(aes(x = diff_inicio_aproval, y = 100 * a_int_cum/sum(area_int))) +
    geom_line() +
    theme_bw() +
    xlab("Period between start of operation and land privatization (years)") +
    ylab("Share of land privatized (%)") +
    geom_vline(xintercept = 0, col = "black", linetype = 2) +
    annotate("text", 
             x = x_annotation_1, 
             y = y_annotation_1, 
             label = "Land\nprivatization", 
             col = "black")
    
  return(p)
  
}

plot_regulated_operation_share_over_time(solar, -1, 90)
plot_regulated_operation_share_over_time(wind, -3, 90)

sensitivity_analysis <- function(s){
  
  s %>% 
    as_tibble() %>% 
    mutate(diff_closure_aproval = as.numeric(d_aprov_n - dat_cls)/365) %>%
    mutate(diff_inicio_aproval = as.numeric(INICIO_ - d_aprov_n)/365) %>% 
    mutate(area_share = area_int/sum(area_int)) %>% 
    mutate(grabbing_type4 = ifelse(diff_closure_aproval < -4,"4: < 4-years", "none")) %>% 
    mutate(grabbing_type4.1 = ifelse(diff_closure_aproval < -4 & diff_inicio_aproval >= 0,"4: < 4-years", "none")) %>% 
    mutate(grabbing_type3 = ifelse(diff_closure_aproval >= -4 & diff_closure_aproval <= -2,"3: -4 to -2 years", "none")) %>% 
    mutate(grabbing_type3.1 = ifelse(diff_closure_aproval >= -4 & diff_closure_aproval <= -2 & diff_inicio_aproval >= 0,"3: -4 to -2 years", "none")) %>% 
    mutate(grabbing_type2 = ifelse(diff_closure_aproval >= -2 & diff_closure_aproval <= 0,"2: -2 to -0 years", "none")) %>% 
    mutate(grabbing_type2.1 = ifelse(diff_closure_aproval >= -2 & diff_closure_aproval <= 0 & diff_inicio_aproval >= 0,"2: -2 to -0 years", "none")) %>% 
    mutate(grabbing_type1 = ifelse(diff_closure_aproval >= 0,"1: >0 years", "none")) %>% 
    mutate(grabbing_type1.1 = ifelse(diff_closure_aproval >= 0 & diff_inicio_aproval >= 0,"1: >0 years", "none")) %>% 
    dplyr::select(grabbing_type1,
                  grabbing_type1.1,
                  grabbing_type2,
                  grabbing_type2.1,
                  grabbing_type3,
                  grabbing_type3.1,
                  grabbing_type4,
                  grabbing_type4.1,
                  area_share)  %>% 
    gather(variable, value, -area_share) %>% 
    mutate(initial_operation = ifelse(str_detect(variable, "\\.1"), 
                                      "Operation\nstart\nconsidered", 
                                      " Operation\nstart\nnot considered")) %>% 
    group_by(value, initial_operation) %>% 
    summarize(area_share = sum(area_share)) %>% 
    filter(value != "none") %>% 
    ggplot(aes(y = value, x = 100 * area_share)) +
    geom_bar(stat = "identity", aes(fill = initial_operation), position = "dodge") +
    xlab("Definition of land tenure") +
    ylab("Share of total area (%)") +
    theme_bw() +
    scale_fill_manual(values = COLORS3)
    
}

sensitivity_analysis(solar)
ggsave("figures/sensitivity-analysis-solar-pv.png", width = 10, height = 6)

sensitivity_analysis(wind)
ggsave("figures/sensitivity-analysis-wind.png", width = 10, height = 6)



wind %>% 
  as_tibble() %>% 
  mutate(diff = as.numeric(dat_cls-INICIO_)/365) %>% 
  mutate(year = round(diff)) %>% 
  group_by(year) %>% 
  summarize(a=sum(area_int)) %>% 
  dplyr::select(year, a) %>% 
  ggplot(aes(x = year, y = a)) + 
  geom_line()


wind %>% 
  as_tibble() %>% 
  dplyr::select(d_aprov_n, dat_cls, area_int) %>% 
  gather(variable, value, -area_int) %>% 
  ggplot(aes(x = value, weight = area_int)) + 
  geom_histogram(binwidth = 365) + 
  facet_wrap(.~variable)


wind.cor <- wind %>% 
  as_tibble() %>% 
  dplyr::select(d_aprov_n, dat_cls) %>% 
  mutate(d_aprov_n = as.numeric(d_aprov_n)) %>% 
  mutate(dat_cls = as.numeric(dat_cls))

weights = wind$area_int %>% unlist()

cov.wt(wind.cor %>% as.matrix(), wt = weights, cor = TRUE)

wind %>% 
  as_tibble() %>% 
  dplyr::select(d_aprov_n, dat_cls, area_int) %>% 
  ggplot(aes(x = d_aprov_n, y= dat_cls)) +
  geom_bin2d(aes(weight = area_int), binwidth =700) +
  geom_abline(slope = 1, intercept = 600)

solar %>% ggplot(aes(x = d_aprov_n, weight = area_int)) + geom_histogram(binwidth = 365)


part1 <- privatizations_timeseries %>% 
  dplyr::select(Key, runid) %>% 
  unique()

part2 <- tibble(year = -10:10)

full_set <- merge(part1, part2, full = TRUE)

privatizations_diff <- privatizations_timeseries %>% 
  mutate(dat_clos = as.Date("2004-01-01")) %>% 
  mutate(dist = as.numeric(t - dat_clos)/365) %>%
  #mutate(dist = as.numeric(t - as.Date("2009-12-14"))/365) %>%
  mutate(year = round(dist)) %>% 
  group_by(year, Key, runid) %>% 
  summarize(area_km2_cum = max(area_km2_cum))

privatizations_diff_full <- full_set %>% 
  left_join(privatizations_diff, by = c("Key" = "Key", "runid" = "runid", "year" = "year")) %>% 
  group_by(Key, runid) %>% 
  arrange(year) %>% 
  fill(area_km2_cum) %>% 
  fill(area_km2_cum, .direction = "up") %>% 
  ungroup() %>% 
  arrange(Key, runid, year)

privatizations_diff_full %>% 
  group_by(runid, year) %>% 
  summarize(area_km2_cum = sum(area_km2_cum)) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y =area_km2_cum)) +
  geom_line(aes(col = runid)) +
  scale_color_manual(values=COLORS3)

privatizations_timeseries %>% 
  mutate(dat_clos = floor_date(dat_clos, "month")) %>% 
  group_by(Key, dat_clos, t, runid) %>% 
  summarize(area_km2_cum = sum(area_km2_cum)) %>%
  ungroup() %>% 
  ggplot(aes(x=dat_clos, y=area_km2_cum)) +
  geom_point(aes(col = runid)) +
  facet_wrap(.~runid)

                                                                                                                        