######GIS
library(WDI)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggthemes)

df <- WDI(indicator = "SH.IMM.MEAS", extra = TRUE)
head(df)
df <- df %>% 
  filter(!is.na(SH.IMM.MEAS)) %>%
  filter(region != "Aggregates")
df_2018 <- df %>%
  filter(year==2018) %>%
  select(country, SH.IMM.MEAS, iso3c) # this viz will focus on 2018

# download mapping data from Natural Earth 
world <- ne_countries(scale = "medium", returnclass = "sf")

world_merged <- left_join(world, df_2018, by = c("iso_a3"="iso3c"))

ggplot(data = world_merged) + 
  geom_sf(aes(fill = SH.IMM.MEAS), color = NA, alpha = 0.8)
ggplot(data = world_merged) + 
  geom_sf(aes(fill = SH.IMM.MEAS), color = NA, alpha = 0.8) + 
  scale_fill_viridis_c(option = "plasma", na.value="gray90", breaks=seq(0,100,20), limits=c(20,100))
ggplot(data = world_merged) + 
  geom_sf(aes(fill = SH.IMM.MEAS), color = NA, alpha = 0.8) + 
  scale_fill_viridis_c(option = "plasma", na.value="gray90", breaks=seq(0,100,20), limits=c(20,100))+
  labs(title = "Fighting Measles with Vaccination",
       subtitle = "a world map of the share of infants vaccinated against measles",
       caption = "Source: World Development Indicators, World Bank 2018",
       fill = "% of infants \nvaccinated against measles") +     theme_map() +
  theme(plot.subtitle=element_text(size=10, color="grey40", lineheight=.9, face="italic"), 
        plot.caption=element_text(size=7, color="grey40"),
        legend.title = element_text(size=7, color="grey40"))
