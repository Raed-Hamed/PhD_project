#Clean workspace
rm(list = ls())
graphics.off()
gc()

#Set directories
dir_in    <-"C:/Users/rhd630/Desktop/PhD/Academic/Modelling/Input_data/"  
dir_USDA <- paste0(dir_in,"/USDA")

#Load packages
library("tidyverse")
library("USAboundaries")
library("SDMTools")
library("maptools")
library("RColorBrewer")

#Load datasets
fname <-file.path(dir_USDA, paste0("soy_usda.csv")) 
USDA_df <-read.csv(fname)
str(USDA_df)

#Define fixed parameters
period <- seq(1981,2016,1)
yield_conv_rate <-0.0673

#usda df manipulation
Yield_ts <- USDA_df %>%
  filter(Data.Item == "SOYBEANS - YIELD, MEASURED IN BU / ACRE" &  
         Period == "YEAR" &
         Year %in% period) %>% 
  mutate(Value = Value * yield_conv_rate) %>% 
  dplyr:: select( State,Year, Value) %>% 
  group_by(State, .drop = TRUE) %>% 
  filter(n() == length(period))

#plot ts
ggplot(Yield_ts, aes(x=Year, y=Value, color=State, group=State)) + 
  geom_line(size=1.2) +
  scale_x_continuous(breaks=seq(1981, 2016,8))  +
  facet_wrap(~ State, scale="fixed") +
  ylab("Yield (t/ha)") +
  ggtitle("dasdas") +
  theme(legend.position = "none")

#linear detrending
resid_yield_ts <-Yield_ts %>% 
  group_map(~ lm(Value ~ Year, data = .))%>% 
  set_names(unlist(group_keys(Yield_ts))) %>%   
  map_df(broom::augment, .id = "State") %>% 
  dplyr:: select( State,Year, .resid) 

#plot detrending
ggplot(resid_yield_ts, aes(x=Year, y=.resid, color=State, group=State)) + 
  geom_line(size=1.2) +
  scale_x_continuous(breaks=seq(1981, 2016,8))  +
  facet_wrap(~ State, scale="fixed") +
  ylab("Yield (t/ha)") +
  ggtitle("Residuals") +
  theme(legend.position = "none")

  
#spatial def 
yield_ts_spatial_df <- us_states() %>%
  rename(State = name) %>%
  mutate(State = str_to_upper(State)) %>% 
  dplyr:: select(State, geometry) %>%
  right_join(Yield_ts, by = "State")

#plot spatial
spatial_usa <- us_states[!(us_states$NAME %in% c("Alaska ", "Alaska ", "Puerto Rico")), ]

ggplot(data = yield_ts_spatial_df) +
  geom_sf(data = newdata) +
  geom_sf(aes(fill = Value)) +
  facet_wrap(~ Year, scale="fixed")+
  ggtitle("dasdas") +
scale_fill_gradientn("Yield (t/ha)",colors = c("RED","YELLOW","GREEN"))


