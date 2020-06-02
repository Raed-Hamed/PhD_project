#Clean workspace---------------------------------------------------------------------------------------
rm(list = ls())
graphics.off()
gc()

#Set directories---------------------------------------------------------------------------------------
dir_in    <-"C:/Users/rhd630/Desktop/PhD/Academic/Modelling/Input_data/"  
dir_USDA <- paste0(dir_in,"/USDA")

#Load functions and packages---------------------------------------------------------------------------
library("tidyverse")
library("USAboundaries")
library("SDMTools")
library("maptools")
library("RColorBrewer")

#'@ Replace Commas Function
#'
#' This function converts a character representation of a number that contains a comma separator with a numeric value.
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

#Load datasets------------------------------------------------------------------------------------------
fname <-file.path(dir_USDA, paste0("soy_usda_full.csv"))  
USDA_df <-read.csv(fname)
str(USDA_df)
USDA_df$Value <- replaceCommas(USDA_df$Value) #quick clean loaded dataset
sf_us_states <-us_states()
spatial_usa <- sf_us_states[!(sf_us_states$name %in% c("Alaska","Puerto Rico","Hawaii")), ]#quick clean loaded dataset

#Define fixed parameters
period <- seq(1981,2016,1)
yield_conv_rate <-0.0673
weight_conv_rate <-0.0272155 
area_conv_rate <-0.404686

#Add spatial dimension to df
USDA_df_spatial <- us_states() %>%
  rename(State = name) %>%
  mutate(State = str_to_upper(State)) %>% 
  dplyr:: select(State, geometry) %>%
  right_join(USDA_df, by = "State")

#filter and compute dfs-----------------------------------------------------------------------------------
#Usda yield ts
Yield_ts <- USDA_df_spatial %>%
  filter(Data.Item == "SOYBEANS - YIELD, MEASURED IN BU / ACRE" &  
           Period == "YEAR" &
           Year %in% period) %>% 
  mutate(Value = Value * yield_conv_rate) %>% 
  dplyr:: select(State,Year, Value) %>% 
  group_by(State, .drop = TRUE) %>% 
  filter(n() == length(period))

#Usda yield ts linear detrending
resid_yield_ts <-Yield_ts %>% 
  group_map(~ lm(Value ~ Year, data = .))%>% 
  set_names(unlist(group_keys(Yield_ts))) %>%   
  map_df(broom::augment, .id = "State") %>% 
  dplyr::select(State,Year, .resid) %>% 
  left_join(Yield_ts, by = c("State", "Year"))

#Usda planted area ts
planted_ts <- USDA_df_spatial %>%
  filter(Data.Item == "SOYBEANS - ACRES PLANTED"  &  
           Period == "YEAR" &
           Year %in% period) %>% 
  mutate(Value = Value * area_conv_rate) %>% 
  dplyr:: select(State,Year, Value) %>% 
  group_by(State, .drop = TRUE) %>% 
  filter(n() == length(period))

#Usda planted area percentage of total
planted_ts_prct <- USDA_df_spatial %>%
  filter(Data.Item == "SOYBEANS - ACRES PLANTED"  &  
           Period == "YEAR" &
           Year %in% period) %>% 
  mutate(Value = Value * area_conv_rate) %>% 
  dplyr:: select(State,Year, Value) %>% 
  group_by(Year, .drop = TRUE) %>% 
  mutate(prct_total = (Value /sum(Value))*100 ) %>% 
  arrange(desc(prct_total)) %>%         
  group_by(State, .drop = TRUE) %>% 
  filter(n() == length(period)) %>% 
  summarise(period_avg_planted_portion = mean(prct_total),    
          sd_var = sd(prct_total))%>%
  arrange(desc(period_avg_planted_portion)) %>%         
  top_n(10, period_avg_planted_portion)# %>% 
 # sum(period_avg_planted_port~)

#Top planted regions in the US
top_10_planting_areas <- planted_ts %>%
  summarise(period_avg = mean(Value),    
            sd_var = sd(Value))%>%
  arrange(desc(period_avg)) %>%         
  top_n(10, period_avg)  
 
#Plot ts and spatial dfs-----------------------------------------------------------------------------------

#plot yield ts
ggplot(Yield_ts, aes(x=Year, y=Value, color=State, group=State)) + 
  geom_line(size=1.2) +
  scale_x_continuous(breaks=seq(1981, 2016,8))  +
  facet_wrap(~ State, scale="fixed") +
  ylab("Yield (t/ha)") +
  ggtitle("dasdas") +
  theme(legend.position = "none")

#plot planted area ts
ggplot(planted_ts, aes(x=Year, y=Value, color=State, group=State)) + 
  geom_line(size=1.2) +
  scale_x_continuous(breaks=seq(1981, 2016,8))  +
  facet_wrap(~ State, scale="fixed") +
  ylab("Planted area (ha)") +
  ggtitle("dasdas") +
  theme(legend.position = "none")

#plot linearily detrended yield ts
ggplot(resid_yield_ts, aes(x=Year, y=.resid, color=State, group=State)) + 
  geom_line(size=1.2) +
  scale_x_continuous(breaks=seq(1981, 2016,8))  +
  facet_wrap(~ State, scale="fixed") +
  ylab("Yield (t/ha)") +
  ggtitle("Residuals") +
  theme(legend.position = "none")

#plot spatial yield ts
ggplot(data = Yield_ts) +
  geom_sf(data = spatial_usa) +
  geom_sf(aes(fill = Value)) +
  facet_wrap(~ Year, scale="fixed")+
  ggtitle("dasdas") +
  scale_fill_gradientn("Yield (t/ha)",colors = c("RED","YELLOW","GREEN"))

#plot spatial yield residuals based on the linear detrending
ggplot(data = resid_yield_ts) +
  geom_sf(data = spatial_usa) +
  geom_sf(aes(fill = .resid, geometry = geometry)) +
  facet_wrap(~ Year, scale="fixed")+
  ggtitle("dasdas") +
  scale_fill_gradientn("Yield (t/ha)",colors = c("RED","YELLOW","GREEN"))





test<-resid_yield_ts %>% 
  dplyr::select(State, Year, .resid)
###########Clustering attempts

#Convert to wide format
wide_df = test %>% 
  spread(Year, .resid)

#convert data to matrix form
wide_matrix <- as.matrix(wide_df[,-1])

#Calculate dissimilarity matrix
library(TSclust)
library(dtwclust)

matrix_dist <- diss(SERIES = wide_matrix, METHOD = "COR")

#put the names of the matrix back in
names(matrix_dist) <- wide_df$State

#use elbow method to determine optimal number of clusters
set.seed(1234)
k_max <-20
data <-matrix_dist
wss <-sapply(1:k_max, function(k){kmeans(data, k , nstart = 50, iter.max = 20)$tot.withinss})

wss
#compute and plot wss for k =2 to k=20
plot(1:k_max, wss, 
     type="b", pch=19, frame=FALSE,
     xlab= "Number of clusters k",
     ylab= "Total within clusters sum of squares")

#use silhouette analysis to determine the optimal number of clusters
library(fpc)
set.seed(1234)

pamk_best <- pamk(matrix_dist)
cat("number of clusters estimated by optimum average silhouette width:", pamk_best$nc, "\n")

#Use the calinsky criterion to determine the optimum number of clusters
library(vegan)
set.seed(1234)
cal_fit <- cascadeKM(matrix_dist, 1, 10, iter=1000)
plot(cal_fit, sortg=TRUE, grpmts.plot =TRUE)

matrix_hclust <- hclust(matrix_dist)
plot(matrix_hclust, main ="Clustering of the # reported yields")


matrix_hclust_cut <-cutree(matrix_hclust,2)
#rect.hclust <- (matrix_hclust, k=6)

#Merge data cluster back to orginal data to get meaningful averages comparisions
df_clust <-as.data.frame(cbind(wide_df,matrix_hclust_cut))

lon_df_clust = df_clust %>% 
  gather(as.character(1981:2016),key = "Year", value= .resid)
lon_df_clust


#plot spatial resid
#spatial resid def 
  lon_df_clust_spatial <- us_states() %>%
  rename(State = name) %>%
  mutate(State = str_to_upper(State)) %>% 
  dplyr:: select(State, geometry) %>%
  right_join(lon_df_clust, by = "State")


ggplot(data = lon_df_clust_spatial) +
  geom_sf(data = spatial_usa) +
  geom_sf(aes(fill = factor(matrix_hclust_cut))) +
  facet_wrap(~ Year, scale="fixed")+
  ggtitle("dasdas") 