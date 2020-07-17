##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,
               gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,THFstyle,ggthemes)

#Clean up the global environment
rm(list = ls())

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Set directory
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Shielding/")

##############################################
################### GEO-DATA #################
##############################################

OA_to_higher_geo <- fread("Other data/Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv", header=TRUE, sep=",", check.names=T)

LSOA_to_higher_geo <- OA_to_higher_geo[, list(LSOA11NM = first(LSOA11NM), MSOA11CD = first(MSOA11CD),
                                              LAD17CD = first(LAD17CD), LAD17NM=first(LAD17NM),
                                              RGN11CD = first(RGN11CD), RGN11NM=first(RGN11NM)), 
                                       by = list(LSOA11CD)]

rm(OA_to_higher_geo)

############################################################
################### Local authority shapefile ##############
############################################################

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Shielding/Shapefiles/Local_Authority_Districts__December_2017__Boundaries_in_the_UK__WGS84_-shp/")

LAD_2019_shp <- readOGR(dsn=".", layer="Local_Authority_Districts__December_2017__Boundaries_in_the_UK__WGS84_")
LAD_2019_shp <- spTransform(LAD_2019_shp, CRS(latlong)) #Set to the same projection

##############################################
################### POPULATION ###############
##############################################

#England and Wales

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Shielding/Other data/Mid-year population estimates/LSOA/")

pop_by_LSOA <- fread("2011-to-2018-pop-post.csv", header=TRUE, sep=",", check.names=T,
                     select=c("lsoa11","pop18","children18","adults16plus18","over65_18","mean_age_18")) %>%
              mutate(.,pct_over65_18=round(over65_18/pop18*100,1))

#Join with geo-indicators

pop_by_LSOA <- dplyr::left_join(pop_by_LSOA,LSOA_to_higher_geo,by=c("lsoa11"="LSOA11CD"))

sum(is.na(pop_by_LSOA$RGN11NM)) #All matched

#Pop by local authority

pop_by_LSOA <- as.data.table(pop_by_LSOA)

pop_by_LA <- pop_by_LSOA[, list(pop18=sum(pop18),children18=sum(children18),
                                adults16plus18=sum(adults16plus18),over65_18=sum(over65_18),
                                mean_age_18=weighted.mean(mean_age_18,pop18),
                                pct_over65_18=weighted.mean(pct_over65_18,pop18),
                                LAD17NM=first(LAD17NM),RGN11CD = first(RGN11CD),
                                RGN11NM=first(RGN11NM)), 
                              by = list(LAD17CD)]

##############################################
################### DEPRIVATION ##############
##############################################

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Shielding/Other data/IMD/")

#LSOA level

IMD2019_LSOA <- fread("IMD_19_15_10.csv", header=TRUE, sep=",", check.names=T)

#LA level

IMD2019_LA <- fread("Local authority/download1677836969253211150.csv", header=TRUE, sep=",", check.names=T) %>%
    filter(.,DateCode==2019)

#LA level - average score

unique(IMD2019_LA$Indices.of.Deprivation)

IMD2019_LA_avgscore_IMD <- filter(IMD2019_LA,Measurement=='Average Score'&
                                    Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
                            rename(.,avgscore_imd=Value) %>% select(.,FeatureCode,avgscore_imd)

IMD2019_LA_avgscore_Health <- filter(IMD2019_LA,Measurement=='Average Score'&
                                    Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
                            rename(.,avgscore_health=Value) %>% select(.,FeatureCode,avgscore_health)

IMD2019_LA_avgscore_Income <- filter(IMD2019_LA,Measurement=='Average Score'&
                                       Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,avgscore_income=Value) %>% select(.,FeatureCode,avgscore_income)

#LA level - % of LSOA among most deprived

IMD2019_LA_pctdep_IMD <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                    Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
                            rename(.,pctdep_IMD=Value) %>% mutate(.,pctdep_IMD=100*pctdep_IMD) %>% select(.,FeatureCode,pctdep_IMD)

IMD2019_LA_pctdep_Health <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                       Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
                            rename(.,pctdep_health=Value) %>% mutate(.,pctdep_health=100*pctdep_health) %>% select(.,FeatureCode,pctdep_health)

IMD2019_LA_pctdep_Income <- filter(IMD2019_LA,Measurement=='Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'&
                                     Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,pctdep_income=Value) %>% mutate(.,pctdep_income=100*pctdep_income) %>% select(.,FeatureCode,pctdep_income)

#Wide format

IMD2019_LA_wide <- left_join(IMD2019_LA_avgscore_IMD,IMD2019_LA_avgscore_Health,by="FeatureCode") %>%
    left_join(.,IMD2019_LA_pctdep_IMD,by="FeatureCode") %>%
    left_join(.,IMD2019_LA_pctdep_Health,by="FeatureCode") %>%
    left_join(.,IMD2019_LA_avgscore_Income,by="FeatureCode") %>%
    left_join(.,IMD2019_LA_pctdep_Income,by="FeatureCode")

# 'Rank of average score'
# 'Average Score'
# 'Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'

############################################################
################### NUMBER OF SHIELDERS BY LA ##############
############################################################

#Number of shielders

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("/Users/sgpeytrignet/Dropbox (Personal)/THF/Shielding/SPL/")

SPL_by_LA <- fread("Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2020-07-09.csv", header=TRUE, sep=",", check.names=T)

# filter(SPL_by_LA,LA.Name=="Hackney")
# SPL_by_LA[grepl("dorset", tolower(SPL_by_LA$LA.Name), fixed = TRUE),]

#Merge in population numbers

SPL_by_LA <- left_join(SPL_by_LA,pop_by_LA,by=c("LA.Code"="LAD17CD"))

#Manually add in populationd data for rows that add together 2 LAs

hackney_city_london <- filter(pop_by_LA,LAD17NM=="Hackney")$pop18+filter(pop_by_LA,LAD17NM=="City of London")$pop18
cornwall_scilly <- filter(pop_by_LA,LAD17NM=="Cornwall")$pop18+filter(pop_by_LA,LAD17NM=="Isles of Scilly")$pop18

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$pop18 <- hackney_city_london
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$pop18 <- cornwall_scilly

#ALl shielders

SPL_by_LA_All <- dplyr::filter(SPL_by_LA,Breakdown.Field=="ALL") %>%
  dplyr::filter(., LA.Code %in% unique(LSOA_to_higher_geo$LAD17CD))

SPL_by_LA_All$Patient.Count <- as.numeric(SPL_by_LA_All$Patient.Count)

SPL_by_LA_All <- dplyr::mutate(SPL_by_LA_All,Shielders_pct=Patient.Count/pop18*100) %>%
                  arrange(.,desc(Shielders_pct)) %>% as.data.table()

#Histogram

ggplot(SPL_by_LA_All, aes(Shielders_pct, fill = cut(Shielders_pct, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% shielders", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

######################################################################
################### Number of shielders vs. deprivation ##############
######################################################################

SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))

SPL_by_LA_All$score_income_cat <- cut(SPL_by_LA_All$avgscore_income, breaks=c(0,0.05,0.075,0.1,0.125,0.150,0.2,Inf), labels=1:7)

#General deprivation

ggplot(SPL_by_LA_All, aes(x=pctdep_IMD, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(SPL_by_LA_All, aes(x=avgscore_imd, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

#Health deprivation

ggplot(SPL_by_LA_All, aes(x=pctdep_health, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(SPL_by_LA_All, aes(x=avgscore_health, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

#Income deprivation

ggplot(SPL_by_LA_All, aes(x=pctdep_income, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(SPL_by_LA_All, aes(x=avgscore_income, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_income_dep <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop18)), 
                         by = list(score_income_cat)]

ggplot(pct_shielding_by_income_dep) +
  geom_col(aes(x=reorder(score_income_cat, Shielders_pct), y = Shielders_pct)) +
  geom_text(aes(x = score_income_cat, y = Shielders_pct + 0.15, label = round(Shielders_pct, 1))) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_economist() +
  labs(title="Shielding population",y = "% shielding",x="Income deprivation score")


######################################################################
################### Number of shielders vs. pct over 65 ##############
######################################################################

ggplot(SPL_by_LA_All, aes(x=pct_over65_18, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

##############################################################
################### Number of shielders per GOR ##############
##############################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

SPL_by_GOR <- SPL_by_LA_All[, list(Shielders_pct=weighted.mean(Shielders_pct,pop18),
                                RGN11NM=first(RGN11NM)), 
                         by = list(RGN11CD)]

ggplot(SPL_by_GOR) + geom_col(aes(x=reorder(RGN11NM, Shielders_pct), y = Shielders_pct)) +
  geom_text(aes(x = RGN11NM, y = Shielders_pct + 0.15, label = round(Shielders_pct, 1))) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme_economist() +
  labs(title="Shielding population",y = "% shielding",x="")

#################################################################################
################### Map of number of shielders per 1,000 residents ##############
#################################################################################

#Add data to shapefile

LAD_2019_shp@data <- left_join(LAD_2019_shp@data,SPL_by_LA_All,by=c("lad17cd"="LA.Code"))

#Create map

#Colors and palette
colors <- colorRampPalette(brewer.pal(9, "Spectral"))(10)[10:1]

pal.function <- colorQuantile(colors, LAD_2019_shp$Shielders_pct, n = 10, probs = seq(0, 1, length.out = 10
                                                  + 1), na.color = "transparent", alpha = FALSE, reverse = FALSE,
              right = FALSE)

#Quantiles
deciles <- round(quantile(LAD_2019_shp$Shielders_pct,
               prob = seq(0, 1, length = 11), type = 5,na.rm=TRUE),1)

#Labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g percent",
  LAD_2019_shp$lad17nm, round(LAD_2019_shp$Shielders_pct,2)
) %>% lapply(htmltools::HTML)

#Map
leaflet(LAD_2019_shp) %>%
  addProviderTiles(providers$Wikimedia) %>%
  setView(lat=51.5095,lng=-0.1245,zoom = 5) %>%
  addPolygons(
    fillColor = ~pal.function(Shielders_pct),weight = 1,opacity = 0.1,color = "black",
    dashArray = "3",fillOpacity = 0.3,
    highlight = highlightOptions(weight = 5,color = "#666",dashArray = "",
                                 fillOpacity = 0.7,bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% addLegend(
    position = "bottomright",
    colors = colors,
    labels = deciles[1:10], opacity = 1
  )