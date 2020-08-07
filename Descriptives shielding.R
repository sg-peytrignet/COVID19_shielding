##########################################################
################### DEVELOPMENT IDEAS ####################
##########################################################

#Gender disparity: cannot look at reasons by gender using this data
#Why are people shielding and does this vary across regions?

#Isolate worst 10% and see why reasons vary (% cancer, % respiratory)
#Do the same by Region
#Bar chart (lowest to highest) for 'Respiratory' with horizontal line for England, and
#bars coloured by shielders per capita

#Same approach for why women have higher rates
#Clustering: what type of shielders per area?

##############################################
################### SETUP ####################
##############################################

#Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,stringr,sp,ggplot2,plyr,readODS,
               gmodels,Rmisc,DescTools,data.table,
               Hmisc,tibble,leaflet,rgeos,raster,plotly,
               pbapply,pbmcapply,here,rgdal,RColorBrewer,ggthemes,
               ggchicklet,tidyverse,showtext,ggchicklet)

#install.packages("ggchicklet", repos = "https://cinc.rud.is")

#Load fonts

#font_add_google("Roboto Mono", "roboto-mono")
#font_add_google("Roboto", "roboto")
font_add_google("Quicksand", "quicksand")
showtext_auto()

#Clean up the global environment
rm(list = ls())

#Projection codes
ukgrid = "+init=epsg:27700"
latlong="+init=epsg:4326"

#Set directory
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("M:/Analytics/Networked Data Lab/Shielding/")

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
setwd("M:/Analytics/Networked Data Lab/Shielding/Shapefiles/Local_Authority_Districts__December_2016__Boundaries_UK-shp/")

LAD_2019_shp <- readOGR(dsn=".", layer="Local_Authority_Districts__December_2016__Boundaries_UK")
LAD_2019_shp <- spTransform(LAD_2019_shp, CRS(latlong)) #Set to the same projection

##############################################
################### POPULATION ###############
##############################################

#England and Wales

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("M:/Analytics/Networked Data Lab/Shielding/Other data/Mid-year population estimates/LSOA/")

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
setwd("M:/Analytics/Networked Data Lab/Shielding/")

#LSOA level

IMD2019_LSOA <- fread("Other data/IMD/IMD_19_15_10.csv", header=TRUE, sep=",", check.names=T)

#LA level

IMD2019_LA <- fread("Other data/IMD/Local authority/download1677836969253211150.csv", header=TRUE, sep=",", check.names=T) %>%
  filter(.,DateCode==2019)

ranks <- select(IMD2019_LA,'Measurement','Value','Indices.of.Deprivation') %>%
  filter(.,Measurement=="Rank of average score"&Indices.of.Deprivation=="a. Index of Multiple Deprivation (IMD)") %>%
  select(.,'Value') %>% unique(.)

#LA level - rank of average score

IMD2019_LA_rank_IMD <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                Indices.of.Deprivation=='a. Index of Multiple Deprivation (IMD)') %>%
  rename(.,rank_imd=Value) %>% select(.,FeatureCode,rank_imd) %>%
  mutate(.,decile_imd=cut(rank_imd, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10))

IMD2019_LA_rank_Health <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='e. Health Deprivation and Disability Domain') %>%
  rename(.,rank_health=Value) %>% select(.,FeatureCode,rank_health) %>%
  mutate(.,decile_health=cut(rank_health, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10))

IMD2019_LA_rank_Income <- filter(IMD2019_LA,Measurement=='Rank of average score'&
                                   Indices.of.Deprivation=='b. Income Deprivation Domain') %>%
  rename(.,rank_income=Value) %>% select(.,FeatureCode,rank_income) %>%
  mutate(.,decile_income=cut(rank_income, breaks=c(0,quantile(ranks$Value, probs = seq(0, 1, 1/10))[-1]), labels=1:10))

#LA level - average score

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
  left_join(.,IMD2019_LA_pctdep_Income,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_IMD,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_Health,by="FeatureCode") %>%
  left_join(.,IMD2019_LA_rank_Income,by="FeatureCode")

# 'Rank of average score'
# 'Average Score'
# 'Proportion of Lower-layer Super Output Areas (LSOAs) in most deprived 10% nationally'

########################################################################
################### NUMBER OF SHIELDERS BY LA: Wales ################### 
########################################################################

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("M:/Analytics/Networked Data Lab/Shielding/SPL/Wales/")

SPL_by_LA_Wales <- read_ods("shielded-patient-list-in-wales-during-the-coronavirus-covid-19-pandemic-as-at-27-july-2020-182.ods", skip=4,sheet="Table_1",col_names=TRUE)
SPL_by_LA_Wales <- dplyr::filter(SPL_by_LA_Wales,is.na(SPL_by_LA_Wales$'Local Authority Code')==FALSE) %>%
  rename(.,LA.Code='Local Authority Code',Patient.Count='All Ages',LA.Name='Local Authority Name') %>%
  select(.,LA.Code,Patient.Count,LA.Name)

############ ALl shielders: Wales

SPL_by_LA_Wales_All <- dplyr::filter(SPL_by_LA_Wales, LA.Code %in% unique(LSOA_to_higher_geo$LAD17CD))

SPL_by_LA_Wales_All$Patient.Count <- as.numeric(SPL_by_LA_Wales_All$Patient.Count)

SPL_by_LA_Wales_All$Breakdown.Field <- "ALL"

####################################################################################
################### NUMBER OF SHIELDERS BY LA: England and Wales ################### 
####################################################################################

setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("M:/Analytics/Networked Data Lab/Shielding/SPL/")

SPL_by_LA <- fread("Coronavirus Shielded Patient List, England - Open Data with CMO DG - LA - 2020-07-30.csv", header=TRUE, sep=",", check.names=T)

SPL_by_LA <- rbind.fill(SPL_by_LA,SPL_by_LA_Wales_All)

############ Merge in population numbers (you will lose some Local Authorities - the way the SPL is structured is non-standard)

SPL_by_LA <- left_join(SPL_by_LA,pop_by_LA,by=c("LA.Code"="LAD17CD"))

#Manually add in population data for rows that add together 2 LAs

hackney_city_london <- filter(pop_by_LA,LAD17NM=="Hackney")$pop18+filter(pop_by_LA,LAD17NM=="City of London")$pop18
cornwall_scilly <- filter(pop_by_LA,LAD17NM=="Cornwall")$pop18+filter(pop_by_LA,LAD17NM=="Isles of Scilly")$pop18

SPL_by_LA[SPL_by_LA$LA.Name %in% c("Hackney and City of London"),]$pop18 <- hackney_city_london
SPL_by_LA[SPL_by_LA$LA.Name %in% c("Cornwall and Isles of Scilly"),]$pop18 <- cornwall_scilly

############ ALl shielders

SPL_by_LA_All <- dplyr::filter(SPL_by_LA,Breakdown.Field=="ALL") %>%
  dplyr::filter(., LA.Code %in% unique(LSOA_to_higher_geo$LAD17CD))

SPL_by_LA_All$Patient.Count <- as.numeric(SPL_by_LA_All$Patient.Count)

SPL_by_LA_All <- dplyr::mutate(SPL_by_LA_All,Shielders_pct=Patient.Count/pop18*100) %>%
  arrange(.,desc(Shielders_pct)) %>% as.data.table()

filter(SPL_by_LA_All,RGN11NM=="Wales")

############ Number of shielders by disease group

#Subset of data
SPL_by_LA_dgroup <- filter(SPL_by_LA,Breakdown.Field=="Disease Group") %>%
  rename(.,Cases.Count=Patient.Count)

#Clean up number variable
SPL_by_LA_dgroup$Cases.Count[which(SPL_by_LA_dgroup$Cases.Count=="*")] <- NA
SPL_by_LA_dgroup$Cases.Count <- as.numeric(SPL_by_LA_dgroup$Cases.Count)

#Simplify groups
SPL_by_LA_dgroup$group <- mapvalues(SPL_by_LA_dgroup$Breakdown.Value,
                                    from = c("Transplants (Solid)","Transplants (Haematological within 6 months)",
                                             "Transplants (Haematological with Immunosuppression)",
                                             "Cancer (Solid with Chemotherapy)","Cancer (Lung with Radical Radiotherapy)",
                                             "Cancer (Haematological within 24 months)","Respiratory (Severe Asthma)",
                                             "Respiratory (Severe COPD)","Respiratory (Severe Permanent)",
                                             "Corticosteroid Safety Card","Rare genetic metabolic and autoimmune diseases",
                                             "Immunosuppression Therapy","Pregnant with Congenital Heart Defect"),
                                    to = c("Transplants","Transplants","Transplants",
                                           "Cancer","Cancer","Cancer",
                                           "Respiratory","Respiratory","Respiratory",
                                           "Steroid","Rare genetic and autoimmune",
                                           "Immunosuppression Therapy",
                                           "Pregnant with Congenital Heart Defect"))

SPL_by_LA_dgroup <- as.data.table(SPL_by_LA_dgroup)

SPL_by_LA_dgroup <- SPL_by_LA_dgroup[, list(Cases.Count=sum(Cases.Count,na.rm=TRUE),
                                            LA.Name=first(LA.Name),
                                            pop18=first(pop18)),
                                     by = list(LA.Code,group)]

filter(SPL_by_LA_dgroup,LA.Name=="Camden")
filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")
filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")

#Statistics to compare 'reasons' between LA and England
SPL_by_LA_dgroup <- within(SPL_by_LA_dgroup, {Cases.Total = ave(Cases.Count,LA.Code,FUN=sum)} )
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Cases.Pct=Cases.Count/Cases.Total*100)
SPL_by_LA_dgroup <- mutate(SPL_by_LA_dgroup,Cases.Prev.1000=Cases.Count/pop18*1000)

#Merge-in the English averages

England.rates <- filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND") %>%
  select(.,group,Cases.Pct) %>% rename(.,Cases.Pct.England=Cases.Pct)

SPL_by_LA_dgroup <- left_join(SPL_by_LA_dgroup,England.rates,by="group") %>%
  mutate(.,Cases.Pct.Differential=Cases.Pct-Cases.Pct.England)

#Merge-in the LA-level shielder rate

SPL_by_LA_dgroup <- left_join(SPL_by_LA_dgroup,dplyr::select(SPL_by_LA_All,LA.Code,RGN11CD,RGN11NM,Shielders_pct),
                              by="LA.Code")

#Flag high rates of certain conditions

SPL_by_LA_dgroup$over50_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=50,1,0)
SPL_by_LA_dgroup$over30_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=30,1,0)
SPL_by_LA_dgroup$over20_pct <- ifelse(SPL_by_LA_dgroup$Cases.Pct>=20,1,0)
SPL_by_LA_dgroup$levels_pct <- cut(SPL_by_LA_dgroup$Cases.Pct, breaks=c(-Inf,40,50,60,Inf), labels=1:4)

filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")
filter(SPL_by_LA_dgroup,LA.Name=="Camden")
filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")

#################################################################
################### Histogram of % shielder per LA ##############
#################################################################

ggplot(SPL_by_LA_All, aes(Shielders_pct, fill = cut(Shielders_pct, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% shielders", y = "n") +
  ggtitle("Histogram") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

###########################################################
############## Gender differences in shielding ############
###########################################################

SPL_by_LA_Gender <- filter(SPL_by_LA,Breakdown.Field=="Gender"&LA.Name=="ENGLAND")

men_england <-  56286961*0.49
women_england <-  56286961*0.51

gender <- c("Men","Women")
rate_shielding <- c(as.numeric(filter(SPL_by_LA_Gender,Breakdown.Value=="Male")$Patient.Count)/men_england*100,
                    as.numeric(filter(SPL_by_LA_Gender,Breakdown.Value=="Female")$Patient.Count)/women_england*100)

by_gender <- cbind.data.frame(gender,rate_shielding)

dotplot_gender <- ggplot(by_gender,
                            aes(rate_shielding, reorder(gender, rate_shielding))) +
  xlim(3, 5) +
  geom_point(aes(col=gender),size=4) +
  geom_text(aes(x=rate_shielding+0.2,label=paste0(round(rate_shielding,1),"%")),
            family="quicksand",size=25) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(family = "quicksand", size = 80),
        legend.position = "none") +
  labs(title="Likelihood of shielding by gender",y = " ",x=" ")

windows()
dotplot_gender

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("M:/Analytics/Networked Data Lab/Shielding/Graphs/")

ggsave("dotplot_gender.png", device="png",width = 5, height = 5,dpi=500)

###########################################################
############## Reasons for shielding dot plot #############
###########################################################

dotplot_condition <- ggplot(filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND"),
                            aes(Cases.Pct, reorder(group, Cases.Pct))) +
  geom_point(size=4,col="springgreen4") +
  geom_text(aes(x=Cases.Pct+3,label=paste0(round(Cases.Pct,1),"%")),
            family="quicksand",size=25) +
  theme(panel.background = element_blank(),axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),axis.ticks.y=element_blank(),
        text = element_text(family = "quicksand", size = 80)) +
  labs(title="Conditions",y = " ",x=" ")

windows()
dotplot_condition

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("dotplot_condition.png", device="png",width = 10, height = 5,dpi=500)

###########################################################################
############## Bar chart per condition - national discrepancy #############
###########################################################################

filter(SPL_by_LA_dgroup,LA.Name=="ENGLAND")
filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")
sum(filter(SPL_by_LA_dgroup,LA.Name=="Liverpool")$Cases.Pct)

#####Respiratory (this is driving differences!)

# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
#   geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
#   ggtitle("Respiratory conditions") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Shielders_pct, y=Cases.Pct)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory"), aes(x=Shielders_pct, y=Cases.Prev.1000)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

respiratory <- ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory")) +
  ylim(0, 10) +
  geom_col(aes(x=reorder(LA.Name, Shielders_pct), y = Shielders_pct,fill = factor(levels_pct))) +
  scale_fill_brewer(palette="Spectral",name = "Respiratory cases",direction = -1,
                    labels = c("<40%", "40-50%","50-60%",">60%")) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "quicksand", size = 85)) +
  geom_hline(aes(yintercept=4),col="black", linetype="dashed") +
  annotate(geom="text", label="4% (average)", x=35, y=4, vjust=-1,size=25) +
  labs(title="Shielding patients by local authority",y = "% shielding",x="Local authority")

windows()
respiratory

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("respiratory.png", device="png",width = 10, height = 7,dpi=500)

#####Cancer

# ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
#   geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
#   ggtitle("Cancer") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Shielders_pct, y=Cases.Pct)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

ggplot(filter(SPL_by_LA_dgroup,group=="Cancer"), aes(x=Shielders_pct, y=Cases.Prev.1000)) +
  geom_point() +
  geom_text(aes(label=LA.Name), size=2)

#####Rare genetic and autoimmune

ggplot(filter(SPL_by_LA_dgroup,group=="Rare genetic and autoimmune"), aes(Cases.Pct, fill = cut(Cases.Pct, 100))) +
  geom_histogram(show.legend = FALSE) + theme_minimal() + labs(x = "% cases", y = "n") +
  ggtitle("Rare genetic and autoimmune") + scale_fill_discrete(h = c(240, 10), c = 120, l = 70)

ggplot(filter(SPL_by_LA_dgroup,group=="Rare genetic and autoimmune"), aes(x=Cases.Pct.Differential, y=Shielders_pct)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

######################################################################
################### Number of shielders vs. deprivation ##############
######################################################################

SPL_by_LA_All <- left_join(SPL_by_LA_All,IMD2019_LA_wide,by=c("LA.Code"="FeatureCode"))

#SPL_by_LA_All$score_income_cat <- cut(SPL_by_LA_All$avgscore_income, breaks=c(0,0.05,0.075,0.1,0.125,0.150,0.2,Inf), labels=1:7)

#General deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_IMD, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_imd, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

#Health deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_health, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_health, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

#Income deprivation

# ggplot(SPL_by_LA_All, aes(x=pctdep_income, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)
# 
# ggplot(SPL_by_LA_All, aes(x=avgscore_income, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_income_dep <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop18)), 
  by = list(decile_income)] %>%
  filter(.,!is.na(decile_income))

cols <- colorRampPalette(brewer.pal(n = 9, name = "RdYlGn"))(10)
deplabs <- c("1 (most deprived)",2:9,"10 (least deprived)")

by_deprivation <- ggplot(pct_shielding_by_income_dep) +
  geom_chicklet(aes(x=decile_income, y = Shielders_pct,fill=decile_income),
                radius = grid::unit(5, 'mm'),width = 0.75) +
  geom_text(aes(x = decile_income, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=25) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(family = "quicksand", size = 80),
        axis.text = element_text(size = 80),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")))  +
  scale_fill_manual(values=cols,guide=FALSE) +
  scale_x_discrete(labels = deplabs) +
  labs(title="Average % residents shielding",y = " ",x="Income deprivation decile")

windows()
by_deprivation

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("by_deprivation.png", device="png",width = 16, height = 4,dpi=500)

######################################################################
################### Number of shielders vs. pct over 65 ##############
######################################################################

# ggplot(SPL_by_LA_All, aes(x=pct_over65_18, y=Shielders_pct)) + 
#   geom_point()+
#   geom_smooth(method=lm, se=FALSE)

head(dplyr::arrange(SPL_by_LA_All, pct_over65_18),n=10)
tail(dplyr::arrange(SPL_by_LA_All, pct_over65_18),n=10)

SPL_by_LA_All <- mutate(SPL_by_LA_All,
                        cat_pct_over65_18=cut(pct_over65_18, breaks=c(0,15,20,25,Inf),
                                              labels=c("15% or less","15-20%","20-25%","25% or more")))

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

pct_shielding_by_ageg <- SPL_by_LA_All[, list(
  Shielders_pct=weighted.mean(Shielders_pct,pop18)), 
  by = list(cat_pct_over65_18)]

cols <- brewer.pal(n = 8, name = "GnBu")[5:8]
agelabs <- c("<15% over 65","15-20% over 65","20-25% over 65",">25% over 65")
  
plot_age <- ggplot(pct_shielding_by_ageg) +
  geom_chicklet(aes(x=cat_pct_over65_18, y = Shielders_pct,fill=cat_pct_over65_18),
                radius = grid::unit(5, 'mm'),width=0.5) +
  geom_text(aes(x = cat_pct_over65_18, y = Shielders_pct + 0.2,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=15) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        text = element_text(family = "quicksand", size = 45),
        axis.text = element_text(size = 45),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"))) +
  scale_fill_manual(values=cols,guide=FALSE) +
  scale_x_discrete(labels = agelabs) +
  labs(title="Average % residents shielding",y = " ",x="Local authorities")

windows()
plot_age

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("over65.png", device="png",width = 5, height = 3,dpi=500)

##############################################################
################### Number of shielders per GOR ##############
##############################################################

SPL_by_LA_All <- as.data.table(SPL_by_LA_All)

SPL_by_GOR <- SPL_by_LA_All[, list(Shielders_pct=weighted.mean(Shielders_pct,pop18),
                                   RGN11NM=first(RGN11NM)), 
                            by = list(RGN11CD)]

#Add other countries

Scotland <- as.data.frame(t(c("Unknown",180000/(5.454*1000000)*100,"Scotland")))
names(Scotland) <- names(SPL_by_GOR)

NI <- as.data.frame(t(c("Unknown",80000/(1.882*1000000)*100,"Northern Ireland")))
names(NI) <- names(SPL_by_GOR)

SPL_by_GOR <- rbind(SPL_by_GOR,Scotland,NI)

SPL_by_GOR$Shielders_pct <- as.numeric(SPL_by_GOR$Shielders_pct)

#chart

plot_by_gor <- ggplot(SPL_by_GOR) +
  geom_chicklet(aes(x=reorder(RGN11NM, Shielders_pct), y = Shielders_pct),
                fill = "firebrick",radius = grid::unit(5, 'mm'),width=0.50) +
  geom_text(aes(x = RGN11NM, y = Shielders_pct + 0.45,
                label = paste0(round(Shielders_pct, 1),"%")),
            family="quicksand",size=25) +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        text = element_text(family = "quicksand", size = 90)) +
  labs(title="Average % residents shielding",y = " ",x="")


windows()
plot_by_gor

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("by_gor.png", device="png",width = 15, height = 4,dpi=500)

##############################################################################
################### Waffle chart for number of people shielding ##############
##############################################################################

library(waffle)
library(extrafont)

# fa_font <- tempfile(fileext = ".ttf")
# download.file("http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/fonts/fontawesome-webfont.ttf?v=4.3.0",
#               destfile = fa_font, method = "curl")
# 
# font_import(paths = dirname(fa_font), prompt = FALSE)

loadfonts(device = "win")

fa_list()

shielding <- c(`Category 1 (10)`= 96 , `Category 2 (20)`= 4)

waffle_plot <- waffle(shielding/2, rows=5, use_glyph = "male",
                      colors=c("darkgray","firebrick"),
                      legend_pos="none")

windows()
waffle_plot

#Save chart
setwd(str_replace_all(path.expand("~"), "Documents", ""))
setwd("C:/Users/SebastienP/OneDrive - The Health Foundation/Shielding/Graphs/")

ggsave("waffle.png", device="png",width = 5, height = 5,dpi=500)

#################################################################################
################### Map of number of shielders per 1,000 residents ##############
#################################################################################

#Add data to shapefile

LAD_2019_shp@data <- left_join(LAD_2019_shp@data,SPL_by_LA_All,by=c("lad16cd"="LA.Code"))

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
  LAD_2019_shp$lad16nm, round(LAD_2019_shp$Shielders_pct,2)
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
	  
# ggplot(filter(SPL_by_LA_dgroup,group=="Respiratory")) +
#   ylim(0, 10) +
#   geom_col(aes(x=reorder(LA.Name, Shielders_pct), y = Shielders_pct,fill = factor(over50_pct))) +
#   scale_fill_manual(values=c("#3288bd","#d53e4f"),name = "Respiratory cases",labels = c("<50%", ">50%")) +
#   theme(panel.border = element_blank(),
#         panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
#         axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#         panel.background = element_blank()) +
#   geom_hline(aes(yintercept=4),col="black", linetype="dashed") +
#   annotate(geom="text", label="4% (average)", x=35, y=4, vjust=-1,size=3) +
#   labs(title="Shielding patients by local authority",y = "% shielding",x="Local authority")
