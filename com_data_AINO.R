
# Read Summit Flora and GLORIA data from db -------------------------------


# sUMMITDiv-Code
# by A.Kulonen
# 11.12.2015

#Code for reading in different data versions odata from sUMMITDiv dropbox
###################################################################################################################

#install.packages("RODBC") #ignore if already installed
library("RODBC")

# !!!! "odbcConnect" does NOT work with 64-bit version of R! change to 32-bit version (tools --> global options --> general) and restart R!!!!!!!!

#build a connection:
# setwd("C:/Users/kulonen/Dropbox/Aino/sUMMITDiv_databasework/DB_work") ##add here path to folder where you have locally stored sUMMITDiv-zip file
summit<-unzip("sUMMITDiv20160308.zip") 
con <- odbcConnectAccess2007(summit)  
 
# if still doesn't work check following help-file
# ?odbcConnect


###################################################################################################################

# SUMMITFLORA DATA

# ------------------------------------------

# 1. Data for comparing historic and recent summit records (always presence/absence data):

# Species data

# This data table includes species lists from those summits which have at least two records for comparison in time. 
# Each row presents one species record.
# Can be used for instance in comparing species composition or richness or comparing extinct species vs resistant species 

spec <- sqlFetch(con, "Table_for_analyses_spec_summit_flora")

# exclude duplicates of species names for each summit record 
# (while converting species names, some old names were pooled to one name which caused duplicates to the lists. 
#       In these cases the record with higher altitude stays in table.)
spec$mtn_year_spec <- paste(spec$mountain_name, spec$year_of_record, spec$acceptedSpeciesCode , sep="#")
spec <- spec[order(spec$mtn_year_spec, -spec$find_altitude ), ] 
spec <- spec[which(!duplicated(spec$mtn_year_spec)),]


# summit meta data

# This data table includes mountain and record specific metadata for the summits. Each row presents one summit record.

# read data:
meta <- sqlFetch(con, "Table_for_analyses_metadata_summit_flora")

# ------------------------------------------

# 2. Data for extinction analyses

# Species data

# spec_E <- sqlFetch(con, "Table_for_analyses_EXTINCTIONS_summit_flora")
# spec_E$mtn_year_spec <- paste(spec_E$mountain_name, spec_E$year_of_record, spec_E$acceptedSpeciesCode , sep="#")
# spec_E <- spec_E[order(spec_E$mtn_year_spec, -spec_E$find_altitude ), ] 
# spec_E <- spec_E[which(!duplicated(spec_E$mtn_year_spec)),]

# meta data

# rec_meta <- sqlFetch(con, "SummitFlora_summit_history")
# sum_meta <- sqlFetch(con, "SummitFlora_summit_metadata")

# ------------------------------------------

# GLORIA DATA

# gloria data is organised differently

# species data: each row presents a species record for a monitoring time
spec.glo <- sqlFetch(con, "GLORIA_SummitAreaSections_species_data")

# summit meta data: each row presents a mountain
meta.glo <- sqlFetch(con, "GLORIA_summit_metadata")

# dates: in this file you will find the exact dates for monitoring times. Rows present records of summit sub areas.
date.glo <- sqlFetch(con, "GLORIA_SummitAreaSections")


# ------------------------------------------

# close connection to database:
close(con)

###################################################################################################################

# IMPORTANT TO KNOW ABOUT THE SUMMIT FLORA DATA.

# 1. Common variables for merging these two files are "mountain_name" and "year_of_record".
# 2. The record chosen as the baseline record is marked as "historic" in column "record_time_class".
# 3. meta_data includes also summits with a zero-record (summit area was sampled but NO species were found). 
#   These are marked as "1" in column "zero_record_summit". In metadata, there are also some records that have 
#   been pooled to another record, and don't have rows in spec-data for this reason. Note of this can be found 
#   in remarks in metadata.
# 4. If you work with species richness, for one record, we only have a value of species richness but no species 
#   names: Sp-richness for Piz_Vadret_da_Pruenas, 1983, 25 spieces 
# 5. Species that have been identified only to genus level are included in the data but are marked with "1" in 
#   column "genus_only".
# 6. Species records were botanist was uncertain about right identification are included but are marked with "1" 
#   in column "find_is_cf".

###################################################################################################################
###################################################################################################################


# Read Precipitation data -------------------------------------------------

# prec <-
# wait until see from Manuel's figures if it works

# Read treeline data ------------------------------------------------------

rel_altitude <- read.csv("relative_altitude_all_summits.csv")
# !!! Schafberg is in twice!!!
rel_altitude <- rel_altitude[-291,]




###################################################################################################################
###################################################################################################################


# Organise meta data in one table ----------------------------------------------

# get year for GLORIA records

# get GLORIA records in 1 table
date.glo <- date.glo[!duplicated(date.glo[,c(3,6)]), c(3,6,7)]
class(date.glo$sDATE)
library(gdata)
date.glo$year <- getYear(date.glo$sDATE)
meta.glo <- merge(date.glo, meta.glo, by="Summit")



# get gloria and summit flora meta in the same table
colnames(meta.glo)
colnames(meta)
colnames(meta.glo)[8] <- "mountain_name"
colnames(meta.glo)[4] <- "year_of_record"
colnames(meta.glo)[9] <- "mtn_altitude"
colnames(meta.glo)[2] <- "record_time_class"
meta.glo$zero_record_summit <- NA
colnames(meta.glo)[6] <- "region"
colnames(meta.glo)[14] <- "geology_main_bedrock"
meta.glo$summit_record_area <- 10
colnames(meta.glo)[11] <- "xcoord"
colnames(meta.glo)[13] <- "ycoord"

# add a column for dataset
meta.glo$dataset <- "GLORIA"
meta$dataset <- "SummitFlora"

rec.all <- rbind(meta.glo[,c(17,8,4,2,15,6,9,14,16,13,11)], meta[,c(32,1,3,4,5,6,8,9,16,29,30)])


# change regions for Gloria data
# class(rec.all$region)
rec.all$region <- as.character(rec.all$region)
# table(rec.all$region )
rec.all$region_2 <- ifelse(rec.all$region=="AT", "EastAlps", rec.all$region)
rec.all$region_2 <- ifelse(rec.all$region_2=="ES", "Pyrenees", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="RO", "Carpathians", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="UK", "Scotland", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="SK", "Tatra", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="SE", "NorthScandes", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="NO", "SouthScandes", rec.all$region_2)


# write.csv(rec.all, "test.data.csv")
# rec.all <- read.csv("test.data.csv")

# add rest after checking in map

# table(rec.all$region_2)
# look for the long limit between EastAlps and WestAlps
# max(rec.all[rec.all$region=="WestAlps",]$xcoord)  #9.47
# devide CH and IT according to this limit
rec.all$region_2 <- ifelse(rec.all$region_2=="CH" & rec.all$xcoord>9.47, "EastAlps", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="CH" & rec.all$xcoord<9.47, "WestAlps", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="IT" & rec.all$xcoord>9.47, "EastAlps", rec.all$region_2)
rec.all$region_2 <- ifelse(rec.all$region_2=="IT" & rec.all$xcoord<9.47, "WestAlps", rec.all$region_2)

# add potential treeline and relative altitude to the table
colnames(rel_altitude)[2] <- "mountain_name"
rec.all <- merge(rec.all, rel_altitude[,c(2,6,7)], by="mountain_name", all.x=T)
summary(rec.all)

# change time classes
extra <- as.character(unique(rec.all[rec.all$record_time_class=="3",]$mountain_name))
# table(rec.all$record_time_class)
rec.all$record_time_class <- ifelse(rec.all$mountain_name %in% extra & rec.all$record_time_class=="2", "intermediate", as.character(rec.all$record_time_class) )
rec.all$record_time_class <- ifelse(rec.all$record_time_class=="2" |rec.all$record_time_class=="3", "recent", as.character(rec.all$record_time_class) )
rec.all$record_time_class <- ifelse(rec.all$record_time_class=="1" , "historic", as.character(rec.all$record_time_class) )


# combine species data ----------------------------------------------------

# combine mountain_name and year from meta.glo
colnames(spec.glo)[3] <- "record_time_class"
spec.glo <- merge(spec.glo, meta.glo[,c(1,2,4,8)], by=c("Summit", "record_time_class"), all.x=T)

# change column names for GLORIA

summary(spec.glo)
# why nas
unique(spec.glo[is.na(spec.glo$mountain_name),]$Summit)
unique(meta.glo$Summit)

# test gloria species data from 3 observations, are they parallel?
# test <- droplevels(spec.glo[spec.glo[,18] %in% extra,])
# table(test$mountain_name, test$record_time_class)
# wait for manuela's answer?


# merge needed columns
class(spec.glo$record_time_class)
spec.glo$record_time_class <-as.character(spec.glo$record_time_class)
spec$record_time_class <-as.character(spec$record_time_class)

spec.all <- rbind(spec[,c(1,2,3,4)], spec.glo[,c(18,17,2,7)])
spec.all$record_time_class <- ifelse(spec.all$mountain_name %in% extra & spec.all$record_time_class=="2", "intermediate", as.character(spec.all$record_time_class) )
spec.all$record_time_class <- ifelse(spec.all$record_time_class=="2" |spec.all$record_time_class=="3", "recent", as.character(spec.all$record_time_class) )
spec.all$record_time_class <- ifelse(spec.all$record_time_class=="1" , "historic", as.character(spec.all$record_time_class) )
summary(spec.all)
summary(meta.glo)


# leave out strange GLORIA summits from both tables
spec.all <-spec.all[!is.na(spec.all$mountain_name),]
# test
# summary(spec.all)
# summary(rec.all)
# length(unique(spec.all$mountain_name))
# length(unique(rec.all$mountain_name))
# setdiff( unique(rec.all$mountain_name), unique(spec.all$mountain_name))
# difference are the summits with only zero records that are only in the record table


# Read trait data ---------------------------------------------------------

traits <- read.csv("com_traits_1may16.csv", sep=",", head=T, na.strings = c("","NA", "-", "n/a", " "))


# Add trait data to species data ------------------------------------------
# colnames(traits)

# choose the traits you want
colnames(traits)
spec.traits <- merge(spec.all, traits[,2:13], all.x=T, by="acceptedSpeciesCode")


#fix plant height for as many as possible 
spec.traits$PlantHeightNEW <- ifelse(!is.na(spec.traits$MAXHeight), as.numeric(spec.traits$MAXHeight), exp(as.numeric(spec.traits$PlantHeight))/100)
summary(spec.traits$PlantHeightNEW)

# Test region devision on map ----------------------------------------------------

# test if regions are devided right
# library(ggplot2)
# library(ggmap)
# library(maps)
# library(mapdata)


# map <- get_map(location = c(lon=9,lat=57), zoom = 4, maptype="satellite")

# ggmap(map) +
#   geom_point(aes(x = xcoord, y = ycoord,  col = region),cex = 5,data = rec.all)  #good enough for now

# not working for some strange reason

# map code from janet

# library(maps)
# library(mapdata)
# library(RgoogleMaps)


# MyMap <- map('worldHires', fill = T, col = "light grey") ###high res world map

# map('worldHires', ylim=c(100, 300))
# map('worldHires')


### Create a Googlemap with plot locations (lat, lon) #### 
# for alps tocheck if division between east and west makes sense
# bb = qbbox(rec.all[rec.all$region_2=="WestAlps" |rec.all$region_2=="EastAlps",]$ycoord,
           # rec.all[rec.all$region_2=="WestAlps" |rec.all$region_2=="EastAlps",]$xcoord) #lats then longs
# print(bb)


# MyMap = GetMap.bbox(bb$lonR, bb$latR, destfile = "world.png", maptype = "satellite", SCALE = 2, zoom = 6)
# MyMap

# tmp = PlotOnStaticMap(MyMap,lat = rec.all$ycoord, lon = rec.all$xcoord,  cex=1.8, pch=21, col=as.factor(rec.all$region_2), add=FALSE)
# works :)


# Add Temperature data to record data -------------------------------------
# Read temperature data ---------------------------------------------------

temp <- read.csv(unzip("sUMMITDiv_climate_data2.zip"), head=T, sep=",")

# remove na rows
temp <- temp[!is.na(temp$mean_tmp_dwsc),]
# subset for summer temperatures
temp_sum <-temp[temp$period==15,]
# temp_sum2 <-temp[temp$period>5 & temp$period<9,]


# write new table with summer temperatures
# write.csv(temp_sum, "temp.sum.csv")

# get mean and n of different datasets for each year

library(plyr)
temp_sum$year <- as.numeric(temp_sum$year)
# temp_sum2$year <- as.numeric(temp_sum2$year)


simple<-ddply(temp_sum, c( "mountain_name","year"), 
              function(df) 
                return(c(mean=mean(df$mean_tmp_dwsc), n=length(df$mean_tmp_dwsc))))
# simple2<-ddply(temp_sum2, c( "mountain_name","year"), 
#               function(df) 
#                 return(c(mean=mean(df$mean_tmp_dwsc), n=length(df$mean_tmp_dwsc))))
summary(simple)
# test
# temp[temp$mountain_name=="Agjek" & temp$year_of_record==2003 & temp$period==15,]
# works

# merge dataset means with year from rec.all

length(unique(temp_sum$mountain_name))
length(unique(rec.all$mountain_name))
colnames(rec.all)
simple2 <- merge(simple, rec.all[,c(1,3)], by="mountain_name")
simple2$year_of_record <- as.numeric(simple2$year_of_record)

sum_mean<-ddply(simple2, c( "mountain_name","year_of_record"), 
              function(df) 
                return(mean(df[df$year < df$year_of_record & 
                                 df$year > (df$year_of_record-11),]$mean
                            )))

# merge to rec.all
colnames(sum_mean)[3] <- "meanSumT"
rec.all <- merge(rec.all, sum_mean, by=c("mountain_name","year_of_record"))

# calculate change in temperature means historic-recent in rec.all
library(reshape2)
rec.all.sub <- rec.all[rec.all$record_time_class=="recent" | rec.all$record_time_class=="historic",]
Delta.T <-dcast(rec.all.sub, mountain_name ~ record_time_class, value.var="meanSumT")
Delta.T$deltaT <- Delta.T$recent - Delta.T$historic

rec.all <-merge(rec.all, Delta.T, by="mountain_name", all.x=T)
colnames(rec.all)
colnames(rec.all)[16] <- "historicT"
colnames(rec.all)[17] <- "recentT"


# Add precipitation data to the rec.all -----------------------------------

prec <- read.csv("summits_downsc_prec_new.csv", sep=",")
levels(prec$clim_source)
str(prec)
# change mountain_names
prec <-merge(temp[!duplicated(temp$mountain_id),c(2,10)],prec , by="mountain_id", all=F)
prec <- prec[,-3]
colnames(prec)[2] <- "mountain_name"
prec <- prec[!is.na(prec$mean_pre_dwsc),]  #no NAs!


library(plyr)
prec.ann <- ddply(prec[prec$period<13,], c( "mountain_name","year"), 
                function(df) 
                  return(sum(df$mean_pre_dwsc)))
# write new table with annual prec sums
# write.csv(prec.ann, "prec.ann.csv")

prec.ann2 <- merge(prec.ann, rec.all[,c(1,2)], by="mountain_name")
prec.ann2$year_of_record <- as.numeric(prec.ann2$year_of_record)

prec.ann2old <- prec.ann2[prec.ann2$year_of_record<1906,]
prec.ann2new <- prec.ann2[prec.ann2$year_of_record>1905,]

ann_mean.new<-ddply(prec.ann2new, c( "mountain_name","year_of_record"), 
                function(df) 
                  return(mean(df[df$year < df$year_of_record & 
                                   df$year > (df$year_of_record-6),]$V1
                  )))
ann_mean.old<-ddply(prec.ann2old, c( "mountain_name","year_of_record"), 
                    function(df) 
                      return(mean(df[df$year>1899 & df$year<1906,]$V1
                      )))
ann.means <-rbind(ann_mean.old,ann_mean.new)


# winter precipitation
prec_wint <-prec[prec$period==13,]
# should be changed to more accurate months, now jan-mar

prec.wint <- ddply(prec_wint, c( "mountain_name","year"), 
                  function(df) 
                    return(mean(df$mean_pre_dwsc)))

prec.wint2 <- merge(prec.wint, rec.all[,c(1,2)], by="mountain_name")
prec.wint2$year_of_record <- as.numeric(prec.wint2$year_of_record)

prec.wint2old <- prec.wint2[prec.wint2$year_of_record<1906,]
prec.wint2new <- prec.wint2[prec.wint2$year_of_record>1905,]

prec.wint.new<-ddply(prec.wint2new, c( "mountain_name","year_of_record"), 
                    function(df) 
                      return(mean(df[df$year < df$year_of_record & 
                                       df$year > (df$year_of_record-6),]$V1
                      )))
prec.wint.old<-ddply(prec.wint2old, c( "mountain_name","year_of_record"), 
                    function(df) 
                      return(mean(df[df$year>1899 & df$year<1906,]$V1
                      )))
prec.wint <-rbind(prec.wint.old,prec.wint.new)

# merge to rec.all

colnames(prec.wint)[3] <- "meanWinPrec"

rec.all <- merge(rec.all, ann.means, by=c("mountain_name","year_of_record"))
rec.all <- merge(rec.all, prec.wint, by=c("mountain_name","year_of_record"))
colnames(rec.all)[19] <- "meanAnnPrec"
# calculate change in precipitation means historic-recent in rec.all
library(reshape2)
rec.all.sub <- rec.all[rec.all$record_time_class=="recent" | rec.all$record_time_class=="historic",]
Delta.P <-dcast(rec.all.sub, mountain_name ~ record_time_class, value.var="meanAnnPrec")
Delta.P$deltaP <- Delta.P$recent - Delta.P$historic

rec.all <-merge(rec.all, Delta.P, by="mountain_name", all.x=T)

Delta.P.wint <-dcast(rec.all.sub, mountain_name ~ record_time_class, value.var="meanWinPrec")
Delta.P.wint$deltaP.wint <- Delta.P.wint$recent - Delta.P.wint$historic
rec.all <-merge(rec.all, Delta.P.wint, by="mountain_name", all.x=T)

colnames(rec.all)[21]<- "historicAnnP"
colnames(rec.all)[22] <- "recentAnnP"

colnames(rec.all)[24]<- "historicWintP"
colnames(rec.all)[25] <- "recentWintP"



# Write tables ------------------------------------------------------------

write.csv(rec.all, "rec.all_1may16.csv")
write.csv(spec.traits, "spec.traits_1may16.csv")
