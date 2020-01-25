## Updated DiseaseDataExploration
## NSF data set as of 01-24-2019

library(tidyverse)
library(ggplot2)
Region_order <- c("Alaska","British Columbia","Washington","Oregon","Bodega Bay","San Diego")
Region_order2 <- c("AK","BC","WA","OR","BB","SD")

## read in and organize 2 output files from EELISA. First is from Dec run, second is from Jan run.
outputOld <- read.csv("NSF_Log_2019_12_16.csv")
outputOld$location <- ordered(outputOld$location,levels=site_order)
outputOld <- separate(outputOld,col=file_name,c("Region","SiteCode","Transect","TidalHeight",NA,NA),sep="[.]",remove = FALSE)
outputOld <- unite(outputOld,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","blade_num"),sep=".",remove=FALSE)
outputOld$Region <- ordered(outputOld$Region,levels=site_order2)

output <- read.csv("NSF_Log_3_01_24_2019.csv")
output$location <- ordered(output$location,levels=Region_order)
output <- separate(output,col=file_name,c("Region","SiteCode","Transect","TidalHeight",NA,NA),sep="[.]",remove = FALSE)
output <- unite(output,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","blade_num"),sep=".",remove=FALSE)
output$Region <- ordered(output$Region,levels=Region_order2)

#Jan run has fewer correct splits than Dec but some are different, so find the SampleIDs that are different
output_new <- anti_join(output,outputOld,
                        by=c("location","year","site","file_name","SampleID",
                             "Region","SiteCode","Transect","TidalHeight"))
#put the two EELISA runs together
output_both <- rbind(outputOld,output_new)
#identify all unique SampleIDs that were scanned - these ones are the ones that we DON'T want blade area
scanned_ID <- data.frame("SampleID"=unique(output_both$SampleID),"Scanned"="Y")
scanned_ID <- separate(scanned_ID,col=SampleID,c("Region","SiteCode","Transect","TidalHeight","blade_num"),sep="[.]",remove=FALSE)

#subset only EELISA scans split properly
output_true <- subset(output,correct_num=="TRUE")
which(duplicated(output_true$SampleID))
which(duplicated(output_true$SampleID,fromLast = TRUE))
output_true[c(379,380,381,1241,369,370,371,1233),]
# rows 1241 and 1233 are both BB.F.2.U.8 - need to add them together? deal with this later, unless the duplicates are an issue

## read in and organize DiseaseBlade samples (1-15 per transect, except 1-16 for SD)
blades <- read.csv("DiseaseBlades.csv") 
blades$SampleID <- gsub("U","U_",blades$SampleID)
blades$SampleID <- gsub("L","L_",blades$SampleID)
blades <- separate(blades,col=SampleID,c("SampleDate","Region","SiteCode","TidalHeight","Transect","Blade"),sep="_")
blades <- unite(blades,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","Blade"),sep=".",remove=FALSE)
blades$Region <- ordered(blades$Region,levels=Region_order2)

## read in and organize the Epiphyte Blades samples (16-20 per transect)
epi_blades <- read.csv("ShootMetrics2.csv") # removed the SD measurements because those blades were never scanned
epi_blades <- epi_blades[,c(3,15)]
epi_blades$SampleID <- gsub("U","U_",epi_blades$SampleID)
epi_blades$SampleID <- gsub("L","L_",epi_blades$SampleID)
epi_blades$SampleID <- gsub("_m4","_16",epi_blades$SampleID)
epi_blades$SampleID <- gsub("_m8","_17",epi_blades$SampleID)
epi_blades$SampleID <- gsub("_m12","_18",epi_blades$SampleID)
epi_blades$SampleID <- gsub("_m16","_19",epi_blades$SampleID)
epi_blades$SampleID <- gsub("_m20","_20",epi_blades$SampleID)

epi_blades <- separate(epi_blades,col=SampleID,c("SampleDate","Region","SiteCode","TidalHeight","Transect","Blade"),sep="_")
epi_blades <- unite(epi_blades,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","Blade"),sep=".",remove=FALSE)
epi_blades$Region <- ordered(epi_blades$Region,levels=Region_order2)

## combine Epiphyte and Disease Blades
blades <- rbind(blades,epi_blades)
# add
blades <- left_join(blades,scanned_ID,by="SampleID")
blades$Scanned <- ifelse(is.na(blades$Scanned),"N","Y")

# combine blades and EElISA output
# once we get the splits right, this will be the full data set
disease <- left_join(blades,output_both_true,by=c("SampleID","Region","SiteCode","TidalHeight","Transect"))
disease <- subset(disease,(Scanned=="Y" & correct_num=="TRUE") | (Scanned=="N" & BladeArea.cm2!="NA")) # techinically redundant since we start with the true output
which(duplicated(disease$SampleID,fromLast = TRUE)) # same 4 as are mentioned above
disease <- disease[-which(duplicated(disease$SampleID)),] #drop the 4 duplicates - known problems, will figure out somethign later
# calculate blade areas from EELISA output for samples that were not measured by hand
disease$BladeArea.cm2 <- ifelse(is.na(disease$BladeArea.cm2) | disease$BladeArea.cm2=="This cell will autocalculate",disease$lesion_area+disease$healthy_area/100,disease$BladeArea.cm2)
# For now, drop samples that have no BladeArea (not measured by hand and no EELISA output)
disease_most <- subset(disease,BladeArea.cm2!="NA") #This is redundant with earlier subset, but leave it in for now
# Remaining samples should have EELISA output and scanned = Y OR scanned = NA, therefore prevelance and severity are 0 (for hand-measured)
disease_most$prevelance <- ifelse(is.na(disease_most$severity),0,disease_most$prevelance)
disease_most$severity <- ifelse(is.na(disease_most$severity)&disease_most$prevelance==0,0,disease_most$severity)
disease_most$lesion_area <- ifelse(is.na(disease_most$lesion_area)&disease_most$prevelance==0,0,disease_most$lesion_area)
disease_most$severity_per <- disease_most$severity/100
disease_most_summ <- disease_most%>%
  group_by(Region,SiteCode,TidalHeight,Transect)%>%
  summarize(prev=mean(prevelance),sev=mean(severity_per,na.rm = TRUE),
            lesion=mean(lesion_area,na.rm=TRUE),count=length(blade_num),
            percent=count/20)
disease_most_summ2 <- disease_most%>%
  group_by(Region,TidalHeight)%>%
  summarize(prev=mean(prevelance),sev=mean(severity_per,na.rm = TRUE),
            lesion=mean(lesion_area,na.rm=TRUE),count=length(unique(SampleID)))
disease_most_summ2$total <- ifelse(disease_most_summ2$Region=="AK" | disease_most_summ2$Region=="BB",360,
                                   ifelse(disease_most_summ2$Region=="SD",240,300))
disease_most_summ2$percent <- disease_most_summ2$count/disease_most_summ2$total
write.csv(disease_most,"Measured_Disease.csv",row.names=FALSE)
write.csv(disease_most_summ,"Measured_Disease_summ.csv",row.names=FALSE)
write.csv(disease_most_summ2,"Measured_Disease_summ2.csv",row.names=FALSE)

