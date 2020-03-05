library(ggplot2)
library(tidyverse)
Region_order <- c("Alaska","British Columbia","Washington","Oregon","Bodega Bay","San Diego")
Region_order2 <- c("AK","BC","WA","OR","BB","SD")

## read in and organize most recent EELISA output.
output <- read.csv("NSF_Log_march.csv")

## Drop extra blade in SD.C.3.U.9_16 and re-number blades correctly
output <- output[-which(output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==8),]
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==0] <- 9
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==1] <- 10
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==2] <- 11
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==3] <- 12
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==4] <- 13
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==5] <- 14
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==6] <- 15
output$blade_num[output$file_name=="SD.C.3.U.9_16.jpg" & output$blade_num==7] <- 16
output$correct_num[output$file_name=="SD.C.3.U.9_16.jpg"] <- "TRUE"

## Fix blade_num for BB.A.5.L.7_11
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==0] <- 7
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==1] <- 8
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==2] <- 9
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==3] <- 10
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==4] <- 12
output$blade_num[output$file_name=="BB.A.5.L.7_11.jpg" & output$blade_num==5] <- 11
output$correct_num[output$file_name=="BB.A.5.L.7_11.jpg"] <- "TRUE"

## Fix extra AK blades - renumber as 21-25
output$blade_num[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==6] <- 21
output$blade_num[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==7] <- 22
output$blade_num[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==8] <- 23
output$blade_num[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==9] <- 24
output$blade_num[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==10] <- 25

output$SampleID[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==21] <- "AK.C.4.L.21"
output$SampleID[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==22] <- "AK.C.4.L.22"
output$SampleID[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==23] <- "AK.C.4.L.23"
output$SampleID[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==24] <- "AK.C.4.L.24"
output$SampleID[output$file_name=="AK.C.4.L.6_10.jpg" & output$blade_num==25] <- "AK.C.4.L.25"

## Fix blade number of one mis-labled BC blade
output$blade_num[output$file_name=="BC.B.5.L.16_17.jpg" & output$blade_num==17] <- 20
## Fix BB.F.2.U.8 - split between two scans
output$healthy_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8] <- sum(output$healthy_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8],
                                                                                       output$healthy_area[output$file_name=="BB.F.2.U.8.jpg"])
output$lesion_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8] <- sum(output$lesion_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8],
                                                                                       output$lesion_area[output$file_name=="BB.F.2.U.8.jpg"])
output$severity[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8] <- 100*output$lesion_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8]/output$healthy_area[output$file_name=="BB.F.2.U.1_8.jpg" & output$blade_num==8]
output <- output[-c(which(output$file_name=="BB.F.2.U.8.jpg")),]

## Construct SampleID from file_name and blade_num
output$location <- ordered(output$location,levels=Region_order)
output <- separate(output,col=file_name,c("Region","SiteCode","Transect","TidalHeight",NA,NA),sep="[.]",remove = FALSE)
output <- unite(output,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","blade_num"),sep=".",remove=FALSE)
output$Region <- ordered(output$Region,levels=Region_order2)

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

# combine blades and EElISA output
# once we get the splits right, this will be the full data set
disease <- full_join(blades,output,by=c("SampleID","Region","SiteCode","TidalHeight","Transect"))
disease <- subset(disease,correct_num=="TRUE" | is.na(correct_num)) # all entries have correct blade numbers
which(duplicated(disease$SampleID,fromLast = TRUE)) # no duplicates in final data
# calculate blade areas from EELISA output for samples that were not measured by hand
disease$BladeArea.cm2 <- ifelse(is.na(disease$BladeArea.cm2),(disease$lesion_area+disease$healthy_area)/100,disease$BladeArea.cm2)
# calculate EELISA blade area and compare to hand measured
disease$BladeArea.cm2.E <- (disease$lesion_area+disease$healthy_area)/100
disease$BladeArea.ratio <- disease$BladeArea.cm2/disease$BladeArea.cm2.E
# assign final blade area as EELISA estimate when the ratio is less than 1 (EELISA estimate is larger),  use EELISA estimate.
# Otherwise, leave as before (hand-measured for some, EELISA for others)
disease$BladeArea.cm2.Final <- ifelse(disease$BladeArea.ratio<1,disease$BladeArea.cm2.E,disease$BladeArea.cm2)
disease$BladeArea.cm2.Final <- ifelse(is.na(disease$BladeArea.cm2.Final),disease$BladeArea.cm2,disease$BladeArea.cm2.Final)
# Create data frame of missing samples (no scan, no hand measured data)
no_sample <- tibble(SampleID=c("BB.A.1.U.14","BB.A.1.U.15","BB.D.3.U.15",
                               "AK.D.6.L.12","AK.D.6.L.13","AK.D.6.L.14",
                               "AK.D.6.L.15","AK.E.5.L.15","BC.E.3.U.15",
                               "OR.A.1.U.15","OR.A.3.U.15","OR.A.4.L.15",
                               "OR.E.6.L.15","SD.D.2.U.9","SD.D.4.L.1",
                               "WA.C.4.L.20","WA.D.1.U.7","WA.D.1.U.8",
                               "WA.D.1.U.9","WA.D.1.U.10","WA.D.1.U.11",
                               "WA.D.1.U.12","WA.D.3.U.15","WA.D.4.L.15"))
# Remove missing samples from disease data set
disease <- anti_join(disease,no_sample,by="SampleID")
# For now, drop samples that have no BladeArea (not measured by hand and no EELISA output)
disease_most <- subset(disease,BladeArea.cm2!="NA") # in final dataset, there are no missing blade areas
disease_not <- anti_join(disease,disease_most,by="SampleID") # this join is empty! So use the full disease dataset
# assign  prevalence = 0 for blades that were measured by hand and not scanned 
disease$prevalence <- ifelse(is.na(disease$severity),0,disease$prevalence)
# assign lesion area = 0 for blades that were measured by hand and not scanned
disease$lesion_area <- ifelse(is.na(disease$lesion_area)&disease$prevalence==0,0,disease$lesion_area)
# re-calculate severity as lesion area / blade area (severities from EELISA are not accurate for blades for which only a fragment was scanned)
disease$severity2 <- ifelse(is.na(disease$severity)&disease$prevalence==0,0,disease$lesion_area/(disease$BladeArea.cm2.Final*100))

#disease$severity_per <- disease$severity2/100
disease_summ <- disease%>% 
  group_by(Region,SiteCode,TidalHeight,Transect)%>%
  summarize(prev=mean(prevalence),sev=mean(severity2,na.rm = TRUE),
            lesion=mean(lesion_area,na.rm=TRUE),count=length(blade_num),
            percent=count/20,BladeArea.cm2=mean(BladeArea.cm2.Final))

write.csv(disease_summ,"Measured_Disease_summ.csv",row.names = FALSE)

disease_summ2 <- disease%>%
  group_by(Region,TidalHeight)%>%
  summarize(prevM=mean(prevalence),sevM=mean(severity2,na.rm = TRUE),
            lesion=mean(lesion_area,na.rm=TRUE),count=length(blade_num),
            prevSD=sd(prevalence),prevSE=prevSD/sqrt(length(prevalence)),
            sevSD=sd(severity2,na.rm=TRUE),sevSE=sevSD/sqrt(length(severity2)))

write.csv(disease_summ2,"Measured_Disease_summ2.csv",row.names=FALSE)

disease_summ3 <- disease%>%
  group_by(Region)%>%
  summarize(prevM=mean(prevalence),sevM=mean(severity2,na.rm = TRUE),
            lesion=mean(lesion_area,na.rm=TRUE),count=length(blade_num),
            prevSD=sd(prevalence),prevSE=prevSD/sqrt(length(prevalence)),
            sevSD=sd(severity2,na.rm=TRUE),sevSE=sevSD/sqrt(length(severity2)))

write.csv(disease_summ3,"Measured_Disease_summ3.csv",row.names=FALSE)

ggplot(disease,aes(x=SiteCode,y=BladeArea.cm2.Final))+geom_boxplot()+
  facet_wrap(~Region,scales="free")
ggplot(disease_most,aes(x=SiteCode,y=severity2))+geom_boxplot()+
  facet_wrap(~Region,scales="fixed")
ggplot(disease_most,aes(x=Region,y=severity2))+geom_boxplot()
  #facet_wrap(~Region,scales="fixed")