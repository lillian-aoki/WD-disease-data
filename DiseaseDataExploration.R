library(tidyverse)
library(ggplot2)
site_order <- c("Alaska","British Columbia","Washington","Oregon","Bodega Bay","San Diego")
site_order2 <- c("AK","BC","WA","OR","BB","SD")

## read in and organize output from EELISA
output <- read.csv("NSF_Log_2019_12_16.csv")
output$location <- ordered(output$location,levels=site_order)
output <- separate(output,col=file_name,c("Region","SiteCode","Transect","TidalHeight",NA,NA),sep="[.]",remove = FALSE)
output <- unite(output,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","blade_num"),sep=".",remove=FALSE)
output$Region <- ordered(output$Region,levels=site_order2)

#subset only EELISA scans split properly
output_true <- subset(output,correct_num=="TRUE")
which(duplicated(output_true$SampleID))
which(duplicated(output_true$SampleID,fromLast = TRUE))
output_true[c(1413,1414,1415,1467,1468,1469),]
#The duplicate SampleIDs come from the file "AK.C.4.L.6_10.tiff" which overlaps.
#Only 3 duplicates for the "true" dataset right now - some of the others were not split evenly.
#We want to keep the later entries (1467,1468,1469) because those are the "correct" numbers (match the surrounidng scans)
#Decide whether to keep the extra 6_10 blades as 21_25?
output_true <- output_true[-c(1413,1414,1415),]

## read in and organize DiseaseBlade samples (1-15 per transect)
blades <- read.csv("DiseaseBlades.csv")
blades$SampleID <- gsub("U","U_",blades$SampleID)
blades$SampleID <- gsub("L","L_",blades$SampleID)
blades <- separate(blades,col=SampleID,c("SampleDate","Region","SiteCode","TidalHeight","Transect","Blade"),sep="_")
blades <- unite(blades,col="SampleID",c("Region","SiteCode","Transect","TidalHeight","Blade"),sep=".",remove=FALSE)
blades$Region <- ordered(blades$Region,levels=site_order2)

## read in and organize the Epiphyte Blades samples (16-20 per transect)
epi_blades <- read.csv("ShootMetrics.csv")
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
epi_blades$Region <- ordered(epi_blades$Region,levels=site_order2)

## combine Epiphyte and Disease Blades
blades <- rbind(blades,epi_blades)

# combine blades and EElISA output
# once we get the splits right, this will be the full data set
disease <- left_join(blades,output,by=c("SampleID","Region","SiteCode","TidalHeight","Transect"))
disease <- subset(disease,correct_num=="TRUE" | is.na(correct_num))
which(duplicated(disease$SampleID,fromLast = TRUE))
disease[which(duplicated(disease$SampleID)),]
disease[c(209,211,213,2484,2485,2486,2487,2488,2994,2995,2996,2997,2998),]
# calculate blade areas from EELISA output for samples that were not measured by hand
disease$BladeArea.cm2 <- ifelse(is.na(disease$BladeArea.cm2),disease$lesion_area+disease$healthy_area/100,disease$BladeArea.cm2)
# For now, drop samples that have no BladeArea (not measured by hand and no EELISA output)
disease_most <- subset(disease,BladeArea.cm2!="NA")
# Remaining samples should have EELISA output OR prevelance and severity are 0 (for hand-measured)
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
            lesion=mean(lesion_area,na.rm=TRUE),count=length(blade_num))

#Transect level prevalence
ggplot(data=disease_most_summ,aes(x=Region,y=prev,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2,position=position_dodge(width=0.75),
               show.legend = FALSE)+
  geom_text(data=disease_most_summ2,aes(x=Region,y=1.1,label=count),position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,1.12),breaks=c(0,0.25,0.5,0.75,1.0))+
  scale_fill_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Prevalence")+
  xlab("Region")+
  labs(title="Wasting Disease Prevalence",
       subtitle = "Transect-level means, n=15-18 transects per region")+
  theme_bw()+
  theme()

#Transect level severity
ggplot(data=disease_most_summ,aes(x=Region,y=sev,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2,show.legend=FALSE,
               position=position_dodge(width=0.75))+
  geom_text(data=disease_most_summ2,aes(x=Region,y=0.52,label=count),position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,0.52),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  scale_fill_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Severity")+
<<<<<<< HEAD
  labs(title="Wasting Disease Severity",
=======
  labs(title="Wasting Disease Prevalence",
>>>>>>> f098f627a1722514532089139650e2ad4410778e
       subtitle = "Transect-level means, n=15-18 transects per region")+
  theme_bw()+
  theme()

#Transect level prevalence, broken out by site
ggplot(data=disease_most_summ,aes(x=SiteCode,y=prev,color=TidalHeight))+
  geom_point(position=position_dodge(width=0.75))+
  facet_wrap(~Region)+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2,color="black",
               show.legend=FALSE,position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.25,0.5,0.75,1.0))+
  scale_color_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Prevalence")+
  labs(title="Wasting Disease Prevalence",
       subtitle="Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

#Transect level severity, broken out by site
ggplot(data=disease_most_summ,aes(x=SiteCode,y=sev,color=TidalHeight))+
  geom_point(position=position_dodge(width=0.75))+
  facet_wrap(~Region)+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2,color="black",
               show.legend=FALSE,position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  scale_color_manual(values=c("royal blue","light blue"))+
  xlab("Site Code")+
  ylab("Wasting Disease Prevalence")+
  labs(title="Wasting Dieseae Severity",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(data=disease_most,aes(x=Region,y=prevelance,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean,geom="point",shape=4,size=4,position=position_dodge(width=0.75))+
  geom_text(data=disease_most_summ2,aes(x=Region,y=1.1,label=count),position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,1.12),breaks=c(0,0.25,0.5,0.75,1.0))+
  scale_fill_manual(values=c("royal blue","light blue"))+
  ylab("Wasting Disease Prevalence")+
  theme_bw()+
  theme()
  
ggplot(disease_most,aes(x=Region,y=prevelance))+geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))

# # for now, look at only the successful splits
# disease2 <- inner_join(blades,output_true,by="SampleID")
# disease2$BladeArea.cm2 <- ifelse(disease2$BladeArea.cm2=="NA",disease2$lesion_area+disease2$healthy_area/100,disease2$BladeArea.cm2)
# # need to double-check that the healthy and lesion areas are in mm2?
# # note that Eelisa areas are less than the length x width areas
# disease2$prevelance <- ifelse(disease2$severity=="NA","0",disease2$prevelance)
# hmm <-anti_join(output_true,blades,by="SampleID")
# disease2_summ <- disease2%>%
#   group_by(location,site_code,tidal_height)%>%
#   summarize(prev=mean(prevelance),sev=mean(severity),lesion=mean(lesion_area),count=length(blade_num))


ggplot(disease,aes(x=Region,y=prevelance))+geom_jitter()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))

ggplot(output_summ,aes(x=location,y=sev))+geom_histogram()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))

sev_dist <- ggplot(output,aes(x=location,y=severity,fill=tidal_height))+geom_boxplot()+
  scale_fill_manual(values=c("royal blue","light blue"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))
sev_dist
ggsave(plot=sev_dist,filename = "Severity_Dist.jpg")

sev_dist2 <- ggplot(output,aes(x=site_code,y=severity,fill=tidal_height))+geom_boxplot()+
  facet_wrap(~location)+
  scale_fill_manual(values=c("royal blue","light blue"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))
sev_dist2

levels(output$location)
ggsave(plot=sev_dist2,filename = "Severity_Dist2.jpg")

ggplot(disease,aes(x=prevelance,fill=Region))+geom_histogram()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90))
