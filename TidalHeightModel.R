# Modeling severity and prevalence as functions of tidal height and site

library(tidyverse)
library(nlme)
library(ggplot2)
Region_order <- c("Alaska","British Columbia","Washington","Oregon","Bodega Bay","San Diego")
Region_order2 <- c("AK","BC","WA","OR","BB","SD")

summ <- read.csv("Disease_Summ_02-24-20.csv")
summ$Region <- ordered(summ$Region,levels=Region_order2)

ggplot(data=summ,aes(x=SiteCode,y=prev,color=TidalHeight))+
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

ggplot(data=summ,aes(x=SiteCode,y=sev,color=TidalHeight))+
  geom_point(position=position_dodge(width=0.75))+
  facet_wrap(~Region)+
  stat_summary(fun.y=mean,geom="point",shape=8,size=2,color="black",
               show.legend=FALSE,position=position_dodge(width=0.75))+
  scale_y_continuous(limits=c(0,0.5),breaks=c(0,0.1,0.2,0.3,0.4,0.5))+
  scale_color_manual(values=c("royal blue","light blue"))+
  xlab("Site Code")+
  ylab("Wasting Disease Severity")+
  labs(title="Wasting Dieseae Severity",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))


class(summ$TidalHeight)
 M1.gls <- gls(sev~TidalHeight*Region,data=summ)
summary(M1.gls) 
hist(M1.gls$residuals)
qqnorm(M1.gls$residuals)
qqline(M1.gls$residuals)

M1.lme <- lme(sev~BladeArea.cm2*TidalHeight*Region,data=summ,
              random=~1|Region/SiteCode)
summary(M1.lme)
E.1 <- resid(M1.lme,type="normalized")
F.1 <- fitted(M1.lme)
plot(E.1~F.1)
plot(E.1~summ$Region)
plot(E.1~summ$TidalHeight)
M2.lme <- lme(sev~BladeArea.cm2*TidalHeight*Region,data=summ,
              random=~1|Region/SiteCode,
              weights=varIdent(form=~1|Region))
summary(M2.lme)
AIC(M1.lme,M2.lme)
E.2 <- resid(M2.lme,type="normalized")
F.2 <- fitted(M2.lme)
plot(E.2~F.2)
plot(E.2~summ$Region)
plot(E.2~summ$TidalHeight)
plot(E.2~summ$BladeArea.cm2)
hist(E.2)
M3.lme <- lme(sev~BladeArea.cm2*TidalHeight*Region,data=summ,
              random=~1|Region/SiteCode,
              weights=varExp(form=~BladeArea.cm2))
summary(M3.lme)
AIC(M2.lme,M3.lme)
E.3 <- resid(M3.lme,type="normalized")
F.3 <- fitted(M3.lme)
plot(E.3~F.3)
hist(E.3)

M4.lme <- lme(sev~BladeArea.cm2*TidalHeight*Region,data=summ,
                        random=~1|Region/SiteCode,
                        weights=varComb(varIdent(form=~1|Region),
                                        varPower(form=~BladeArea.cm2)))
summary(M4.lme)
AIC(M4.lme,M2.lme)
E.4 <- resid(M4.lme,type="normalized")
F.4 <- fitted(M4.lme)
plot(E.4~F.4) # these look a bit better but still higher values at higher fitted
hist(E.4) # residuals look better and better though
plot(E.4~summ$Region)
plot(E.4~summ$TidalHeight)
plot(E.4~summ$BladeArea.cm2)

M4.full <- lme(sev~BladeArea.cm2+TidalHeight+Region+BladeArea.cm2:TidalHeight+
                 BladeArea.cm2:Region+TidalHeight:Region+BladeArea.cm2:TidalHeight:Region,data=summ,
               random=~1|Region/SiteCode,
               weights=varComb(varIdent(form=~1|Region),
                               varPower(form=~BladeArea.cm2)),
               method="ML")
M4.drop1 <- lme(sev~BladeArea.cm2+TidalHeight+Region+BladeArea.cm2:TidalHeight+
                  BladeArea.cm2:Region+TidalHeight:Region,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M4.full,M4.drop1)
## 3-way interaction is significant
M4.drop2 <- lme(sev~BladeArea.cm2+TidalHeight+Region+BladeArea.cm2:TidalHeight+
                  BladeArea.cm2:Region+BladeArea.cm2:TidalHeight:Region,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M4.full,M4.drop2) # TidalHeight:Region is marginally significant
M4.drop3 <- lme(sev~BladeArea.cm2+TidalHeight+Region+BladeArea.cm2:TidalHeight+
                  BladeArea.cm2:TidalHeight:Region,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M4.full,M4.drop3) # BladeArea:Region is not significant
M4.drop4 <- lme(sev~BladeArea.cm2+TidalHeight+Region+BladeArea.cm2:TidalHeight:Region,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
summary(M4.drop4)

## should only have Region in the random section
M5.lme <- lme(sev~BladeArea.cm2*TidalHeight,data=summ,
              random=~1|Region/SiteCode,
              weights=varComb(varIdent(form=~1|Region),
                              varPower(form=~BladeArea.cm2)))

summary(M5.lme)
AIC(M5.lme)
E.5 <- resid(M5.lme,type = "normalized")
F.5 <- fitted(M5.lme)
plot(E.5~F.5)
plot(E.5~summ$Region)
plot(E.5~summ$TidalHeight)
plot(E.5~summ$BladeArea.cm2)
hist(E.5)

M6.lme <- lme(sev~BladeArea.cm2*TidalHeight,data=summ,
              random=~1|Region/SiteCode,
              weights=varIdent(form=~1|Region))
AIC(M5.lme,M6.lme)
# so better with the two variance structures - use M5.lme

M5.full <- lme(sev~BladeArea.cm2+TidalHeight+BladeArea.cm2:TidalHeight,data=summ,
               random=~1|Region/SiteCode,
               weights=varComb(varIdent(form=~1|Region),
                               varPower(form=~BladeArea.cm2)),
               method="ML")
M5.drop1 <- lme(sev~BladeArea.cm2+TidalHeight,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M5.full,M5.drop1)
# interaction is not significant - note that blade length may vary with tidal height?

M5.drop2 <- lme(sev~BladeArea.cm2,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M5.drop1,M5.drop2)
# Tidal height is not significant

M5.drop3 <- lme(sev~1,data=summ,
                random=~1|Region/SiteCode,
                weights=varComb(varIdent(form=~1|Region),
                                varPower(form=~BladeArea.cm2)),
                method="ML")
anova(M5.drop2,M5.drop3)
## BladeArea is highly significant - but I need to see if this varies by region

hist(M5.drop3$residuals)
summary(M5.drop3)
