library(ggplot2)
library(xlsx)
library(ggpmisc)
library(ggspectra)
library(dplyr)
library(plyr)
library(Rcpp)
library(nlme)
library(lme4)
library(gmodels)
library(plotrix)
library(nlme)
library(effects)

setwd(work_dir)
setwd("//atkk/home/b/brelsfor/Documents/CombinedLammiYears/")

t.data<- read.csv("LammiPigmentsBothYears.csv")

t.data$Plot <- factor(t.data$Plot)
t.data$Flav<- as.numeric(t.data$Flav)
t.data$Chl<- as.numeric(t.data$Chl)
t.data$Anth<- as.numeric(t.data$Anth)

str(t.data)

View(t.data)
names(t.data)

levels(t.data$Treatment)
levels(t.data$Species)

summary.data<-ddply(t.data,.(Species, Treatment, side, Week, StandType, Stand, REALPLOT, YEAR),
                    summarise, 
                    
                    Flav.sd=std.error(Flav,na.rm=TRUE),
                    Flav.mean=mean(Flav,na.rm=TRUE),
                    Anth.sd=std.error(Anth,na.rm=TRUE),
                    Anth.mean=mean(Anth,na.rm=TRUE),
                    Chl.sd=std.error(Chl,na.rm=TRUE),
                    Chl.mean=mean(Chl,na.rm=TRUE))

summary.data <- subset (summary.data, side=="sup")

summary.data$Week<- as.numeric(summary.data$Week)
summary.data$Date<-strptime(as.character(summary.data$Date), "%d/%m/%Y")

str(summary.data)

levels(summary.data$REALPLOT)

levels(summary.data$Stand)

levels(summary.data$Treatment)

summary.data <- subset(summary.data, Treatment!="Ambient")

summary.data<-subset(summary.data, Species!="Lab Transplant")

levels(summary.data$Species)

summary.data <- subset (summary.data, Species!="Fuchsia lycioides")
summary.data <- subset (summary.data, Species!="P.argantea")
summary.data <- subset (summary.data, Species!="P.tremula")

Mb.data <- subset (summary.data, Species=="M.bifolium")
Mb.data <- subset (Mb.data, StandType=="Evergreen")
Mbupper <- subset (Mb.data, side=="sup")
Mblower <- subset (Mb.data, side=="inf")

Am.data <- subset (summary.data, Species=="A.nemorosa")
Amupper <- subset (Am.data, side=="sup")
Amlower <- subset (Am.data, side=="inf")

Acer.data <- subset (summary.data, Species=="A.platanoides")
Acerupper <- subset (Acer.data, side=="sup")
Acerlower <- subset (Acer.data, side=="inf")

Aeg.data <- subset (summary.data, Species=="A.podagraria")
Aegupper <- subset (Aeg.data, side=="sup")
Aeglower <- subset (Aeg.data, side=="inf")

Ox.data <- subset (summary.data, Species=="O.acetosella")
Oxupper <- subset (Ox.data, side=="sup")
Oxlower <- subset (Ox.data, side=="inf")

Fra.data <- subset (summary.data, Species=="F.vesca")
Fraupper <- subset (Fra.data, side=="sup")
Fralower <- subset (Fra.data, side=="inf")

Pop.data <- subset (summary.data, Species=="P.tremula")
Popupper <- subset (Pop.data, side=="sup")
Poplower <- subset (Pop.data, side=="inf")

Oak.data <- subset (summary.data, Species=="Q.robur")
Oak.data <- subset (Oak.data, StandType!="Evergreen")
Oakupper <- subset (Oak.data, side=="sup")
Oaklower <- subset (Oak.data, side=="inf")

Rcas.data <- subset (summary.data, Species=="R.cassubicus")
Rcasupper <- subset (Rcas.data, side=="sup")
Rcaslower <- subset (Rcas.data, side=="inf")

dodge <- position_dodge(width=0.65) 
upperdata<-subset(summary.data, side=="sup")
lowerdata<-subset(summary.data, side=="inf")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#plot the data just to visualise it first

summary.data$Species <- factor(summary.data$Species, levels=c("A.platanoides", "Q.robur", "A.nemorosa","A.podagraria", "R.cassubicus", "F.vesca","M.bifolium", "O.acetosella"))

Decid <- subset(summary.data, StandType=="Deciduous")
Ever <- subset (summary.data, StandType=="Evergreen")

p <- ggplot(Decid, aes_string(x = "Week", y="Flav.mean", col="Treatment"),na.action=na.omit)#linetype="Timing"
p <- p + scale_color_manual(values=c("#000000","#009E73","#E69F00","#F0E442"))
p <- p + stat_summary(fun.data=mean_se, size=0.6,position=position_dodge(width=0.4), alpha ="0.8")
p <- p + geom_point(position=dodge, size=2,alpha=0.8)
p <- p + facet_grid(StandType~Species)#Group
#p <- p + geom_rect(aes(xmin=22, xmax=35, ymin=0, ymax=Inf))
p <-p + annotate("rect", xmin = 22, xmax = 35, ymin = -Inf, ymax = Inf,
                 alpha = .2)
#p <- p + ylim(0,40)
p <- p + ylab(expression(paste("Flavonol content ")))
p <- p + theme_bw()
p <- p + theme(text = element_text(size=10),axis.text.x=element_text(angle=90))
p <- p+ 
  theme(strip.background =element_rect(fill="white"),strip.text = element_text(face="italic"), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

p

p1 <- ggplot(Ever, aes_string(x = "Week", y="Flav.mean", col="Treatment"),na.action=na.omit)#linetype="Timing"
p1 <- p1 + scale_color_manual(values=c("#000000","#009E73","#E69F00","#F0E442"))
p1 <- p1 + stat_summary(fun.data=mean_se, size=0.6,position=position_dodge(width=0.4), alpha ="0.8")
p1 <- p1 + geom_point(position=dodge, size=2,alpha=0.8)
p1 <- p1 + facet_grid(StandType~Species)#Group
#p1 <- p1 + geom_rect(aes(xmin=22, xmax=35, ymin=0, ymax=Inf))
p1 <-p1 + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                 alpha = .2)
#p1 <- p1 + ylim(0,0.6)
p1 <- p1 + ylab(expression(paste("Flavonol content")))
p1 <- p1 + theme_bw()
p1 <- p1 + theme(text = element_text(size=10),axis.text.x=element_text(angle=90))
p1 <- p1 + 
  theme(strip.background =element_rect(fill="white"),strip.text = element_text(face="italic"), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

p1

library(ggpubr)

figure <- ggarrange(p, p1,
                    ncol = 1, nrow = 2)
figure

ggsave("FlavBothYears.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')

#########Statistics#########
vf1 <- varFixed(~Flav.mean)
vf2 <- varIdent(form= ~1|Flav.mean)
vf3 <- varPower(form= ~Flav.mean)
vf4 <- varExp(form= ~Flav.mean)
vf5 <- varConstPower(form=~Flav.mean)


m1<- lme(Flav.mean~Week*StandType*Treatment, random=~1|YEAR/Week/StandType/Stand/REALPLOT, data=Fraupper,  na.action=na.exclude)#family= Gamma weights=vf3,
summary(m1)
anova(m1)

##Model validation##
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
E2 <- resid(m1)  
F2 <- fitted(m1)  #Fitted 
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)

##identify outliers 
library(predictmeans)
CookD(m1)
View(Mblower)

##same in ggplot#
MyX  <- c("Week", "StandType", "Treatment")
MyMultipanel.ggp2(Z = Acerupper, 
                  varx = MyX, 
                  vary = "E2", 
                  ylab = "Residuals",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = TRUE)

####applying a GAMM####

library(mgcv)
library(nlme)
library (gamm4)

m3 <- gamm(Flav.mean ~ StandType*Treatment + s(Week, bs='fs')+ti(Week, Treatment, bs='fs')+t2(YEAR, Week, Stand, StandType, REALPLOT),
           data=Fraupper, na.action=na.exclude) #+ ti(Week, Treatment, bs='fs') #+ ti(Week, StandType, bs='fs')

m4 <- gamm(Flav.mean ~ s(Week, by = Treatment)+t2(YEAR, Week, Stand, StandType, REALPLOT),
           data=Fraupper,na.action=na.exclude)#+ ti(Week, Treatment, bs='fs') #+ ti(Week, StandType, bs='fs')

##now compare the models with the bam package to see which, is better, although cannot same random terms, or power structure

bam3 <- bam(Flav.mean ~ Treatment*StandType + s(Week, bs = 'fs')+ ti(Week, Treatment, bs='fs'),
            data=Acerlower, weights=vf3, random=list(REALPLOT=~1)) #+ ti(Week, Treatment, bs='fs') #+ ti(Week, StandType, bs='fs')

bam4 <- bam(Flav.mean ~ Treatment*StandType + s(Week, by = Treatment)+ ti(Week, Treatment, bs='fs'),
            data=Acerlower, random=list(REALPLOT=~1), na.action=na.omit)

library(itsadug)

compareML(bam3,bam4)

m3$gam %>% summary()
m3$gam %>% anova()

m4$gam %>% summary()
m4$gam %>% anova()

E2 <- resid(m3$gam)#Residuals

F2 <- fitted(m3$gam)#Fitted values
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2, 
       addSmoother = TRUE,
       addRegressionLine = TRUE,
       addHorizontalLine = TRUE)


c##same in ggplot#
MyX  <- c("Week", "StandType", "Treatment")
MyMultipanel.ggp2(Z = Acerlower, 
                  varx = MyX, 
                  vary = "E2", 
                  ylab = "Residuals",
                  addSmoother = TRUE,
                  addRegressionLine = TRUE,
                  addHorizontalLine = TRUE)

#checking for overdispersion
E1 <- resid(m3$gam, type = "pearson")
N  <- nrow(Acerlower)
p <- length(coef(m3$gam))
sum(E1^2) / (N - p)

#plot model in ggplot
library(installr)
library(effects)
library(tidyverse)

##to plot model in ggplot

m1plot <- gamm4(Flav.mean~Week*Treatment*StandType,
          data=Fraupper, random=~(1|YEAR/Week/StandType/Stand/REALPLOT), na.action=na.exclude)

pred <- predict(m3$gam, se.fit=TRUE)

#checking that model and data have some number of values
str(Fraupper)
str(pred)

#plot GAM in lme

Fraupper$Treatment<-ordered(Fraupper$Treatment, levels=c("Control","NoUV<350nm","NoUV","NoBlue"))

p <- ggplot(Fraupper, aes_string(x = "Week", y="Flav.mean", col="Treatment"),na.action=na.exclude)
p <- p + scale_color_manual(values=c("#000000","#F0E442","#E69F00","#009E73")) 
#p <- p + scale_linetype_manual(values=c("solid", "solid"))
p <- p + facet_grid(Treatment~StandType) 
p <- p + geom_point(position=dodge, size=2,alpha=0.3) 
p <- p + stat_summary(fun.data=mean_se, size=0.6, position=dodge, alpha ="0.4")  
p <- p + geom_ribbon(aes(ymin=pred$fit-1.96*pred$se.fit,  
                         ymax=pred$fit+1.96*pred$se.fit), alpha=0.2, fill="grey")
p <- p + geom_line(aes(y=pred$fit), col="blue", lwd=1) 
p <- p + theme_bw() 
p <- p + theme(text = element_text(size=10),axis.text.x=element_text(angle=90))
p <- p + ylab(expression(paste("Adaxial epidermal flavonol content")))
p <- p + theme(plot.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank() )
p <- p + theme(panel.border= element_blank())    
p <- p + 
  theme(strip.background =element_rect(fill="white"),strip.text = element_text(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))

p

ggsave("FraupperGAMFlav.tiff", units="in", width=10, height=6, dpi=300, compression = 'lzw')

