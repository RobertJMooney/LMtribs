#GENERAL DATAFRAME MANAGEMENT
#load necessary packages
library(car)
library(lmerTest)
library(lme4)
library(MASS)
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(scales)


#Import data file
LMtribsPNAS <- read.csv("Mooney_LMtribs_EDI_dataset.csv", header = T)

#Check data matrix
summary(LMtribsPNAS )

#Calculate and add new data coloums to dataframe
LMtribsPNAS $developed<-((LMtribsPNAS $agriculture)+(LMtribsPNAS $urban)) #developed land

LMtribsPNAS $TPyield <-((LMtribsPNAS $TPload_kgday)/(LMtribsPNAS $Areakm2)) #TP yield
LMtribsPNAS $TNyield <-((LMtribsPNAS $TNload_kgday)/(LMtribsPNAS $Areakm2)) #TNyield

LMtribsPNAS $TN_hydraulicProp<- (LMtribsPNAS $percentTN)/(LMtribsPNAS $percentwater) #TN load efficiceny (proportional lake-wide nutrient input of a tributary relative to its proportional lake-wide water input)
LMtribsPNAS $TP_hydraulicProp<- (LMtribsPNAS $percentTP)/(LMtribsPNAS $percentwater) #TP load efficiceny (proportional lake-wide nutrient input of a tributary relative to its proportional lake-wide water input)

#log10 transform data to improve normality
LMtribsPNAS$log10TPload <- log10(LMtribsPNAS $TPload_kgday) 
LMtribsPNAS$log10TNload <- log10(LMtribsPNAS $TNload_kgday)
LMtribsPNAS$log10flow <- log10(LMtribsPNAS $Flowlitersday)
LMtribsPNAS$log10area <- log10(LMtribsPNAS $Areakm2)
LMtribsPNAS$log10TP <- log10(LMtribsPNAS $TPmgL)
LMtribsPNAS$log10TN <- log10(LMtribsPNAS $TNmgL)
LMtribsPNAS$log10SRP <- log10(LMtribsPNAS $SRPmgL)
LMtribsPNAS$log10DIN <- log10(LMtribsPNAS $DINmgL)
LMtribsPNAS$log10TPyield <- log10(LMtribsPNAS $TPyield)
LMtribsPNAS$log10TNyield<- log10(LMtribsPNAS $TNyield)

#MULTIPLE LINEAR REGRESSION MODELS FOR NUTRIENT METRICS
#BIC model selection and final selected (reduced) model output for 11 nutrient metrics

#TP (mg/L)
TPfull<-lm(log10TP~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

TPstepBIC<-stepAIC(TPfull, direction='backward', criterion='BIC', k = log(235))

TPreducedBIC<- lm(log10TP ~ agriculture + wetland +urban, data=LMtribsPNAS ) 
summary(TPreducedBIC)  
anova(TPreducedBIC)

#TN (mg/L)
TNfull<-lm(log10TN~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

TNstepBIC<-stepAIC(TNfull, direction='backward', criterion='BIC', k = log(235))

TNreducedBIC<- lm(log10TN ~ agriculture + log10area + wetland+urban, data=LMtribsPNAS ) 
summary(TNreducedBIC) 
anova(TNreducedBIC) 

#SRP (mg/L)
SRPfull<-lm(log10SRP~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

SRPstepBIC<-stepAIC(SRPfull, direction='backward', criterion='BIC', k =log(235))

SRPreducedBIC<- lm(log10SRP~ agriculture + log10area + damspresent + wetland + 
                     urban + agriculture:log10area + agriculture:damspresent + 
                     log10area:damspresent + agriculture:log10area:damspresent, data=LMtribsPNAS ) 
summary(SRPreducedBIC) 
anova(SRPreducedBIC) 

#DIN (mg/L)
DINfull<-lm(log10DIN~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

DINstepBIC<-stepAIC(DINfull, direction='backward', criterion='BIC', k =log(235))

DINreducedBIC<- lm(log10DIN ~ agriculture + log10area + urban, data=LMtribsPNAS ) 
summary(DINreducedBIC) 
anova(DINreducedBIC) 


#Proportion of TP composed of SRP (%)

SRPpercentfull<-lm(SRP.percent.of.TP~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

SRPpercentstepBIC<-stepAIC(SRPpercentfull, direction='backward', criterion='BIC', k = log(235))

SRPpercentreducedBIC<- lm(SRP.percent.of.TP ~ agriculture + log10area, data=LMtribsPNAS ) 
summary(SRPpercentreducedBIC) 
anova(SRPpercentreducedBIC)

#Proportion of TN composed of DIN (%)
DINpercentfull<-lm(DIN.percent.of.TN~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

DINpercentstepBIC<-stepAIC(DINpercentfull, direction='backward', criterion='BIC', k = log(235))

DINpercentreducedBIC<- lm(DIN.percent.of.TN ~ agriculture + log10area + wetland, data=LMtribsPNAS ) 
summary(DINpercentreducedBIC) 
anova(DINpercentreducedBIC) 

#Molar N:P
NPfull<-lm(log10(Load.N.P)~agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS )

NPstepBIC<-stepAIC(NPfull, direction='backward', criterion='BIC', k =log(235))

NPreducedBIC<- lm(log10(Load.N.P) ~ agriculture + log10area + damspresent + urban, data=LMtribsPNAS ) 
summary(NPreducedBIC) 
anova(NPreducedBIC)

#TP load (kg/day)
log10TPloadfull<- lm(log10TPload ~ agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data = LMtribsPNAS ) 
summary(log10TPloadfull)

logTPloadstepBIC<-stepAIC(log10TPloadfull, direction='backward', criterion='BIC', k = log(235))

logTPloadreducedBIC<- lm(log10TPload ~ agriculture + log10area + damspresent + wetland + 
                            agriculture:log10area, data=LMtribsPNAS ) 
summary(logTPloadreducedBIC)
anova(logTPloadreducedBIC)

#TN load (kg/day)
log10TNloadfull<- lm(log10TNload ~ agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data=LMtribsPNAS ) 

logTNloadstepBIC<-stepAIC(log10TNloadfull, direction='backward', criterion='BIC', k = log(235))

logTNloadreducedBIC<- lm(log10TNload ~ log10area + urban+ wetland, data=LMtribsPNAS ) 
summary(logTNloadreducedBIC)
anova(logTNloadreducedBIC)


#TP yield 
logTPyieldfull<- lm(log10TPyield ~ agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data=LMtribsPNAS )

logTPyieldstepBIC<-stepAIC(logTPyieldfull, direction='backward', criterion='BIC', k = log(235))

logTPyieldreducedBIC<- lm(log10TPyield ~ agriculture + log10area + damspresent + wetland + 
                             agriculture:log10area, data=LMtribsPNAS ) 
summary(logTPyieldreducedBIC)
anova(logTPyieldreducedBIC)

#TN yield
logTNyieldfull<- lm(log10TNyield ~ agriculture*log10area*damspresent + wetland*log10area*damspresent + urban*log10area*damspresent, data=LMtribsPNAS )

logTNyieldstepBIC<-stepAIC(logTNyieldfull, direction='backward', criterion='BIC', k = log(235))

logTNyieldreducedBIC<- lm(log10TNyield ~ log10area + wetland+urban, data=LMtribsPNAS ) 
summary(logTNyieldreducedBIC)
anova(logTNyieldreducedBIC)

#MAIN TEXT FIGURES

my_theme = theme(axis.title.x = element_text(size = 16, face = "bold"),
                 axis.text.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16, face = "bold"),
                 axis.text.y = element_text(size = 16))

#Figure 1 was produced in arcmap, and is not included in this script
#Figure 2A
PmetricsPNAS <- ggplot(LMtribsPNAS, aes(x = developed, y = TPmgL, color = SRP.percent.of.TP))+ theme_classic()+
  
  scale_y_log10()+
  xlab("Developed land (%)")+
  ylab("TP (mg/L)")+
  my_theme+
  geom_point(size = 3.5)+
  scale_color_gradient(low = "yellow",  high = "red", name = "% SRP")+
  theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

PmetricsPNAS+expand_limits(y=c(0, 1))+annotation_logticks(base = 10, sides = "l")

#Figure 2B
Nmetrics <- ggplot(LMtribsPNAS, aes(x = developed, y = TNmgL, color = DIN.percent.of.TN))+ theme_classic()+
  
  scale_y_log10()+
  xlab("Developed land (%)")+
  ylab("TN (mg/L)")+
  my_theme+
  geom_point(size = 3.5)+
  scale_color_gradient(low = "yellow",  high = "red", name = "% DIN")+
  theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

Nmetrics+expand_limits(y=c(0.10, 10.00))+annotation_logticks(base = 10, sides = "l")

#Figure 3A 
TPcontour<-ggplot()+
  geom_point(data = LMtribsPNAS, aes(x = Flowlitersday, y = TPmgL,color=developed, size = TPyield))+scale_size(trans="log10")+labs(size=expression ("TP yield"))+
  scale_x_log10(limits = c(500000, 20000000000), breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(0.0015, 1))+
  theme_classic()+xlab("Discharge (L/day)")+ylab("TP (mg/L)")+
  scale_color_gradient(low = "yellow", high = "red", name = "% Developed land",
                       breaks = c(0,25,50,75,100))+my_theme+
  expand_limits(y=c(0.1, 10))+annotation_logticks(base = 10, sides = "l")+
  coord_trans()+
  geom_abline(intercept = 4, slope = -1)+
  geom_abline(intercept = 5, slope = -1)+
  geom_abline(intercept = 6, slope = -1)+
  geom_abline(intercept = 7, slope = -1)+
  geom_abline(intercept = 8, slope = -1)+
  geom_abline(intercept = 9, slope = -1)

TPcontour+theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

#Figure 3B
TNcontour<-ggplot()+
  geom_point(data = LMtribsPNAS, aes(x = Flowlitersday, y = TNmgL,color=developed, size = TNyield))+scale_size(trans="log10", breaks = c(0.1,1))+labs(size=expression ("TN yield"))+
  scale_x_log10(limits = c(500000, 20000000000), breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(0.1, 10))+
  theme_classic()+xlab("Discharge (L/day)")+ylab("TN (mg/L)")+
  scale_color_gradient(low = "yellow", high = "red", name = "% Developed land",
                       breaks = c(0,25,50,75,100))+my_theme+
  expand_limits(y=c(0.1, 10))+annotation_logticks(base = 10, sides = "l")+
  coord_trans()+
  geom_abline(intercept = 4, slope = -1)+
  geom_abline(intercept = 5, slope = -1)+
  geom_abline(intercept = 6, slope = -1)+
  geom_abline(intercept = 7, slope = -1)+
  geom_abline(intercept = 8, slope = -1)+
  geom_abline(intercept = 9, slope = -1)+
  geom_abline(intercept = 10, slope = -1)

TNcontour+theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

#Figure 4A
propTP <- ggplot(LMtribsPNAS, aes(x = Areakm2, y = TP_hydraulicProp, color=TPmgL))+scale_color_gradient(trans = "log",low = "yellow",  high = "red", name = "TP (mg/L)", breaks = c(0, 0.01,0.1, 0.5, 0.75))+
  theme_classic()+
  xlab("Area"~(km^2))+
  ylab("TP loading efficiency \n (TP contribution/water contribution)")+
  scale_x_log10(breaks = c(1,10,100,1000,10000))+
  scale_y_log10(breaks = c(0.0, 0.1, 1, 5, 10.0, 15))+
  my_theme+
  geom_point(size = 4)+
  geom_abline(intercept = 0, slope = 0, color = "red", size = 1.5)+
  annotation_logticks(base = 10, sides = "l")+
  theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

propTP

#Figure4B
propTN <- ggplot(LMtribsPNAS, aes(x = Areakm2, y = TN_hydraulicProp, color=TNmgL))+scale_color_gradient(trans = "log",low = "yellow",  high = "red", name = "TN (mg/L)", breaks = c(0.001,0.1,1.0, 2.51, 5.0,10.0))+
  theme_classic()+
  xlab("Area"~(km^2))+
  ylab("TN loading efficiency \n (TN contribution/water contribution)")+
  scale_x_log10(breaks = c(1,10,100,1000,10000))+
  scale_y_log10(breaks = c(0.5,1, 5, 10.0, 15))+
  my_theme+
  geom_point(size = 4)+
  geom_abline(intercept = 0, slope = 0, color = "red", size = 1.5)+
  annotation_logticks(base = 10, sides = "l")+
  theme(legend.text=element_text(size=14))+theme(legend.title=element_text(size=14))

propTN

#END SCRIPT
