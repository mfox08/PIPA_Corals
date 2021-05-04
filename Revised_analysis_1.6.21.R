##### PIPA Final Analyses 
library(scales)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(splitstackshape)
library(patchwork)
extrafont::loadfonts() 

#mixed effects modeling
library(lme4)
library(broom.mixed)
library(jtools)
library(lmerTest)
library(emmeans)
library(car)
library(MASS)
library(merTools) #predicting model fit for regression plotting
library(huxtable) #for exporting summ 
library(MuMIn)

#mapping
library(sf)
library(maptools)
library(raster)
library(tibble)
library(smoothr)
library(ggrepel)
library(GISTools) #north arrows

#community analysis
library(vegan)
library(pairwiseAdonis) #for pairwise tests of permanova

#create a global plot theme
newtheme <- theme_classic() + theme(text = element_text(size=11))+
  theme(axis.text.x = element_text(size=11,colour="black"), axis.text.y = element_text(size=11,colour="black"))+
  theme(plot.margin = unit(c(5.5,5.5,5.5,20), "pt"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(axis.line = element_line(size = 0))


#Load the official PIPA coral cover time series & site list file trimmed to study locations 
# extract only leeward or less exposed windward sites 
############################################################################################


setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Benthic Data/Finalized data for analysis")
pipa<-read.csv(file="PIPA_Functional_Groups_2002-2018_for_analysis_MDF_07112020.csv",header=T)
sites<-read.csv(file="Long_term_site_KAN_RAW_ORO_NIKU_2002_2018_MDF_07.10.2020.csv",header=T)

#sites we do not want to include in analysis
trim<-c("K11","K29","K15","N8","N7","N14","O11","O13","O19","O1","O3","R4","R6","R7")

sites2<-sites %>% filter(!Site.Number %in% trim)

# # Create maps of sites around each island for supplemental materials
# ######################################
# #load the smoothed PIPA shapefile and plot the sites around each island for supplementary materials
# setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Benthic Data")
# 
# pipa_smooth<-st_read("PIPA_smoothed.shp")
# 
# #set up colors and symbol shapes
# map.cols = c("steelblue4","red","gray95","gray60","gray95","gray95","gray25","gray85","gray95")
# map.shapes = c(21,24)
# 
# quartz()
# kan<-ggplot()+geom_sf(data=pipa_smooth,aes(fill=L4_ATTRIB))+scale_fill_manual(values=map.cols)+
#   ylim(-2.88,-2.75)+xlim(-171.74,-171.61)+
#   geom_point(data=sites2,aes(x=Lon1,y=Lat1,fill=as.factor(Only.2015)),pch=21,size=5,col="black")+
#   geom_point(data=sites2,aes(x=Lon2018,y=Lat2018,shape=as.factor(All.3.ENSO)),fill="seagreen3",col="black",size=3)+
#   coord_sf(crs=4326)+scale_shape_manual(values=map.shapes)+
#   geom_text_repel(data=subset(sites2,Site.Number!="K24"),aes(x=Lon1,y=Lat1,label=Site.Number),nudge_x=-.05,nudge_y=-.005,size=3)+
#   geom_text_repel(data=subset(sites2,Site.Number=="Simons"),aes(x=Lon2018,y=Lat2018,label=Site.Number),nudge_x=-.05,nudge_y=-.005,size=3)+
#   geom_text_repel(data=subset(sites2,Site.Number=="K24" | Site.Number=="SW.corner"),aes(x=Lon2018,y=Lat2018,label=Site.Number),nudge_x=-.03,nudge_y=-.02,size=3)+
#   ylab('Latitude')+xlab('Longitude')+
#   ggtitle("Kanton")+
#   maptheme+theme(legend.position="none")
# kan
# 
# nik<- ggplot()+geom_sf(data=pipa_smooth,aes(fill=L4_ATTRIB))+scale_fill_manual(values=map.cols)+
#   ylim(-4.705,-4.64)+xlim(-174.57,-174.49)+
#   geom_point(data=sites2,aes(x=Lon1,y=Lat1,fill=as.factor(Only.2015)),pch=21,size=5,col="black")+
#   geom_point(data=sites2,aes(x=Lon2018,y=Lat2018,shape=as.factor(All.3.ENSO)),fill="seagreen3",col="black",size=3)+
#   coord_sf(crs=4326)+scale_shape_manual(values=map.shapes)+
#   geom_text_repel(data=subset(sites2,Site.Number!="N8"& Site.Number!="N7"),aes(x=Lon1,y=Lat1,label=Site.Number),nudge_x=-.05,nudge_y=-.005,size=3)+
#   ylab('Latitude')+xlab('Longitude')+
#   ggtitle("Nikumaroro")+
#   maptheme+theme(legend.position="none")
# nik
# 
# oro<- ggplot()+
#   geom_sf(data=pipa_smooth,aes(fill=L4_ATTRIB))+scale_fill_manual(values=map.cols)+
#   ylim(-4.55,-4.47)+xlim(-172.24,-172.12)+
#   geom_point(data=sites2,aes(x=Lon1,y=Lat1,fill=as.factor(Only.2015)),pch=21,size=5,col="black")+
#   geom_point(data=sites2,aes(x=Lon2018,y=Lat2018,shape=as.factor(All.3.ENSO)),fill="seagreen3",col="black",size=3)+
#   coord_sf(crs=4326)+scale_shape_manual(values=map.shapes)+
#   geom_text_repel(data=subset(sites2,Site.Number!="O3"& Site.Number!="O1" & Site.Number!="O19"& Site.Number!="O13"),aes(x=Lon1,y=Lat1,label=Site.Number),nudge_x=-.05,nudge_y=.005,size=3)+
#   ylab('Latitude')+xlab('Longitude')+
#   ggtitle("Orona")+
#   maptheme+theme(legend.position="none")
# oro
# 
# Rawaki<-ggplot()+
#   geom_sf(data=pipa_smooth,aes(fill=L4_ATTRIB))+scale_fill_manual(values=map.cols)+
#   ylim(-3.73,-3.71)+xlim(-170.728,-170.7)+
#   geom_point(data=sites2,aes(x=Lon1,y=Lat1,fill=as.factor(Only.2015)),pch=21,size=5,col="black")+
#   geom_point(data=sites2,aes(x=Lon2018,y=Lat2018,shape=as.factor(All.3.ENSO)),fill="seagreen3",col="black",size=3)+
#   coord_sf(crs=4326)+scale_shape_manual(values=map.shapes)+
#   geom_text_repel(data=subset(sites2,Site.Number!="R4" & Site.Number!="R6" & Site.Number!="R7"),aes(x=Lon1,y=Lat1,label=Site.Number),nudge_x=-.004,nudge_y=-.002,size=3)+
#   ylab('Latitude')+xlab('Longitude')+
#   ggtitle("Rawaki")+
#   maptheme+theme(legend.position="none")
# Rawaki

# site.maps<-kan+Rawaki+oro+nik+plot_layout(ncol=2,heights=c(2,2,2,2),widths=c(2,2,2,2))
# quartz()
# site.maps

#save maps
#setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
# 
# ggsave(plot = kan,filename = "Kanton_sitemap.pdf",
#        width=6,height=6,dpi=300,useDingbats=FALSE)
# ggsave(plot = nik,filename = "Niku_sitemap.pdf",
#        width=6,height=6,dpi=300)
# ggsave(plot = oro,filename = "Orona_sitemap.pdf",
#        width=6,height=6,dpi=300)
# ggsave(plot = Rawaki,filename = "Rawaki_sitemap.pdf",
#        width=6,height=6,dpi=300)

######################################

#### plot changes in mean coral cover across PIPA over time 
#### calculate estimated proportional change in coral cover during 3 ENSO events using bootstrapped resampling 

#restrict benthic data only to selected sites

pipa2<-pipa %>% filter(!Site.Num %in% trim)

pipa.means<-pipa2 %>% group_by(Year) %>% summarize(
  N=sum(!is.na(Coral)),
  Cor = mean(Coral,na.rm=T),
  SD = sd(Coral,na.rm=T),
  SE = SD/sqrt(N))
pipa.means$Month<-c(6,6,6,6,9,5)
pipa.means$Day<-rep(15,6)
pipa.means$Date<-as.POSIXct(mdy(paste(pipa.means$Month,pipa.means$Day,pipa.means$Year)))+days(1)

#### read in the total hot spot time series to add to the coral time series 
#### plot mean coral cover through time -- Fig. 1

## hotspot time series 
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Pathfinder DHW")
hot<-read.csv(file="PIPA_wide_DHW_timeseries_for_total_hotspot.csv",header=T)  
hot$Date<-as.POSIXct(mdy(paste(hot$Month,hot$Day,hot$Year)))+days(1)

#create line size column
hot$line<-ifelse(hot$TH>1,2,.1)

#### plot the coral time series
event.cols=c(rep('#ffa41b',2),rep('#00a8cc',2),rep('#142850',2))

#Original colors
#plot.cols=c("turquoise3","magenta","steelblue4")


#create a plot background that includes bars for enso events with their width scaled to amount of time > DHW 4
enso.bars<-ggplot()+ geom_rect(data = data.frame(Date = as.POSIXct("2002-10-16"),
                                                 Date1 = as.POSIXct("2003-03-12"),
                                                 Cor = -Inf,
                                                 Cor1 = Inf),
                               aes(xmin = Date, xmax = Date1,ymin = Cor, ymax = Cor1),
                               fill = "gray", alpha = 0.5)+
  #add second rectangle for 2009
  geom_rect(data = data.frame(Date = as.POSIXct("2009-11-18"),
                              Date1 = as.POSIXct("2010-03-17"),
                              Cor = -Inf,
                              Cor1 = Inf),
            aes(xmin = Date, xmax = Date1,ymin = Cor, ymax = Cor1),
            fill = "gray", alpha = 0.5)+
  #add third rectangle (2015)
  geom_rect(data = data.frame(Date = as.POSIXct("2015-05-13"),
                              Date1 = as.POSIXct("2016-08-03"),
                              Cor = -Inf,
                              Cor1 = Inf),
            aes(xmin = Date, xmax = Date1,ymin = Cor, ymax = Cor1),
            fill = "gray", alpha = 0.5)+
  ylab("% Coral Cover")+scale_x_datetime(breaks=date_breaks("2 year"),date_labels = "%Y",
                                         limits =c(
                                           as.POSIXct("2001-06-01"),
                                           as.POSIXct("2018-12-31")))

coral.cover<-enso.bars+
  geom_line(data=pipa.means,mapping=aes(Date,Cor))+
  geom_errorbar(data=pipa.means,mapping=aes(x=Date,y=Cor,ymin=Cor-SE,ymax=Cor+SE),width=0)+
  geom_point(data=pipa.means,mapping=aes(Date,Cor),fill=event.cols,pch=21,size=3,col='black')+
  scale_fill_manual(values=event.cols)+
  geom_text(data=pipa.means,mapping=aes(x=Date,y=Cor,label = paste("(",N,")")),parse=TRUE,size=2.8,
            vjust = 0,hjust=1.5)+
  newtheme
coral.cover

#add the hotspot time series
coeff=1
Fig.1A<-coral.cover+
  geom_bar(data=subset(hot,TH>0),mapping=aes(Date,TH/2,size=.75),stat="identity",col='black',width=1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     
                     # Features of the first axis
                     name = "% Coral Cover",
                     
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(~.*coeff, name="Total Hotspot",
                                         breaks = c(0,5,10,15,20),labels=c(0,10,20,30,40)))+theme(legend.position = 'none')
#  geom_ribbon(data=dhw.series,mapping=aes(Date,dhwP,ymin=0,ymax=dhwP*2),fill='orange',alpha=0.75,col='black')
Fig.1A

############### Create Supplemental plot showing all raw data with individual sites connected temporally through time

#add date to raw data for plotting on main graph
pipa2$Day<-15
pipa2<-pipa2 %>% mutate(Month=ifelse(Year<2015,6,
                               ifelse(Year==2015,9,5))) %>% mutate(Date=as.POSIXct(mdy(paste(Month,Day,Year)))+days(1))


##now add data at the site level and plot by island
Fig.S1<-enso.bars+
  geom_line(data=pipa.means,mapping=aes(Date,Cor),size=3,alpha=0.4,lineend='round',col='black')+ ### transparent mean coral cover line
  geom_point(data=pipa2,mapping=aes(x=Date,y=Coral,group=Year),fill="#00a8cc",size=2,pch=21,col='black')+
  geom_line(data=pipa2,mapping=aes(x=Date,y=Coral,group=Site.Num),color="#00a8cc")+
  #geom_errorbar(data=pipa2,mapping=aes(x=Date,y=Coral,ymin=Coral-SD.Coral,ymax=Coral+SD.Coral),width=0)+
  newtheme+facet_wrap(~Island)+
  xlab('Year')
Fig.S1



# setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
# ggsave(plot = Fig.S1,filename = "Coral_hotspot.pdf",
# width=10,height=6,dpi=300,useDingbats=FALSE)

###########
########### ESTIMATE PROPORTIONAL CHANGE IN CORAL COVER DURING EACH EVENT
###########

#simplify the pipa df to only have coral island and year then nest it for resampling
pipa3<- pipa2 %>% dplyr::select(Island,Year,Coral) 

nested<-pipa3 %>% group_by(Year) %>% nest() %>% ungroup() %>% 
  mutate(n=c(12,12,14,18,28,19),
         year=c(2002,2005,2009,2012,2015,2018))# add sample sizes (survey sites per year)

ptm <- proc.time()
dat<-NULL

n<-10 #to change the number of permutations 10,000 was used in the manuscript
# 10 is shown here as an example to make the code run faster

for(i in 1:n){  
  
  # randomly sample each of the source populations with replacement for the number of samples for each group
  samps<-nested %>% mutate(samp=map2(data,n,sample_n,replace=T))
                                
  #unnest iterative sampled lists -- this generates 1 new df with randomly sampled means across PIPA for each year 
  samps2<-samps %>% dplyr::select(-data) %>% unnest(samp) %>% dplyr::select(n,Island,Coral) %>% 
    mutate(Year=c(rep(2002,12),rep(2005,12),rep(2009,14),rep(2012,18),rep(2015,28),rep(2018,19)))
  
  #  calculate mean coral cover per year for each randomly sampled group then calculate proportional losses per Event
  #  then take averages for each year and record loop iteration
  means<-samps2 %>% group_by(Year) %>% summarize(Cor=mean(Coral,na.rm=T),.groups="keep") %>% spread(Year,Cor) %>% 
        mutate(Iteration=i) %>% ungroup()
  
  #calculate direct declines and proportional losses
   change<-means %>% group_by(Iteration) %>% mutate(ENSO1=(means$"2005"-means$"2002"),
                                                   ENSO2=(means$"2012"-means$"2009"),
                                                   ENSO3=(means$"2018"-means$"2015"),
                                                   ENSO1p=(means$"2005"-means$"2002")/means$"2002",
                                                   ENSO2p=(means$"2012"-means$"2009")/means$"2009",
                                                   ENSO3p=(means$"2018"-means$"2015")/means$"2015") %>% ungroup()
  
  #gather results into simplified long format df
   change2<-change %>% gather(Event,Decline,c(ENSO1:ENSO3)) %>% dplyr::select(Iteration,Event,Decline)
   loss<-change %>% gather(Event,Loss,c(ENSO1p:ENSO3p))
   
   #convert proportional loss into a percent
   change2$Loss<-loss$Loss*100
  
   dat<-rbind(dat,change2)
}


#take mean, median, and 95% CI of coral decline and proportional loss respectively

change.ci<-dat %>%
  group_by(Event) %>% #calculate summary stats for each spp at the class level
  summarize(
    Mean.decline = mean(Decline,na.rm=TRUE),
    Median.decline = median(Decline,na.rm=TRUE),
    Upper.CI.decline = quantile(Decline,0.975),
    Lower.CI.decline = quantile(Decline,0.025),
    decline.75.upper = quantile(Decline,0.875),
    decline.75.lower = quantile(Decline,0.125),
    Mean.loss = mean(Loss,na.rm=TRUE),
    Median.loss = median(Loss,na.rm=TRUE),
    Upper.CI.loss = quantile(Loss,0.975),
    Lower.CI.loss = quantile(Loss,0.025))

###plot them all together - decline
plot.cols<-c("#ffa41b","#00a8cc","#142850") 

Fig.1B<-ggplot()+geom_errorbar(data=change.ci,aes(x=Event,ymin=Lower.CI.decline,ymax=Upper.CI.decline,color=Event),width=0,size=1)+
  geom_errorbar(data=change.ci,aes(x=Event,ymin=decline.75.lower,ymax=decline.75.upper,color=Event),width=0,size=2)+
  geom_point(data=change.ci,aes(x=Event,y=Mean.decline,fill=Event),pch=21,color='black',size=6)+
  ylab("Change in Live Coral Cover (%)")+scale_x_discrete(labels=c("2002-2005","2009-2012","2015-2018"))+
  scale_y_continuous(breaks=seq(-50,20,10))+xlab('Years')+
  scale_fill_manual(values=plot.cols)+scale_color_manual(values=plot.cols)+geom_hline(yintercept = 0,lty=2)+
  newtheme
Fig.1B


# ### Use linear mixed effects model with random effect of island to control for geography
# ### Analyze only the declines in coral cover because proportaional estimates inflate 
# ### estimates of change at sites w/ low coral cover. This increases the variance in our observations and masks
# ### the true trend of lower loss of coral in 2015. The true proportional changes are best inferred through the bootstrapping analysis
# ### and comparisons of 95% CI are useful for interpreting confidence in our estimate of coral loss. 
# 
# #compare the temporal changes in coral cover --- decline
# 
# qqp(change.sites$Loss,"norm") #normality of coral cover declines looks good
# 
# decline.mod<-lmer(Decline~Event+(1|Island),data=change.sites,REML=T)
# anova(decline.mod)
# summary(decline.mod)
# ranova(decline.mod)
# #check residuals
# plot(decline.mod)
# hist(resid(decline.mod))
# 
# emm = emmeans(decline.mod, ~ Event)
# pairs(emm)
# 
cor.sites<-pipa2 %>% group_by(Year,Island,Site,Site.Num) %>% summarize(Cor=mean(Coral,na.rm=T)) 

#isolate events and calculate changes in coral cover for each site over time (this will be used later for Fig. 1C)
cor1<-cor.sites %>% dplyr::filter(Year<2006) %>% spread(Year,Cor) %>% mutate(Decline=`2005`-`2002`,
                                                                             Loss=((`2005`-`2002`)/`2002`)*100,
                                                                             Event="ENSO1") #%>% 
#dplyr::select(-'2002',-'2005') #remove coral cover 

cor2<-cor.sites %>% dplyr::filter(Year>2005 & Year<2015) %>% spread(Year,Cor) %>% mutate(Decline=`2012`-`2009`,
                                                                                         Loss=((`2012`-`2009`)/`2009`)*100,
                                                                                         Event="ENSO2") #%>% 
# dplyr::select(-'2009',-'2012') #remove coral cover 

cor3<-cor.sites %>% dplyr::filter(Year>2012) %>% spread(Year,Cor) %>% mutate(Decline=`2018`-`2015`,
                                                                             Loss=((`2018`-`2015`)/`2015`)*100,
                                                                             Event="ENSO3")  #%>% 
# dplyr::select(-'2015',-'2018') #remove coral cover 

#combine change and proportional loss for all events
change.sites<-rbind(cor1,cor2,cor3)


#summarize proportaional losses at each site over the 3 events
site.summary<-change.sites %>% dplyr::select(Island,Site.Num,Loss,Event) %>% spread(Event,Loss)

#############################################
############################################# Environmental data for PIPA across all three El Nino events
#############################################
#############################################

############ SST TIMESERIES PIPA WIDE
#########
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/IGOSS SST")

###identify raw files 
files=dir(pattern='csv') # tell r to assign all files ending with csv to this list
files #check the list to make sure all your files appear

#load file and create island column based on file name

#create empty df for sst data
sst<-NULL

for(i in 1:length(files)){
  
  cat(i,fill=T) #tells you which file the code is working on in the console 
  
  #read the first file
  a<-data.frame(read.csv(file=files[i],header=TRUE)) 
  
  #add Island factor based on file name
  a$Island<-unlist(strsplit(files[i],"_"))[1]
  
  ##### split date column into consitutent parts -focus day as start of week
  ##        -first get rid of second dates for weeks that span years
  b<-cSplit(a,"Time"," - ")
  b$Time_2<-NULL
  
  #split into days,month,year on space
  b<-cSplit(b,"Time_1"," ")
  
  #now only keep first day
  b<-cSplit(b,"Time_1_1","-")
  b$Time_1_1_2<-NULL
  
  #reorder and rename
  b<-b[,c(2,5,3,4,1)]
  colnames(b)<-c("Island","Day","Month","Year","SST")
  
  #create date column
  b$Date<-as.POSIXct(mdy(paste(b$Month,b$Day,b$Year)))+days(1)
  
  #now combine all SST data for each island into single df
  sst<-rbind(b,sst)
  
}

#### IDENTIFY PEAK TEMPS DURING EACH EVENT####
#using IGOSS weekly sst, isolate warming and cooling periods before/after each event to calculate rates of onset or rates of cooling
########### NOW split SST data into year bins on either side of 2002 and 2015 ENSO events
###plot annual temps for each island during those years 
peaks<-sst %>% filter(Year <=2003 & Year>=2001 |Year<=2010 & Year >=2009| Year<=2016 & Year>=2014) %>% group_by(Island) %>% 
  mutate(Week=row_number()) %>% 
  mutate(Event= ifelse(Year <=2003,"ENSO1",
                       ifelse(Year<=2010,"ENSO2","ENSO3")))

#find peak SST for each ENSO event  -- peak in 2009 is in early 2010
peakT<-sst %>% filter(Year == 2002 | Year==2010 | Year==2015) %>% group_by(Island,Year) %>%  slice(which.max(SST))
#rename SST as PeakSST
colnames(peakT)[5] <- "PeakTemp"
peakT<-peakT %>% mutate(Event= ifelse(Year <=2003,"ENSO1",
                                      ifelse(Year<=2010,"ENSO2","ENSO3")))


##### MIN TEMPS PRECEEDING DHW ACCUMULATION ######
#find time of closest minimum temp to start of DHW accumulation in 2002 
minT02<-sst %>% filter(Year == 2002) %>% group_by(Island) %>%  slice(which.min(SST))

#fall min for 2009
min09<-sst %>% filter(Year==2010)
ggplot(min09,aes(Date,SST,color=Island))+geom_path() # between july and october 
min09<-sst %>% filter(Year==2009 & Date>as.Date("2009-07-01"))
minT09<-min09 %>% group_by(Island) %>%  slice(which.min(SST))

#do the same for 2015 but because some dhw accumulation starts in late 2014 we need the minimum SST preceeding that
# but BEFORE the seaonal warming in early 2014. So find the max and find the min after that

maxT14<-sst %>% filter(Year==2014) %>% group_by(Island,Year) %>%  slice(which.max(SST)) #may 2014 is peak
ggplot(subset(sst,Year==2014),aes(Date,SST,group=Island,color=Island))+geom_path()
minT14<-sst %>% filter(Year==2014 & Date>as.Date("2014-05-25")) %>% group_by(Island) %>%  slice(which.min(SST))  #may 2014 is peak


minT<-rbind(minT02,minT09,minT14)
colnames(minT)[5] <- "MinTemp"
minT<-minT %>% mutate(Event= ifelse(Year <=2003,"ENSO1",
                                    ifelse(Year<=2010,"ENSO2","ENSO3")))


#Combine all max and min temps into 1df
temps<-inner_join(minT,peakT,by=c("Island","Event"))
temps<-temps[,-c(2:4,8:10)]
colnames(temps)[c(3,6)]<-c("Min.Date","Peak.Date")

#define DHW start dates for each island and event...subtract 3 days to match start of week dates of IGOSS weekly time series (based on dates determind below)
kan02dhw<-as.Date("2002-05-22")-3
nik02dhw<-as.Date("2002-08-21")-3
oro02dhw<-as.Date("2002-09-11")-3
raw02dhw<-as.Date("2002-05-22")-3

kan10dhw<-as.Date("2009-10-07")-3
nik10dhw<-as.Date("2009-09-23")-3
oro10dhw<-as.Date("2009-09-30")-3
raw10dhw<-as.Date("2009-10-14")-3

#2015 enso
kan15dhw<-as.Date("2015-02-04")-3
nik15dhw<-as.Date("2014-11-19")-3
oro15dhw<-as.Date("2014-12-03")-3
raw15dhw<-as.Date("2014-12-10")-3

##add those to temps df in alphabetical island order and then by event
##add those to temps df in alphabetical island order and then by event
dhw.dates<-c(kan02dhw,nik02dhw,oro02dhw,raw02dhw,kan10dhw,nik10dhw,oro10dhw,raw10dhw,kan15dhw,nik15dhw,oro15dhw,raw15dhw)
temps$Dhw.Date=dhw.dates


#### LOOP for extracting and regressing onset rates#### #### #### #### #### 
#### Extract windows of SST data to calculate rate of onset for each segment of time
a<-NULL 
b<-NULL
c<-NULL
d<-NULL
e<-NULL
f<-NULL
g<-NULL
h<-NULL

slopes1<-NULL
slopes2<-NULL
slopes3<-NULL
slopes4<-NULL
slopes5<-NULL
slopes6<-NULL
slopes7<-NULL
slopes8<-NULL

week4<-NULL
week8<-NULL
week12<-NULL
week4.C<-NULL
week8.C<-NULL
week12.C<-NULL
prime90<-NULL
prime<-NULL
pipa.prime<-NULL

for(i in 1:nrow(temps)){
  
  #break out min to peak and combine across islands and events
  a<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=temps$Min.Date[[i]] & Date<=temps$Peak.Date[[i]])
  a$Duration<-"Min"
  
  slopes1<-rbind(slopes1,a)
  
  #shirnk time window to first dhw accumulation to peak        
  b<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=temps$Dhw.Date[[i]] & Date<=temps$Peak.Date[[i]])     
  b$Duration<-"DHW"
  
  slopes2<-rbind(slopes2,b) 
  
  #4 weeks before Peak
  week4<-temps$Peak.Date[[i]]-days(28) #define new cutoff date
  c<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >= week4 & Date<=temps$Peak.Date[[i]])  
  c$Duration<-"wk4"
  slopes3<-rbind(slopes3,c)
  week4<-NULL #erase before re-writing
  
  #8 weeks
  week8<-temps$Peak.Date[[i]]-days(56)
  d<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=week8 & Date<=temps$Peak.Date[[i]])  
  d$Duration<-"wk8"
  slopes4<-rbind(slopes4,d)
  week8<-NULL
  
  #12 weeks
  week12<-temps$Peak.Date[[i]]-days(84)
  e<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=week12 & Date<=temps$Peak.Date[[i]])     
  e$Duration<-"wk12"
  slopes5<-rbind(slopes5,e)
  week12<-NULL
  
  #cooling - 4 weeks after peak
  week4.C<-temps$Peak.Date[[i]]+days(28)
  f<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=temps$Peak.Date[[i]] & Date <=week4.C)     
  f$Duration<-"wk4_cool"
  slopes6<-rbind(slopes6,f)
  
  #cooling - 8 weeks after peak
  week8.C<-temps$Peak.Date[[i]]+days(56)
  g<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=temps$Peak.Date[[i]] & Date <= week8.C)     
  g$Duration<-"wk8_cool"
  slopes7<-rbind(slopes7,g)
  
  #cooling - 12 weeks after peak
  week12.C<-temps$Peak.Date[[i]]+days(84)
  h<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date >=temps$Peak.Date[[i]] & Date <=week12.C)     
  h$Duration<-"wk12_cool"
  slopes8<-rbind(slopes8,h)
  
  #### also extract SST time series to look for "priming" events within 90 day period prior to peak temp per Ainsworth 2016
  prime90<-temps$Peak.Date[[i]]-days(90)
  prime<-sst %>% filter(Island==temps$Island[[i]]) %>% filter(Date<=temps$Peak.Date[[i]] & Date>=prime90)
  
  pipa.prime<-rbind(pipa.prime,prime)
}

#plot the priming timeseries
pipa.prime<-pipa.prime %>% mutate(Event= ifelse(Year <=2003,"ENSO1",
                                    ifelse(Year<=2010,"ENSO2","ENSO3")))

ggplot(pipa.prime,aes(Date,SST,color=Island,group=Island))+geom_line()+facet_wrap(~Event,scales='free_x')

#plot the temperature distributions for each event
ggplot(pipa.prime,aes(x=SST,y=..scaled..,facets=Island,fill=Event))+geom_density(alpha=0.5,adjust=1.25)+facet_grid(.~Island)+
  scale_fill_manual(values=plot.cols)+newtheme

library(mousetrap)
#evaluate biomodality coefficent per McClanahan et al. 2019 
#calculate bimodality of temperature -- not very much seperation between islands
bimodality_SST<-pipa.prime %>% group_by(Island,Event) %>% summarize(
  Bimodality = bimodality_coefficient(SST,na.rm=T)) 

#combine all pre peak temp slopes
warming<-rbind(slopes1,slopes2,slopes3,slopes4,slopes5)

#combine all post peak temp slopes
cooling<-rbind(slopes6,slopes7,slopes8)

#add event factor
warming<-warming %>% mutate(Event= ifelse(Year <=2003,"ENSO1",
                                          ifelse(Year<=2010,"ENSO2","ENSO3")))

cooling<-cooling %>% mutate(Event= ifelse(Year <=2003,"ENSO1",
                                          ifelse(Year<=2010,"ENSO2","ENSO3")))

#### now do linear regressions for each island/event/and duration combination
#add a week count for each island/event
warming<- warming %>%  group_by(Island,Event,Duration) %>% mutate(Week = row_number())

Onsets.warming<- warming %>% group_by(Island,Event,Duration) %>% 
  nest() %>% 
  mutate(
    fit = purrr::map(data, ~ lm(SST ~ Week, data = .x)),
    tidied = purrr::map(fit, tidy)
  ) %>% 
  unnest(tidied)
Onsets.warming

cooling<-cooling %>%  group_by(Island,Event,Duration) %>% mutate(Week = row_number())

Onsets.cooling<- cooling %>% group_by(Island,Event,Duration) %>% 
  nest() %>% 
  mutate(
    fit = purrr::map(data, ~ lm(SST ~ Week, data = .x)),
    tidied = purrr::map(fit, tidy)
  ) %>% 
  unnest(tidied)
Onsets.cooling

#extract rates of onset for each island and event
slopes.warming<-Onsets.warming %>% filter(term=="Week")
slopes.cooling<-Onsets.cooling %>% filter(term=="Week")
slopes.warming$estimate<-round(slopes.warming$estimate, digits = 5)
slopes.cooling$estimate<-round(slopes.cooling$estimate, digits = 5)
slopes.warming<- dplyr::select(slopes.warming,-data,-fit,-term)
slopes.cooling<- dplyr::select(slopes.cooling,-data,-fit,-term)

##### make plots comparing slopes for each time window
#plot.cols=c("turquoise3","magenta","steelblue4")
plot.cols<-c("#ffa41b","#00a8cc","#142850") 


#order the duration metrics from shortest to longest variables
warming$Duration<-ordered(warming$Duration,levels=c("wk4","wk8","wk12","DHW","Min"),
                          labels=c("4 weeks","8 Weeks",
                                   "12 Weeks","1st DHW","Min Temp prior DHW"))

cooling$Duration<-ordered(cooling$Duration,levels=c("wk4_cool","wk8_cool","wk12_cool"),
                          labels=c("4 weeks","8 weeks",
                                   "12 weeks"))

#order the islands 
warming$Island<-ordered(warming$Island,levels=c("Kanton","Rawaki","Orona","Nikumaroro"))
cooling$Island<-ordered(cooling$Island,levels=c("Kanton","Rawaki","Orona","Nikumaroro"))

### warming and cooling plots for supplement
warming.plots<-ggplot(warming,aes(x=Week,y=SST,group=Event,fill=Event,color=Event))+
  geom_point(size=2,pch=21,color="black")+
  geom_smooth(method='lm',alpha=0.25)+facet_grid(rows=vars(Island),cols=vars(Duration),scales="free")+
  scale_fill_manual(values=plot.cols,labels=c("2002","2009","2015"))+
  scale_color_manual(values=plot.cols,labels=c("2002","2009","2015"))+
  ylab("SST (°C)")+
  newtheme
warming.plots  

# setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
# ggsave(plot = warming.plots,filename = "Fig_S_warming.pdf",
#        width=8,height=6,dpi=300,useDingbats=FALSE)



cooling.plots<-ggplot(cooling,aes(x=Week,y=SST,group=Event,fill=Event,color=Event))+
  geom_point(size=2,pch=21,color="black")+
  geom_smooth(method='lm',alpha=0.25)+facet_grid(rows=vars(Island),cols=vars(Duration),scales="free")+
  scale_fill_manual(values=plot.cols,labels=c("2002","2009","2015"))+
  scale_color_manual(values=plot.cols,labels=c("2002","2009","2015"))+
  ylab("SST (°C)")+
  newtheme
cooling.plots  

# ggsave(plot = cooling.plots,filename = "Fig_S_cooling.pdf",
#        width=8,height=6,dpi=300,useDingbats=FALSE)

### do ancovas to compare slopes at each island and event for each duration metric 
### p values for interaction terms for event*week at each island are reported in supplemental figures. 
ancovas.warm<- warming %>% group_by(Island,Duration) %>% 
  nest() %>% 
  mutate(
    fit = purrr::map(data, ~ aov(SST ~ Week*Event, data = .x)),
    tidied = purrr::map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data,-fit) #drop model formula and unnecessary data column
ancovas.warm

ancovas.cool<-cooling %>% group_by(Island,Duration) %>% 
  nest() %>% 
  mutate(
    fit = purrr::map(data, ~ aov(SST ~ Week*Event, data = .x)),
    tidied = purrr::map(fit, tidy)) %>% 
  unnest(tidied) %>% 
  dplyr::select(-data,-fit) #drop model formula and unnecessary data column
ancovas.cool


test<-aov(SST ~ Week*Event, data = subset(cooling,Island=="Kanton" & Duration=="8 weeks"))
Anova(test,type="III")
TukeyHSD(test)


######################## Comparison of DHW between events and calculation of Total Hotspot
###########
########### Degree Heating Week Data and Total Hotspot (Mollica et al. 2019)
########### pathfinder 5.3 nightonly 4km 
########### Multiple sites per island with unique temp records averaged together to account for spatial variation
############ Kanton: K22, K17, K24    Niku: N11, N5, N3    Orona: O8, O17, O18, and Rawaki R2 (all sites same because so small)

### DHW data are calculated via Nathan's Matlab code and presented as both traditional and percentile derivations. Total Hotspot as a metric
### of cumulative heat stress is defined as the sum of DHW across the entire event divided by 12 weeks. This metric is calculated using the percentile
### DHW.
########################
# load traditional dhw and percentile DHW metrics
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Pathfinder DHW")
dhw.p<-read.csv(file="PIPA_DHW_percentile.csv",header=T)
dhw.t<-read.csv(file="PIPA_DHW_traditional.csv",header=T) # load traditional

#convert to long format and take island means, add island letter, and summarize by island 
P<-dhw.p %>%gather(Site,DHW,c(K22:R2)) %>% mutate(Island = substr(Site,1,1)) %>% group_by(Island,Time) %>% 
  summarize(N=sum(!is.na(DHW)),
            dhwP = mean(DHW,na.rm=T),
            sd.dhwP = sd(DHW,na.rm=T)) 

t<-dhw.t %>%gather(Site,DHW,c(K22:R2)) %>%mutate(Island = substr(Site,1,1)) %>% group_by(Island,Time) %>% 
  summarize(N=sum(!is.na(DHW)),
            dhwT = mean(DHW,na.rm=T),
            sd.dhwT = sd(DHW,na.rm=T))


#combine P and T then fix dates
dhw<-right_join(P,t,by=c("Island","Time"))
dhw[6]<-NULL
colnames(dhw)[3]<-c("N")

#break out time and create date
dhw<-cSplit(dhw,"Time","/")
colnames(dhw)[7:9]<-c("Month","Day","Year")
dhw$Date<-as.POSIXct(mdy(paste(dhw$Month,dhw$Day,dhw$Year)))+days(1)

#recode island names
dhw$Island<-dplyr::recode(dhw$Island, K = "Kanton", N = "Nikumaroro", O="Orona",R="Rawaki")

#Pick out peak DHW for all islands in both events -- mostly the same timing except for Niku and Rawaki
# Note - Niku has a slightly higher peak in 2016 during the second phase of the warming event but to be consistent with other 
# islands we have specified the first peak in 2015. 

peakDHWp<-dhw %>% filter(Year == 2002 | Year==2010| Year==2015) %>% group_by(Island,Year) %>%  slice(which.max(dhwP)) %>% dplyr::select(-c(dhwT,sd.dhwT))
peakDHWt<-dhw %>% filter(Year == 2002 | Year==2010| Year==2015) %>% group_by(Island,Year) %>%  slice(which.max(dhwT)) %>% dplyr::select(-c(dhwP,sd.dhwP))


#### Identify start and end times for each island/event to calculate total hotspot values 
#plot each event and identify appropriate dhw start time for each island using Percentile method
#start time defined as base of main peak
enso1<-filter(dhw,Year<=2003 & Year >=2001)
#ggplot(enso1,aes(Date,dhwP,color=Island))+geom_line()

#### 2002 start times
kan02start<-as.Date("2002-05-22")
nik02start<-as.Date("2002-08-21")
oro02start<-as.Date("2002-09-11")
raw02start<-as.Date("2002-05-22")

#find end times - minima in 2003 then visually make sure first reported 0 or lowest number in that year
enso1.end<-dhw %>% filter(Year == 2003) %>% group_by(Island) %>%  slice(which.min(dhwP))

kan02end<-as.Date("2003-10-08")
nik02end<-as.Date("2003-10-08")
oro02end<-as.Date("2003-08-27")
raw02end<-as.Date("2003-09-24")

enso2<-filter(dhw,Year<=2010 & Year >=2009)
#ggplot(enso2,aes(Date,dhwP,color=Island))+geom_line()

kan10start<-as.Date("2009-10-07")
nik10start<-as.Date("2009-09-23")
oro10start<-as.Date("2009-09-30")
raw10start<-as.Date("2009-10-14")

#find end times - minima in 2010 then visually make sure first reported 0 or lowest number in that year
enso2.end<-dhw %>% filter(Year == 2010) %>% group_by(Island) %>% arrange(Date) %>%  slice(which.min(dhwP))

kan10end<-as.Date("2010-08-04")
nik10end<-as.Date("2010-09-08")
oro10end<-as.Date("2010-08-11")
raw10end<-as.Date("2010-08-11")

enso3<-filter(dhw,Year<=2016 & Year >=2014)
ggplot(enso3,aes(Date,dhwP,color=Island))+geom_line()

#ENSO 3 start times
kan15start<-as.Date("2015-02-04")
nik15start<-as.Date("2014-11-19")
oro15start<-as.Date("2014-12-03")
raw15start<-as.Date("2014-12-10")

#find end times - minima in 2016 all these times are correct at first appearence of minima
enso3.end<-dhw %>% filter(Year == 2016) %>% group_by(Island) %>%  slice(which.min(dhwP))

#ENSO 2 end times 
kan15end<-as.Date("2016-10-19")
nik15end<-as.Date("2016-10-26")
oro15end<-as.Date("2016-10-12")
raw15end<-as.Date("2016-11-23")

### isolate 2002, 2010 and 2015 events for each island
### isolate 2002 and 2015 events for each island
kan<-filter(dhw,Island=="Kanton") %>% 
  filter(Date>=kan02start & Date <=kan02end | 
           Date>=kan10start & Date<=kan10end| 
           Date>=kan15start & Date<=kan15end) %>% 
  mutate(Event = ifelse(Year <=2003,"ENSO1",
                        ifelse(Year<=2010,"ENSO2","ENSO3"))) #add event factor for averaging across years

nik<-filter(dhw,Island=="Nikumaroro") %>%  
  filter(Date>=nik02start & Date <=nik02end | 
           Date>=nik10start & Date<=nik10end |
           Date>=nik15start & Date<=nik15end) %>% 
  mutate(Event = ifelse(Year <=2003,"ENSO1",
                        ifelse(Year<=2010,"ENSO2","ENSO3"))) #add event factor for averaging across years

oro<-filter(dhw,Island=="Orona")  %>% 
  filter(Date>=oro02start & Date  <=oro02end | 
           Date>=oro10start & Date <=oro10end | 
           Date>=oro15start & Date<=oro15end) %>% 
  mutate(Event = ifelse(Year <=2003,"ENSO1",
                        ifelse(Year<=2010,"ENSO2","ENSO3"))) #add event factor for averaging across years

raw<-filter(dhw,Island=="Rawaki") %>% 
  filter(Date>=raw02start & Date <=raw02end | 
           Date>=raw10start & Date <=raw10end |
           Date>=raw15start & Date <=raw15end) %>% 
  mutate(Event = ifelse(Year <=2003,"ENSO1",
                        ifelse(Year<=2010,"ENSO2","ENSO3"))) #add event factor for averaging across years

#combine them all and summarize by percentile dhw for each event
ts<-rbind(kan,nik,oro,raw)
ts<- ts %>%  group_by(Island,Event) %>% arrange(Date) %>% mutate(Week = row_number()) #add week counter for dhw plotting


TS <- ts %>% group_by(Island, Event) %>% summarize(
  hotspot = sum(dhwP)/12)

#event average across PIPA
pipa_Hotspot<-TS %>% group_by(Event) %>% summarize(hs=mean(hotspot))

#####

####### Combine all Temperature metrics into a master table 
########################
# peak temperatures for each event
peakT


# dhw P and T
peakDHWp <-peakDHWp %>% mutate(Event = ifelse(Year <=2003,"ENSO1",
                                              ifelse(Year<=2010,"ENSO2","ENSO3")))
peakDHWt <- peakDHWt %>%  mutate(Event = ifelse(Year <=2003,"ENSO1",
                                                ifelse(Year<=2010,"ENSO2","ENSO3")))
# Total hotspot (based on dhw P)
TS

#merge metrics by event and clean up metadata accordingly
#peak temps and hotspot
temps<-right_join(peakT,TS,by=c("Island","Event"))  

#add DHWs
temps<-right_join(temps,peakDHWp,c("Island","Event"))
temps<-right_join(temps,peakDHWt,c("Island","Event"))
temps<-temps %>% dplyr::select(Island,Year.x,Event,PeakTemp,hotspot,dhwP,dhwT) %>% rename(Year=Year.x)

#add biomodality coefficents
temps<-right_join(temps,bimodality_SST,by=c("Island","Event"))

########## Create full figure 2 
#####
#degree heating week plots
ts$Island<-ordered(ts$Island,levels=c("Kanton","Rawaki","Orona","Nikumaroro"))
ts$Event<-ordered(ts$Event,levels=c("ENSO3","ENSO1","ENSO2"))
#plot.cols.rev<-c("steelblue4","magenta","turquoise3") 
plot.cols.rev<-c("#142850","#ffa41b","#00a8cc") 

Fig.2A<-ggplot(ts,aes(x=Week,y=dhwP,ymin=0,ymax=dhwP,fill=Event,facets=Island))+geom_ribbon(color='black',alpha=.85)+facet_grid(.~Island)+
  ylab(bquote('DHW ('*94.4^th~perct*')'))+scale_y_continuous(breaks=seq(0,12,3),lim=c(0,13))+theme(legend.position = 'none')+
  theme(strip.text.x = element_blank())+#remove island labels from facets 
  scale_fill_manual(values=plot.cols.rev,labels=c("2015","2002","2010"))+
  #ggtitle("Degree heating weeks")+
  newtheme
Fig.2A

#total hotspot plots
TS$Island<-ordered(TS$Island,levels=c("Kanton","Rawaki","Orona","Nikumaroro"))

Fig.2B<-ggplot(TS,aes(x=Island,y=hotspot,fill=Event))+geom_bar(stat='identity',position=position_dodge(),color='black',alpha=.85)+
  ylab("Total Hotspot")+scale_y_continuous(breaks=seq(0,60,12),lim=c(0,60))+scale_fill_manual(values=plot.cols,labels=c('2002','2010','2015'))+
 # ggtitle("Cumulative thermal stress")+
  newtheme
Fig.2B


#########################################-
#########################################-
#########################################- CORAL MORTALITY ANALYSIS with different temperature metrics + cloud cover 
######

############# coral mortality in each event will be analyzed as a function of different environmental metrics using 
############# linear mixed effects models with site nested in islanda as a random effect.


##first combine temperature metrics and coral mortality data
#clean up benthic data and make a new df with only changes for each event

coral<-inner_join(change.sites,temps, by=c("Island","Event"))


## now add the cloud fraction data produced from the Cloud_analysis file. 
## cloud data is used to compare the cloudiness during the major enso events
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Cloud data")
clouds<-read.csv(file="PIPA_cloud_fractions_ENSO_events.csv",header=T)


#add clouds to coral df 
coral2<-inner_join(coral,clouds,by=c("Island","Event"))

## load PAR data for September-January (at peak heat stress)
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/PAR data")
par<-read.csv(file="PIPA_Par_8day_anomalies_Sep-Jan_allENSO.csv",header=T)

#add the monthly averaged irradiance for January (peak heat each event) and as early as data exists in 2002
par$N<-NULL

coral3<-inner_join(coral2,par,by=c("Island","Event"))

############ MODELING ENVIRONMENTAL DRIVERS OF CORAL CHANGE
####### ALL environmental 

#extract the key metrics from the main df
envi.metrics<-coral3 %>% dplyr::select(Island,Site.Num,Event,Decline,Loss,Year,PeakTemp,hotspot,dhwP,dhwT,
                                       Bimodality,Clouds.1mo,Clouds.2wk,Clouds.event,Par)



#normal distributions of variables
qqp(envi.metrics$Decline, "norm") #normality of coral cover declines looks good
qqp(envi.metrics$Loss, "norm") #normality of proportional declines is skewed due to small changes in coral that lead to large estimates of % increase

####predictors - no major deviations
qqp(envi.metrics$PeakTemp)
qqp(envi.metrics$hotspot)
qqp(envi.metrics$dhwP)
qqp(envi.metrics$dhwT)
qqp(envi.metrics$Bimodality)
qqp(envi.metrics$Clouds.1mo)
qqp(envi.metrics$Clouds.2wk)
qqp(envi.metrics$Clouds.event)
qqp(envi.metrics$Par) # transformation doesn't really improve normality only skewed at most extreme values


effectsE=NULL #compile effects estimates for 75 and 95% ci 
modsE=list() #compile all models run for export summary 
aE=NULL
bE=NULL
cE=NULL
dE=NULL

### MAKE SURE TO CHANGE THIS BASED ON # OF VARIABLES
for(i in 1:9){
  
  j=i+7 #add number to account for metadata columns
  
  ###models will be fit using REML=F because they vary in fixed effect(temp metric) and we want to compare how each fixed effect models the data
  #full models
  ## Site will not be included as a random effect as they are not uniformly sampled through time. Instead we will control for geography and island-specific
  #  trends by including island as a random effect
  
  full<-lmer(Decline~envi.metrics[[j]]+(1|Island/Site),data=envi.metrics,REML=F)
  
  modsE[[length(modsE)+1]] = full #compile models into a list
 
   #now calculate effect estimates and corresponding ci for plotting
  #method "boot" methods take a bit longer than "profile" but are more conservtive and also don't get confused around 0 and fail to report
  # a negative confidence (week4 estimate)
  eff95E<-summ(full,conf.method="boot",confint=TRUE,scale=TRUE,ci.width=0.95,pvals=TRUE)
  eff75E<-summ(full,conf.method="boot",confint=TRUE,scale=TRUE,ci.width=0.75,pvals=FALSE)
  
  #extract and organize estimates and CIs for plotting
  aE<-as.data.frame(eff95E$coeftable) #95% ci
  bE<-as.data.frame(eff75E$coeftable) #75% ci
  cE<-bE[,(2:3)] #remove only 75% ci and then combine it with 95% df
  dE<-cbind(aE,cE)
  dE$Metric <- names(envi.metrics)[j]
  colnames(dE)=c("Estimate","CI.95lower","CI.95upper","T","d.f","p","CI.75lower","CI.75upper","Metric")
  effectsE<-rbind(effectsE,dE)

} 

mod.names=colnames(envi.metrics[8:16]) #name the models by their predictor variables

#residuals of all models mostly normal
hist(residuals(modsE[[1]]))
hist(residuals(modsE[[2]]))
hist(residuals(modsE[[3]]))
hist(residuals(modsE[[4]]))
hist(residuals(modsE[[5]]))
hist(residuals(modsE[[6]]))
hist(residuals(modsE[[7]]))
hist(residuals(modsE[[8]]))
hist(residuals(modsE[[9]])) #par is the worst in terms of normal distribution but transformation doesn't help and deviation isn't horrible

#save output of all models 
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Final Analyses/Revisions")
#export_summs(modsE,scale=T,error_format = "[{conf.low}, {conf.high}],p={p.value}",model.names=mod.names,to.file="xlsx",file.name="Revised_Temp_Model_output_1.6.21.xlsx")

#now create new file with corrected p-values to combine with supplemental table
 p=NULL
 p.mod=NULL
 mod_adj=NULL

 for(i in 1:9){
  mod_adj<-summary(modsE[[i]]) #summarize each model
 
  p.mod = mod_adj$coefficients[10]
  p=rbind(p,p.mod)
}
 
 #adjust the pvalues using holms
 p.adj=stats::p.adjust(p, method = "holm", n = length(p))
 
#ammend to supplemental table
p.adj


#Effect size plot for supplemental figure S5
#fix column names for estimated effect sizes 
#remove intercept values 
library(stringr)
effectsE$names<-rownames(effectsE)
effects.envi<-effectsE %>% filter(!str_detect(names,"(Intercept)"))


###order metrics by type (normal - warming - cooling - light)
effects.envi$Metric<-ordered(effects.envi$Metric,levels=rev(c("PeakTemp","Bimodality","hotspot","dhwP","dhwT",
                                                              "Clouds.2wk","Clouds.1mo","Clouds.event","Par")),
                                                              
                             labels=rev(c("Peak Temp","Bimodality","Total Hotspot","DHW (Percent)","DHW",
                                          "Clouds 2wk peak DHW","Clouds 1mo peak DHW","Mean Cloud Cover","Irradiance")))


envi.cols=c(rep('red',5),rep('gold',4))

#plot supplemental effects plot that includes all predictor variables
ENVI.effects<-ggplot(effects.envi,aes(x=Estimate,y=Metric))+
  geom_errorbarh(mapping=aes(xmin=CI.75lower,xmax=CI.75upper),height=0,size=1,color=envi.cols)+
  geom_errorbarh(mapping=aes(xmin=CI.95lower,xmax=CI.95upper),height=0,size=.5,color=envi.cols)+ylab('')+
  geom_point(pch=21,fill=envi.cols,size=2)+
  geom_vline(xintercept=0,linetype=2)+xlab("Standardized Effect Size")+
  newtheme # + coord_flip()
ENVI.effects

setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
# ggsave(plot = ENVI.effects,filename = "Fig_S_all_effects.pdf",
#        width=4,height=6,dpi=300,useDingbats=FALSE)


########## Select main SST metrics for main text fig. 
### combine with SST metrics in fig2
select.effects<-c("Peak Temp","DHW (Percent)","DHW","Total Hotspot","Bimodality")

select<-effects.envi %>% filter(Metric %in% select.effects)

#order and rename the variables
select$Metric<-ordered(select$Metric,levels=rev(c("Peak Temp","Bimodality","Total Hotspot","DHW (Percent)","DHW")),
                       labels=rev(c("Peak SST","SST Pre-conditioning","Total Hotspot",
                                    "DHW (Percent)","DHW (Traditional)")))

Fig.2C<-ggplot(select,aes(x=Estimate,y=Metric))+
  geom_errorbarh(mapping=aes(xmin=CI.75lower,xmax=CI.75upper),height=0,size=1)+
  geom_errorbarh(mapping=aes(xmin=CI.95lower,xmax=CI.95upper),height=0,size=.5)+ylab('')+
  geom_point()+geom_vline(xintercept=0,linetype=2)+
  xlab("Standardized Effect Size")+
  #ggtitle('Estimated effect on coral mortality')+
  newtheme #+ coord_flip()
Fig.2C


########################################
####### FULL FIGURE 2 ###############
########################################
Fig.2.layout <- "
AAAACC
BBBBCC
"

quartz()
Fig.2_full<-Fig.2A+Fig.2B+Fig.2C+
  plot_layout(design=Fig.2.layout)+
  #plot_layout(guides = "collect") & theme(legend.position = 'right')+
  plot_annotation(tag_levels = 'A')
Fig.2_full

setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Final Analyses/Revisions")
 #ggsave(plot = Fig.2_full,filename = "Fig_2_revised_Jan2021.pdf",
  #      width=10,height=6,dpi=300,useDingbats=FALSE)
########################################


## Re-run hotspot model for residual plotting - Figure 1 
THmod<-lmer(Decline~hotspot+(1|Island/Site),data=envi.metrics,REML=F)
summ(THmod)
hist(residuals(THmod))

summary(lm(Decline~hotspot,data=envi.metrics))

TH.plot<-effect_plot(THmod,pred=hotspot,interval=T,plot.points=T)

#pull out components of the effects plot
p1<- ggplot_build(TH.plot)

#extract data from the plot -- specifically dfs 2 (line) and 3(ci) for the model fits
TH.fit<-as.data.frame(p1$data[2])
TH.ci<-as.data.frame(p1$data[3])

#plot the raw data plus the model fits -- don't show fit line or stats
Fig.1C<-ggplot()+
  #full model fit line
 # geom_ribbon(data=TH.ci,mapping=aes(x=TH.ci$x,ymin=TH.ci$ymin,ymax=TH.ci$ymax),fill='gray',alpha=0.25)+
 # geom_line(data=TH.fit,mapping=aes(x=TH.fit$x,TH.fit$y),lty=2)+
  geom_point(data=envi.metrics,aes(x=hotspot,y=Decline,fill=Event,shape=Island),size=3,col='black')+
  #guides(fill = guide_legend(override.aes=list(shape=21)))+
  guides(fill=FALSE)+ #turn off fill guide
  guides(shape = guide_legend(override.aes=list(fill='black')))+
  scale_shape_manual(values=c(21,24,22,23))+scale_fill_manual(values=plot.cols)+
  ### forces color into legend points that would otherwise be black due to scale shape glitch
  xlab('Total Hotspot')+ylab("Change in Live Coral Cover (%)")+
 # annotate(geom="text",x=50,y=-80, label="r^2==0.01",parse=T,size=3)+
 # annotate(geom="text",x=49.6,y=-70, label="p=0.47",parse=F,size=3)+
  newtheme
  Fig.1C

########################################
####### FULL FIGURE 1 ###############
########################################

Fig.1.layout <- "
AAAA
BBCC
"
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
quartz()
Fig.1_full<-Fig.1A+Fig.1B+Fig.1C+plot_layout(design=Fig.1.layout)+plot_annotation(tag_levels = 'A')
Fig.1_full

# ggsave(plot = Fig.1_full,filename = "Fig_1.pdf",
#        width=9,height=7,dpi=300,useDingbats=FALSE)


########################################
####### Confronting multicollinearity in our environmental perdictors using PCA
               # The model selection procedure applied here follows Graham 2003.
###############
########################################

#the different metrics of thermal stress likely interact to drive bleaching and mortality and 
# the effect of thermal stress is likely further modified by patterns of irradiance during bleaching

### here we combine all 9 environmental metrics into a PCA to evaluate their interactions and create
### orthogonal principal components that can be used in our linear modeling approach to replace the 
### highly collinear predictors. 

### First PCA with all metrics (except DHWt) -- summarize to island level to avoid repetitive data points for each site
pc.all<-envi.metrics %>% dplyr::select(-Site.Num,-Decline,-Loss,-Year,-dhwT) %>% 
  group_by(Island,Event) %>% summarize_all(mean) %>% dplyr::select(-Site)
pc.dat<-pc.all[-c(1:2)]
pc<-princomp(pc.dat,cor=T)
biplot(pc)
summary(pc)

#plot the pc using ggfortify
library(ggfortify)
pc.plot1<-autoplot(princomp(pc.all[,-c(1:2)],cor=T), data = pc.all,x=1,y=2,loadings = TRUE,loadings.colour="black",
                   loadings.label=T,loadings.label.size=3,loadings.label.colour='black',frame=F,colour = 'Event',shape='Island',size=4)+
  newtheme+scale_color_manual(values=plot.cols)  # export 535x355
pc.plot1 

# simplify the pca to only include Par with temp metrics
pc.par<-pc.all %>% dplyr::select(!starts_with("Clouds")) 

pc.dat2<-pc.par[-c(1:2)]
pc2<-princomp(pc.dat2,cor=T)
biplot(pc2)
summary(pc2) # 
loadings(pc2)

##### Now extract scores, combine with full data set and use model selection to determine which
##### principal components are most informative. 

#extract scores
scores<-as.data.frame(pc2$scores)

#combine with island scale metrics
scores.meta<-cbind(pc.par[1:2],scores[1:5]) 

envi.new<-right_join(envi.metrics,scores.meta,by=c("Island","Event"))  

mod.5<-lmer(Decline~Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+(1|Island/Site.Num),data=envi.new,REML=F)
summ(mod.5) #remove 1

mod.4<-lmer(Decline~Comp.1+Comp.2+Comp.3+Comp.5+(1|Island/Site.Num),data=envi.new,REML=F)
summ(mod.4) #remove 4

mod.3<-lmer(Decline~Comp.1+Comp.2+Comp.3+(1|Island/Site.Num),data=envi.new,REML=F)
summ(mod.3) #remove 2

mod.2<-lmer(Decline~Comp.1+Comp.3+(1|Island/Site.Num),data=envi.new,REML=F)
summ(mod.2) #remove 1

mod.1<-lmer(Decline~Comp.3+(1|Island/Site.Num),data=envi.new,REML=F)
summ(mod.1) 

AICc(mod.5,mod.4,mod.3,mod.2,mod.1) #select full m2 = pc1 and pc3


#now make second pca plot to match model 2 -- pc 1 and pc 3
pc.plot2<-autoplot(princomp(pc.par[,-c(1:2)],cor=T), data = pc.par,x=1,y=2,loadings = TRUE,loadings.colour="black",
                   loadings.label=T,loadings.label.size=3,loadings.label.colour='black',frame=F,colour = 'Event',shape='Island',size=4)+
  newtheme+scale_color_manual(values=plot.cols) # export 535x355
pc.plot2

#combine for supplemental figure
pc.plot1+pc.plot2+plot_layout(ncol=1)+plot_annotation(tag_levels = 'A')
 

# ######## Supplemental plot for Irradiance data
# ## Re-run hotspot model for residual plotting - Figure 1 
# par.mod<-lmer(Decline~Par+(1|Island),data=envi.metrics,REML=F)
# summ(par.mod)
# hist(residuals(par.mod))

# 
# par.plot<-effect_plot(par.mod,pred=Par,interval=T,plot.points=T)
# 
# #pull out components of the effects plot
# p1<- ggplot_build(par.plot)
# 
# #extract data from the plot -- specifically dfs 2 (line) and 3(ci) for the model fits
# par.fit<-as.data.frame(p1$data[2])
# par.ci<-as.data.frame(p1$data[3])
# 
# #plot the raw data plus the model fits 
# Fig.S.par<-ggplot()+
#   #full model fit line
#   geom_ribbon(data=par.ci,mapping=aes(x=par.ci$x,ymin=par.ci$ymin,ymax=par.ci$ymax),fill='gray',alpha=0.25)+
#   geom_line(data=par.fit,mapping=aes(x=par.fit$x,par.fit$y),lty=2)+
#   geom_point(data=envi.metrics,aes(x=Par,y=Decline,fill=Event,shape=Island),size=3,col='black')+
#   #guides(fill = guide_legend(override.aes=list(shape=21)))+
#   guides(fill=FALSE)+ #turn off fill guide
#   guides(shape = guide_legend(override.aes=list(fill='black')))+
#   scale_shape_manual(values=c(21,24,22,23))+scale_fill_manual(values=plot.cols)+
#   ### forces color into legend points that would otherwise be black due to scale shape glitch
#   xlab('Mean Irradiance Sept-Jan (E m-2 s-1)')+ylab("Change in Live Coral Cover (%)")+
#   annotate(geom="text",x=48.5,y=-80, label="r^2 == 0.22",parse=T,size=3)+
#   annotate(geom="text",x=48.5,y=-70, label="p < 0.01",parse=F,size=3)+
#   newtheme
# Fig.S.par
# 
# ggsave(plot = Fig.S.par,filename = "Fig_S_par.model.pdf",
#        width=6,height=4,dpi=300,useDingbats=FALSE)










####################################
####################################
## ANALYZE GENERA level patterns
setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Benthic Data/Finalized data for analysis")
gen<-read.csv(file="PIPA_Coral_2009-2018_for_analysis_MDF_07112020.csv",header=T)

##trim to only sites used in temporal examination of % coral cover
gen<-gen %>% filter(!Site.Num %in% trim)

#switch to long format remove 0s and count unique genera each year/island
gen.num<-gen %>% gather(Genus,Cover,c(Acropora:Turbinaria))
gen.num<-gen.num %>% filter(Cover>0)
gen.names<-gen.num %>% group_by(Island,Year) %>% summarize(Name=unique(Genus))
gen.count<-gen.names %>% group_by(Island,Year) %>% summarize(Count=sum(!is.na(Name)))

#collapse geneara into top 8 most abundant
gen.sum<-gen %>% summarize_at(vars(Acropora:Turbinaria),mean, na.rm = TRUE)
gen.sum<-as.data.frame(t(gen.sum))

#identify top 8 most abundant genera
top<-c("Montipora","Porites","Favia","Echinopora","Hydnophora","Pocillopora","Acropora","Pavona")
gen2<-cbind(gen[,1:4],gen[,names(gen) %in% top])

#calculate an other category
other<-gen %>% dplyr::select(-Montipora,-Porites,-Favia,-Echinopora,-Hydnophora,-Pocillopora,-Acropora,-Pavona) %>% 
  mutate(All.other=rowSums(.[5:35]))

gen3<-cbind(gen2,other$All.other)

gen3<-gen3 %>% rename(Other='other$All.other')


#calculate the relative abundance of each genera
gen.rel<-gen3 %>% gather(Genus,Cover,c(Acropora:Other)) %>%
  group_by(Island,Year,Site) %>% mutate(Total=sum(Cover,na.rm=T)) %>% 
  group_by(Island,Year,Site,Genus) %>% mutate(Rel.Prop = Cover/Total*100) 

#convert to wide format for permanova 
gen.rel2<-gen.rel %>% dplyr::select(-Cover,-Total) %>% spread(Genus,Rel.Prop)


#permanova - use relative percent cover to examine temporal variation across islands

rel.data<-gen.rel2[,-c(1:4)] #
rel.permanova<-adonis2(rel.data~Island*as.factor(Year), data=gen.rel2,permutations=9999, 
                       method="bray", sqrt.dist = TRUE)
rel.permanova # significant effect of island (p<0.001) but not interaction with time (p-0.21)

# pairwise.adonis -- all islands distinct in their relative abundance of the different taxa with
# niku and orona being most similar (p=0.07)
isl.rel<-pairwise.adonis(gen.rel2[,-c(1:4)],gen.rel2$Island)
isl.rel

#a weak time effect (p=0.06) -- pairwise test just to see where the differences are most notable
# only difference is between 2012 and 2015... but not 2018 so the changes are not persistent.
time.rel<-pairwise.adonis(gen.rel2[,-c(1:4)],gen.rel2$Year) 
time.rel

###stacked bar charts of common genera through time and across islands (supplement)

gen.mean<-gen.rel %>% group_by(Year,Genus) %>% summarize(cor=mean(Rel.Prop,na.rm=T))        #calculate mean cover by each genera across PIPA
      #calculate mean cover by each genera for all islands
#order genera
gen.mean$Genus<-ordered(gen.mean$Genus,levels=rev(c("Montipora","Porites","Favia","Echinopora","Other","Hydnophora","Pocillopora","Acropora","Pavona")))

#custom.ramp<-rev(c('#081d58',"#164373","#24698E","#328FA9","#41B6C4","#70B199","#A0AD6F","#CFA845","#FFA41B")) #original ramp
custom.ramp2<-rev(c('#04102f',"#1c5391","#397c9e","#41B6C4","#61b494","#92ae7d","#baa76f","#cb9705","#ffb62a"))

####################
####Figure 4.
####################
Fig.4<-ggplot(gen.mean,aes(x=as.factor(Year),y=cor,fill=Genus,stat="Identity"))+       #VIRIDIS PALETTE: #scale_fill_viridis_d(option='cividis',direction= -1)+
  geom_bar(stat="identity",colour="black")+scale_fill_manual(values=custom.ramp2)+                                     #Brewer: scale_fill_brewer(palette='YlGnBu')+ 
  # labs(title = "Benthic Composition")+
  labs(x="Year",y="Relative Percent Cover (%)")+newtheme
Fig.4


setwd("~/Documents/WHOI/Publications/PIPA benthic communities/drafts/Figures/Final Figures")
ggsave(plot = Fig.4,filename = "Fig_4.pdf",
       width=6,height=6,dpi=300,useDingbats=FALSE)

# supplemental figure showing all islands
gen.isl<-gen.rel %>% group_by(Year,Island,Genus) %>% summarize(cor=mean(Rel.Prop,na.rm=T))  
gen.isl$Genus<-ordered(gen.isl$Genus,levels=rev(c("Montipora","Porites","Favia","Echinopora","Other","Hydnophora","Pocillopora","Acropora","Pavona")))


Fig.4S<-ggplot(gen.isl,aes(x=as.factor(Year),y=cor,fill=Genus,stat="Identity"))+       #VIRIDIS PALETTE: #scale_fill_viridis_d(option='cividis',direction= -1)+
  geom_bar(stat="identity",colour="black")+scale_fill_manual(values=custom.ramp2)+                                     #Brewer: scale_fill_brewer(palette='YlGnBu')+
  # labs(title = "Benthic Composition")+
  labs(x="Year",y="Percent of total coral cover (%)")+facet_wrap(~Island)+
  newtheme
Fig.4S


setwd("~/Documents/WHOI/Publications/PIPA benthic communities/Analysis/Final Analyses/Revisions")
ggsave(plot = Fig.4S,filename = "Fig_4s_island_genera_supplement.pdf",
       width=6,height=6,dpi=300,useDingbats=FALSE)
