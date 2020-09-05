library(stats)
library(drc) # automatic fitting of logistic and log-logistic models

setwd() # set working folder of the analysis
travel_modes<-c("biking","walking","pt")

# READ IN DATA
# -------------------------------------------------------------------------------------------------------------------------------------------------
pk_codes<-read.csv2("./../Finland_post_codes/alueryhmittely_posnro_2019_fi.csv",header=T) # load post codes of Greater Hki area
pk_codes<-pk_codes[(pk_codes$Kunnan.nimi %in% c("Helsinki","Espoo","Vantaa","Kauniainen")),] # remove other than Greater Hki Area codes

HSL_traveldata<-read.csv2("./../HSL_Liikkumistutkimus_2019_aineisto.csv",header=T)
HSL_traveldata<-HSL_traveldata[(HSL_traveldata$LP_POSTI %in% pk_codes$Postinumeroalue),]; HSL_traveldata<-HSL_traveldata[(HSL_traveldata$MP_POSTI %in% pk_codes$Postinumeroalue),]
HSL_traveldata$duration<-HSL_traveldata$KESTO/60 # seconds to minutes
HSL_recreation <- HSL_traveldata[(HSL_traveldata$LP %in% "Oma koti tai asuinpaikka"),]; HSL_recreation <- HSL_recreation[(HSL_recreation$MP %in% "Liikunta- tai ulkoilupaikka"),] # trips from home to recreation or sports area

# PUBLIC TRANSPORT
# -------------------------------------------------------------------------------------------------------------------------------------------------
# Read in travel time distribution data
pt_percentages<-HSL_recreation[(HSL_recreation$PKTAPA %in% c("Bussi","Juna","Metro","Raitiovaunu")),] # public transport includes travels by: bus, train, metro, tram
pt_percentages$time_class<-cut(pt_percentages$duration,breaks=seq(from=0,to=max(pt_percentages$duration),by=4)) # pool travel times to bins

# Crete a table of reverse cumulative frequencies
pt_percentages<-data.frame(time_class=seq(from=2,to=max(pt_percentages$duration)-2,by=4),frequency=as.numeric(table(pt_percentages$time_class)))
pt_percentages$frequency<-pt_percentages$frequency/sum(pt_percentages$frequency); pt_percentages$cumulative<-rev(cumsum(rev(pt_percentages$frequency)))

# Fit a log-logistic curve
logistic_curve_pt<-drm(cumulative~time_class,data=pt_percentages,fct=LL.3(),type="continuous") # LL.3 = log-logistic curve, L.3 = logistic curve
plot(logistic_curve_pt,type="all",log="")
saveRDS(logistic_curve_pt,"model_pt_recreation.rds")

# Plot curves
plot(y=pt_percentages$cumulative,x=pt_percentages$time_class,type="p",pch=16,bty="n",xlab="Time (min)",ylab="Proportion of travels of at least x min",main="Cumulative share of public transport times",xlim=c(0,150),cex.main=1.5,cex.lab=1.5,cex.axis=1.25)
lines(x=pt_percentages$time_class,y=predict(logistic_curve_pt,newdata=data.frame(time_class=pt_percentages$time_class),interval="none"),col=2)

# BIKING
# -------------------------------------------------------------------------------------------------------------------------------------------------
# Read in travel time distribution data
biking_percentages<-HSL_recreation[(HSL_recreation$PKTAPA %in% c("Polkupyörä")),] # include only travel mode = biking
biking_percentages$time_class<-cut(biking_percentages$duration,breaks=seq(from=0,to=max(biking_percentages$duration),by=2)) # pool into bins

# Crete a table of reverse cumulative frequencies
biking_percentages<-data.frame(time_class=seq(from=1,to=max(biking_percentages$duration)-1,by=2),frequency=as.numeric(table(biking_percentages$time_class)))
biking_percentages$frequency<-biking_percentages$frequency/sum(biking_percentages$frequency); biking_percentages$cumulative<-rev(cumsum(rev(biking_percentages$frequency)))

# Fit a logistic curve
logistic_curve_biking<-drm(cumulative~time_class,data=biking_percentages,fct=L.3(),type="continuous") # LL.3 = log-logistic curve, L.3 = logistic curve
plot(logistic_curve_biking,type="all",log="")
saveRDS(logistic_curve_biking,"model_biking_recreation.rds")

# Plot curves
plot(y=biking_percentages$cumulative,x=biking_percentages$time_class,type="p",pch=16,bty="n",xlab="Time (min)",ylab="Proportion of travels of at least x min",main="Cumulative share of biking times",xlim=c(0,50),cex.main=1.5,cex.lab=1.5,cex.axis=1.25)
lines(x=biking_percentages$time_class,y=predict(logistic_curve_biking,newdata=data.frame(time_class=biking_percentages$time_class),interval="none"),col=2)

# WALKING
# -------------------------------------------------------------------------------------------------------------------------------------------------
# Read in travel time distribution data
walking_percentages<-HSL_recreation[(HSL_recreation$PKTAPA %in% c("Kävely (sis. avustavat välineet)")),] # include only travel mode = walking
walking_percentages$time_class<-cut(walking_percentages$duration,breaks=seq(from=0,to=max(walking_percentages$duration),by=2)) # pool into bins

# Crete a table of reverse cumulative frequencies
walking_percentages<-data.frame(time_class=seq(from=1,to=max(walking_percentages$duration)-1,by=2),frequency=as.numeric(table(walking_percentages$time_class)))
walking_percentages$frequency<-walking_percentages$frequency/sum(walking_percentages$frequency); walking_percentages$cumulative<-rev(cumsum(rev(walking_percentages$frequency)))

# Fit a logistic curve
logistic_curve_walking<-drm(cumulative~time_class,data=walking_percentages,fct=L.3(),type="continuous")

plot(logistic_curve_walking,type="all",log="")
saveRDS(logistic_curve_walking,"model_walking_recreation.rds")

# Plot curves
plot(y=walking_percentages$cumulative,x=walking_percentages$time_class,type="p",pch=16,bty="n",xlab="Time (min)",ylab="Proportion of travels at least x min",main="Cumulative share of walking times",xlim=c(0,80),cex.main=1.5,cex.lab=1.5,cex.axis=1.25)
lines(x=walking_percentages$time_class,y=predict(logistic_curve_walking,newdata=data.frame(time_class=walking_percentages$time_class),interval="none"),col=2)
