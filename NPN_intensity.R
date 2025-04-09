# status intensity data analysis - USA NPN data
# MAC 11/18/20

library(rnpn)

npnData<-npn_download_status_data(
                                request_source="MCButtFace",
                                years=c(2010:2021),
                                states = c("AZ"),
                                additional_fields = c("Observed_Status_Conflict_Flag","Partner_Group"))

# subset only plants
npnDatasub<-subset(npnData, kingdom=="Plantae")
npnDatasub<-subset(npnDatasub, phenophase_description=="Open flowers" | phenophase_description=="Open flowers (1 location)" |
                phenophase_description=="Open flowers (grasses/sedges)" | phenophase_description=="Open flowers (lilac)")
npnDatasub<-subset(npnDatasub, observed_status_conflict_flag=="-9999")

npnDatasub$observation_date<-as.Date(npnDatasub$observation_date, format="%Y-%m-%d")
npnDatasub$year<-as.numeric(format(npnDatasub$observation_date, "%Y"))

save(npnDatasub, file = "NPN_flowering_AZ_2010_2021.RData")
load("NPN_flowering_AZ_2010_2021.RData")

pPhase<- as.data.frame(table(npnDatasub$phenophase_description)) 
spp<-as.data.frame(table(npnDatasub$common_name))

# plotting for maps and subsetting
library(sp)
library(raster)
xy <- npnDatasub[,c(6,5)]

spNpnData <- sp::SpatialPointsDataFrame(coords = xy, data = npnDatasub,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Tucson area -111.361267,-110.529053,31.896168,32.625887
# arizona 
npnTucson<-crop(spNpnData, extent(-111.361267,-110.529053,31.896168,32.625887))
npnTucson<-npnTucson@data

# look at time series counts
npnTucson$observation_date<-as.Date(npnTucson$observation_date,"%Y-%m-%d")
npnTucson$year<-as.numeric(format(npnTucson$observation_date,"%Y"))
name_year<-as.data.frame(table(npnTucson$common_name, npnTucson$year))
sppTucson<-as.data.frame(table(npnTucson$common_name))


# plot up heat map of single year status 
temp<-subset(npnTucson, year==2021)
temp<-subset(npnTucson, common_name=="saguaro") # saguaro, ocotillo, desert zinnia, creosote bush
temp<-temp[,c("individual_id","phenophase_description","phenophase_status","observation_date")]
library(ggplot2)
ggplot(temp, aes(x = observation_date, as.factor(individual_id))) +
        geom_tile(aes(fill = as.factor(phenophase_status)))


# DOY of yes~spp
temp<-subset(npnDatasub, phenophase_status==1)
ggplot(temp, aes(as.factor(common_name),day_of_year))+
        geom_boxplot(varwidth = TRUE)


test<- npnDatasub %>% group_by(common_name) %>% summarize(medDOY=median(day_of_year),
                                                          n=n())

# look at all data by spp
temp<-subset(npnDatasub, common_name=="ocotillo") 

# status yes
flwr<-subset(temp, phenophase_status==1)
        ggplot(flwr, aes(as.factor(year),day_of_year))+
                geom_boxplot(varwidth = TRUE)

# map data
library(dplyr)
sites<- temp %>%  group_by(site_id) %>% summarize(latitude=min(latitude),
                                                 longitude=min(longitude),
                                                 count=n())
library(leaflet)
library(htmltools)
pal <- colorNumeric(
        palette = "YlOrRd",
        domain = sites$count)

leaflet(data = sites) %>% addTiles() %>%
        addCircleMarkers(~longitude, ~latitude, color = ~pal(count), label = ~htmlEscape(count))

temp<-temp[,c("individual_id","phenophase_description","phenophase_status","observation_date")]
library(ggplot2)
ggplot(temp, aes(x = observation_date, as.factor(individual_id))) +
        geom_tile(aes(fill = as.factor(phenophase_status)))+
        scale_fill_manual(values=c("blue","green","red"))


# plot of spp counts
library(dplyr)
sppCts<- npnDatasub %>%  group_by(common_name) %>% summarize(count=n())
sppCts<- subset(sppCts, count>=500)
ggplot(sppCts, aes(reorder(common_name,count), count))+
        geom_bar(stat = "identity")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


 # filter out case
 #indivCase<-subset(phenoData, Individual_ID==16126 & Phenophase_Description=="Open flowers")

##### CLIMATE DATA ##### 
# get climate data for spp
library(dplyr)
temp<-subset(npnDatasub, common_name=="ocotillo") 

# status yes
flwr<-subset(temp, phenophase_status==1)
#ggplot(flwr, aes(as.factor(year),day_of_year))+
#        geom_boxplot(varwidth = TRUE)

sites<- flwr %>%  group_by(site_id) %>% summarize(latitude=min(latitude),
                                                  longitude=min(longitude),
                                                  count=n())

# download data in JSON format and convert
sitelist<-list()

library(RCurl)
library(jsonlite)

for(i in 1:nrow(sites)){
        lon<-sites$longitude[i]
        lat<-sites$latitude[i]
        jsonQuery<-paste('{"loc":"',lon,',',lat,'","sdate":"2009-01-01","edate":"2020-12-31","grid":"21","elems":"maxt,mint,pcpn"}')

        out<-postForm("http://data.rcc-acis.org/GridData", 
                      .opts = list(postfields = jsonQuery, 
                                   httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
        out<-jsonlite::fromJSON(out)
        
        # get data from list
        data<-data.frame(out$data)
        colnames(data)<-c("date","t_max","t_min","precip")
        data$date<-as.Date(paste(data$date,"-01",sep=""))
        data$site_id<-sites$site_id[i]
        
        # convert columns to numeric
        unfactorize<-c("t_max","t_min","precip")
        data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))
        
        # cumulative metrics
        # calc GDDs
        data$year<-as.numeric(format(data$date, "%Y"))
        baseT<-32
        data$t_mean<-(data$t_max+data$t_min)/2
        data$baseTemp <-ifelse(data$t_mean>=baseT, data$t_mean-baseT, 0)
        # need to substract base temp...
        
        cumClim <- data %>% 
                dplyr::group_by(year) %>% # still doesn't quite work adjYear kicks off before adjDOY
                #dplyr::summarise(value = sum(tmean)) %>%
                #dplyr::mutate(csum = cumsum(value))
                dplyr::mutate(cumT = cumsum(baseTemp),
                              cumP = cumsum(precip))
        data<-cumClim

        sitelist[[i]] <- data 
        
print(sites$site_id[i])
}
# combine to master climate dataframe for climwin
FullClimData = do.call(rbind, sitelist)

# look for climate windows in flwr set
firstYes<-flwr %>% group_by(individual_id,year) %>%
        summarize(minDOY = min(day_of_year),minDate=min(observation_date))
flwr<-merge(firstYes, flwr, by.x=c("individual_id","minDate"), by.y=c("individual_id","observation_date"), all.x=FALSE)
colnames(flwr)[2]<-"observation_date"

climlist<-list()
maxPrecip<-list()
for(i in 1:nrow(flwr)){
        dataTemp<-sitelist[which(sites$site_id==flwr$site_id[i])][[1]]
        ##
        event<-which(dataTemp$date==flwr$observation_date[i])
        dataTemp<-dataTemp[(event-21):event,]
        maxPrecip[[i]]<-dataTemp[which.max(dataTemp$precip),]
        climlist[[i]]<-dataTemp
}

climData = do.call(rbind, climlist)
maxPrecip = do.call(rbind, maxPrecip)

# histograms of variables
onlyRain<-subset(climData, precip>0)
hist(onlyRain$precip)

hist(maxPrecip$precip,breaks = seq(0,3,0.1))
 
hist(firstYes$minDOY)

# phenoR with firstYes
library(pheno)
pheno<-firstYes[,c("minDOY","year","individual_id")]
R <- pheno.flm.fit(pheno)
R2<- pheno.lad.fit(pheno,limit=1000)

# 

##### climwin analysis test #####
library(climwin)
biol<-flwr[,c("individual_id","observation_date","minDOY.x","site_id")]
clim<-FullClimData[,c("date","t_mean","precip","site_id")]

doyWin <- slidingwin(xvar = list(Precip = clim$precip, Temp=clim$t_mean),
                      cdate = clim$date,
                      bdate = biol$observation_date,
                      baseline = lm(minDOY.x ~ 1, data = biol),
                      cinterval = "day",
                      range = c(180, 0),
                      #type = "absolute", refday = c(20, 05),
                      type = "relative",   
                      stat = "mean",
                      func = "lin", spatial = list(biol$site_id, clim$site_id))
head(doyWin[[1]]$Dataset)
head(doyWin[[1]]$BestModelData)
doyOutput <- doyWin[[1]]$Dataset
plotdelta(dataset = doyOutput)
plotweights(dataset = doyOutput)
plotbetas(dataset = doyOutput)
plotwin(dataset = doyOutput)
#####

# survival analysis test
library(survival)
library(survminer)

temp<-subset(npnDatasub, common_name=="creosote bush") 
temp<-temp[,c("year","day_of_year","phenophase_status","individual_id")]
temp<-subset(temp, phenophase_status!=-1)

survival_model = survival::survfit(Surv(time = day_of_year, event = phenophase_status, type='right') ~ 1, data = temp)
model_estimates = summary(survival_model)$table
#model_mean = model_estimates[5]
#model_median = model_estimates[7]

ggsurvplot(survival_model, color = "#2E9FDF",
           ggtheme = theme_minimal())


# all status up to first yes
# get climate data for spp
temp<-subset(npnDatasub, common_name=="ocotillo") 
# status yes
flwr<-subset(temp, phenophase_status==1)

# look for climate windows in flwr set
firstYes<-flwr %>% group_by(individual_id,year) %>%
        summarize(minDOY = min(day_of_year),minDate=min(observation_date))
flwr<-merge(firstYes, flwr, by.x=c("individual_id","minDate"), by.y=c("individual_id","observation_date"), all.x=FALSE)
colnames(flwr)[2]<-"observation_date"

# loop through firstYes list to create dataframe
subList<-list()
for(i in 1:nrow(firstYes)){        
        tempSubset<-subset(temp, individual_id==firstYes$individual_id[i] & year==firstYes$year[i])
        tempSubset<-tempSubset[1:which(tempSubset$day_of_year==firstYes$minDOY[i]),]
        # add in cumulative climate
        dataTemp<-sitelist[which(sites$site_id==tempSubset$site_id[1])][[1]]
        tempSubset<-merge(tempSubset, dataTemp, by.x="observation_date", by.y="date")
        #
        subList[[i]]<-tempSubset
        print(i)
}
firstYesData = do.call(rbind, subList)

firstYesData<-firstYesData[,c("observation_date","year.x","day_of_year","phenophase_status","individual_id","cumP","cumT","site_id.x","precip","t_mean")]
firstYesData<-subset(firstYesData, phenophase_status!=-1)
firstYesData<-subset(firstYesData, day_of_year<=180)

# look at climate variables from sitelist
#climlist<-list()
maxPrecip<-list()
#cumList<-list()
for(i in 1:nrow(firstYesData)){
        dataTemp<-sitelist[which(sites$site_id==firstYesData$site_id.x[i])][[1]]
        ##
        event<-which(dataTemp$date==firstYesData$observation_date[i])
        # get cumulative metrics
        #cumList[[i]]<-dataTemp[event,]
        # get max and window
        dataTemp<-dataTemp[(event-30):event,]
        dataTemp<-dataTemp[which.max(dataTemp$precip),]
        dataTemp$eventDate<-firstYesData$observation_date[i]
        dataTemp$individual_id<-firstYesData$individual_id[i]
        maxPrecip[[i]]<-dataTemp
        #climlist[[i]]<-dataTemp
}

#climData = do.call(rbind, climlist)
maxPrecip = do.call(rbind, maxPrecip)
maxPrecip<-maxPrecip[,c("eventDate","precip","individual_id")]
colnames(maxPrecip)[2]<-"maxPrecip"
firstYesData$maxPrecip<-maxPrecip$maxPrecip
#cumData = do.call(rbind, cumList)
#

#survival_model = survival::survfit(Surv(time = day_of_year, event = phenophase_status, type='right') ~ 1, data = firstYesData)
#ggsurvplot(survival_model, color = "#2E9FDF",
#           ggtheme = theme_minimal())

# add climate based factors
perc.rank<-function(x) trunc(rank(x,ties.method = "average"))/length(x)
firstYesData$percRank<-perc.rank(firstYesData$cumP)
        # names
        firstYesData$PanomName<-"normalP"
        firstYesData$PanomName[firstYesData$percRank<=0.33] <- "dry"
        firstYesData$PanomName[firstYesData$percRank>=0.66] <- "wet"
firstYesData$tempRank<-perc.rank(firstYesData$cumT)
        # names
        firstYesData$TanomName<-"normalT"
        firstYesData$TanomName[firstYesData$tempRank<=0.33] <- "cool"
        firstYesData$TanomName[firstYesData$tempRank>=0.66] <- "warm"        
        

library(ggfortify) # https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
trt_fit <- survfit(Surv(time = day_of_year, event = phenophase_status) ~ PanomName, data=firstYesData)
autoplot(trt_fit)

survival_model = survival::coxph(Surv(time = day_of_year, event = phenophase_status) ~ cumT+cumP, data = firstYesData)
        summary(survival_model)
ggadjustedcurves(survival_model, data=firstYesData)
ggforest(survival_model)
ftest<-cox.zph(survival_model)
        ggcoxzph(ftest)
#ggsurvplot(survfit(survival_model), color = "#2E9FDF",
#          ggtheme = theme_minimal(), data=firstYesData)
cox_fit <- survfit(survival_model)
#plot(cox_fit, main = "cph model", xlab="Days")
autoplot(cox_fit)

survival_model = survival::aareg(Surv(time = day_of_year, event = phenophase_status) ~ cumP+cumT, data = firstYesData)
autoplot(survival_model)

autoplot(survival_model)

# ranger random forest
library(ranger)
temp1<-subset(firstYesData, phenophase_status!=-1)
r_fit <- ranger(Surv(time = day_of_year, event = phenophase_status) ~ cumP+cumT,
                data = temp1,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(firstYesData)[1]), 20)){
        lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)
## Prediction Error = 1 - Harrell's c-index =  0.3087233

# logistic regression
model <- glm( phenophase_status~cumP, data = firstYesData, family = binomial)
        summary(model)$coef

        
library(ggplot)
        ggplot(firstYesData, aes(maxPrecip, phenophase_status)) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "glm", method.args = list(family = "binomial")) 

