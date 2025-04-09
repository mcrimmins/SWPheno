# first yes data analysis - USA NPN data
# MAC 11/18/20

library(rnpn)
library(dplyr)
library(ggplot2)

npnData <- npn_download_individual_phenometrics(
  request_source="MCButtFace",
  years=c(2010:2022),
  states = c("AZ","NM"),
  additional_fields = c("Observed_Status_Conflict_Flag","Partner_Group"))

# subset only plants
npnDatasub<-subset(npnData, kingdom=="Plantae")
# flowers
npnDatasub<-subset(npnDatasub, phenophase_description=="Open flowers" | phenophase_description=="Open flowers (1 location)" |
                     phenophase_description=="Open flowers (grasses/sedges)" | phenophase_description=="Open flowers (lilac)")
# leaves
#npnDatasub<-subset(npnDatasub, phenophase_description=="Leaves" | phenophase_description=="Leaves (forbs)" |
#                     phenophase_description=="Leaves (grasses)" )

# conflict flag
npnDatasub<-subset(npnDatasub, observed_status_conflict_flag=="-9999")

# thin out multiple first y's -- only one individual_id per year for a single phenophase
npnDatasub<-npnDatasub %>% group_by(individual_id,first_yes_year) %>% filter(first_yes_doy==min(as.numeric(as.character(first_yes_doy))))

# thin out prior no's = -9999
npnDatasub<-npnDatasub[!(npnDatasub$numdays_since_prior_no==-9999),]
# thin out prior no's on value
npnDatasub<-npnDatasub[!(as.numeric(as.character(npnDatasub$numdays_since_prior_no))>15),] # change to smaller? 15?

# add dates
npnDatasub$observation_date<-as.Date(paste0(npnDatasub$first_yes_year,"-",npnDatasub$first_yes_month,"-",npnDatasub$first_yes_day),
                                     format="%Y-%m-%d")
npnDatasub$year<-as.numeric(format(npnDatasub$observation_date, "%Y"))

save(npnDatasub, file = "NPN_Firstflowering_AZ_NM_2010_2022.RData")
load("NPN_Firstflowering_AZ_NM_2010_2022.RData")
#load("NPN_Leaves_AZ_NM_2010_2020.RData")


# counts of phases and species
pPhase<- as.data.frame(table(npnDatasub$phenophase_description)) 
spp<-as.data.frame(table(npnDatasub$common_name))


# basic stats of spp
temp<-subset(npnDatasub, common_name=="soaptree yucca") 
ggplot(temp, aes(as.factor(first_yes_year),first_yes_doy))+
  geom_boxplot(varwidth = TRUE)
#


#####
# mapping of species subset
# look at all data by spp
  temp<-subset(npnDatasub, common_name=="soaptree yucca") 
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
#####  

  ##### CLIMATE DATA ##### 
  # get climate data for spp
  temp<-subset(npnDatasub, common_name=="ocotillo") 
  
  # filter out values past June 1st
  temp<-subset(temp, first_yes_doy<=181)
  #temp<-subset(temp, partner_group!="Desert Botanical Garden")
  
  #ggplot(temp, aes(as.factor(first_yes_year),first_yes_doy))+
  #  geom_boxplot(varwidth = TRUE)
  
  # status yes
  #flwr<-subset(temp, phenophase_status==1)
  #ggplot(flwr, aes(as.factor(year),day_of_year))+
  #        geom_boxplot(varwidth = TRUE)
  flwr<-temp
  
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
    jsonQuery<-paste('{"loc":"',lon,',',lat,'","sdate":"2009-01-01","edate":"2022-12-31","grid":"21","elems":"maxt,mint,pcpn"}')
    
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
    
    sitelist[[i]] <- as.data.frame(data) 
    
    print(sites$site_id[i])
  }
  # combine to master climate dataframe for climwin
  FullClimData = do.call(rbind, sitelist)
   
# look at climate variables from sitelist
  climlist<-list()
  maxPrecip<-list()
  cumList<-list()
  for(i in 1:nrow(flwr)){
    dataTemp<-sitelist[which(sites$site_id==flwr$site_id[i])][[1]]
    ##
    event<-which(dataTemp$date==flwr$observation_date[i])
    # get cumulative metrics
    cumList[[i]]<-dataTemp[event,]
    # get max and window
    dataTemp<-dataTemp[(event-21):event,]
    maxPrecip[[i]]<-dataTemp[which.max(dataTemp$precip),]
    climlist[[i]]<-dataTemp
  }
  
  climData = do.call(rbind, climlist)
  maxPrecip = do.call(rbind, maxPrecip)
  cumData = do.call(rbind, cumList)
  
  # histograms of variables
  onlyRain<-subset(climData, precip>0)
  hist(onlyRain$precip)
  hist(maxPrecip$precip,breaks = seq(0,2,0.1))
  hist(firstYes$minDOY)
  
  hist(cumData$cumT)
  cumData$doy<-as.numeric(format(cumData$date, "%j"))
  
####
  
  
#### Climate Window analysis ####  
  library(climwin)
  biol<-flwr[,c("individual_id","observation_date","first_yes_doy","site_id")]
  clim<-FullClimData[,c("date","t_mean","precip","site_id")]
  
  #biol$climate<-1
  biol$year<-as.numeric(format(biol$observation_date, "%Y"))
  
  # GDD 272 for 10/1, 181 for 1/1
  doyWin <- slidingwin(xvar = list(Precip = clim$precip),
                       cdate = clim$date,
                       bdate = biol$observation_date,
                       baseline = lm(first_yes_doy ~ 1, data = biol),
                       #baseline = lmer(first_yes_doy ~ 1 + (1|individual_id), data = biol, REML = F),
                       cinterval = "day",
                       #range = c(272, 0),type = "absolute", refday = c(30, 6),
                       type = "relative", range = c(90,0), refday = c(30,6),   
                       stat = c("max","sum"),
                       #stat = c("sum"), upper=c(50),
                       func = c("lin"), spatial = list(biol$site_id, clim$site_id))
 
  doyWin<- slidingwin(xvar = list(Temp = clim$t_mean),
                         cdate = clim$date,
                         bdate = biol$observation_date,
                         baseline = lm(first_yes_doy ~ 1, data = biol),
                         cinterval = "day",
                         range = c(181, 0),
                         type = "absolute", refday = c(30, 6),
                         #type = "relative",
                         #stat = c("sum","mean"),
                         stat = c("sum"), upper=c(50),
                         func = "lin", spatial = list(biol$site_id, clim$site_id))
  # merge results
  #output<-merge_results(doyWin, doyWinGDD)
  
  # combo results
  doyWin$combos
  summary(doyWin[[1]]$BestModel)
  
  # examine results
  head(doyWin[[1]]$Dataset) # summary output
  head(doyWin[[1]]$BestModelData) # data to plot from best model
  summary(doyWin[[1]]$BestModel)
  paste(as.Date(paste0(2000,"-",6,"-",30), format="%Y-%m-%d")-doyWin[[1]]$Dataset$WindowOpen[1]," to ",
        as.Date(paste0(2000,"-",6,"-",30), format="%Y-%m-%d")-doyWin[[1]]$Dataset$WindowClose[1])
  plot(doyWin[[1]]$BestModelData$climate, doyWin[[1]]$BestModelData$yvar)

  plotbetas(dataset = doyWin[[1]]$Dataset, arrow=TRUE)
  plotwin(dataset = doyWin[[1]]$Dataset)
  plotdelta(dataset = doyWin[[2]]$Dataset, arrow = TRUE)
  plotweights(dataset = doyWin[[1]]$Dataset)
  
  plotbest(dataset = doyWin[[1]]$Dataset,
           bestmodel = doyWin[[1]]$BestModel, 
           bestmodeldata = doyWin[[1]]$BestModelData)
  
  # single model
 doySingle <- singlewin(xvar = list(Temp = clim$t_mean),
                          cdate = clim$date,
                          bdate = biol$observation_date,
                          baseline = lm(first_yes_doy ~ 1, data = biol),
                          cinterval = "day",
                          range = c(77, 57),
                          type = "absolute", refday = c(30, 6),
                          stat = c("sum"), upper=c(50),
                          func = "lin",spatial = list(biol$site_id, clim$site_id))
  # 
  
  # randomization - very slow
  doyWinRand <- randwin(repeats = 5,
                        xvar = list(Precip = clim$precip, Temp = clim$t_mean),
                        cdate = clim$date,
                        bdate = biol$observation_date,
                        baseline = lm(first_yes_doy ~ 1, data = biol),
                        cinterval = "day",
                        range = c(365, 0),
                        type = "absolute", refday = c(30, 6),
                        #type = "relative",   
                        stat = c("sum","mean"),
                        #stat = c("sum"), upper=c(50),
                        func = "lin", spatial = list(biol$site_id, clim$site_id))
  pvalue(dataset = doyWin[[1]]$Dataset, datasetrand = doyWinRand[[1]], metric = "C", sample.size = 10)
  #
 
  plotall(dataset = doyWin[[1]]$Dataset,
          datasetrand = doyWinRand[[1]],
          bestmodel = doyWin[[1]]$BestModel, 
          bestmodeldata = doyWin[[1]]$BestModelData)
    
  # doyOutput <- doyWin[[1]]$Dataset
  # plotdelta(dataset = doyWin[[1]]$Dataset)
  # plotweights(dataset = doyOutput)
  # plotbetas(dataset = doyOutput)
  # plotwin(dataset = doyOutput)
#####
  

  
  
  
  
