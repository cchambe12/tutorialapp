### Run some simple analyses for Mike
# Started 17 March 2022 by Cat

#Perhaps the equation is something like 
# (CI expenditures)*(growth/yr) + (partner expenditure)*(growth/yr) [for n partners at each time step]… 
#where ‘growth’ is the % of the overall expenditures that are allocated on our priorities/goals?

## I'm not sure I have this sort of information... but I will do the best I can with it I suppose

set.seed(12321)

if(FALSE){
  
  cepf = "cepf"
  gef = "gef"
  worldbank = "worldbank"
  europeaid = "eu"
  dfreal = brazil
  
  
  
}


realfunc <- function(cepf, gef, worldbank, europeaid, dfreal){
  
  namestopick <- c(worldbank, gef, cepf, europeaid)
  
  txctl <- dfreal[dfreal$type%in%namestopick,]
  
  mylist <- list(txctl)
  
  return(mylist)
  
}


if(FALSE){
  
  ccpainvest = 150
  csfinvest = csfarminvest = 100
  restoreinvest = 200
  
  numccpa <- 20
  numcsf <- numcsfarm <- 20
  numrestore <- 20
  
  list <- foreaction(protectinvest, manageinvest, restoreinvest, brazil)
  
  
}



foreaction <- function(ccpainvest,csfinvest,csfarminvest,
                       restoreinvest, brazil){
  
  set.seed(12321)
  
  newbrazil <- brazil
  
  ##### Let's simulate forecasted data here:
  newbrazilbase <- subset(newbrazil, select=c("lc_ha_rate", "intervention", "pdsi",
                                              "numrds", "mst", "elev", "slope", "pas", "forest",
                                              "yrsofproject", "accum.cost", "prevforest", "year"))
  
  newbrazilbase <- newbrazilbase[!duplicated(newbrazilbase),]
  newbrazilbase <- newbrazilbase[complete.cases(newbrazilbase),]
  
  numccpa <- 20
  numcsf <- numcsfarm <- 20
  numrestore <- 20
  
  ccpaprojs <- as.numeric(numccpa) 
  csfprojs <- as.numeric(numcsf) 
  csfarmprojs <- as.numeric(numcsfarm) 
  restoreprojs <- as.numeric(numrestore)
  
  futuredf <- data.frame(intervention=c(rep("climate-critical protected areas", ccpaprojs), 
                                        rep("control climate-critical protected areas", ccpaprojs),
                                        rep("climate-smart forestry: manage", csfprojs), 
                                        rep("control climate-smart forestry: manage", csfprojs),
                                        rep("climate-smart farming", csfprojs), 
                                        rep("control climate-smart farming", csfprojs),
                                        rep("forest and wetland restoration", restoreprojs), 
                                        rep("control forest and wetland restoration", restoreprojs)))
  
  
  ccpayears <- as.numeric(rep(c(2022:2031), each=round(((ccpaprojs)/10), digits=0)))
  csfyears <- as.numeric(rep(c(2022:2031), each=round(((csfprojs)/10), digits=0)))
  csfarmyears <- as.numeric(rep(c(2022:2031), each=round(((csfarmprojs)/10), digits=0)))
  restoreyears <- as.numeric(rep(c(2022:2031), each=round(((restoreprojs)/10), digits=0)))
  
  futuredf$start <- ifelse(futuredf$intervention %in% c("climate-critical protected areas", 
                                                        "control climate-critical protected areas"),
                           ccpayears, NA)
  futuredf$start <- ifelse(futuredf$intervention %in% c("climate-smart forestry: manage", 
                                                        "control climate-smart forestry: manage"),
                           csfyears, futuredf$start)
  futuredf$start <- ifelse(futuredf$intervention %in% c("climate-smart farming", 
                                                        "control climate-smart farming"),
                           csfyears, futuredf$start)
  futuredf$start <- ifelse(futuredf$intervention %in% c("forest and wetland restoration", 
                                                        "control forest and wetland restoration"),
                           restoreyears, futuredf$start)
  
  futuredf$end <- futuredf$start + 4
  
  
  futdf <- data.frame()
  
  for(i in c(1:nrow(futuredf))) { #i=1
    
    numreps <- futuredf$end[i]-futuredf$start[i] + 1
    
    intervention <- rep(futuredf$intervention[i], each=numreps)
    start <- rep(futuredf$start[i], each=numreps)
    end <- rep(futuredf$end[i], each=numreps)
    year <- futuredf$start[i]:futuredf$end[i]
    
    fooadd <- data.frame(intervention, start, end, year)
    
    futdf <- rbind(futdf, fooadd)
    
  }
  
  
  params <- newbrazilbase[newbrazilbase$year==2020,]
  params$lc_ha_rate <- params$prevforest <- NULL
  
  #params$intervention <- ifelse(params$intervention %in% c("protect forest", "protect wetland"), "protect", params$intervention)
  #params$intervention <- ifelse(params$intervention %in% c("control protect forest", "control protect wetland"), "control protect", params$intervention)
  #params$intervention <- ifelse(params$intervention %in% c("manage forest", "manage agriculture"), "manage", params$intervention)
  #params$intervention <- ifelse(params$intervention %in% c("control manage forest", "control manage agriculture"), "control manage", params$intervention)
  #params$intervention <- ifelse(params$intervention %in% c("restore forest"), "restore", params$intervention)
  #params$intervention <- ifelse(params$intervention %in% c("control restore forest"), "control restore", params$intervention)
  
  params$year <- params$forest <- params$yrsofproject <- NULL
  
  futdf <- dplyr::left_join(futdf, params)
  futdf <- futdf[!duplicated(futdf),]
  futdf <- futdf[complete.cases(futdf),]
  
  for(i in c(1:nrow(futdf))){ #i=1
      
      futdf$mst[i] <- rnorm(1, futdf$mst[i] + 0, 0.05)
      futdf$pdsi[i] <- rnorm(1, futdf$pdsi[i] - 0, 0.05)
      #futdf$pas[i] <- rnorm(1, futdf$pas[i], 0.1)
      #futdf$numrds[i] <- rnorm(1, futdf$numrds[i], 50)
      futdf$cost[i] <- 0 #+ ((futdf$start[i]-2020)*1000)
      
  }
  
  if(TRUE){
  for(i in c(1:nrow(futdf))){
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="climate-critical protected areas",
                            futdf$cost[i] + ((ccpainvest*1000000)/ccpaprojs), 
                            futdf$cost[i])
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="climate-smart forestry: manage",
                            futdf$cost[i] + ((csfinvest*1000000)/csfprojs),
                            futdf$cost[i])
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="climate-smart farming",
                            futdf$cost[i] + ((csfarminvest*1000000)/csfarmprojs),
                            futdf$cost[i])
    
    futdf$cost[i] <- ifelse(futdf$intervention[i]=="forest and wetland restoration",
                            futdf$cost[i] + ((restoreinvest*1000000)/restoreprojs),
                            futdf$cost[i])
    
  }
  }
  
  brazil <- futdf[!duplicated(futdf),]
  
  
  brazil.stan <- subset(brazil, 
                        select=c("start", "end", "intervention", "cost", "year",
                                 "pdsi", "numrds", "mst", "elev", "slope", "pas"))
  
  
  brazil.stan <- brazil.stan %>% dplyr::group_by(start, end, intervention, cost) %>%
    dplyr::arrange(year)
  
  brazil.stan$yrsofproject <- (brazil.stan$year - brazil.stan$start) + 1
  
  brazil.stan <- brazil.stan[(brazil.stan$yrsofproject >= 0),]
  
 # Establish investments per year per project and then look at accumulated investment over time
  controls <- c("control climate-critical protected areas", "control climate-smart forestry: manage", 
                "control climate-smart farming", "control forest and wetland restoration")
  
  brazil.stan$cost <- ifelse(brazil.stan$intervention %in% controls, 0, brazil.stan$cost)
  brazil.stan$numyrs <- (brazil.stan$end - brazil.stan$start) #+ 1
  brazil.stan$costperyr <- brazil.stan$cost/brazil.stan$numyrs
  
  brazil.stan$costperyr <- ifelse(brazil.stan$year > brazil.stan$end, 0, brazil.stan$costperyr)
  brazil.stan$costperyr <- ifelse(brazil.stan$year < brazil.stan$start, 0, brazil.stan$costperyr)
  

  
  brazil.stan$accum.cost <- ave(brazil.stan$costperyr, brazil.stan$start, brazil.stan$end, brazil.stan$intervention,
                                 FUN=cumsum)
  
  
  
  
  
  #### Let's explore rate of temperature and precip changes
  brazil.stan <- brazil.stan %>%
    group_by(intervention, start, end, cost) %>% 
    arrange(year) %>%
    dplyr::mutate(prevpdsi = lag(pdsi, default = first(pdsi))) %>%
    dplyr::mutate(prevmst = lag(mst, default = first(mst)))
  
  brazil.stan <- brazil.stan[brazil.stan$yrsofproject>0,]
  
  brazil.stan$mstrate <- (brazil.stan$mst-brazil.stan$prevmst) ## Annual rate of change
  brazil.stan$pdsirate <- (brazil.stan$pdsi-brazil.stan$prevpdsi) ## annual rate of change
  
  brazilsub <- brazil.stan[brazil.stan$year <= brazil.stan$end,]
  brazilsub$yrsofproject <- ifelse(brazilsub$intervention %in% controls, 0, brazilsub$yrsofproject)
  
  brazilsub$yrs.z <- (brazilsub$yrsofproject-mean(brazilsub$yrsofproject,na.rm=TRUE))/(2*sd(brazilsub$yrsofproject,na.rm=TRUE))
  brazilsub$mst.z <- (brazilsub$mst-mean(brazilsub$mst,na.rm=TRUE))/(2*sd(brazilsub$mst,na.rm=TRUE))
  brazilsub$pdsi.z <- (brazilsub$pdsi-mean(brazilsub$pdsi,na.rm=TRUE))/(2*sd(brazilsub$pdsi,na.rm=TRUE))
  brazilsub$rds.z <- (brazilsub$numrds-mean(brazilsub$numrds,na.rm=TRUE))/(2*sd(brazilsub$numrds,na.rm=TRUE))
  brazilsub$elev.z <- (brazilsub$elev-mean(brazilsub$elev,na.rm=TRUE))/(2*sd(brazilsub$elev,na.rm=TRUE))
  brazilsub$slope.z <- (brazilsub$slope-mean(brazilsub$slope,na.rm=TRUE))/(2*sd(brazilsub$slope,na.rm=TRUE))
  brazilsub$pas.z <- (brazilsub$pas-mean(brazilsub$pas,na.rm=TRUE))/(2*sd(brazilsub$pas,na.rm=TRUE))
  
  brazilsub$acc.cost.z <- (brazilsub$accum.cost-mean(brazilsub$accum.cost,na.rm=TRUE))/(2*sd(brazilsub$accum.cost,na.rm=TRUE))
  brazilsub$mstrate.z <- (brazilsub$mstrate-mean(brazilsub$mstrate,na.rm=TRUE))/(2*sd(brazilsub$mstrate,na.rm=TRUE))
  brazilsub$prate.z <- (brazilsub$pdsirate-mean(brazilsub$pdsirate,na.rm=TRUE))/(2*sd(brazilsub$pdsirate,na.rm=TRUE))
  
  
  ##### Predict response now instead with new data...
  newdat <- brazilsub

  mylist <- list(newdat)
  
  return(mylist)
  
  
}


if(FALSE){
  
  action = "climate-critical protected areas"
  investment = 10
  start = 2022
  end = 2027
  
  rds <- 10
  pas <- 0
  mst <- 2
  pdsi <- -2
  
  list <- foreuser(action, investment, years, rds, pas, temp, pdsi, brazil)
  
  
}

foreuser <- function(action, investment, start, end, rds, pas, mst, pdsi, brazil){
  
  set.seed(12321)
  
  action <- as.character(action)
  investment <- as.numeric(investment) 
  startyr <- as.numeric(start) 
  endyr <- as.numeric(end) 
  rds <- as.numeric(rds) 
  pas <- as.numeric(pas) 
  temp <- as.numeric(mst) 
  pdsi <- as.numeric(pdsi) 
  
  newbrazil <- brazil
  
  ##### Let's simulate forecasted data here:
  newbrazilbase <- subset(newbrazil, select=c("lc_ha_rate", "intervention", "pdsi",
                                              "numrds", "mst", "elev", "slope", "pas", "forest",
                                              "yrsofproject", "accum.cost", "prevforest", "year"))
  
  newbrazilbase <- newbrazilbase[!duplicated(newbrazilbase),]
  newbrazilbase <- newbrazilbase[complete.cases(newbrazilbase),]
  
  params <- newbrazilbase
  
  intstokeep <- c(action, paste("control", action, sep=" "))
  
  params <- params[params$intervention %in% intstokeep,]
  
  futuredf <- data.frame(intervention=c(rep(action, 1), rep(paste("control", action, sep=" "), 1)))
  
  futuredf$start <- startyr
  futuredf$end <- endyr
  
  
  futdf <- data.frame()
  
  for(i in c(1:nrow(futuredf))) { #i=1
    
    numreps <- futuredf$end[i]-futuredf$start[i] + 1
    
    intervention <- rep(futuredf$intervention[i], each=numreps)
    start <- rep(futuredf$start[i], each=numreps)
    end <- rep(futuredf$end[i], each=numreps)
    year <- futuredf$start[i]:futuredf$end[i]
    
    fooadd <- data.frame(intervention, start, end, year)
    
    futdf <- rbind(futdf, fooadd)
    
  }
  
  
  params <- params[params$year==2020,]
  params$lc_ha_rate <- params$prevforest <- NULL
  
  params$year <- params$forest <- params$yrsofproject <- NULL
  
  
  futdf <- dplyr::left_join(futdf, params)
  futdf <- futdf[!duplicated(futdf),]
  futdf <- futdf[complete.cases(futdf),]
  
  for(i in c(1:nrow(futdf))){ #i=1
    
    futdf$mst[i] <- rnorm(1, futdf$mst[i] + temp, 0.05)
    futdf$pdsi[i] <- rnorm(1, futdf$pdsi[i] + pdsi, 0.05)
    futdf$pas[i] <- rnorm(1, futdf$pas[i] + pas, 0.1)
    futdf$numrds[i] <- rnorm(1, futdf$numrds[i] + rds, 50)
    futdf$cost[i] <- investment*1000000
    
  }
  
  
  brazilnew <- futdf[!duplicated(futdf),]
  
  
  brazil.stan <- subset(brazilnew, 
                        select=c("start", "end", "intervention", "cost", "year",
                                 "pdsi", "numrds", "mst", "elev", "slope", "pas"))
  
  
  brazil.stan <- brazil.stan %>% dplyr::group_by(start, end, intervention, cost) %>%
    dplyr::arrange(year)
  
  brazil.stan$yrsofproject <- (brazil.stan$year - brazil.stan$start) + 1
  
  brazil.stan <- brazil.stan[(brazil.stan$yrsofproject >= 0),]
  
  # Establish investments per year per project and then look at accumulated investment over time
  controls <- c("control climate-critical protected areas", "control climate-smart forestry: manage", 
                "control climate-smart farming", "control forest and wetland restoration")
  
  brazil.stan$cost <- ifelse(brazil.stan$intervention %in% controls, 0, brazil.stan$cost)
  brazil.stan$numyrs <- (brazil.stan$end - brazil.stan$start) #+ 1
  brazil.stan$costperyr <- (brazil.stan$cost/brazil.stan$numyrs)
  
  brazil.stan$costperyr <- ifelse(brazil.stan$year > brazil.stan$end, 0, brazil.stan$costperyr)
  brazil.stan$costperyr <- ifelse(brazil.stan$year < brazil.stan$start, 0, brazil.stan$costperyr)
  
  intstokeep <- c("climate-critical protected areas", "climate-smart farming",
                  "climate-smart forestry:manage", "forest and wetland restoration")
  
  for(i in c(1:nrow(brazil.stan))) { #i=1
    brazil.stan$costperyr[i] <- ifelse(brazil.stan$intervention[i] %in% intstokeep,
                                       brazil.stan$costperyr[i] + rnorm(1, 100, 5), brazil.stan$costperyr[i])
  }
  
  
  brazil.stan$accum.cost <- ave(brazil.stan$costperyr, brazil.stan$start, brazil.stan$end, brazil.stan$intervention,
                                FUN=cumsum)
  
  
  #### Let's explore rate of temperature and precip changes
  brazil.stan <- brazil.stan %>%
    group_by(intervention, start, end, cost) %>% 
    arrange(year) %>%
    dplyr::mutate(prevpdsi = lag(pdsi, default = first(pdsi))) %>%
    dplyr::mutate(prevmst = lag(mst, default = first(mst)))
  
  brazil.stan <- brazil.stan[brazil.stan$yrsofproject>0,]
  
  brazil.stan$mstrate <- (brazil.stan$mst-brazil.stan$prevmst) ## Annual rate of change
  brazil.stan$pdsirate <- (brazil.stan$pdsi-brazil.stan$prevpdsi) ## annual rate of change
  
  brazilsub <- brazil.stan[brazil.stan$year <= brazil.stan$end,]
  brazilsub$yrsofproject <- ifelse(brazilsub$intervention %in% controls, 0, brazilsub$yrsofproject)
  
  ### Adding in 18 May 2022 to fix z-score values
  brazilsub$flag <- "new"
  oldbrazil <- newbrazil
  oldbrazil$flag <- "old"
  
  brazilsub <- full_join(brazilsub, oldbrazil)
  
  brazilsub$yrs.z <- (brazilsub$yrsofproject-mean(brazilsub$yrsofproject,na.rm=TRUE))/(2*sd(brazilsub$yrsofproject,na.rm=TRUE))
  brazilsub$mst.z <- (brazilsub$mst-mean(brazilsub$mst,na.rm=TRUE))/(2*sd(brazilsub$mst,na.rm=TRUE))
  brazilsub$pdsi.z <- (brazilsub$pdsi-mean(brazilsub$pdsi,na.rm=TRUE))/(2*sd(brazilsub$pdsi,na.rm=TRUE))
  brazilsub$rds.z <- (brazilsub$numrds-mean(brazilsub$numrds,na.rm=TRUE))/(2*sd(brazilsub$numrds,na.rm=TRUE))
  brazilsub$elev.z <- (brazilsub$elev-mean(brazilsub$elev,na.rm=TRUE))/(2*sd(brazilsub$elev,na.rm=TRUE))
  brazilsub$slope.z <- (brazilsub$slope-mean(brazilsub$slope,na.rm=TRUE))/(2*sd(brazilsub$slope,na.rm=TRUE))
  brazilsub$pas.z <- (brazilsub$pas-mean(brazilsub$pas,na.rm=TRUE))/(2*sd(brazilsub$pas,na.rm=TRUE))
  
  brazilsub$acc.cost.z <- (brazilsub$accum.cost-mean(brazilsub$accum.cost,na.rm=TRUE))/(2*sd(brazilsub$accum.cost,na.rm=TRUE))
  brazilsub$mstrate.z <- (brazilsub$mstrate-mean(brazilsub$mstrate,na.rm=TRUE))/(2*sd(brazilsub$mstrate,na.rm=TRUE))
  brazilsub$prate.z <- (brazilsub$pdsirate-mean(brazilsub$pdsirate,na.rm=TRUE))/(2*sd(brazilsub$pdsirate,na.rm=TRUE))
  
  brazilsub <- brazilsub[!brazilsub$flag=="old",]
  
  ##### Predict response now instead with new data...
  newdat <- brazilsub
  
  mylist <- list(newdat)
  
  return(mylist)
  
  
}

