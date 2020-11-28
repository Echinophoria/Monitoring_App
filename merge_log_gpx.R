merge_log_gpx <- function(log, gpxfile, ln_max, st_date){
  
  # load the GPX file.
  track <-readGPX(gpxfile, metadata = TRUE, bounds = TRUE, 
                  waypoints = TRUE, tracks = TRUE, routes = TRUE)$track
  tc <- data.frame(lon=0, lat=0, ele= 0, time=0)
  for (i in 1:length(track)){
    for (j in 1:length(track[[i]])){
      tc <- rbind(tc, track[[i]][[j]])
    }
  }
  track = tc[-1,]
  track$time<-as.POSIXct(strptime(track$time, format = '%Y-%m-%dT%H:%M:%SZ', tz='UTC'))
  
  
  ## check the number of species in the log
  sps <- table(log$species)
  if(length(sps)>1){ # simultaneous monitoring of different things
    mainSp <-  names(sps[sps==max(sps)])  # main species, always logged, even when absent
    nm <- names(sps) # all species
  }else{
    mainSp <- log[2,3]  # the first tracked is the main species
  }
  # do first the main species
  log_mainSp <- subset(log, species==mainSp)
  log_mainSp <- rbind(log[1,], log_mainSp) # adding the timestamp of the logging start
  log_mainSp$line <- seq(ln_max, (ln_max+nrow(log_mainSp)-1),1)-1
  log_mainSp <- subset(log_mainSp)
  track$ele <- NULL
  
  # merge track and log to create a lines file with properties.
  log_mainSp <- subset(log_mainSp, time>=min(track$time) & time<=max(track$time))
  
  track <- data.table(track)
  track$project <- NA 
  track$survey <- NA
  track$species <- NA
  track$log <-NA
  track$notes <- NA
  track$line <- NA
  log_mainSp <- data.table(log_mainSp)
  
  for (i in 1:nrow(log_mainSp)){
    dt = as.numeric(log_mainSp[i,'time'])
    y = match.closest(dt, as.numeric(track$time), tolerance=600)
    track$project[y] <- as.character(log_mainSp[i,1])
    track$survey[y] <- as.character(log_mainSp[i,2])
    track$species[y] <- as.character(log_mainSp[i,3])
    track$log[y] <- as.character(log_mainSp[i,4])
    track$notes[y] <- as.character(log_mainSp[i,6])
    track$line[y] <- as.numeric(log_mainSp[i,7])
  }
  
  library(tidyr)
  track <- track %>% fill(c(project, survey,species,log,notes,line), .direction = "up")
  track <- subset(track, !is.na(log))
  track <- subset(track, log != 'start_log')
  track <- subset(track, log != 'end_log')
  
  # do the secondary species if present
  if( length(names(sps))>1){ # there were several species and need to take care of the rest
    sps <- sps[names(sps)!=mainSp]
    for (i in 1:length(names(sps))){
      log2 <- subset(log, species==names(sps[i]))
      for (j in 1:nrow(log2)){ # finding the lines where this species was noted
        dt = as.numeric(log2[j,'time'])
        y = match.closest(dt, as.numeric(log_mainSp$time), tolerance=600)
        tr <- subset(track, line==as.numeric(log_mainSp[y,7]))
        tr$species <- names(sps[i])
        track <- rbind(track, tr)
      }
      
    }
  }
 
  
  return(track)
}
