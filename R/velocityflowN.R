#' Extract and manage data concerning flow velocity (Global Ocean Wavec Analysis and Forecast)
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_flow temperature data from copernicus
#' @return the path to the mean value of flow for each sites
#' @export
#'

flow_N <- function(arms_id, meta_and_data, data_flow) {
  
  #meta_and_data <- targets::tar_read("metadata_data") 
  #arms_id <- targets::tar_read("campain_id")
  #data_flow <- targets::tar_read("data_flowvelo") 
  
  library(ggplot2)
  #### load data and metadata ####
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  dat <- read.csv(data_path)
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)
  
  
  #### import raster ####
  dat.ncfd <- ncdf4::nc_open(data_flow)
  
  arms_name <- meta$arms_name
  latitude <- as.numeric(meta$latitude)
  longitude <- as.numeric(meta$longitude)
  tab <- as.data.frame(cbind(arms_name, latitude, longitude))
  lat.pool <- tapply(latitude, arms_name, mean)
  long.pool <- tapply(longitude, arms_name, mean)
  tab <- as.data.frame(cbind(long.pool,lat.pool,levels(arms_name)))
  tab[24,1] = 55.455
  tab[23,1] = 55.455
  tab[22,1] = 55.455
  tab[27,2] = -21.38 
  tab[26,2] = -21.38
  tab[25,2] = -21.38
  
  #### load ncdf ####
  library(ncdf4)
  attributes(dat.ncfd$var)
  attributes(dat.ncfd$dim)
  lat <- ncdf4::ncvar_get(dat.ncfd, "latitude")
  long <- ncdf4::ncvar_get(dat.ncfd, "longitude")
  depth <- ncdf4::ncvar_get(dat.ncfd, "depth")
  time <- ncdf4::ncvar_get(dat.ncfd, "time")
  head(time)
  tunits <- ncatt_get(dat.ncfd, "time", "units") #check units
  nt <- dim(time)
  lswt_array <- ncvar_get(dat.ncfd, "vo") #North
  dim(lswt_array)
  time_obs <- as.POSIXct(time*60*60, origin = "1950-01-01", tz="GMT")
  range(time_obs)
  length(time_obs)
  
  
  lonlattime <- as.matrix(expand.grid(long,lat,time_obs))
  lswt_vec_long <- as.vector(lswt_array)
  lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
  colnames(lswt_obs) <- c("x","y","z","l")
  lswt_obs$l <- as.numeric(lswt_obs$l)
  lswt_obs_clean <- na.omit(lswt_obs)
  
  
  n <- length(levels(as.factor(lswt_obs_clean$z)))
  
  ####curve RUNA1####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[1,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA1 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  
  
  
  #### curve RUNA2&3 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[4,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA2_3 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  
  
  #### curve RUNA 4-6 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[14,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA4_6 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  #### curve RUNA 7 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[20,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA7 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  #### curve RUNA 8 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[23,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA8 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  #### curve RUNA 9 ####
  DATE <- levels(as.factor(lswt_obs_clean$z))
  flow.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.flow <- raster::extract(rast.moy, tab[27,], df = TRUE)
    
    flow.moy[i] <- rast.flow$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  flow.moy <- as.numeric(unlist(flow.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  F.RUNA9 <- as.data.frame(cbind(flow.moy, as.Date(timeline)))
  
  #### Extract data ####
  
  data.F <- data.frame(cbind(F.RUNA1$flow.moy,F.RUNA2_3$flow.moy, F.RUNA4_6$flow.moy, F.RUNA7$flow.moy, F.RUNA8$flow.moy, F.RUNA9$flow.moy, timeline))
  colnames(data.F) <- c("RUNA1", "RUNA2_3", "RUNA4_6", "RUNA7", "RUNA8", "RUNA9", "date")
  write.csv(data.F, "outputs/data_flow.csv")
  
  #### plot the curves ####
  
  #Appliquer la fenetre de 7 jours
  F.RUNA1$flow.smoothed <- zoo::rollmean(F.RUNA1$flow.moy, k=7, align = "center", fill = NA )
  F.RUNA2_3$flow.smoothed <- zoo::rollmean(F.RUNA2_3$flow.moy, k=7, align = "center", fill = NA )
  F.RUNA4_6$flow.smoothed <- zoo::rollmean(F.RUNA4_6$flow.moy, k=7, align = "center", fill = NA )
  F.RUNA7$flow.smoothed <- zoo::rollmean(F.RUNA7$flow.moy, k=7, align = "center", fill = NA )
  F.RUNA8$flow.smoothed <- zoo::rollmean(F.RUNA8$flow.moy, k=7, align = "center", fill = NA )
  F.RUNA9$flow.smoothed <- zoo::rollmean(F.RUNA9$flow.moy, k=7, align = "center", fill = NA )
  
  
  
  
  flow_all_sites <-  data.frame(day = as.Date(timeline),
                                f.RUNA1 = F.RUNA1$flow.smoothed,
                                f.RUNA2_3 = F.RUNA2_3$flow.smoothed,
                                f.RUNA4_6 = F.RUNA4_6$flow.smoothed,
                                f.RUNA7 = F.RUNA7$flow.smoothed,
                                f.RUNA8 = F.RUNA8$flow.smoothed,
                                f.RUNA9 = F.RUNA9$flow.smoothed)
  
  p1 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1), col = "blue", linewidth = 1.1) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)") +
    xlab("time")
  
  p2 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p3 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p4 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p5 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA9))+
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p6 <- ggplot(flow_all_sites, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9), col = "blue", linewidth = 1.1)+
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  cowplot::plot_grid(p1,
                     p2,
                     p3,
                     p4,
                     p5,
                     p6,
                     labels = c("RUNA1", "RUNA2_3", "RUNA4_6","RUNA7","RUNA8", "RUNA9"),
                     ncol = 2, 
                     nrow = 3)
  path_to_flowcurv <- paste0("outputs/flowcurvN_", arms_id,".pdf")
  ggsave(filename =  path_to_flowcurv , width = 15, height = 12)
  
  
  
  
  
  #### mean Temperature on the year ####
  m <- aggregate(l~x+y, lswt_obs_clean, mean)
  rast.m <- raster::rasterFromXYZ(m, digits = 3)
  tab.T.mean <- raster::extract(rast.m, tab, df = TRUE)
  #### standard deviation of the temperature on the year ####
  s <- aggregate(l~x+y, lswt_obs_clean, sd)
  s
  rast.sd <- raster::rasterFromXYZ(s, digits = 3)
  tab.T.sd <- raster::extract(rast.sd, tab, df = TRUE)
  #### maximum temperature on the year ####
  ma <- aggregate(l~x+y, lswt_obs_clean, max)
  ma
  rast.max <- raster::rasterFromXYZ(ma, digits = 3)
  tab.T.max <- raster::extract(rast.max, tab, df = TRUE)
  #### minimum temperature on the year ####
  mi <- aggregate(l~x+y, lswt_obs_clean, min)
  mi
  rast.min <- raster::rasterFromXYZ(mi, digits = 3)
  tab.T.min <- raster::extract(rast.min, tab, df = TRUE)
  
  tab.T <- as.data.frame(cbind(tab.T.mean$l,
                               tab.T.sd$l,
                               tab.T.max$l,
                               tab.T.min$l))
  
  tab.T <- as.data.frame(cbind(row.names(tab),tab.T))
  
  colnames(tab.T) <- c("site","mean", "sd", "max", "min")
  
  path_to_FN <- paste0("outputs/flowN_", arms_id, ".csv")
  
  
  write.table(tab.T, 
              file = path_to_FN, 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  #### plot raster and points ####
  
  rast_flow_name <- paste0("rast_flowN_", arms_id, ".pdf")
  rast_flow_path <- here::here("outputs", rast_flow_name)
  pdf(file =  rast_flow_path, width = 10, height = 10)
  
  raster::plot(rast.m, main = "Raster de la moyenne courants (North) sur un an avec les sites RUNA (légende en m)")
  points(tab$long.pool, tab$lat.pool)
  runname <- substr(row.names(tab), 1, 5)
  text(tab$long.pool,tab$lat.pool, labels = runname, adj = 1.3, cex = 1.3 )
  
  dev.off()
  
  flow_all_sites_N <- flow_all_sites
  
  path_to_FN_full <- paste0("outputs/flowN_full_", arms_id, ".csv")
  
  write.table(flow_all_sites_N, 
              file = path_to_FN_full , 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  
  return(c(path_to_FN, path_to_FN_full))
} 



