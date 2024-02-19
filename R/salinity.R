#' Extract and manage data concerning Salinity (Global Ocean Wavec Analysis and Forecast)
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_salin salinity data from copernicus
#' @return the path to the mean value of salinity for each sites
#' @export
#'

salinity <- function(arms_id, meta_and_data, data_salin) {
  
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # data_salin <- targets::tar_read("data_salinity")

  
  #### load data and metadata ####
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  dat <- read.csv(data_path)
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)
  
  #### import raster ####
  dat.ncfd <- ncdf4::nc_open(data_salin)

  arms_name <- meta$arms_name
  latitude <- as.numeric(meta$latitude)
  longitude <- as.numeric(meta$longitude)
  tab <- as.data.frame(cbind(arms_name, latitude, longitude))
  lat.pool <- tapply(latitude, arms_name, mean)
  long.pool <- tapply(longitude, arms_name, mean)
  tab <- as.data.frame(cbind(long.pool,lat.pool,levels(arms_name)))
  tab[27,2] = -21.38 
  tab[26,2] = -21.38
  tab[25,2] = -21.38
  tab[24,1] = 55.455
  tab[23,1] = 55.455
  tab[22,1] = 55.455

  library(ncdf4)
  attributes(dat.ncfd$var)
  attributes(dat.ncfd$dim)
  lat <- ncdf4::ncvar_get(dat.ncfd, "latitude")
  long <- ncdf4::ncvar_get(dat.ncfd, "longitude")
  time <- ncdf4::ncvar_get(dat.ncfd, "time")
  # depth <- ncdf4::ncvar_get(dat.ncfd, "depth")
  head(time)
  tunits <- ncatt_get(dat.ncfd, "time", "units") #check units
  nt <- dim(time) #should be 2622
  lswt_array <- ncvar_get(dat.ncfd, "sob")
  dim(lswt_array)
  time_obs <- as.POSIXct(time*60*60, origin = "1950-01-01", tz="GMT")
  range(time_obs)
  
  lonlattime <- as.matrix(expand.grid(long,lat,time_obs))
  lswt_vec_long <- as.vector(lswt_array)
  lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
  colnames(lswt_obs) <- c("x","y","t","l")
  lswt_obs$l <- as.numeric(lswt_obs$l)
  lswt_obs_clean <- na.omit(lswt_obs)

  library(ggplot2)
  n <- length(levels(as.factor(lswt_obs_clean$t)))
  
  ####curve RUNA1####
  
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[1,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA1 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  
  
  
  #### curve RUNA2&3 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[4,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA2_3 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  
  
  #### curve RUNA 4-6 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[14,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA4_6 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  #### curve RUNA 7 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[20,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA7 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  #### curve RUNA 8 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[23,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA8 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  #### curve RUNA 9 ####
  DATE <- levels(as.factor(lswt_obs_clean$t))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, t == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.temp <- raster::extract(rast.moy, tab[27,], df = TRUE)
    
    temp.moy[i] <- rast.temp$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  temp.moy <- as.numeric(unlist(temp.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  T.RUNA9 <- as.data.frame(cbind(temp.moy, as.Date(timeline)))
  
  #### Extract data ####
  
  data.T <- data.frame(cbind(T.RUNA1$temp.moy,T.RUNA2_3$temp.moy, T.RUNA4_6$temp.moy, T.RUNA7$temp.moy, T.RUNA8$temp.moy, T.RUNA9$temp.moy, timeline))
  colnames(data.T) <- c("RUNA1", "RUNA2_3", "RUNA4_6", "RUNA7", "RUNA8", "RUNA9", "date")
  write.csv(data.T, "outputs/data_temperature.csv")
  
  #### plot the curves ####
  
  #Appliquer la fenetre de 7 jours
  T.RUNA1$temp.smoothed <- zoo::rollmean(T.RUNA1$temp.moy, k=7, align = "center", fill = NA )
  T.RUNA2_3$temp.smoothed <- zoo::rollmean(T.RUNA2_3$temp.moy, k=7, align = "center", fill = NA )
  T.RUNA4_6$temp.smoothed <- zoo::rollmean(T.RUNA4_6$temp.moy, k=7, align = "center", fill = NA )
  T.RUNA7$temp.smoothed <- zoo::rollmean(T.RUNA7$temp.moy, k=7, align = "center", fill = NA )
  T.RUNA8$temp.smoothed <- zoo::rollmean(T.RUNA8$temp.moy, k=7, align = "center", fill = NA )
  T.RUNA9$temp.smoothed <- zoo::rollmean(T.RUNA9$temp.moy, k=7, align = "center", fill = NA )
  
  
  
  
  temp_all_sites <-  data.frame(day = as.Date(timeline),
                                t.RUNA1 = T.RUNA1$temp.smoothed,
                                t.RUNA2_3 = T.RUNA2_3$temp.smoothed,
                                t.RUNA4_6 = T.RUNA4_6$temp.smoothed,
                                t.RUNA7 = T.RUNA7$temp.smoothed,
                                t.RUNA8 = T.RUNA8$temp.smoothed,
                                t.RUNA9 = T.RUNA9$temp.smoothed)
  
  
  p1 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1), col = "blue", linewidth = 1.1) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Salinity (psu)") +
    xlab("time")
  
  p2 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Salinity (psu)")+
    xlab("time")
  
  p3 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Salinity (psu)")+
    xlab("time")
  
  p4 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Salinity (psu)")+
    xlab("time")
  
  p5 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA9))+
    ylab("Salinity (psu)")+
    xlab("time")
  
  p6 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9), col = "blue", linewidth = 1.1)+
    ylab("Salinity (psu)")+
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
  path_to_salcurv <- paste0("outputs/salcurv_", arms_id,".pdf")
  ggsave(filename =  path_to_salcurv , width = 15, height = 12)
  
  
  
  
  
  
  #### mean Temperature on the year ####
  m <- aggregate(l~x+y, lswt_obs_clean, mean)
  rast.m <- raster::rasterFromXYZ(m, digits = 3)
  tab.sal.mean <- raster::extract(rast.m, tab, df = TRUE)
  #### standard deviation of the temperature on the year ####
  s <- aggregate(l~x+y, lswt_obs_clean, sd)
  rast.sd <- raster::rasterFromXYZ(s, digits = 3)
  tab.sal.sd <- raster::extract(rast.sd, tab, df = TRUE)
  #### maximum temperature on the year ####
  ma <- aggregate(l~x+y, lswt_obs_clean, max)
  rast.max <- raster::rasterFromXYZ(ma, digits = 3)
  tab.sal.max <- raster::extract(rast.max, tab, df = TRUE)
  #### minimum temperature on the year ####
  mi <- aggregate(l~x+y, lswt_obs_clean, min)
  rast.min <- raster::rasterFromXYZ(mi, digits = 3)
  tab.sal.min <- raster::extract(rast.min, tab, df = TRUE)
  
  
  tab.sal <- as.data.frame(cbind(tab.sal.mean$l,
                                 tab.sal.sd$l,
                                 tab.sal.max$l,
                                 tab.sal.min$l))
  
  tab.sal <- as.data.frame(cbind(row.names(tab),tab.sal))
  
  colnames(tab.sal) <- c("site","mean", "sd", "max", "min")
  
  path_to_sal <- paste0("outputs/salin_", arms_id, ".csv")
  
  write.table(tab.sal, 
              file = path_to_sal, 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  #### plot raster and points ####
  
  rast_sal_name <- paste0("rast_sal_", arms_id, ".pdf")
  rast_sal_path <- here::here("outputs", rast_sal_name)
  pdf(file =  rast_sal_path, width = 10, height = 10)
  
  raster::plot(rast.m, main = "Raster de la salinité moyenne annuelle (légende en practical salinity unit)")
  points(tab$long.pool,tab$lat.pool)
  runname <- substr(row.names(tab), 1, 5)
  text(tab$long.pool,tab$lat.pool, labels = runname, adj = 1.3, cex = 1.3 )
  
  dev.off()
  
  return(path_to_sal)
} 
