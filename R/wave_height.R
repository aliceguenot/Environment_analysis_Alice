#' Extract and manage data concerning wave height (Global Ocean Wavec Analysis and Forecast)
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_w_h wave height data from copernicus
#' @return the path to the mean value of wave height for each sites
#' @export
#'
wave_height <- function(arms_id, meta_and_data, data_w_h) {
  
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # data_w_h <- targets::tar_read("data_wave_height")
  
  
  #### load data and metadata ####
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  dat <- read.csv(data_path)
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)

  
  #### import raster ####
  dat.ncfd <- ncdf4::nc_open(data_w_h)
  dat.rast <- raster::raster(data_w_h)

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
   
  library(ncdf4)
  attributes(dat.ncfd$var)
  attributes(dat.ncfd$dim)
  lat <- ncdf4::ncvar_get(dat.ncfd, "latitude")
  long <- ncdf4::ncvar_get(dat.ncfd, "longitude")
  time <- ncdf4::ncvar_get(dat.ncfd, "time")
  head(time)
  tunits <- ncatt_get(dat.ncfd, "time", "units") #check units
  nt <- dim(time) #should be 2622
  lswt_array <- ncvar_get(dat.ncfd, "VHM0")
  dim(lswt_array)
  time_obs <- as.POSIXct(time*60*60, origin = "1950-01-01", tz="GMT")
  range(time_obs)
  
  lonlattime <- as.matrix(expand.grid(long,lat,time_obs))
  lswt_vec_long <- as.vector(lswt_array)
  lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
  colnames(lswt_obs) <- c("x","y","z","l")
  lswt_obs$l <- as.numeric(lswt_obs$l)
  lswt_obs_clean <- na.omit(lswt_obs)
  
  n <- length(levels(as.factor(lswt_obs_clean$z)))
  
  ####curve RUNA1####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[1,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA1 <- as.data.frame(cbind(wave.moy, timeline))
  W.RUNA1 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA1, mean)
   
  
  #### curve RUNA2&3 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[4,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA2_3 <- as.data.frame(cbind(wave.moy, as.Date(timeline)))
  W.RUNA2_3 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA2_3, mean)
  
  
  #### curve RUNA 4-6 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[14,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA4_6 <- as.data.frame(cbind(wave.moy, as.Date(timeline)))
  W.RUNA4_6 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA4_6, mean)
  
  #### curve RUNA 7 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[20,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA7 <- as.data.frame(cbind(wave.moy, as.Date(timeline)))
  W.RUNA7 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA7, mean)
  
  #### curve RUNA 8 ####
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[23,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA8 <- as.data.frame(cbind(wave.moy, as.Date(timeline)))
  W.RUNA8 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA8, mean)
  
  #### curve RUNA 9 ####
  DATE <- levels(as.factor(lswt_obs_clean$z))
  wave.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
    moy <- aggregate(l~x+y, sub, mean)   
    
    rast.moy <- raster::rasterFromXYZ(moy, digits = 3)
    
    rast.wave <- raster::extract(rast.moy, tab[27,], df = TRUE)
    
    wave.moy[i] <- rast.wave$l
    
    timeline[i] <- DATE[i]  
    
  }
  
  wave.moy <- as.numeric(unlist(wave.moy))
  timeline <- as.vector(unlist(timeline))
  timeline <- substr(timeline, 1, 10)
  W.RUNA9 <- as.data.frame(cbind(wave.moy, as.Date(timeline)))
  W.RUNA9 <- aggregate(as.numeric(wave.moy)~timeline, W.RUNA9, mean)
  
  #### extract data ####
  
  data.W <- data.frame(cbind(W.RUNA1$`as.numeric(wave.moy)`,W.RUNA2_3$`as.numeric(wave.moy)`, W.RUNA4_6$`as.numeric(wave.moy)`, W.RUNA7$`as.numeric(wave.moy)`, W.RUNA8$`as.numeric(wave.moy)`, W.RUNA9$`as.numeric(wave.moy)`, W.RUNA1$timeline))
  colnames(data.W) <- c("RUNA1", "RUNA2_3", "RUNA4_6", "RUNA7", "RUNA8", "RUNA9", "date")
  
  write.csv(data.W, "outputs/data_wave_height.csv")
  
  #### plot the curves ####
  
  #Appliquer la fenetre de 7 jours
  W.RUNA1$wave.smoothed <- zoo::rollmean(W.RUNA1$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  W.RUNA2_3$wave.smoothed <- zoo::rollmean(W.RUNA2_3$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  W.RUNA4_6$wave.smoothed <- zoo::rollmean(W.RUNA4_6$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  W.RUNA7$wave.smoothed <- zoo::rollmean(W.RUNA7$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  W.RUNA8$wave.smoothed <- zoo::rollmean(W.RUNA8$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  W.RUNA9$wave.smoothed <- zoo::rollmean(W.RUNA9$`as.numeric(wave.moy)`, k=7, align = "center", fill = NA )
  
  
  library(ggplot2)
  
  wave_all_sites <-  data.frame(day = as.Date(W.RUNA9$timeline),
                                W.RUNA1 = W.RUNA1$wave.smoothed,
                                W.RUNA2_3 = W.RUNA2_3$wave.smoothed,
                                W.RUNA4_6 = W.RUNA4_6$wave.smoothed,
                                W.RUNA7 = W.RUNA7$wave.smoothed,
                                W.RUNA8 = W.RUNA8$wave.smoothed,
                                W.RUNA9 = W.RUNA9$wave.smoothed)
  
  # Faire une courbe d'écart à la moyenne
  
  
  wave_all_sites$deviation <- rowMeans(data.frame(cbind(wave_all_sites$W.RUNA1,
                                                        wave_all_sites$W.RUNA2_3,
                                                        wave_all_sites$W.RUNA4_6,
                                                        wave_all_sites$W.RUNA7,
                                                        wave_all_sites$W.RUNA8,
                                                        wave_all_sites$W.RUNA9)))
  dev_RUNA1 <- wave_all_sites$W.RUNA1-wave_all_sites$deviation
  dev_RUNA2_3 <- wave_all_sites$W.RUNA2_3-wave_all_sites$deviation
  dev_RUNA4_6 <- wave_all_sites$W.RUNA4_6-wave_all_sites$deviation
  dev_RUNA7 <- wave_all_sites$W.RUNA7-wave_all_sites$deviation
  dev_RUNA8 <- wave_all_sites$W.RUNA8-wave_all_sites$deviation
  dev_RUNA9 <- wave_all_sites$W.RUNA9-wave_all_sites$deviation
  
  wav_deviation <- data.frame(day = as.Date(W.RUNA9$timeline),
                               R1 = dev_RUNA1,
                               R2_3 = dev_RUNA2_3,
                               R4_6 = dev_RUNA4_6,
                               R7 = dev_RUNA7,
                               R8 = dev_RUNA8,
                               R9 = dev_RUNA9)
  
  
  palette1 <- wesanderson::wes_palette("Darjeeling1", n = 5)
  palette2 <- wesanderson::wes_palette("Royal2", n = 1)
  palette3 <- wesanderson::wes_palette("Chevalier1", n = 1)
  

  jj <- ggplot()  +
    geom_line(data = wav_deviation, aes(y = R1, x = day, colour = "RUNA1"), linewidth = 1.1) + 
    geom_line(data = wav_deviation, aes(y = R2_3, x = day, colour = "RUNA2_3"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R4_6, x = day, colour = "RUNA4_6"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R7, x = day, colour = "RUNA7"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R8, x = day, colour = "RUNA8"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R9, x = day, colour = "RUNA9"), linewidth = 1.1) +
    ylab("Wave height deviation to the mean (m)") +
    xlab("time") + 
    scale_color_manual(name = "Sites", values = c("RUNA1" = palette1[1], "RUNA2_3" = palette1[2], "RUNA4_6" = "purple", "RUNA7" = palette1[4], "RUNA8" = palette1[5], "RUNA9" = "darkblue"))
  
  path_to_wavcurvdev <- paste0("outputs/wavcurvdev_", arms_id,".pdf")
  ggsave(filename =  path_to_wavcurvdev, plot = jj, width = 12, height = 5)
  
  #pour oral
  
  vv <- ggplot()  +
    geom_line(data = wav_deviation, aes(y = R1, x = day, colour = "RUNA1"), linewidth = 1.1) + 
    geom_line(data = wav_deviation, aes(y = R2_3, x = day, colour = "RUNA2_3"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R4_6, x = day, colour = "RUNA4_6"), linewidth = 1.1) +
    geom_line(data = wav_deviation, aes(y = R7, x = day, colour = "RUNA7"), linewidth = 1.1, linetype = "longdash") +
    geom_line(data = wav_deviation, aes(y = R8, x = day, colour = "RUNA8"), linewidth = 1.1, linetype = "dotted") +
    geom_line(data = wav_deviation, aes(y = R9, x = day, colour = "RUNA9"), linewidth = 1.1) +
    ylab("Ecart à la moyenne de la hauteur de houle dans chaque site (m)") +
    xlab("Temps") + 
    scale_color_manual(name = "Sites", values = c("RUNA1" = palette1[1], "RUNA2_3" = "darkblue", "RUNA4_6" = palette1[4], "RUNA7" = "forestgreen", "RUNA8" = "forestgreen", "RUNA9" = "forestgreen"))
  
  path_to_wavcurvdevoral <- paste0("outputs/wavcurvdevoral_", arms_id,".pdf")
  ggsave(filename =  path_to_wavcurvdevoral, plot = vv, width = 12, height = 5)
  
  p1 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1), col = "blue", linewidth = 1.1) + 
    geom_line(aes(y = W.RUNA2_3)) +
    geom_line(aes(y = W.RUNA4_6)) +
    geom_line(aes(y = W.RUNA7)) +
    geom_line(aes(y = W.RUNA8)) +
    geom_line(aes(y = W.RUNA9)) +
    ylab("Wave height (m)") +
    xlab("time")
  
  p2 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1)) + 
    geom_line(aes(y = W.RUNA2_3), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = W.RUNA4_6)) +
    geom_line(aes(y = W.RUNA7)) +
    geom_line(aes(y = W.RUNA8)) +
    geom_line(aes(y = W.RUNA9)) +
    ylab("Wave height (m)")+
    xlab("time")
  
  p3 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1)) + 
    geom_line(aes(y = W.RUNA2_3)) +
    geom_line(aes(y = W.RUNA4_6), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = W.RUNA7)) +
    geom_line(aes(y = W.RUNA8)) +
    geom_line(aes(y = W.RUNA9)) +
    ylab("Wave height (m)")+
    xlab("time")
  
  p4 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1)) + 
    geom_line(aes(y = W.RUNA2_3)) +
    geom_line(aes(y = W.RUNA4_6)) +
    geom_line(aes(y = W.RUNA7), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = W.RUNA8)) +
    geom_line(aes(y = W.RUNA9)) +
    ylab("Wave height (m)")+
    xlab("time")
  
  p5 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1)) + 
    geom_line(aes(y = W.RUNA2_3)) +
    geom_line(aes(y = W.RUNA4_6)) +
    geom_line(aes(y = W.RUNA7)) +
    geom_line(aes(y = W.RUNA8), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = W.RUNA9))+
    ylab("Wave height (m)")+
    xlab("time")
  
  p6 <- ggplot(wave_all_sites, aes(x=day))  +
    geom_line(aes(y = W.RUNA1)) + 
    geom_line(aes(y = W.RUNA2_3)) +
    geom_line(aes(y = W.RUNA4_6)) +
    geom_line(aes(y = W.RUNA7)) +
    geom_line(aes(y = W.RUNA8)) +
    geom_line(aes(y = W.RUNA9), col = "blue", linewidth = 1.1)+
    ylab("Wave height (m)")+
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
  path_to_wavecurv <- paste0("outputs/wavecurv_", arms_id,".pdf")
  ggsave(filename =  path_to_wavecurv , width = 15, height = 12)
  
  
  
  
  
  
  #### mean Temperature on the year ####
  
  wav_deviation <- na.omit(wav_deviation)
  dev <- colMeans(wav_deviation[,2:ncol(wav_deviation)])
  d <- data.frame(ID = 1:27,
                  d = c(rep(dev[1],3), 
                        rep(dev[2],6),
                        rep(dev[3],9),
                        rep(dev[4],3),
                        rep(dev[5],3),
                        rep(dev[6],3)))
  
  m <- aggregate(l~x+y, lswt_obs_clean, mean)
  rast.m <- raster::rasterFromXYZ(m, digits = 3)
  tab.wave.height.mean <- raster::extract(rast.m, tab, df = TRUE)
  #### standard deviation of the temperature on the year ####
  s <- aggregate(l~x+y, lswt_obs_clean, sd)
  s
  rast.sd <- raster::rasterFromXYZ(s, digits = 3)
  tab.wave.height.sd <- raster::extract(rast.sd, tab, df = TRUE)
  #### maximum temperature on the year ####
  ma <- aggregate(l~x+y, lswt_obs_clean, max)
  ma
  rast.max <- raster::rasterFromXYZ(ma, digits = 3)
  tab.wave.height.max <- raster::extract(rast.max, tab, df = TRUE)
  #### minimum temperature on the year ####
  mi <- aggregate(l~x+y, lswt_obs_clean, min)
  mi
  rast.min <- raster::rasterFromXYZ(mi, digits = 3)
  
  tab.wave.height.min <- raster::extract(rast.min, tab, df = TRUE)
  
  tab.wave.height <- as.data.frame(cbind(tab.wave.height.mean$l,
                                         tab.wave.height.sd$l,
                                         tab.wave.height.max$l,
                                         d$d,
                                         tab.wave.height.min$l))
  
  tab.wave.height <- as.data.frame(cbind(row.names(tab),tab.wave.height))
  
  colnames(tab.wave.height) <- c("site","mean", "sd", "max", "dev", "min")
  
  path_to_wave_height <- paste0("outputs/w_h_", arms_id, ".csv")
  
  write.table(tab.wave.height, 
              file = path_to_wave_height, 
              dec = ",", 
              sep = ";",
              row.names = FALSE)

  
    #### plot raster and points ####
  
  rast_wave_name <- paste0("rast_wave_", arms_id, ".pdf")
  rast_wave_path <- here::here("outputs", rast_wave_name)
  pdf(file =  rast_wave_path, width = 10, height = 10)
  
  raster::plot(rast.m, main = "Raster de la hauteur des vagues moyenne (légende en m)")
  points(tab$long.pool,tab$lat.pool)
  runname <- substr(row.names(tab), 1, 5)
  text(tab$long.pool,tab$lat.pool, labels = runname, adj = 1.3, cex = 1.3 )
  
  dev.off()

  return(path_to_wave_height)
} 



