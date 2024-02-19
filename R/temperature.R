#' Extract and manage data concerning Temperature (Global Ocean Wavec Analysis and Forecast)
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_temp temperature data from copernicus
#' @return the path to the mean value of temperature for each sites
#' @export
#'

temperature <- function(arms_id, meta_and_data, data_temp, data_crypt) {
# 
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # data_temp <- targets::tar_read("data_temperature")
  # data_crypt <- targets::tar_read("data_cryptique")
  
  library(ggplot2)
  #### load data and metadata ####
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  dat <- read.csv(data_path)
  
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)] 
  meta <- read.csv(meta_path)
  
  
  #### import raster ####
  dat.ncfd <- ncdf4::nc_open(data_temp)
  
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
  lswt_array <- ncvar_get(dat.ncfd, "thetao")
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
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
   sub <- subset(lswt_obs_clean, z == DATE[i])
   
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
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
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
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
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
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
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
  
  DATE <- levels(as.factor(lswt_obs_clean$z))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
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
  DATE <- levels(as.factor(lswt_obs_clean$z))
  temp.moy <- NULL
  timeline <- NULL
  
  for (i in 1:n) {
    
    sub <- subset(lswt_obs_clean, z == DATE[i])
    
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
  

  write.csv(temp_all_sites, "outputs/data_temp_rollmean_model.csv")
  # Faire une courbe d'écart à la moyenne
  

  temp_all_sites$deviation <- rowMeans(data.frame(cbind(temp_all_sites$t.RUNA1,
                                                        temp_all_sites$t.RUNA2_3,
                                                        temp_all_sites$t.RUNA4_6,
                                                        temp_all_sites$t.RUNA7,
                                                        temp_all_sites$t.RUNA8,
                                                        temp_all_sites$t.RUNA9)))
  
  dev_RUNA1 <- temp_all_sites$t.RUNA1-temp_all_sites$deviation
  dev_RUNA2_3 <- temp_all_sites$t.RUNA2_3-temp_all_sites$deviation
  dev_RUNA4_6 <- temp_all_sites$t.RUNA4_6-temp_all_sites$deviation
  dev_RUNA7 <- temp_all_sites$t.RUNA7-temp_all_sites$deviation
  dev_RUNA8 <- temp_all_sites$t.RUNA8-temp_all_sites$deviation
  dev_RUNA9 <- temp_all_sites$t.RUNA9-temp_all_sites$deviation
  
  temp_deviation <- data.frame(day = as.Date(timeline),
                               R1 = dev_RUNA1,
                               R2_3 = dev_RUNA2_3,
                               R4_6 = dev_RUNA4_6,
                               R7 = dev_RUNA7,
                               R8 = dev_RUNA8,
                               R9 = dev_RUNA9)

  
  palette1 <- wesanderson::wes_palette("Darjeeling1", n = 5)
  palette2 <- wesanderson::wes_palette("Royal2", n = 1)
  palette3 <- wesanderson::wes_palette("Chevalier1", n = 1)

  temp_deviation$day
  jj <- ggplot()  +
    geom_line(data = temp_deviation, aes(y = R1, x = day, colour = "RUNA1"), linewidth = 1.1) + 
    geom_line(data = temp_deviation, aes(y = R2_3, x = day, colour = "RUNA2_3"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R4_6, x = day, colour = "RUNA4_6"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R7, x = day, colour = "RUNA7"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R8, x = day, colour = "RUNA8"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R9, x = day, colour = "RUNA9"), linewidth = 1.1) +
    ylab("Température deviation to the mean (°C)") +
    xlab("time") + 
    scale_color_manual(name = "Sites", values = c("RUNA1" = palette1[1], "RUNA2_3" = palette1[2], "RUNA4_6" = "purple", "RUNA7" = palette1[4], "RUNA8" = palette1[5], "RUNA9" = "darkblue"))
  
  path_to_tempcurvdev <- paste0("outputs/tempcurvdev_", arms_id,".pdf")
  ggsave(filename =  path_to_tempcurvdev, plot = jj, width = 12, height = 5)
  
  # For oral presentation JS
  vv <- ggplot()  +
    geom_line(data = temp_deviation, aes(y = R1, x = day, colour = "RUNA1"), linewidth = 1.1) + 
    geom_line(data = temp_deviation, aes(y = R2_3, x = day, colour = "RUNA2_3"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R4_6, x = day, colour = "RUNA4_6"), linewidth = 1.1) +
    geom_line(data = temp_deviation, aes(y = R7, x = day, colour = "RUNA7"), linewidth = 1.1, linetype = "longdash") +
    geom_line(data = temp_deviation, aes(y = R8, x = day, colour = "RUNA8"), linewidth = 1.1, linetype = "dotted") +
    geom_line(data = temp_deviation, aes(y = R9, x = day, colour = "RUNA9"), linewidth = 1.1) +
    ylab("Ecart à la moyenne des températures pour chaque site (°C)") +
    xlab("Temps") + 
    scale_color_manual(name = "Sites", values = c("RUNA1" = palette1[1], "RUNA2_3" = "darkblue", "RUNA4_6" = palette1[4], "RUNA7" = "forestgreen", "RUNA8" = "forestgreen", "RUNA9" = "forestgreen"))
  
  path_to_tempcurvdevoral <- paste0("outputs/tempcurvdevoral_", arms_id,".pdf")
  ggsave(filename =  path_to_tempcurvdevoral, plot = vv, width = 12, height = 5)
  
  
  # Faire des courbes de moyenne
  p1 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1), col = "blue", linewidth = 1.1) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Température (°C)") +
    xlab("time")
  
  p2 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Température (°C)")+
    xlab("time")
  
  p3 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Température (°C)")+
    xlab("time")
  
  p4 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9)) +
    ylab("Température (°C)")+
    xlab("time")
  
  p5 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = t.RUNA9))+
    ylab("Température (°C)")+
    xlab("time")
  
  p6 <- ggplot(temp_all_sites, aes(x=day))  +
    geom_line(aes(y = t.RUNA1)) + 
    geom_line(aes(y = t.RUNA2_3)) +
    geom_line(aes(y = t.RUNA4_6)) +
    geom_line(aes(y = t.RUNA7)) +
    geom_line(aes(y = t.RUNA8)) +
    geom_line(aes(y = t.RUNA9), col = "blue", linewidth = 1.1)+
    ylab("Température (°C)")+
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
  path_to_tempcurv <- paste0("outputs/tempcurv_", arms_id,".pdf")
  ggsave(filename =  path_to_tempcurv , width = 15, height = 12)
    

  #### test on delta temperature ####
  
  temp_all_sites$t.RUNA1
  rownames(temp_all_sites) <- temp_all_sites[,1]
  temp_all_sites <- temp_all_sites[,-1]
  temp_all_sites <- na.omit(temp_all_sites)
  
  temp_all_sites_dist <- dist(t(temp_all_sites), method = "manhattan")
  cluster <- hclust(temp_all_sites_dist, method = "average")
  plot(cluster)
  
  
  temp_all_sites$t.RUNA3 <- temp_all_sites$t.RUNA2_3
  temp_all_sites$t.RUNA5 <- temp_all_sites$t.RUNA4_6
  temp_all_sites$t.RUNA6 <- temp_all_sites$t.RUNA4_6
  
  colnames(temp_all_sites) <- c("RUNA1", "RUNA2", "RUNA4", "RUNA7", "RUNA8", "RUNA9", "RUNA3", "RUNA5", "RUNA6")
  colnames_sorted <- sort(colnames(temp_all_sites))
  temp_all_sites <- temp_all_sites[, colnames_sorted]
  
  write.csv(temp_all_sites, file = "data/derived-data/daily_temperature.csv", row.names = TRUE)
  
  temp_all_sites <- data.frame(cbind(temp_all_sites$RUNA1,
                                     temp_all_sites$RUNA1,
                                     temp_all_sites$RUNA1,
                                     temp_all_sites$RUNA2,
                                     temp_all_sites$RUNA2,
                                     temp_all_sites$RUNA2,
                                     temp_all_sites$RUNA3,
                                     temp_all_sites$RUNA3,
                                     temp_all_sites$RUNA3,
                                     temp_all_sites$RUNA4,
                                     temp_all_sites$RUNA4,
                                     temp_all_sites$RUNA4,
                                     temp_all_sites$RUNA5,
                                     temp_all_sites$RUNA5,
                                     temp_all_sites$RUNA5,
                                     temp_all_sites$RUNA6,
                                     temp_all_sites$RUNA6,
                                     temp_all_sites$RUNA6,
                                     temp_all_sites$RUNA7,
                                     temp_all_sites$RUNA7,
                                     temp_all_sites$RUNA7,
                                     temp_all_sites$RUNA8,
                                     temp_all_sites$RUNA8,
                                     temp_all_sites$RUNA8,
                                     temp_all_sites$RUNA9,
                                     temp_all_sites$RUNA9,
                                     temp_all_sites$RUNA9))

  
  matrix.dist.temp <- dist(t(temp_all_sites), method = "manhattan")
  
 
  data_cryptique <-  read.table(file = data_crypt, header=TRUE, sep=";", dec=",")
  class(data_cryptique)
  row.names(data_cryptique) <- data_cryptique$arms
  data_cryptique <- data_cryptique[,-1]
  data_cryptique[] <- lapply(data_cryptique, as.numeric)
  matrix.hel.crypt = vegan::decostand(data_cryptique, "hellinger")
  matrix.dist.crypt = vegan::vegdist(matrix.hel.crypt)

  
  
  
  
  aa = as.vector(matrix.dist.crypt)
  tt = as.vector(matrix.dist.temp)
  #new data frame with vectorized distance matrices
  mat = data.frame(aa,tt)
  
  mm1 = ggplot(mat, aes(y = aa, x = tt)) + 
    geom_point(size = 3, alpha = 0.5, color = "black") + 
    labs(x = "Delta SST between sites",
         y = "Cryptobenthic community dissimilarity") +
    geom_smooth(method = "lm", 
                colour = "red", 
                alpha = 0.2, 
                fill = "red") +
    theme( axis.text.x = element_text(face = "bold",
                                      colour = "black",
                                      size = 12), 
           axis.text.y = element_text(face = "bold",
                                      size = 11, 
                                      colour = "black"), 
           axis.title = element_text(face = "bold", 
                                     size = 14, 
                                     colour = "black"), 
           panel.background = element_blank(), 
           panel.border = element_rect(fill = NA,
                                       colour = "black")) 
  
  correl = vegan::mantel(matrix.dist.crypt,matrix.dist.temp, method = "pearson")
  p <- correl$signif
  if (p < 0.001) {
    significativite <- "***"
  } else if (p >= 0.001 && p < 0.01) {
    significativite <- "**"
  } else if (p >= 0.01 && p < 0.05) {
    significativite <- "*"
  } else {
    significativite <- " NS"
  }
  
  
  mm1 = mm1 + annotate(geom = "text",  x = min(mat$tt), y = max(mat$aa), label = paste0("Mantel R = ",  round(correl$statistic, 3), "; Pval = ", correl$signif,significativite),
                       color = "black", size = 5, hjust = 0, vjust = 1)
  
  mantel_name <- paste0("mantel_temp_crypt.pdf")
  mantel_path <- here::here("outputs",  mantel_name)
  
  
  ggplot2::ggsave(mantel_path, plot = mm1, width = 8, height = 8)
  
  ####mean temperature deviation on the year ####

  temp_deviation <- na.omit(temp_deviation)
  dev <- colMeans(temp_deviation[,2:ncol(temp_deviation)])
  d <- data.frame(ID = 1:27,
             d = c(rep(dev[1],3), 
                   rep(dev[2],6),
                   rep(dev[3],9),
                   rep(dev[4],3),
                   rep(dev[5],3),
                   rep(dev[6],3)))
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
  
  #### curtosis ####
  # install.packages("e1071")
  library(e1071)
  
  kurtosis_table <- data.frame(Site = character(0), Kurtosis = numeric(0))
  ?e1071::kurtosis
  
  plot(density(temp_all_sites$X5))
  
  for (col in names(temp_all_sites)) {
    kurt <- kurtosis(temp_all_sites[[col]])
    kurtosis_table <- rbind(kurtosis_table, data.frame(Site = col, Kurtosis = kurt))
  }
  
  
  tab.T <- data.frame(site = rownames(tab),
                      mean = tab.T.mean$l,
                      sd = tab.T.sd$l,
                      max = tab.T.max$l,
                      min = tab.T.min$l,
                      mean_dev = d$d,
                      kurt = kurtosis_table$Kurtosis)
  
  
  path_to_T <- paste0("outputs/temp_", arms_id, ".csv")
  
  
  write.table(tab.T, 
              file = path_to_T, 
              dec = ",", 
              sep = ";",
              row.names = FALSE)
  
  #### plot raster and points ####
  
  rast_temp_name <- paste0("rast_temp_", arms_id, ".pdf")
  rast_temp_path <- here::here("outputs", rast_temp_name)
  pdf(file =  rast_temp_path, width = 10, height = 10)
  
  raster::plot(rast.m, main = "Raster de la moyenne des temperature annuelle avec les sites RUNA (légende en m)")
  points(tab$long.pool, tab$lat.pool)
  runname <- substr(row.names(tab), 1, 5)
  text(tab$long.pool,tab$lat.pool, labels = runname, adj = 1.3, cex = 1.3 )
  
  dev.off()
  
  return(path_to_T)
} 



