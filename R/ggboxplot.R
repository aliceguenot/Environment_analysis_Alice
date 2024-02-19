#' Plot the boxplot for T, Sal, and WH, and plot the clustering for this variable set.
#'
#' @param stat_salinity salinity parameters for each site
#' @param stat_temperature temperature pameters for each site
#' @param dat_w_h wave height parameteres for each site
#' @param arms_id the ID of the arms to subset for
#'
#' @return the path to the subseted raw data file
#' @export
#'

boxplot_environment <- function(arms_id, stat_salinity, stat_temperature, stat_w_h, in_situ_t) {
  
  # in_situ_t <- targets::tar_read("in_situ_temperature")
  # stat_salinity <- targets::tar_read("sal")
  # stat_temperature <- targets::tar_read("temp")
  # stat_w_h <- targets::tar_read("wave_h")
  # arms_id <- targets::tar_read("campain_id")

  #### wave height ####
  w_h <- read.csv(stat_w_h, header = TRUE, sep = ";", dec = ",")
  w_h <- as.data.frame(w_h)
  w_h$station <- substr(w_h$site, 1, 5)
  
  w_h_mean <- aggregate(mean ~ station, w_h, mean)
  w_h_sd <- aggregate(sd ~ station, w_h, mean)
  w_h_max <- aggregate(max ~ station, w_h, mean)
  w_h_min <- aggregate(min ~ station, w_h, mean)
  w_h <- as.data.frame(cbind(w_h_mean, w_h_sd$sd, w_h_max$max, w_h_min$min))
  colnames(w_h) <- c("station","mean","sd","max","min")
  library(ggplot2)
  darj <- wesanderson::wes_palette(n=5, name="Darjeeling1")
  caval <- wesanderson::wes_palette(n=5, name="Cavalcanti1")
  col <- c(rep(darj[1],1),
           rep(darj[2],1),
           rep(darj[3],1),
           rep(darj[4],1),
           rep(darj[5],1),
           rep(caval[2],1),
           rep(caval[3],1),
           rep(caval[4],1),
           rep(caval[5],1))
  

       
                    
  a <- ggplot(w_h,aes(x = station)) +
       geom_boxplot(aes(lower = mean - sd,
                        upper = mean + sd,
                        middle = mean,
                        ymin = min,
                        ymax = max),
                   stat = "identity",
                   colour = "black",
                   fill = col,
                   alpha = 0.7) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      xlab("sites") +
      ylab("average wave height over a year (m)")
  a
  #### Temperature ####
  t <- read.csv(stat_temperature, header = TRUE, sep = ";", dec = ",")
  t <- as.data.frame(t)
  t$station <- substr(t$site, 1, 5)

  t_mean <- aggregate(mean ~ station, t, mean)
  t_sd <- aggregate(sd ~ station, t, mean)
  t_max <- aggregate(max ~ station, t, mean)
  t_min <- aggregate(min ~ station, t, mean)
  t <- as.data.frame(cbind(t_mean, t_sd$sd, t_max$max, t_min$min))
  colnames(t) <- c("station","mean","sd","max","min")
  b <- ggplot(t,aes(x = station)) +
    geom_boxplot(aes(lower = mean - sd,
                     upper = mean + sd,
                     middle = mean,
                     ymin = min,
                     ymax = max),
                 stat = "identity",
                 colour = "black",
                 fill = col,
                 alpha = 0.7) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2") + 
    xlab("sites") +
    ylab("average temperature over a year (°C)")
  
  #### salinity ####
  s <- read.csv(stat_salinity, header = TRUE, sep = ";", dec = ",")
  s <- as.data.frame(s)
  s$station <- substr(s$site, 1, 5)
  
  s_mean <- aggregate(mean ~ station, s, mean)
  s_sd <- aggregate(sd ~ station, s, mean)
  s_max <- aggregate(max ~ station, s, mean)
  s_min <- aggregate(min ~ station, s, mean)
  s <- as.data.frame(cbind(s_mean, s_sd$sd, s_max$max, s_min$min))
  colnames(s) <- c("station","mean","sd","max","min")
  
  c <- ggplot(s,aes(x = station)) +
    geom_boxplot(aes(lower = mean - sd,
                     upper = mean + sd,
                     middle = mean,
                     ymin = min,
                     ymax = max),
                 stat = "identity",
                 colour = "black",
                 fill = col,
                 alpha = 0.7) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2") +
    xlab("sites") +
    ylab("average salinity over a year (psu)")
  
  #### import Temperature insitu data ####
  in_situ_temperature <- read.csv(in_situ_t, header = TRUE, sep = ";", dec = ",")
  
  i_s_t_summer <- subset(in_situ_temperature, in_situ_temperature$season == c("summer","winter")) 
  
  mean1 <- mean(i_s_t_summer$mean.RUNA1)
  mean2 <- mean(i_s_t_summer$mean.RUNA2)
  mean3 <- mean(i_s_t_summer$mean.RUNA2)
  mean8 <- mean(i_s_t_summer$mean.RUNA8)
  mean9 <- mean(i_s_t_summer$mean.RUNA9)
  sd1 <- sd(i_s_t_summer$mean.RUNA1)
  sd2 <- sd(i_s_t_summer$mean.RUNA2)
  sd3 <- sd(i_s_t_summer$mean.RUNA3)
  sd8 <- sd(i_s_t_summer$mean.RUNA8)
  sd9 <- sd(i_s_t_summer$mean.RUNA9)
  max1 <- max(i_s_t_summer$mean.RUNA1)
  max2 <- max(i_s_t_summer$mean.RUNA2)
  max3 <- max(i_s_t_summer$mean.RUNA3)
  max8 <- max(i_s_t_summer$mean.RUNA8)
  max9 <- max(i_s_t_summer$mean.RUNA9)
  min1 <- min(i_s_t_summer$mean.RUNA1)
  min2 <- min(i_s_t_summer$mean.RUNA2)
  min3 <- min(i_s_t_summer$mean.RUNA3)
  min8 <- min(i_s_t_summer$mean.RUNA8)
  min9 <- min(i_s_t_summer$mean.RUNA9)
  summer_df <- as.data.frame(cbind(rbind(mean1,mean2,mean3,mean8,mean9),rbind(sd1,sd2,sd3,sd8,sd9),rbind(max1,max2,max3,max8,max9),rbind(min1,min2,min3,min8,min9)))
  rownames(summer_df) <- c("RUNA1","RUNA2","RUNA3","RUNA8","RUNA9")
  colnames(summer_df) <- c("mean","sd","max","min")

  #### plot insitu temperature ####
  col.2 <- c(rep(darj[1],1),
           rep(darj[2],1),
           rep(darj[3],1),
           rep(darj[4],1),
           rep(darj[5],1))
  
  d <- ggplot(summer_df,aes(x = rownames(summer_df))) +
    geom_boxplot(aes(lower = mean - sd,
                     upper = mean + sd,
                     middle = mean,
                     ymin = min,
                     ymax = max),
                 stat = "identity",
                 colour = "black",
                 fill = col.2,
                 alpha = 0.7) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2") +
    xlab("sites") +
    ylab("average daily T° measured by in situ thermographs (°C)")
  
  #### Graph #####
  xdat <- as.numeric(row.names(in_situ_temperature))
  
  columns_to_process <- c("mean.RUNA1", "mean.RUNA2", "mean.RUNA3", "mean.RUNA8", "mean.RUNA9")
  
  for (col in columns_to_process) {
    in_situ_temperature[, col] <- zoo::rollmean(as.numeric(in_situ_temperature[, col]), k = 7, align = "center", fill = NA)
  }
  
  p1 <- ggplot(in_situ_temperature, aes(x =  xdat)) +
    geom_line(aes(y = mean.RUNA1, col = "mean.RUNA1"), linewidth = 1.1) +
    geom_line(aes(y = mean.RUNA2, col = "mean.RUNA2"), linewidth = 1.1) +
    geom_line(aes(y = mean.RUNA3, col = "mean.RUNA3"), linewidth = 1.1) +
    geom_line(aes(y = mean.RUNA8, col = "mean.RUNA8"), linewidth = 1.1) +
    geom_line(aes(y = mean.RUNA9, col = "mean.RUNA9"), linewidth = 1.1) +
    ylab("Temperature mean (°C)") +
    xlab("Time") 
  
  in_situ_temperature <- as.data.frame(in_situ_temperature[,c(1,8:12)])
  rownames(in_situ_temperature) <- in_situ_temperature$date
  in_situ_temperature <- in_situ_temperature[,-1]
  
  in_situ_temperature$mean_all_site <- rowMeans(in_situ_temperature)
  
  dev_1 <- in_situ_temperature$mean.RUNA1 - in_situ_temperature$mean_all_site
  dev_2 <- in_situ_temperature$mean.RUNA2 - in_situ_temperature$mean_all_site
  dev_3 <- in_situ_temperature$mean.RUNA3 - in_situ_temperature$mean_all_site
  dev_8 <- in_situ_temperature$mean.RUNA8 - in_situ_temperature$mean_all_site
  dev_9 <- in_situ_temperature$mean.RUNA9 - in_situ_temperature$mean_all_site
  
  temp_deviation <- data.frame(Date = rownames(in_situ_temperature),
                               D_1 = dev_1,
                               D_2 = dev_2,
                               D_3 = dev_3,
                               D_8 = dev_8,
                               D_9 = dev_9)
  
  
  p2 <- ggplot(temp_deviation, aes(x = xdat)) +
    geom_line(aes(y = D_1, col = "D_1"), linewidth = 1.1) +
    geom_line(aes(y = D_2, col = "D_2"), linewidth = 1.1) +
    geom_line(aes(y = D_3, col = "D_3"), linewidth = 1.1) +
    geom_line(aes(y = D_8, col = "D_8"), linewidth = 1.1) +
    geom_line(aes(y = D_9, col = "D_9"), linewidth = 1.1) +
    ylab("Temperature deviation (°C)") +
    xlab("Time")
  
  Temp_dev_path <- here::here("outputs/Temperature_in_sit_graph.pdf")
  
  cow <- cowplot::plot_grid(p1,
                     p2,
                     labels = c("a", "b"),
                     ncol = 1, 
                     nrow = 2)
  
  ggsave(filename =  Temp_dev_path, plot = cow , width = 12, height = 14)
  
  #### compare with model ####
  data_t_model <- as.data.frame(read.csv("outputs/data_temp_rollmean_model.csv", header = TRUE, sep = ",", dec = "."))
  
  in_situ_temperature <- in_situ_temperature[c(4:294),]
  nrow(in_situ_temperature)
  data_t_model <- data_t_model[c(58:348),]
  nrow(data_t_model)
  
  rownames(data_t_model) <- data_t_model$day
  
  df_t <- data.frame(day = data_t_model$day,
                     tis1 = in_situ_temperature$mean.RUNA1,
                     tis2 = in_situ_temperature$mean.RUNA2,
                     tis3 = in_situ_temperature$mean.RUNA3,
                     tis8 = in_situ_temperature$mean.RUNA8,
                     tis9 = in_situ_temperature$mean.RUNA9,
                     tmod1 = data_t_model$t.RUNA1,
                     tmod2 = data_t_model$t.RUNA2_3,
                     tmod3 = data_t_model$t.RUNA2_3,
                     tmod8 = data_t_model$t.RUNA8,
                     tmod9 = data_t_model$t.RUNA9)
  
  class(df_t)
  
  p3 <- ggplot(df_t, aes(x =  c(1:nrow(df_t)))) +
    geom_line(aes(y = tis1, col = "mean.RUNA1"), linewidth = 1.1) +
    geom_line(aes(y = tis2, col = "mean.RUNA2"), linewidth = 1.1) +
    geom_line(aes(y = tis3, col = "mean.RUNA3"), linewidth = 1.1) +
    geom_line(aes(y = tis8, col = "mean.RUNA8"), linewidth = 1.1) +
    geom_line(aes(y = tis9, col = "mean.RUNA9"), linewidth = 1.1) +
    ylab("Temperature mean in situ in 2020 (°C)") +
    xlab("Time") 
  
  p4 <- ggplot(df_t, aes(x =  c(1:nrow(df_t)))) +
    geom_line(aes(y = tmod1, col = "mean.RUNA1"), linewidth = 1.1, linetype = 2) +
    geom_line(aes(y = tmod2, col = "mean.RUNA2"), linewidth = 1.1, linetype = 2) +
    geom_line(aes(y = tmod3, col = "mean.RUNA3"), linewidth = 1.1, linetype = 2) +
    geom_line(aes(y = tmod8, col = "mean.RUNA8"), linewidth = 1.1, linetype = 2) +
    geom_line(aes(y = tmod9, col = "mean.RUNA9"), linewidth = 1.1, linetype = 2) +
    ylab("Temperature mean from copernicus model in 2021 (°C)") +
    xlab("Time") 
  
  
  Temp_mod_insit_path <- here::here("outputs/Temperature_mod_insit.pdf")
  
  cow <- cowplot::plot_grid(p3,
                            p4,
                            labels = c("a", "b"),
                            ncol = 1, 
                            nrow = 2)
  
  ggsave(filename =  Temp_mod_insit_path, plot = cow , width = 12, height = 14)
  
  
  #### plot and save the plot ####
  cowplot::plot_grid(a,
                     b,
                     c,
                     d,
                     labels = c("A", "B", "C","D"),
                     ncol = 2, 
                     nrow = 2)
  path_to_boxplot <- paste0("outputs/boxplot_", arms_id,".pdf")
  ggsave(filename =  path_to_boxplot , width = 10, height = 10)

  
  return(path_to_boxplot) 
}
  

  
  