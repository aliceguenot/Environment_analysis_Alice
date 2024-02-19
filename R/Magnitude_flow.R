' convert eastward flow velocity and northward flow velocity in magnitude (Global Ocean Wavec Analysis and Forecast)
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_flow temperature data from copernicus
#' @return the path to the mean value of flow for each sites
#' @export
#'

magnitude_flow <- function(flow_E, flow_N) {
  
  # flow_E <- targets::tar_read("flowveloc")
  # flow_N <- targets::tar_read("flowveloc_N")
  library(ggplot2)
  #flow_E
  flow_E <- flow_E[2]
  data_flow_E <- read.table(file = flow_E, header=TRUE, sep=";", dec=",")
  data_flow_E <- data_flow_E[-c(1:3),]
 
  #flow_N
  flow_N <- flow_N[2]
  data_flow_N <- read.table(file = flow_N, header=TRUE, sep=";", dec=",")
  data_flow_N <- data_flow_N[-c(1:3),]

  
  
  magnitude <- sqrt((data_flow_N[2:ncol(data_flow_N)])^2 + (data_flow_E[,2:ncol(data_flow_E)])^2)
  
  magnitude <- data.frame(day = as.Date(data_flow_E$day), magnitude)
  
  p1 <- ggplot(magnitude, aes(x=day))  +
    geom_line(aes(y = f.RUNA1), col = "blue", linewidth = 1.1) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)") +
    xlab("time")
  
  p2 <- ggplot(magnitude, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p3 <- ggplot(magnitude, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p4 <- ggplot(magnitude, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA8)) +
    geom_line(aes(y = f.RUNA9)) +
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p5 <- ggplot(magnitude, aes(x=day))  +
    geom_line(aes(y = f.RUNA1)) + 
    geom_line(aes(y = f.RUNA2_3)) +
    geom_line(aes(y = f.RUNA4_6)) +
    geom_line(aes(y = f.RUNA7)) +
    geom_line(aes(y = f.RUNA8), col = "blue", linewidth = 1.1) +
    geom_line(aes(y = f.RUNA9))+
    ylab("Flow velocity (m.s-1)")+
    xlab("time")
  
  p6 <- ggplot(magnitude, aes(x=day))  +
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
  
  path_to_magnitude <- paste0("outputs/magnitudecurv.pdf")
  ggsave(filename =  path_to_magnitude , width = 15, height = 12)
  
 return(path_to_magnitude)
  
}