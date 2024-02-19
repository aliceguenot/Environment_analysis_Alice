#' Correlogramme
#'
#' @param data_transect the data of the benthos transect
#' @param data_cryp the data of the cryptobenthic community
#' 
#' @return 

correl <- function(data_trans, data_crypt, dat_msp_pool, dat_transect_pool, arms_id, meta_and_data) {

  # data_trans <- targets::tar_read("data_transect")
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # dat_msp_pool <- targets::tar_read("data_msp_pool")
  # dat_transect_pool <- targets::tar_read("data_transect_pool")
  
  
  #### load and format data ####
  
  # data benthos #
  data <- read.table(file = data_trans, header=TRUE, sep=";", dec=",")
  transect <- substr(data$Name, 9, 10)
  
  rownames(data) <- substr(data$Name, 9, 13)
  data_full <- data[,-c(1,2,3)]
  dat <- aggregate(data_full, by = list(transect), FUN = mean)
  dat <- dat[,-1]
  rownames(dat) <- levels(as.factor(transect))
  dat_trans <- dat
  
  # data crypt #
  data_path <- meta_and_data[!grepl("metadata", meta_and_data)]
  meta_path <- meta_and_data[grepl("metadata", meta_and_data)]
  
  meta <- read.csv(meta_path)
  
  dat_crypt <- read.csv(data_path)
  dat_crypt <- as.data.frame(lapply(dat_crypt, as.numeric))
  dat_crypt <- aggregate(dat_crypt, by = list(meta$arms_name), FUN = mean)
  
  row.names(dat_crypt) <- dat_crypt$Group.1
  dat_crypt <- dat_crypt[,-1]
  dat_crypt <- dat_crypt[-c(22,23,24),]
  
  # data crypt pool #
  
  data_pool <- read.table(file = dat_msp_pool, header=TRUE, sep=";", dec=",")
  data_pool <- subset(data_pool, data_pool$prefixe == arms_id)
  data_pool <- data_pool[,20:ncol(data_pool)]
  data_pool <- aggregate(data_pool, by = list(meta$arms_name), FUN = mean)
  data_pool <- data_pool[-c(22,23,24),]
  rownames(data_pool) <- data_pool$Group.1
  data_pool <- data_pool[,-1]
  
  # data benthos pool #
  data_benthos_pool <- read.table(file = dat_transect_pool, header=TRUE, sep=";", dec=",")
  
  
  
  #### Correlation plot between pool msp crypt and benthos ####
  
  corr_pool <- cor(data_pool, dat_trans)
  
  corr_pool <- as.matrix(corr_pool) 
  
  corr_pool[corr_pool > -0.7 & corr_pool < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # corr_pool[corr_pool <= 0.7 & corr_pool >= -0.7] <- NA  # Répète l'opération pour les valeurs égales à -0.7 ou 0.7
  
  data_corr_pool <- as.data.frame(corr_pool)
  
  data_corr_pool[is.na(data_corr_pool)] <- 0
  
  data_corr_pool <- data_corr_pool[, colSums(data_corr_pool) != 0]
  data_corr_pool <- data_corr_pool[ rowSums(data_corr_pool) != 0,]
  
  corr1_name <- paste0("corr1_", arms_id, ".pdf")
  corr1_path <- here::here("outputs", corr1_name)
  
  pdf(file =  corr1_path, width = 10, height = 10)
  
  corrplot::corrplot(as.matrix(data_corr_pool), method = "circle") 
  
  dev.off()
  
  #### Correlation plot between pool msp cypt and pool categories benthos ####
  corr_all_pool <- cor(data_pool, data_benthos_pool)
  
  corr_all_pool <- as.matrix(corr_all_pool) 
  
  corr_all_pool[corr_all_pool > -0.7 & corr_all_pool < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # corr_all_pool[corr_all_pool <= 0.7 & corr_all_pool >= -0.7] <- NA  # Répète l'opération pour les valeurs égales à -0.7 ou 0.7
  
  data_corr_all_pool <- as.data.frame(corr_all_pool)
  
  data_corr_all_pool[is.na(data_corr_all_pool)] <- 0
  
  data_corr_all_pool <- data_corr_all_pool[, colSums(data_corr_all_pool) != 0]
  data_corr_all_pool <- data_corr_all_pool[ rowSums(data_corr_all_pool) != 0,]
  
  corr2_name <- paste0("corr2_", arms_id, ".pdf")
  corr2_path <- here::here("outputs", corr2_name)
  
  pdf(file =  corr2_path, width = 10, height = 10)
  
  
  corrplot::corrplot(as.matrix(data_corr_all_pool), method = "circle")
  
  dev.off()
  
  #### Correlation plot between full data set crypt and benthos ####
  
  corr <- cor(dat_crypt, dat_trans)
  
  corr <- as.matrix(corr) 
  
  corr[corr > -0.7 & corr < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # corr[corr <= 0.7 & corr >= -0.7] <- NA  # Répète l'opération pour les valeurs égales à -0.7 ou 0.7
  
  data_corr <- as.data.frame(corr)
  
  data_corr[is.na(data_corr)] <- 0
  
  data_corr <- data_corr[, colSums(data_corr) != 0]
  data_corr <- data_corr[ rowSums(data_corr) != 0,]
  
  
  corr3_name <- paste0("corr3_", arms_id, ".pdf")
  corr3_path <- here::here("outputs", corr3_name)
  
  pdf(file =  corr3_path, width = 10, height = 10)
  
  corrplot::corrplot(as.matrix(data_corr), method = "circle")
  
  dev.off()
  
  #### Correlation plot between full data set crypt and pooled categories benthos ####
  
  corr <- cor(dat_crypt, data_benthos_pool)
  
  corr <- as.matrix(corr) 
  
  corr[corr > -0.7 & corr < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # corr[corr <= 0.7 & corr >= -0.7] <- NA  # Répète l'opération pour les valeurs égales à -0.7 ou 0.7
  
  data_corr <- as.data.frame(corr)
  
  data_corr[is.na(data_corr)] <- 0
  
  data_corr <- data_corr[, colSums(data_corr) != 0]
  data_corr <- data_corr[ rowSums(data_corr) != 0,]
  
  
  corr4_name <- paste0("corr4_", arms_id, ".pdf")
  corr4_path <- here::here("outputs", corr4_name)
  
  pdf(file =  corr4_path, width = 10, height = 10)
  
  corrplot::corrplot(as.matrix(data_corr), method = "circle")
  
  dev.off()
  
  
  #### Correlation between full dataset crypt and pooled by growth form surrounding corals #### 
  
  Other <- data.frame(cbind(data_benthos_pool[,2:ncol(data_benthos_pool)]))
  
  MA <- rowSums(Other[,8:10]) #Macroalgae
  
  BC <- rowSums(dat_trans[,c(2,17)]) #Branching corals
  
  
  MC <- rowSums(dat_trans[, c(1,3:16,18,19,23)]) #Massive Corals
  
  data_benthos_growth <- data.frame(cbind(BC, MC, MA, Other$Turf, Other$Turf_sand, Other$Sand, Other$CCA, data_benthos_pool$SC))
  
  colnames(data_benthos_growth) <- c("Branching_Corals", "Massive_Corals", "Macroalgae", "Turf", "Turf_sand", "Sand", "CCA", "Soft_Corals")
  
  path_coral_growth <- "data/derived-data/data_growth.csv"
  write.csv(data_benthos_growth, file = path_coral_growth, row.names = TRUE)
  
  corr_growth <- cor(dat_crypt, data_benthos_growth)
  
  corr_growth <- as.matrix(corr_growth) 
  
  corr_growth[corr_growth > -0.7 & corr_growth < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # corr[corr <= 0.7 & corr >= -0.7] <- NA  # Répète l'opération pour les valeurs égales à -0.7 ou 0.7
  
  data_corr <- as.data.frame(corr_growth)
  
  data_corr[is.na(data_corr)] <- 0
  
  data_corr <- data_corr[, colSums(data_corr) != 0]
  data_corr <- data_corr[ rowSums(data_corr) != 0,]
  
  corr5_name <- paste0("corr5_", arms_id, ".pdf")
  corr5_path <- here::here("outputs", corr5_name)
  
  pdf(file =  corr5_path, width = 10, height = 10)
  
  corrplot::corrplot(as.matrix(data_corr), method = "circle")
  
  dev.off()
  
  return(path_coral_growth)
  
}