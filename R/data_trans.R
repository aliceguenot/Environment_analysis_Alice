#' Creating data table for RUNTRANS
#'
#' @param data_trans the path to the raw data file
#'
#' @return the path to the different data paths
#' @export
#'

fun_data_runtrans <- function(data_trans, path_labels){
 
  # CPC OK Le 20/10/2023
  
  # data_trans <- targets::tar_read(data_transect)
  # path_labels <- targets::tar_read(labels)

  #### data ####
  data_trans <- read.table(data_trans, 
                     header = TRUE, 
                     sep = ";", 
                     dec = ",")
  # View(data_trans)
  
  trans <-  substr(data_trans$Image.name, 9, 10)
  rownames(data_trans) <- substr(data_trans$Image.name, 9, 13)
  data_trans <- subset(data_trans, select = -c(Unknown, UNAV, Image.name))
  data_trans <- vegan::decostand(data_trans, method = "total")
  data_trans <- data_trans*100
  cols_to_remove <- names(data_trans[colSums(data_trans) == 0])
  data_trans <- data_trans[, !names(data_trans) %in% cols_to_remove]
  # data set is missing 5 lines corresponding to "7A_20", "7C_19", "7C_20", "9B_19" "9B_20"
  
  path_to_data_trans <- here::here("data/derived-data/data_trans.csv")
  write.csv(data_trans, file = path_to_data_trans, row.names = TRUE)
  
  #### mean ####
  data_trans_mean <- aggregate(. ~ trans, data = data_trans, FUN = mean)
  rownames(data_trans_mean) <- data_trans_mean$trans
  data_trans_mean <- subset(data_trans_mean, select = -c(trans))
  
  path_to_data_trans_mean <- here::here("data/derived-data/data_trans_mean.csv")
  write.csv( data_trans_mean, file = path_to_data_trans_mean, row.names = TRUE)
  
  
  #### category ####
  df_labels <- read.table(path_labels, 
                           header = TRUE, 
                           sep = ";", 
                           dec = ",")
  
  t_data_trans <- as.data.frame(t(data_trans)) 
  sup_col <- df_labels$Label_name[!df_labels$Label_name %in% rownames(t_data_trans)]
  df_labels <- df_labels[!(df_labels$Label_name %in% sup_col), ]
  data_trans_cat <- aggregate(. ~ df_labels$Type, data = t_data_trans, FUN = mean)
  data_trans_cat <- as.data.frame(t(data_trans_cat))
  colnames(data_trans_cat) <- data_trans_cat[1,]
  data_trans_cat <- data_trans_cat[-1,]
  data_trans_cat <- as.data.frame(lapply(data_trans_cat, as.numeric))
  rownames(data_trans_cat) <- rownames(data_trans)
  
  path_to_data_trans_cat <- here::here("data/derived-data/data_trans_cat.csv")
  write.csv(data_trans_cat, file =  path_to_data_trans_cat, row.names = TRUE)
  
  #### category mean ####
  
  data_trans_cat_mean <- aggregate(. ~ trans, data = data_trans_cat, FUN = mean)
  rownames(data_trans_cat_mean) <- data_trans_cat_mean$trans
  data_trans_cat_mean <- subset(data_trans_cat_mean, select = -c(trans))

  path_to_data_trans_mean_cat <- here::here("data/derived-data/data_trans_mean_cat.csv")
  write.csv( data_trans_cat_mean, file = path_to_data_trans_mean_cat , row.names = TRUE)
  
  #### category 2####
  
  data_trans_cat2 <- aggregate(. ~ df_labels$Coral_type, data = t_data_trans, FUN = mean)
  data_trans_cat2 <- as.data.frame(t(data_trans_cat2))
  colnames(data_trans_cat2) <- data_trans_cat2[1,]
  data_trans_cat2 <- data_trans_cat2[-1,]
  data_trans_cat2 <- as.data.frame(lapply(data_trans_cat2, as.numeric))
  rownames(data_trans_cat2) <- rownames(data_trans)
  
  path_to_data_trans_cat2 <- here::here("data/derived-data/data_trans_cat2.csv")
  write.csv(data_trans_cat2, file = path_to_data_trans_cat2, row.names = TRUE)
  
  #### category 2 mean ####
  
  data_trans_cat_mean2 <- aggregate(. ~ trans, data = data_trans_cat2, FUN = mean)
  rownames(data_trans_cat_mean2) <- data_trans_cat_mean2$trans
  data_trans_cat_mean2 <- subset(data_trans_cat_mean2, select = -c(trans))
  
  path_to_data_trans_mean_cat2 <- here::here("data/derived-data/data_trans_mean_cat2.csv")
  write.csv(data_trans_cat_mean2, file =  path_to_data_trans_mean_cat2, row.names = TRUE)
  
  #### category 3####

  data_trans_cat3 <- aggregate(. ~ df_labels$Coral_gender, data = t_data_trans, FUN = mean)
  data_trans_cat3 <- as.data.frame(t(data_trans_cat3))
  colnames(data_trans_cat3) <- data_trans_cat3[1,]
  data_trans_cat3 <- data_trans_cat3[-1,]
  data_trans_cat3 <- as.data.frame(lapply(data_trans_cat3, as.numeric))
  rownames(data_trans_cat3) <- rownames(data_trans)

  path_to_data_trans_cat3 <- here::here("data/derived-data/data_trans_cat3.csv")
  write.csv(data_trans_cat3, file = path_to_data_trans_cat3, row.names = TRUE)
  
  #### category 3 mean ####
  
  data_trans_cat_mean3 <- aggregate(. ~ trans, data = data_trans_cat3, FUN = mean)
  rownames(data_trans_cat_mean3) <- data_trans_cat_mean3$trans
  data_trans_cat_mean3 <- subset(data_trans_cat_mean3, select = -c(trans))
  
  path_to_data_trans_mean_cat3 <- here::here("data/derived-data/data_trans_mean_cat3.csv")
  write.csv(data_trans_cat_mean3, file =  path_to_data_trans_mean_cat3, row.names = TRUE)
  
  paths = as.character(c(path_to_data_trans, path_to_data_trans_cat,  path_to_data_trans_cat2, path_to_data_trans_mean, path_to_data_trans_mean_cat, path_to_data_trans_mean_cat2, path_to_data_trans_cat3,  path_to_data_trans_mean_cat3))
  
  return (paths)
}
  