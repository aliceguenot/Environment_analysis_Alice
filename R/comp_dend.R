#' Build a tanglegram comparison between all 
#'
#' @param data_cryptique the data of the benthos transect
#' 
#' @return 
#'
comp_clust <- function(data_cryptique, clustering_trans) {
  
  #data_cryptique <- targets::tar_read("data_cryptique")
  #clustering_trans <- targets::tar_load("clust_trans")
  
  #### load data from ARMS ####
  data_crypt <-  read.table(file = data_cryptique, header=TRUE, sep=";", dec=",")
  row.names(data_crypt) <- data_crypt$arms
  data_crypt <- data_crypt[,-1]
  data_crypt[] <- lapply(data_crypt, as.numeric)
  matrix.hel.crypt = vegan::decostand(data_crypt, "hellinger")
  matrix.dist.crypt = vegan::vegdist(matrix.hel.crypt)
  clustering_arms <- hclust(matrix.dist.crypt, method = "average")
  
  dend_list <- dendextend::dendlist(clustering_arms, clustering_trans)
   
  
}
