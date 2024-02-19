#' Build a clustering based on environment parameters (and compare with cryptic community)
#'
#' @param data_crypt the data of the arms community
#' @param temperature temperature data
#' @param salinity salinity data 
#' @param wave_height wave_height data
#' @return 
#'
comp_clust_env <- function(data_crypt, temperature, salinity, wave_height) {
  
  # data_crypt <- targets::tar_read("data_cryptique")
  # temperature <- targets::tar_read("temp")
  # salinity <- targets::tar_read("sal")
  # wave_height <- targets::tar_read("wave_h")

  #### load data arms ####
  data_cryptique <-  read.table(file = data_crypt, header=TRUE, sep=";", dec=",")
  row.names(data_cryptique) <- data_cryptique$arms
  data_cryptique <- data_cryptique[,-1]
  data_cryptique[] <- lapply(data_cryptique, as.numeric)
  matrix.hel.crypt = vegan::decostand(data_cryptique, "hellinger")
  matrix.dist.crypt = vegan::vegdist(matrix.hel.crypt)
  
  #### load temp data/sal/wave h ####
  data_temp <-  read.table(file = temperature, header=TRUE, sep=";", dec=",")
  data_temp$ampl <- data_temp$max - data_temp$min
  data_sal <- read.table(file = salinity, header=TRUE, sep=";", dec=",")
  data_sal$ampl <- data_sal$max - data_sal$min
  data_wave_h <- read.table(file = wave_height, header=TRUE, sep=";", dec=",")
  data_wave_h$ampl <- data_wave_h$max - data_wave_h$min
  
  #### create matrix.env ####
  matrix.env <- as.data.frame(cbind(data_temp[,c(1,2,4,5,6)], data_sal[,c(2,4,5,6)], data_wave_h[,c(2,4,5,6)]))
  row.names(matrix.env) <- matrix.env$site
  matrix.env <- matrix.env[,-1]
  matrix.dist.env <-vegan::vegdist(matrix.env, method = "euclidean")
  
  #### clean plot of env clustering ####
  clustering_env <- hclust(matrix.dist.env, method = "average")
  penalty <- maptree::kgs(cluster = clustering_env, 
                          diss = matrix.dist.env, 
                          maxclust = 26)
  penalty <- sort(penalty)
  
  penalty_min <- as.numeric(names(penalty[1]))
  
  
  clean_clust_env_name <- paste0("clean_clust_env.pdf")
  clean_clust_env_path <- here::here("outputs",  clean_clust_env_name)
  
  library(ggplot2)
  library(ggdendro)
  dendr <- ggdendro::dendro_data(clustering_env, type="rectangle") 
  
  clust <- cutree(clustering_env, k=penalty_min)                    # find 2 clusters
  clust.df <- data.frame(label=names(clust), cluster=factor(clust))
  
  dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
  
  aa <- ggplot() + 
    geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
    geom_text(data=label(dendr), aes(x, y, label=label, hjust=-0.16, color=cluster)) +
    scale_colour_brewer("Clusters", palette = "Set1") +
    coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
    theme(axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(fill="white"),
          panel.grid=element_blank()) + ggtitle(paste0(" "))
  
  
  ggsave(clean_clust_env_path, aa, width = 8, height = 12)
  
  
  #### tanglegram ####
  clustering_arms <- hclust(matrix.dist.crypt, method = "average")
  
  clustering_env <- hclust(matrix.dist.env, method = "average")
  
  
  dend_arms <- as.dendrogram(clustering_arms)
  dend_env <- as.dendrogram(clustering_env)
  
  tangle_name <- "tanglegram_env.pdf"
  tangle_path <- here::here("outputs", tangle_name)
  pdf(file =  tangle_path, width = 14, height = 8)
  
  dend_list <- dendextend::dendlist(dend_arms, dend_env)
  dendextend::tanglegram(dend_arms, dend_env)
  dendextend::tanglegram(dend_arms, dend_env,
                         sort = TRUE,
                         lab.cex = 0.8,
                         main = paste("entanglement =", round(dendextend::entanglement(dend_list), 2)))
  dev.off()
  
  #### mantel test ####
  
  matrix.dist.crypt
  matrix.dist.env
  
  
  
  aa = as.vector(matrix.dist.crypt)
  tt = as.vector(matrix.dist.env)
  #new data frame with vectorized distance matrices
  mat = data.frame(aa,tt)
  
  mm1 = ggplot(mat, aes(y = aa, x = tt)) + 
    geom_point(size = 3, alpha = 0.5, color = "black") + 
    labs(x = "Environmental parameters ditances (T°, salinity, wave height) ",
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
  
  correl = vegan::mantel(matrix.dist.crypt,matrix.dist.env, method = "pearson")
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
  
  mantel_name <- paste0("mantel_env_crypt.pdf")
  mantel_path <- here::here("outputs",  mantel_name)
  
  
  ggplot2::ggsave(mantel_path, plot = mm1, width = 8, height = 8)
  
  return(tangle_path)
  
}
  
  