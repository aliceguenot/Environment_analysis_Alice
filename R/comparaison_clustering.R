#' Build a clustering based on habitat transects and plot box of transect data
#'
#' @param data_transect the data of the benthos transect
#' @param data_cryp the data of the cryptobenthic community
#' 
#' @return a dendrogram that can be used in tanglgram function
#'
comp_clust <- function(data_trans, data_crypt) {
  
  # data_trans <- targets::tar_read("data_transect")
  # data_crypt <- targets::tar_read("data_cryptique")

  #### load and format data ####
  data <- read.table(file = data_trans, header=TRUE, sep=";", dec=",")
  transect <- substr(data$Name, 9, 10)
  
  rownames(data) <- substr(data$Name, 9, 13)
  data_full <- data[,-c(1,2,3)]
  dat <- aggregate(data_full, by = list(transect), FUN = mean)
  dat <- dat[,-1]
  rownames(dat) <- levels(as.factor(transect))
  
 
  
  #### clustering ####
  matrix.hel = vegan::decostand(dat, "hellinger")
  matrix.dist = vegan::vegdist(matrix.hel)
  cluster <- hclust(matrix.dist, method = "average")

  plot(cluster)
  braycurtis <- function(x) {
    x <- as.matrix(x)
    x <- t(x)
    res <- vegan::vegdist(x)
    res <- as.dist(res)
    attr(res, "method") <- "braycurtis"
    return(res)
  }
  
  plot0 <- pvclust::pvclust(t(matrix.hel),
                            method.hclust = "average",
                            method.dist = braycurtis,
                            parallel = TRUE)
 
  plot(plot0)
  #### testing the best clustering algorithm ####
  
  coph <- stats::cophenetic(cluster)
  cor(matrix.dist, coph)
  
  # 2-norm value computing
  dnorm <- clue::cl_dissimilarity(matrix.dist,
                                  cluster,
                                  method = "spectral")    
  
  # plot the graph of the correlation
  plot2 <- plot(matrix.dist,coph,
                xlab = paste0("distance computing by UPGMA algorithm"),
                ylab = "Cophenetic distance",
                asp = 1,
                xlim = c(0.1, 0.5),
                ylim = c(0.1, 0.4),
                panel.first = abline(0, 1),
                main = paste("Cophenetic correlation =", round(cor(matrix.dist, coph), 3), "\n 2-norm = ", round(dnorm,3)))
  
  
  clustering_transect <- cluster
  
  #### plot a clean clustering ####

  
  penalty <- maptree::kgs(cluster = clustering_transect, 
                          diss = matrix.dist, 
                          maxclust = 23)
  penalty <- sort(penalty)
  
  penalty_min <- as.numeric(names(penalty[1]))
  
  
  clean_clust_trans_name <- paste0("clean_clust_trans.pdf")
  clean_clust_trans_path <- here::here("outputs",  clean_clust_trans_name)
  
  library(ggplot2)
  library(ggdendro)
  
  #convert cluster object to use with ggplot
  dendr <- ggdendro::dendro_data(clustering_transect, type="rectangle") 
  
  clust <- cutree(clustering_transect, k=penalty_min)                    # find 2 clusters
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
  
  
  ggsave(clean_clust_trans_path, aa, width = 8, height = 12)
  
  #### load arms data and make the clustering ####
  data_cryptique <-  read.table(file = data_crypt, header=TRUE, sep=";", dec=",")
  row.names(data_cryptique) <- data_cryptique$arms
  data_cryptique <- data_cryptique[,-1]
  data_cryptique[] <- lapply(data_cryptique, as.numeric)
  data_cryptique <- data_cryptique[-c(24,23,22),]
  rownames(data_cryptique)  <- rownames(matrix.hel)
  matrix.hel.crypt = vegan::decostand(data_cryptique, "hellinger")
  matrix.dist.crypt = vegan::vegdist(matrix.hel.crypt)
  clustering_arms <- hclust(matrix.dist.crypt, method = "average")
  
  #### tanglegram : comparison of two clusters ####
  
  dend_arms <- as.dendrogram(clustering_arms)
  plot(dend_arms)
  dend_transect <- as.dendrogram(clustering_transect)
  
  tangle_name <- "tanglegram.pdf"
  tangle_path <- here::here("outputs", tangle_name)
  pdf(file =  tangle_path, width = 12, height = 8)
  
  
  dend_list <- dendextend::dendlist(dend_arms, dend_transect)


  dendextend::tanglegram(dend_arms, dend_transect,
                         sort = TRUE,
                         main = paste("entanglement =", round(dendextend::entanglement(dend_list), 2)))

  
  dev.off()

  #### Mantel test as in Pearman et al. 2020 ####
  
  matrix.dist.crypt
  matrix.dist.trans <-  matrix.dist
  
  
  ##### jacc #####
  aa = as.vector(matrix.dist.crypt)
  tt = as.vector(matrix.dist.trans)
  #new data frame with vectorized distance matrices
  mat = data.frame(aa,tt)
  
  mm1 = ggplot(mat, aes(y = aa, x = tt)) + 
    geom_point(size = 3, alpha = 0.5, color = "black") + 
    labs(x = "Benthic habitat community",
         y = "Cryptobenthic community dissimilarity") +
    geom_smooth(method = "gam", 
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
  
  correl = vegan::mantel(matrix.dist.crypt, matrix.dist.trans, method = "pearson")
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
    
  mantel_name <- paste0("mantel_trans_crypt.pdf")
  mantel_path <- here::here("outputs",  mantel_name)
  
  
  ggplot2::ggsave(mantel_path, plot = mm1, width = 8, height = 8)
  
  
  return(mantel_path)
}

