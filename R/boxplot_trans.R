#' Boxplot of the transect categories
#'
#' @param data_transect the data of the benthos transect

#' 
#' @return a dendrogram that can be used in tanglgram function
#'

box_trans <- function(path_to_data_trans) {
  
  library(ggplot2)
  library(forcats)
  # path_to_data_trans <- targets::tar_read("data_trans_paths")
  
  #### Diversité corallienne par site ####
  # data_trans_mean <- read.table(file = path_to_data_trans[4], header=TRUE, sep=",", dec=".")
  # rownames(data_trans_mean) <- data_trans_mean$X
  # data_trans_mean <- subset(data_trans_mean, select = -X)
  # data_site_mean <- aggregate(data_trans_mean, by = list(substr(rownames(data_trans_mean), 1, 1)), FUN = mean)
  # data_site_mean <-  subset(data_site_mean, select = -c(Group.1))
  # div_HC <- data_site_mean[, c(1:44)]
  # div_HC <- vegan::decostand(div_HC, "pa")
  # div_HC <- rowSums(div_HC)
  # div_HC
  
  
  
  #### Boxplot ####
  data_trans_cat3 <- read.table(file = path_to_data_trans[7], header=TRUE, sep=",", dec=".")
  rownames(data_trans_cat3) <- data_trans_cat3$X
  data_trans_cat3 <- subset(data_trans_cat3, select = -X)
  site <- substr(rownames(data_trans_cat3), 1, 1)
  site <- paste0("RUNA",site)
  View(data_trans_cat3)
  genus_means <- colMeans(data_trans_cat3)  # Exclude the first column with row names
  
  
  
  
  # Sort the mean cover values in descending order
  sorted_genus_means <- genus_means[order(genus_means, decreasing = TRUE)]
  
  
  
  col1 <- c("#56B4E9", "pink", "purple", "blue", "#F2AD00", "#F98400", "#FD6467", "lightgrey", "#FAEFD1")                     
  col2 <- c("#E69F00", "#5B1A18" , "red",  "darkgreen",  "lightgreen", "grey" , "#F2AD00", "#FAEFD1", "lightgrey", "#FD6467", "#F98400", "#F2AD00", "blue", "purple", "pink", "#56B4E9") 
  
  nom <- c(expression(paste(italic("Porites"))), expression(paste(italic("Pocillopora"))), expression(paste(italic("Acropora"))), expression(paste(italic("Astreopra"))), "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae")
  
  v1 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Acropora)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Acropora")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v1
  
  v2 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Pocillopora)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Pocillopora")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v2
  
  v3 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Porites)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Porites")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v3
  
  v4 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Astreopora)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Astreopora")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v4
  
  v5 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Turf_and_sand)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Turf_and_sand")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v5
  
  v6 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$CCA)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" CCA")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none")
  
  v6
  
  v7 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Turf_algae)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Turf_algae")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v7
  
  v8 <- ggplot(data_trans_cat3, aes(x = site, y = data_trans_cat3$Sand)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = expression(paste("Percentage cover for", italic(" Sand")))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  v8
  
  cowplot::plot_grid(v1, v2, v3, v4, v5, v6, 
                     ncol = 2, 
                     nrow = 3)
  
  boxcoral_name <- paste0("box_trans_10_23.pdf")
  boxcoral_path <- here::here("outputs", boxcoral_name)
  
  ggsave(boxcoral_path, width = 12, height = 13.5)
  
  #### boxplot de diversité corallienne ####
  data_trans <- read.table(file = path_to_data_trans[1], header=TRUE, sep=",", dec=".")
  rownames(data_trans) <- data_trans$X
  data_trans <- subset(data_trans, select = -X)
  data_trans_DIV_HC <- data_trans[, c(1:44)]
  data_trans_DIV_HC <- vegan::decostand(data_trans_DIV_HC, "pa")
  data_trans_DIV_HC$div_HC <- rowSums(data_trans_DIV_HC)
  
  box_trans_div <- ggplot(data_trans_DIV_HC, aes(x = site, y = data_trans_DIV_HC$div_HC)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = "Hard coral alpha diversity ") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  boxcoraldiv_name <- paste0("box_trans_div_10_23.pdf")
  boxcoraldiv_path <- here::here("outputs", boxcoraldiv_name)
  
  ggsave(boxcoraldiv_path, width = 11, height = 12)
  
  data_trans_mean <- read.table(file = path_to_data_trans[4], header=TRUE, sep=",", dec=".")
  rownames(data_trans_mean) <- data_trans_mean$X
  data_trans_mean <- subset(data_trans_mean, select = -X)
  data_trans_mean_DIV_HC <- data_trans_mean[, c(1:44)]
  data_trans_mean_DIV_HC <- vegan::decostand(data_trans_mean_DIV_HC, "pa")
  data_trans_mean_DIV_HC$div_HC <- rowSums(data_trans_mean_DIV_HC)
  site <- substr(rownames(data_trans_mean_DIV_HC), 1,1)
  
  box_trans_mean_div <- ggplot(data_trans_mean_DIV_HC, aes(x = site, y = data_trans_mean_DIV_HC$div_HC)) +
    geom_boxplot() +
    labs(title = "",
         x = "",
         y = "Hard coral alpha diversity ") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none") 
  
  boxcoraldiv_name <- paste0("box_trans_mean_div_10_23.pdf")
  boxcoraldiv_path <- here::here("outputs", boxcoraldiv_name)
  ##### Ex boxplots ######
  
  # #1
  # 
  # coral_df.1 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.1)),
  #                          Percentage = c(NewDat.1$Porites, NewDat.1$Pocillopora, NewDat.1$Acropora, NewDat.1$Astreopora, NewDat.1$`Other HC` , NewDat.1$`Total HC cover`, NewDat.1$`Soft coral`, NewDat.1$`Dead coral`, NewDat.1$Echinodermata, NewDat.1$Molluscs, NewDat.1$Sand, NewDat.1$`Turf sand`,NewDat.1$Turf, NewDat.1$CCA, NewDat.1$Cyanobacteria, NewDat.1$`Other algae`  ))
  # 
  # coral_df.1$CoralType <- factor(coral_df.1$CoralType, levels = unique(coral_df.1$CoralType)[order(match(unique(coral_df.1$CoralType), col1))])
  # 
  # 
  # v1 <- ggplot(coral_df.1, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values = col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # 
  # #2
  # coral_df.2 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.2)),
  #                          Percentage = c(NewDat.2$Porites, NewDat.2$Pocillopora, NewDat.2$Acropora, NewDat.2$Astreopora, NewDat.2$`Other HC` , NewDat.2$`Total HC cover`, NewDat.2$`Soft coral`, NewDat.2$`Dead coral`, NewDat.2$Echinodermata, NewDat.2$Molluscs, NewDat.2$Sand, NewDat.2$`Turf sand`,NewDat.2$Turf, NewDat.2$CCA, NewDat.2$Cyanobacteria, NewDat.2$`Other algae`  ))
  # 
  # coral_df.2$CoralType <- factor(coral_df.2$CoralType, levels = unique(coral_df.2$CoralType)[order(match(unique(coral_df.2$CoralType), col1))])
  # 
  # v2 <- ggplot(coral_df.2, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # 
  # #3
  # coral_df.3 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.3)),
  #                          Percentage = c(NewDat.3$Porites, NewDat.3$Pocillopora, NewDat.3$Acropora, NewDat.3$Astreopora, NewDat.3$`Other HC` , NewDat.3$`Total HC cover`, NewDat.3$`Soft coral`, NewDat.3$`Dead coral`, NewDat.3$Echinodermata, NewDat.3$Molluscs, NewDat.3$Sand, NewDat.3$`Turf sand`,NewDat.3$Turf, NewDat.3$CCA, NewDat.3$Cyanobacteria, NewDat.3$`Other algae`  ))
  # 
  # coral_df.3$CoralType <- factor(coral_df.3$CoralType, levels = unique(coral_df.3$CoralType)[order(match(unique(coral_df.3$CoralType), col1))])
  # 
  # v3 <- ggplot(coral_df.3, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # #4
  # coral_df.4 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.4)),
  #                          Percentage = c(NewDat.4$Porites, NewDat.4$Pocillopora, NewDat.4$Acropora, NewDat.4$Astreopora, NewDat.4$`Other HC` , NewDat.4$`Total HC cover`, NewDat.4$`Soft coral`, NewDat.4$`Dead coral`, NewDat.4$Echinodermata, NewDat.4$Molluscs, NewDat.4$Sand, NewDat.4$`Turf sand`,NewDat.4$Turf, NewDat.4$CCA, NewDat.4$Cyanobacteria, NewDat.4$`Other algae`  ))
  # 
  # coral_df.4$CoralType <- factor(coral_df.4$CoralType, levels = unique(coral_df.4$CoralType)[order(match(unique(coral_df.4$CoralType), col1))])
  # 
  # v4 <- ggplot(coral_df.4, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #5
  # coral_df.5 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.5)),
  #                          Percentage = c(NewDat.5$Porites, NewDat.5$Pocillopora, NewDat.5$Acropora, NewDat.5$Astreopora, NewDat.5$`Other HC` , NewDat.5$`Total HC cover`, NewDat.5$`Soft coral`, NewDat.5$`Dead coral`, NewDat.5$Echinodermata, NewDat.5$Molluscs, NewDat.5$Sand, NewDat.5$`Turf sand`,NewDat.5$Turf, NewDat.5$CCA, NewDat.5$Cyanobacteria, NewDat.5$`Other algae`  ))
  # 
  # coral_df.5$CoralType <- factor(coral_df.5$CoralType, levels = unique(coral_df.5$CoralType)[order(match(unique(coral_df.5$CoralType), col1))])
  # 
  # v5 <- ggplot(coral_df.5, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #6
  # 
  # coral_df.6 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.6)),
  #                          Percentage = c(NewDat.6$Porites, NewDat.6$Pocillopora, NewDat.6$Acropora, NewDat.6$Astreopora, NewDat.6$`Other HC` , NewDat.6$`Total HC cover`, NewDat.6$`Soft coral`, NewDat.6$`Dead coral`, NewDat.6$Echinodermata, NewDat.6$Molluscs, NewDat.6$Sand, NewDat.6$`Turf sand`,NewDat.6$Turf, NewDat.6$CCA, NewDat.6$Cyanobacteria, NewDat.6$`Other algae`  ))
  # 
  # coral_df.6$CoralType <- factor(coral_df.6$CoralType, levels = unique(coral_df.6$CoralType)[order(match(unique(coral_df.6$CoralType), col1))])
  # 
  # v6 <- ggplot(coral_df.6, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #7
  # 
  # coral_df.7 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.7)),
  #                          Percentage = c(NewDat.7$Porites, NewDat.7$Pocillopora, NewDat.7$Acropora, NewDat.7$Astreopora, NewDat.7$`Other HC` , NewDat.7$`Total HC cover`, NewDat.7$`Soft coral`, NewDat.7$`Dead coral`, NewDat.7$Echinodermata, NewDat.7$Molluscs, NewDat.7$Sand, NewDat.7$`Turf sand`,NewDat.7$Turf, NewDat.7$CCA, NewDat.7$Cyanobacteria, NewDat.7$`Other algae`  ))
  # 
  # coral_df.7$CoralType <- factor(coral_df.7$CoralType, levels = unique(coral_df.7$CoralType)[order(match(unique(coral_df.7$CoralType), col1))])
  # 
  # v7 <- ggplot(coral_df.7, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #8
  # 
  # # coral_df.8 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.8)),
  # #                          Percentage = c(NewDat.8$Porites, NewDat.8$Pocillopora, NewDat.8$Acropora, NewDat.8$Astreopora, NewDat.8$`Other HC` , NewDat.8$`Total HC cover`, NewDat.8$`Soft coral`, NewDat.8$`Dead coral`, NewDat.8$Echinodermata, NewDat.8$Molluscs, NewDat.8$Sand, NewDat.8$`Turf sand`,NewDat.8$Turf, NewDat.8$CCA, NewDat.8$Cyanobacteria, NewDat.8$`Other algae`  ))
  # # 
  # # coral_df.8$CoralType <- factor(coral_df.8$CoralType, levels = unique(coral_df.8$CoralType)[order(match(unique(coral_df.8$CoralType), col1))])
  # 
  # # v8 <- ggplot(coral_df.8, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  # #   geom_boxplot() +
  # #   labs(title = "",
  # #        x = "",
  # #        y = "Percentage cover for benthic categories") +
  # #   scale_x_discrete(labels=nom) +
  # #   scale_fill_manual(values=col1) +
  # #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  # #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #9
  # 
  # coral_df.9 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Dead coral", "Echinodermata", "Molluscs", "Sand", "Turf sand", "Turf", "CCA","Cyanobacteria", "Other algae"), each = nrow(NewDat.9)),
  #                          Percentage = c(NewDat.9$Porites, NewDat.9$Pocillopora, NewDat.9$Acropora, NewDat.9$Astreopora, NewDat.9$`Other HC` , NewDat.9$`Total HC cover`, NewDat.9$`Soft coral`, NewDat.9$`Dead coral`, NewDat.9$Echinodermata, NewDat.9$Molluscs, NewDat.9$Sand, NewDat.9$`Turf sand`,NewDat.9$Turf, NewDat.9$CCA, NewDat.9$Cyanobacteria, NewDat.9$`Other algae`  ))
  # 
  # coral_df.9$CoralType <- factor(coral_df.9$CoralType, levels = unique(coral_df.9$CoralType)[order(match(unique(coral_df.9$CoralType), col1))])
  # 
  # v9 <- ggplot(coral_df.9, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # cowplot::plot_grid(v1, v2, v3, v4, v5, v6, v7, v9,
  #                    labels = c("RUNA1","RUNA2", "RUNA3", "RUNA4", "RUNA5", "RUNA6","RUNA7", "RUNA9"),
  #                    ncol = 3, 
  #                    nrow = 3)
  # 
  # boxcoral_name <- paste0("box_coral.pdf")
  # boxcoral_path <- here::here("outputs", boxcoral_name)
  # 
  # ggsave(boxcoral_path, width = 16, height = 11)
  # 
  # 
  # 
  # # without categories that doesn't contributes
  # 
  # col1 <- c("#56B4E9", "pink", "purple", "blue", "#F2AD00", "#F98400", "#FD6467", "#FAEFD1", "grey", "lightgreen", "darkgreen", "#FF0000","#E69F00")                           
  # 
  # 
  # nom <- c(expression(paste(italic("Porites"))), expression(paste(italic("Pocillopora"))), expression(paste(italic("Acropora"))), expression(paste(italic("Astreopra"))), "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae")
  # 
  # library(forcats)
  # 
  # #1
  # 
  # coral_df.1 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.1)),
  #                          Percentage = c(NewDat.1$Porites, NewDat.1$Pocillopora, NewDat.1$Acropora, NewDat.1$Astreopora, NewDat.1$`Other HC` , NewDat.1$`Total HC cover`, NewDat.1$`Soft coral`, NewDat.1$Echinodermata, NewDat.1$Sand, NewDat.1$`Turf sand`,NewDat.1$Turf, NewDat.1$CCA, NewDat.1$`Other algae`))
  # 
  # coral_df.1$CoralType <- factor(coral_df.1$CoralType, levels = unique(coral_df.1$CoralType)[order(match(unique(coral_df.1$CoralType), col1))])
  # 
  # 
  # v1 <- ggplot(coral_df.1, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values = col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # 
  # #2
  # coral_df.2 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.2)),
  #                          Percentage = c(NewDat.2$Porites, NewDat.2$Pocillopora, NewDat.2$Acropora, NewDat.2$Astreopora, NewDat.2$`Other HC` , NewDat.2$`Total HC cover`, NewDat.2$`Soft coral`, NewDat.2$Echinodermata, NewDat.2$Sand, NewDat.2$`Turf sand`,NewDat.2$Turf, NewDat.2$CCA, NewDat.2$`Other algae`))
  # 
  # coral_df.2$CoralType <- factor(coral_df.2$CoralType, levels = unique(coral_df.2$CoralType)[order(match(unique(coral_df.2$CoralType), col1))])
  # 
  # v2 <- ggplot(coral_df.2, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # 
  # #3
  # coral_df.3 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.3)),
  #                          Percentage = c(NewDat.3$Porites, NewDat.3$Pocillopora, NewDat.3$Acropora, NewDat.3$Astreopora, NewDat.3$`Other HC` , NewDat.3$`Total HC cover`, NewDat.3$`Soft coral`, NewDat.3$Echinodermata, NewDat.3$Sand, NewDat.3$`Turf sand`,NewDat.3$Turf, NewDat.3$CCA, NewDat.3$`Other algae`))
  # 
  # coral_df.3$CoralType <- factor(coral_df.3$CoralType, levels = unique(coral_df.3$CoralType)[order(match(unique(coral_df.3$CoralType), col1))])
  # 
  # v3 <- ggplot(coral_df.3, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # #4
  # coral_df.4 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.4)),
  #                          Percentage = c(NewDat.4$Porites, NewDat.4$Pocillopora, NewDat.4$Acropora, NewDat.4$Astreopora, NewDat.4$`Other HC` , NewDat.4$`Total HC cover`, NewDat.4$`Soft coral`, NewDat.4$Echinodermata, NewDat.4$Sand, NewDat.4$`Turf sand`,NewDat.4$Turf, NewDat.4$CCA, NewDat.4$`Other algae`))
  # 
  # coral_df.4$CoralType <- factor(coral_df.4$CoralType, levels = unique(coral_df.4$CoralType)[order(match(unique(coral_df.4$CoralType), col1))])
  # 
  # v4 <- ggplot(coral_df.4, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #5
  # coral_df.5 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.5)),
  #                          Percentage = c(NewDat.5$Porites, NewDat.5$Pocillopora, NewDat.5$Acropora, NewDat.5$Astreopora, NewDat.5$`Other HC` , NewDat.5$`Total HC cover`, NewDat.5$`Soft coral`, NewDat.5$Echinodermata, NewDat.5$Sand, NewDat.5$`Turf sand`,NewDat.5$Turf, NewDat.5$CCA, NewDat.5$`Other algae`))
  # 
  # coral_df.5$CoralType <- factor(coral_df.5$CoralType, levels = unique(coral_df.5$CoralType)[order(match(unique(coral_df.5$CoralType), col1))])
  # 
  # v5 <- ggplot(coral_df.5, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #6
  # 
  # coral_df.6 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.6)),
  #                          Percentage = c(NewDat.6$Porites, NewDat.6$Pocillopora, NewDat.6$Acropora, NewDat.6$Astreopora, NewDat.6$`Other HC` , NewDat.6$`Total HC cover`, NewDat.6$`Soft coral`, NewDat.6$Echinodermata, NewDat.6$Sand, NewDat.6$`Turf sand`,NewDat.6$Turf, NewDat.6$CCA, NewDat.6$`Other algae`))
  # 
  # coral_df.6$CoralType <- factor(coral_df.6$CoralType, levels = unique(coral_df.6$CoralType)[order(match(unique(coral_df.6$CoralType), col1))])
  # 
  # v6 <- ggplot(coral_df.6, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  # #7
  # 
  # coral_df.7 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA","Other algae"), each = nrow(NewDat.7)),
  #                          Percentage = c(NewDat.7$Porites, NewDat.7$Pocillopora, NewDat.7$Acropora, NewDat.7$Astreopora, NewDat.7$`Other HC` , NewDat.7$`Total HC cover`, NewDat.7$`Soft coral`, NewDat.7$Echinodermata, NewDat.7$Sand, NewDat.7$`Turf sand`,NewDat.7$Turf, NewDat.7$CCA, NewDat.7$`Other algae`))
  # 
  # coral_df.7$CoralType <- factor(coral_df.7$CoralType, levels = unique(coral_df.7$CoralType)[order(match(unique(coral_df.7$CoralType), col1))])
  # 
  # v7 <- ggplot(coral_df.7, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  # 
  #8
  
  # coral_df.8 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.8)),
  #                          Percentage = c(NewDat.8$Porites, NewDat.8$Pocillopora, NewDat.8$Acropora, NewDat.8$Astreopora, NewDat.8$`Other HC` , NewDat.8$`Total HC cover`, NewDat.8$`Soft coral`, NewDat.8$Echinodermata, NewDat.8$Sand, NewDat.8$`Turf sand`,NewDat.8$Turf, NewDat.8$CCA, NewDat.8$`Other algae`))
  # 
  # coral_df.8$CoralType <- factor(coral_df.8$CoralType, levels = unique(coral_df.8$CoralType)[order(match(unique(coral_df.8$CoralType), col1))])
  
  # v8 <- ggplot(coral_df.8, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  
  #9
  
  # coral_df.9 <- data.frame(CoralType = rep(c("Porites", "Pocillopora", "Acropora", "Astreopora", "Other HC", "Total HC", "Soft coral", "Echinodermata", "Sand", "Turf sand", "Turf", "CCA", "Other algae"), each = nrow(NewDat.9)),
  #                          Percentage = c(NewDat.9$Porites, NewDat.9$Pocillopora, NewDat.9$Acropora, NewDat.9$Astreopora, NewDat.9$`Other HC` , NewDat.9$`Total HC cover`, NewDat.9$`Soft coral`, NewDat.9$Echinodermata, NewDat.9$Sand, NewDat.9$`Turf sand`,NewDat.9$Turf, NewDat.9$CCA, NewDat.9$`Other algae`))
  # 
  # coral_df.9$CoralType <- factor(coral_df.9$CoralType, levels = unique(coral_df.9$CoralType)[order(match(unique(coral_df.9$CoralType), col1))])
  # 
  # v9 <- ggplot(coral_df.9, aes(x = fct_inorder(CoralType, ordered = NA), y = Percentage, fill = CoralType)) +
  #   geom_boxplot() +
  #   labs(title = "",
  #        x = "",
  #        y = "Percentage cover for benthic categories") +
  #   scale_x_discrete(labels=nom) +
  #   scale_fill_manual(values=col1) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #   theme(legend.position = "none") + ylim(0, 100)
  
  # cowplot::plot_grid(v1, v2, v3, v4, v5, v6, v7, v9,
  #                    labels = c("RUNA1","RUNA2", "RUNA3", "RUNA4", "RUNA5", "RUNA6","RUNA7", "RUNA9"),
  #                    ncol = 3, 
  #                    nrow = 3)
  # 
  # boxcoral_name <- paste0("box_coral_red.pdf")
  # boxcoral_path <- here::here("outputs", boxcoral_name)
  # 
  # ggsave(boxcoral_path, width = 16, height = 11)
  # 
  #### return #####
  
  return(boxcoral_path)
   
}
  