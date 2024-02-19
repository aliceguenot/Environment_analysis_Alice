#' Modelling 
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_temp temperature data from copernicus
#' @return the path to the mean value of temperature for each sites
#' @export
#'

model_msp_pool <- function(arms_id, 
                           meta_and_data, 
                           data_crypt_pool, 
                           temperature, 
                           wave_height, 
                           path_to_data_trans, 
                           flow_E, 
                           flow_N) {
  library(knitr)
  library(gt)
  library(dplyr)
  library(tidyverse)
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # data_crypt_pool <- targets::tar_read("data_cryptique_msp_pool")
  # temperature <- targets::tar_read("temp")
  # wave_height <- targets::tar_read("wave_h")
  # path_to_data_trans <- targets::tar_read("data_trans_paths")
  # flow_E <- targets::tar_read("flowveloc")
  # flow_N <- targets::tar_read("flowveloc_N")


  #### load response variables ####
  
  metadata_cryptique <- read.table(data_crypt_pool, header = TRUE, sep = ";", dec = ",")
  metadata_cryptique <-  metadata_cryptique[ metadata_cryptique$prefixe == "RUNA", ]
  data_cryptique <-  metadata_cryptique[,20:ncol( metadata_cryptique)]
  
  data_cryptique <- aggregate(data_cryptique, by = list(arms_name = metadata_cryptique$arms_name), FUN = mean)
  row.names(data_cryptique) <- data_cryptique$arms_name
  data_cryptique <- data_cryptique[,-1]
  aa <- as.data.frame(summary(data_cryptique))

  data_cryptique[] <- lapply(data_cryptique, as.numeric)
  data_cryptique <- data_cryptique[,-c((ncol(data_cryptique)-1):ncol(data_cryptique))]

  
  # path <- paste0("outputs/y.csv")
  # 
  # write.table(data_cryptique, 
  #             file = path, 
  #             dec = ",", 
  #             sep = ";",
  #             row.names = T)
  
  #### load explanatory variables ####
  #temp
  data_temp <-  read.table(file = temperature, header=TRUE, sep=";", dec=",")
  data_temp$mean
  #wave height
  data_wave <-  read.table(file = wave_height, header=TRUE, sep=";", dec=",")
  data_wave$mean
  data_wave$dev
  #flow_East
  
  flow_E <- flow_E[1]
  data_flow_E <- read.table(file = flow_E, header=TRUE, sep=";", dec=",")
  data_flow_E$mean
  #flow_N
  flow_N <- flow_N[1]
  data_flow_N <- read.table(file = flow_N, header=TRUE, sep=";", dec=",")
  data_flow_N$mean
  #Computing global seawater velocity (magnitude)
  
  magnitude <- sqrt((data_flow_N$mean)^2 + (data_flow_E$mean)^2)
  
  #surrouding habitat
  data_trans_mean <- read.table(file = path_to_data_trans[4], header=TRUE, sep=",", dec=".")
  rownames(data_trans_mean) <- data_trans_mean$X
  data_trans_mean <- subset(data_trans_mean, select = -X)
  data_trans_mean_DIV_HC <- data_trans_mean[, c(1:44)]
  data_trans_mean_DIV_HC <- vegan::decostand(data_trans_mean_DIV_HC, "pa")
  data_trans_mean_DIV_HC$div_HC <- rowSums(data_trans_mean_DIV_HC)
  site <- substr(rownames(data_trans_mean_DIV_HC), 1,1)
  
  
  ##Acropora digit/abro et pocillo
  Acro_digit <- rowSums(data.frame(data_trans_mean$Acropora_branching, data_trans_mean$Acropora_digitifera))
  Acro_digit <- rep(tapply(Acro_digit, site, mean), each = 3)
  
  Acro_abro <- rep(tapply(data_trans_mean$Acropora_abrotanoides, site, mean), each = 3)
  
  Acro_robu <- rep(tapply(data_trans_mean$Acropora_Robusta, site, mean), each = 3)
  
  data_trans_mean_cat3 <- read.table(file = path_to_data_trans[8], header=TRUE, sep=",", dec=".")
  rownames(data_trans_mean_cat3) <- data_trans_mean_cat3$X
  data_trans_mean_cat3 <- subset(data_trans_mean_cat3, select = -X)
  
  Pocillo <- rep(tapply(data_trans_mean_cat3$Pocillopora, site, mean), each = 3)
  
  ##Acropora, Pocillopora, Astreopora, Porites
  data_trans_genus <- read.table(file = path_to_data_trans[8], header=TRUE, sep=",", dec=".")
  rownames(data_trans_genus) <- data_trans_mean$X
  data_trans_genus <- subset(data_trans_genus, select = -X)
  
  sort(colSums(data_trans_genus))
  Porites <- rep(tapply(data_trans_genus$Porites, site, mean), each = 3)
  Astreopora <- rep(tapply(data_trans_genus$Astreopora, site, mean), each = 3)
  Acropora <- rep(tapply(data_trans_genus$Acropora, site, mean), each = 3)
  
  
  data_trans_cat_mean <- read.table(file = path_to_data_trans[5], header=TRUE, sep=",", dec=".")
  rownames(data_trans_cat_mean) <- data_trans_cat_mean$X
  data_trans_cat_mean <- subset(data_trans_cat_mean, select = -X)
  
  Soft_coral <- rep(tapply(data_trans_cat_mean$Soft_Coral, site, mean), each = 3)
  
  ##DIVERSITE EN CORAUX DURS
  diversité_c <- vegan::specnumber(data_trans_mean_DIV_HC, site)
  
  # diversité_c <- rep(tapply(data_trans_mean_DIV_HC$div_HC, site, mean), each = 3)
  # diversité_c <- rep(tapply(data_trans_mean_DIV_HC$div_HC, site, sd), each = 3)
  ##ABONDANCE CORAUX DURS
  data_trans_mean_cat2 <- read.table(file = path_to_data_trans[6], header=TRUE, sep=",", dec=".")
  rownames(data_trans_mean_cat2) <- data_trans_mean_cat2$X
  data_trans_mean_cat2 <- subset(data_trans_mean_cat2, select = -X)
  site <- substr(rownames(data_trans_mean_cat2), 1,1)
  
  branching_coral <- rep(tapply(data_trans_mean_cat2$Branching_Coral, site, mean), each = 3)
  
  massive_coral <- rep(tapply(data_trans_mean_cat2$Massive_Coral, site, mean), each = 3)
  
  data_trans_mean_cat2$comb_algal_turf <- rowSums(data_trans_mean_cat2[, c("Turf_algae", "Turf_and_sand")])
  algal_turf <- rep(tapply(data_trans_mean_cat2$comb_algal_turf, site, mean), each = 3)
  
  names = names(algal_turf)
  #MERMAID
  data_mermaid <- data.frame(names, c(1,1,1, 3,3,3, 3,3,3, 3,3,3, 3,3,3, 5,5,5, 2,2,2, 3,3,3, 4,4,4))
  colnames(data_mermaid) <- c("site", "topo")
  
  #SANCTUAIRE
  data_sanct <- data.frame(names, c("non", "non", "non", "oui", "oui", "oui", "oui", "oui", "oui", "oui", "oui", "oui", "oui", "oui", "oui","non", "non", "non", "oui", "oui", "oui","non", "non", "non","non", "non", "non"))
  colnames(data_sanct) <- c("site", "sanct")
  
  #Env
  # env <- data.frame(cbind(data_sanct$sanct, 
  #                         data_mermaid$topo,  
  #                         as.numeric(diversité_c), 
  #                         as.numeric(Pocillo), 
  #                         as.numeric(Acro_robu), 
  #                         as.numeric(Acro_digit), 
  #                         as.numeric(Acro_abro), 
  #                         as.numeric(massive_coral), 
  #                         as.numeric(algal_turf), 
  #                         as.numeric(data_flow_N$mean), 
  #                         as.numeric(data_flow_E$mean), 
  #                         as.numeric(data_wave$mean), 
  #                         as.numeric(data_temp$kurt)))
  
  # env <- data.frame(cbind(data_sanct$sanct, 
  #                         data_mermaid$topo,  
  #                         as.numeric(diversité_c), 
  #                         as.numeric(Pocillo), 
  #                         as.numeric(Acropora), 
  #                         as.numeric(Astreopora), 
  #                         as.numeric(Porites), 
  #                         as.numeric(algal_turf),
  #                         as.numeric(Soft_coral),
  #                         as.numeric(data_flow_N$mean), 
  #                         as.numeric(data_flow_E$mean), 
  #                         as.numeric(data_wave$dev), 
  #                         as.numeric(data_temp$mean_dev)))
  env <- data.frame(cbind(data_sanct$sanct, 
                          as.numeric(diversité_c), 
                          as.numeric(Pocillo), 
                          as.numeric(Acropora), 
                          as.numeric(Astreopora), 
                          as.numeric(Porites), 
                          as.numeric(algal_turf),
                          as.numeric(Soft_coral),
                          as.numeric(magnitude), 
                          as.numeric(data_wave$dev), 
                          as.numeric(data_temp$mean_dev),
                          data_mermaid$topo))
  
  
  # env <- data.frame(cbind(data_sanct$sanct, data_mermaid$topo,  as.numeric(diversité_c), as.numeric(branching_coral), as.numeric(massive_coral), as.numeric(algal_turf), as.numeric(data_flow_N$mean), as.numeric(data_flow_E$mean), as.numeric(data_wave$mean), as.numeric(data_temp$kurt)))
  # colnames(env) = c("sanctuaire", "complexite_topo", "div_c", "branching_coral", "massive_coral", "algal_turf", "flow1", "flow2", "wave_height", "temperature_kur")
  # colnames(env) = c("sanctuaire", "complexite_topo", "div_c", "Pocillo", "Acro_robu", "Acro_digit", "Acro_abro", "massive_coral", "algal_turf", "flow1", "flow2", "wave_height", "temperature")
  colnames(env) = c("sanctuaire", "div_c", "Pocillo", "Acropora", "Astreopora", "Porites", "algal_turf", "Soft_coral", "flow_veloc_magnitude", "wave_height_dev", "temperature_dev","complexite_topo")
  
  env <- env[,-1]
  
  for (col in 1:ncol(env)) {
    env[, col] <- as.numeric(env[, col])
  }
  
  
  
  # 
  # path <- paste0("outputs/x.csv")
  # 
  # write.table(env, 
  #             file = path, 
  #             dec = ",", 
  #             sep = ";",
  #             row.names = T)
  # 
  #### correl test ####
  #correl variables explicative 
  class(env$branching_coral)
  corr <- cor(env[2:ncol(env)])
  corr_pool <- as.matrix(corr) 
  corr_pool[corr_pool > -0.7 & corr_pool < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  data_corr_pool <- as.data.frame(corr_pool)
  data_corr_pool[is.na(data_corr_pool)] <- 0
  data_corr_pool <- data_corr_pool[, colSums(data_corr_pool) != 0]
  data_corr_pool <- data_corr_pool[ rowSums(data_corr_pool) != 0,]
  
  corrplot::corrplot(as.matrix(data_corr_pool), 
                     method = "square", 
                     type = "lower", 
                     diag = F)
  
  corrplot::corrplot(as.matrix(data_corr_pool),
                     method = "square",
                     type = "lower",
                     diag = F,
                     # ordre = "original",
                     cl.pos = "r",
                     tl.col = "white")
  
  # text(0, -4:6, expression("Hard coral diversity", 
  #                          paste(italic("Pocillopora")),
  #                         paste(italic("Acropora")), 
  #                          paste(italic("Astreopora")), 
  #                          paste(italic("Porites")),
  #                          "Algal turf", 
  #                          "Soft Coral",
  #                          "Northwar flow velocity",
  #                          "Eastward flow velocity",
  #                          "Wave height deviation",
  #                          "Temperature deviation"))
  
  # correl var expicative et var reponse
  
  for (col in 1:ncol(env)) {
    env[, col] <- scale(env[, col])
  }
  
  class(env$div_c)
  
  corr <- cor(data_cryptique, env)
  corr_pool <- as.matrix(corr)
  # corr_pool[corr_pool > -0.7 & corr_pool < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  data_corr_pool <- as.data.frame(corr_pool)
  data_corr_pool[is.na(data_corr_pool)] <- 0
  data_corr_pool <- data_corr_pool[, colSums(data_corr_pool) != 0]
  data_corr_pool <- data_corr_pool[ rowSums(data_corr_pool) != 0,]
  
  corrplot::corrplot(as.matrix(data_corr_pool))
  
  
  corrplot::corrplot(as.matrix(data_corr_pool), 
                     method = "square",
                     cl.pos = "r",
                     tl.col = "white")
  
  
  # #### GAM####
  # 
  # #### Visualisation
  # 
  # data_comb = data.frame(cbind(data_cryptique, env))
  # 
  # ggplot(data_comb, aes(x =wave_height, y = Polysyncraton_rostrum_ascc)) +
  #   geom_point() +            # Add scatterplot points
  #   geom_smooth(se = FALSE, method = "lm")  
  # 
  # ggplot(data_comb, aes(x = complexite_topo, y = LCBD)) +
  #   geom_point() +            # Add scatterplot points
  #   geom_smooth(se = FALSE, method = "lm")  
  # 
  # 
  # 
  # model <- mgcv::gam(data_cryptique$Polysyncraton_rostrum_ascc ~ mgcv::s(env$data_wave.mean, bs = "cr") + mgcv::s(env$Branching_Corals, bs = "cr"))
  # 
  #### model François ####
  # --> Minimum adequate model (Hard  Book - crawley)
  #### Abiot ####
  
  
  env_abiot <- env[,c("wave_height_dev",
                      "flow_veloc_magnitude", 
                      "complexite_topo")]
  
  for (col in 1:ncol(env_abiot)) {
    env_abiot[, col] <- scale(env_abiot[, col])
  }
  
  
  # pour sp allant de y = colnames(data_cryptique)[1] à y = colnames(data_cryptique)[ncol(data_cryptique)]
  
  
  rest_fin <- lapply(colnames(data_cryptique), function(u) {
    #u <- "Polysyncraton_rostrum_ascc"
    
    y <- data_cryptique[,u]
    
    sort_variable <-  function(y, env_abiot) {
      
      mod_mat <- data.frame(y, env_abiot)
      
      mod <- lm(y ~ ., data = mod_mat)
      
      #if (shapiro.test(residuals(mod))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
      
      Rtot <- summary(mod)$r.squared
      
      deltas <- sapply(names(env_abiot), function(v) {
        
        #v = "wave_height"
        
        mod_mat_v <- mod_mat[, names(mod_mat) != v]
        
        mod_v <- lm(y ~ ., data = mod_mat_v)
        
        Rtot - summary(mod_v)$r.squared
        
      })
      
      deltas <- sort(deltas, decreasing = TRUE)
      
      names(deltas)
      
    }
    
    var_list <- sort_variable(y = y, env_abiot = env_abiot)
    
    reduce_mod <- function(y, env_abiot) {
      
      mod_mat_sort <- data.frame(y = y, env_abiot)
      
      #if(length(var_list) == 0) stop("No more variables")
      
      mod_full <- lm(y ~ ., data = mod_mat_sort)
      
      #if (shapiro.test(residuals(mod_full))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
      
      coef_mod      <- summary(mod_full)$coefficients
      
      coef_mod      <- as.data.frame(subset(coef_mod, rownames(coef_mod) != "(Intercept)"))
      
      max_row_index <- which.max(coef_mod[,4])
      
      var_supp      <- rownames(coef_mod)[max_row_index]
      
      max_p_value   <- coef_mod[max_row_index, 4]
      
      if (max_p_value < 0.05) {
        message("Toutes les variables sont significatives !")
        return(list(sum = summary(mod_full), shap = shapiro.test(residuals(mod_full))))
      } else {
        
        env_abiot <- data.frame(subset(env_abiot, select = names(env_abiot) != var_supp), check.names = FALSE)
        
        if (ncol(env_abiot) == 0) {
          message("Aucune variable explicative n'est significative")
          return("NA")
        } else {
          
          Recall(y = y, env_abiot = env_abiot)
          
        }
        
      }
    }
    
    res <- reduce_mod(y = y, env_abiot = env_abiot[, var_list])
    
  })
  
  names(rest_fin) <- colnames(data_cryptique)
  #check homoscedasticity
  # ?fitted
  
  # final_mod <- res$sum$call
  # 
  # residuals_plot <- ggplot(data = data.frame(cbind(y,env)), aes(x = fitted(final_mod), y = resid(final_mod))) +
  #   geom_point() +
  #   geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  #   labs(x = "Fitted Values", y = "Residuals") +
  #   ggtitle("Residuals vs Fitted Values") +
  #   theme_minimal()
  # 
  # print(residuals_plot)
  
  
  # Transformer la liste en un tableau de données
  tab_vide <- as.data.frame(matrix(NA, nrow = ncol(data_cryptique), ncol = length(colnames(env_abiot))))
  colnames(tab_vide) <- colnames(env_abiot)
  rownames(tab_vide) <- colnames(data_cryptique)
  tab_vide$shap <- NA
  tab_vide$R <- NA
  
  
  # resultat <- lapply(names(rest_fin), function(x) {
  # 
  #   # x <- "CCA"
  # 
  #   if ("NA" %in% rest_fin[[x]]) {
  #     tab_vide[x, ] <- NA
  #     } else {
  # 
  #     data_x <- as.data.frame(rest_fin[[x]]$sum$coefficients)
  # 
  #     data_x <- as.data.frame(subset(data_x, rownames(data_x) != "(Intercept)"))
  # 
  #     p_values <- data_x$`Pr(>|t|)`
  # 
  #     names(p_values) <- rownames(data_x)
  # 
  #     tab_vide[x, names(p_values)] <- p_values
  # 
  #     tab_vide[x, "shap"] <-   rest_fin[[x]]$shap$p.value
  # 
  #   }
  # 
  
  for (x in names(rest_fin)) {
    if ("NA" %in% rest_fin[[x]]) {
      tab_vide[x, ] <- NA
    } else {
      data_x <- as.data.frame(rest_fin[[x]]$sum$coefficients)
      data_x <- as.data.frame(subset(data_x, rownames(data_x) != "(Intercept)"))
      estim <- data_x$Estimate
      names(estim) <- rownames(data_x)
      tab_vide[x, names(estim)] <- estim
      if (rest_fin[[x]]$shap$p.value > 0.05) {
        tab_vide[x, "shap"] <- "OK"  
        tab_vide[x, "R2"] <- rest_fin[[x]]$sum$adj.r.squared
      } else {
        tab_vide[x, "shap"] <- "Not OK"}
    }
  }
  
  
  tab_vide_OK <- subset(tab_vide, shap == "OK")
  # tab_vide_OK <- data.frame(Species = rownames(tab_vide_OK), tab_vide_OK)
  
  path_to_tab <- paste0("outputs/tab_abiot_msp_pool.csv")
  
  
  write.table(tab_vide_OK, 
              file = path_to_tab, 
              dec = ",", 
              sep = ";",
              row.names = T)
  
  # 
  #   abiot_table <- tab_vide_OK %>%
  #     kable("html") %>%
  #     kable_styling("striped", full_width = FALSE)
  
  # htmltools::save_html(abiot_table, file = "abiot_table_27_01.html")
  
  
  # ggplot(data_comb, aes(x = env$wave_height, y = LCBD)) +
  #   geom_point() +            # Add scatterplot points
  #   geom_smooth(se = FALSE, method = "lm")  
  # 
  # 
  
  #### Biot ####
  # On retire div_c car correllé a Pocillo, 
  
  # env_biot <- env[, c("Pocillo",
  #                     "Acro_robu",
  #                     "Acro_digit",
  #                     "Acro_abro",
  #                     "algal_turf", 
  #                     "massive_coral")]
  env_biot <- env[, c("Pocillo",
                      "Acropora",
                      "Porites",
                      "Soft_coral",
                      "algal_turf")]
  # env_biot <- env[, c("div_c", 
  #                     "branching_coral", 
  #                     # "algal_turf", 
  #                     "massive_coral")]
  
  for (col in 1:ncol(env_biot)) {
    env_biot[, col] <- scale(env_biot[, col])
  }
  
  
  # pour sp allant de y = colnames(data_cryptique)[1] à y = colnames(data_cryptique)[ncol(data_cryptique)]
  
  
  
  rest_fin <- lapply(colnames(data_cryptique), function(u) {
    #u <- "Hydrozoa"
    
    y <- data_cryptique[,u]
    
    sort_variable <-  function(y, env_biot) {
      
      mod_mat <- data.frame(y, env_biot)
      
      mod <- lm(y ~ ., data = mod_mat)
      
      #if (shapiro.test(residuals(mod))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
      
      Rtot <- summary(mod)$r.squared
      
      deltas <- sapply(names(env_biot), function(v) {
        
        #v = "wave_height"
        
        mod_mat_v <- mod_mat[, names(mod_mat) != v]
        
        mod_v <- lm(y ~ ., data = mod_mat_v)
        
        Rtot - summary(mod_v)$r.squared
        
      })
      
      deltas <- sort(deltas, decreasing = TRUE)
      
      names(deltas)
      
    }
    
    var_list <- sort_variable(y = y, env_biot = env_biot)
    
    reduce_mod <- function(y, env_biot) {
      
      mod_mat_sort <- data.frame(y = y, env_biot)
      
      #if(length(var_list) == 0) stop("No more variables")
      
      mod_full <- lm(y ~ ., data = mod_mat_sort)
      
      #if (shapiro.test(residuals(mod_full))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
      
      coef_mod      <- summary(mod_full)$coefficients
      
      coef_mod      <- as.data.frame(subset(coef_mod, rownames(coef_mod) != "(Intercept)"))
      
      max_row_index <- which.max(coef_mod[,4])
      
      var_supp      <- rownames(coef_mod)[max_row_index]
      
      max_p_value   <- coef_mod[max_row_index, 4]
      
      if (max_p_value < 0.05) {
        message("Toutes les variables sont significatives !")
        return(list(sum = summary(mod_full), shap = shapiro.test(residuals(mod_full))))
      } else {
        
        env_biot <- data.frame(subset(env_biot, select = names(env_biot) != var_supp), check.names = FALSE)
        
        if (ncol(env_biot) == 0) {
          message("Aucune variable explicative n'est significative")
          return("NA")
        } else {
          
          Recall(y = y, env_biot = env_biot)
          
        }
        
      }
    }
    
    res <- reduce_mod(y = y, env_biot = env_biot[, var_list])
    
    
  })
  
  names(rest_fin) <- colnames(data_cryptique)
  
  # Transformer la liste en un tableau de données
  tab_vide <- as.data.frame(matrix(NA, nrow = ncol(data_cryptique), ncol = length(colnames(env_biot))))
  colnames(tab_vide) <- colnames(env_biot)
  rownames(tab_vide) <- colnames(data_cryptique)
  tab_vide$shap <- NA
  tab_vide$R <- NA
  
  # 
  # resultat <- lapply(names(rest_fin), function(x) {
  #   
  #   # x <- "CCA"
  #   
  #   if ("NA" %in% rest_fin[[x]]) {
  #     tab_vide[x, ] <- NA
  #     } else {
  #   
  #     data_x <- as.data.frame(rest_fin[[x]]$sum$coefficients)
  #   
  #     data_x <- as.data.frame(subset(data_x, rownames(data_x) != "(Intercept)"))
  #   
  #     p_values <- data_x$`Pr(>|t|)`
  #   
  #     names(p_values) <- rownames(data_x)
  #   
  #     tab_vide[x, names(p_values)] <- p_values
  #   
  #     tab_vide[x, "shap"] <-   rest_fin[[x]]$shap$p.value
  #     
  #   }
  
  
  for (x in names(rest_fin)) {
    if ("NA" %in% rest_fin[[x]]) {
      tab_vide[x, ] <- NA
    } else {
      data_x <- as.data.frame(rest_fin[[x]]$sum$coefficients)
      data_x <- as.data.frame(subset(data_x, rownames(data_x) != "(Intercept)"))
      estim <- data_x$Estimate
      names(estim) <- rownames(data_x)
      tab_vide[x, names(estim)] <- estim
      if (rest_fin[[x]]$shap$p.value > 0.05) {
        tab_vide[x, "shap"] <- "OK"  
        tab_vide[x, "R"] <- rest_fin[[x]]$sum$adj.r.squared
      } else {
        tab_vide[x, "shap"] <- "Not OK"}
    }
  }
  
  
  tab_vide_OK <- subset(tab_vide, shap == "OK")
  
  
  path_to_tab <- paste0("outputs/tab_biot_msp_pool.csv")
  
  
  write.table(tab_vide_OK, 
              file = path_to_tab, 
              dec = ",", 
              sep = ";",
              row.names = T)
  
  
  # biot_table <- tab_vide_OK %>%
  #   kable("html") %>%
  #   kable_styling("striped", full_width = FALSE)
  
  # htmltools::save_html(biot_table, file = "biot_table_27_01.html")
  
  #### Random Forest Eric ####
  

  # https://bradleyboehmke.github.io/HOML/random-forest.html
  # https://uc-r.github.io/random_forests#tune
  # https://rpubs.com/chidungkt/449576
  # https://evansmurphy.wixsite.com/evansspatial/random-forest-sdm

  # Tuning: Random forests are fairly easy to tune since there are only a handful of tuning parameters. Typically, the primary concern when starting out is tuning the number of candidate variables to select from at each split. However, there are a few additional hyperparameters that we should be aware of. Although the argument names may differ across packages, these hyperparameters should be present:
  # 
  # ntree: number of trees. We want enough trees to stabilize the error but using too many trees is unnecessarily inefficient, especially when using large data sets.
  # mtry: the number of variables to randomly sample as candidates at each split. When mtry = p, the model equates to bagging. When mtry = 1, the split variable is completely random, so all variables get a chance but can lead to overly biased results. A common suggestion is to start with 5 values evenly spaced across the range from 2 to p.
  # sampsize: the number of samples to train on. The default value is 63.25% of the training set since this is the expected value of unique observations in the bootstrap sample. Lower sample sizes can reduce the training time but may introduce more bias than necessary. Increasing the sample size can increase performance but at the risk of overfitting because it introduces more variance. Typically, when tuning this parameter we stay near the 60-80% range.
  # nodesize: minimum number of samples within the terminal nodes. Controls the complexity of the trees. Smaller node size allows for deeper, more complex trees and smaller node results in shallower trees. This is another bias-variance tradeoff where deeper trees introduce more variance (risk of overfitting) and shallower trees introduce more bias (risk of not fully capturing unique patters and relationships in the data).
  # maxnodes: maximum number of terminal nodes. Another way to control the complexity of the trees. More nodes equates to deeper, more complex trees and less nodes result in shallower trees.

  # Packages
  # if (!require("pacman")) 
  # install.packages("pacman")
  pacman::p_load(readr, rsample, randomForest, rfUtilities, ranger, caret, h2o, tidyverse, terra, tidyterra, ggplot2, ggpmisc, ggthemes, usdm)
  # install.packages("targz/h2o-3.44.0.2/R/h2o_3.44.0.2.tar.gz",
                     # repos = NULL, type = "source")
  library(h2o)
  # ============================================================================
  # Call for 'Sp'
  Sp <- data_cryptique
  # ============================================================================
  # Call for 'Environment' 
  Environment <- env
  # Detecting Multicollinearity: Some variables may act as "hingepin" variables, falsely suggesting multicollinearity when they are not truly redundant. To address this, we can conduct a "leave one out" test, assessing whether any variables are influencing others to falsely appear multicollinear
  cl <- multi.collinear(Environment, p = 0.05)
  for(l in cl) {
    cl.test <- Environment[,-which(names(Environment)==l)]
    print(paste("Remove variable", l, sep=": "))
    multi.collinear(cl.test, p=0.05) 
  }
  Environment <- Environment[,-which(names(Environment) %in% cl )]
  rm(cl.test, cl, l)

  # ============================================================================
  Metrics.RF.all <- list()
  Variable.importance.RF.all <- list()
  Best.model.all <- list()
  Prediction.all <- list()

  # ============================================================================
  # Calculate Random Forest with optimised search
  
  # Environment = env[,c(1,2)]
  
  # i = 5
  
  h2o.init(max_mem_size = "5g")
  
  for (i in 1:ncol(Sp)) {
    
    if (i > 1) {
      
      rm(Data.RF, h2o_rf1, hyper_grid, response, predictors, n_features,  search_criteria, random_grid, random_grid_perf, best_grid, best_model_h2o_rf, best_model, metrics, Environment.h2o, pred_h2o)
      
    }
    
    # start up h2o
    Species <- as.vector(Sp[, i])
    df <- cbind.data.frame(Species, Environment)
    df <- lapply(df, as.vector)
    Data.RF  <- as.h2o(df)
    # Split the dataset into a train and valid set:

    Data.split <- h2o.splitFrame(data = Data.RF, ratios = 0.8, seed = 123)
    Data_train <- Data.split[[1]]
    Data_valid <- Data.split[[2]]
    # Data_train  <- Data.RF # Wit h no split
    
    # convert training data to h2o object
    train_h2o <- as.h2o(Data_train)
    # set the response column to the given variable
    response <- names(Data_train[1])
    # set the predictor names
    predictors <- setdiff(colnames(Data_train), response)
    # number of features
    n_features <- length(setdiff(names(Data_train), response))
    
    # The following fits a default random forest model with h2o
    h2o_rf1 <- h2o.randomForest(
      x = predictors, 
      y = response,
      training_frame = Data_train, 
      ntrees = n_features * 10,
      seed = 123,
      nfolds = 4
    )
    
    # hyperparameter grid
    hyper_grid <- list(
      ntrees = seq(50, 1000, by = 50),
      mtries = seq(2, 9, by = 1),
      min_rows = c(1, 3, 5),
      max_depth = c(5, 10, 20, 30, 40, 50),  # https://crunchingthedata.com/max-depth-in-random-forests/
      sample_rate = c(.55, .632, .70, .80)
    )
    
    # random grid search strategy
    search_criteria <- list(
      strategy = "RandomDiscrete",
      stopping_metric = "RMSE",
      stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
      stopping_rounds = 10,         # over the last 10 models
      max_runtime_secs = 60*10      # or stop search after 10 min.
    )
    
    # perform grid search 
    random_grid <- h2o.grid(
      algorithm = "randomForest",
      grid_id = "rf_random_grid",
      x = predictors, 
      y = response, 
      training_frame = Data_train,
      hyper_params = hyper_grid,
      seed = 123,
      nfolds = 4,
      stopping_metric = "RMSE",   
      stopping_rounds = 10,           # stop if last 10 trees added 
      stopping_tolerance = 0.001,     # don't improve RMSE by 0.1%
      search_criteria = search_criteria
    )
    
    # collect the results and sort by our model performance metric 
    # of choice
    random_grid_perf <- h2o.getGrid(
      grid_id = "rf_random_grid", 
      sort_by = "mse", 
      decreasing = FALSE
    )
    best_grid <- random_grid_perf@summary_table
    
    # Best RF: 
    best_model <- h2o.getModel(random_grid_perf@model_ids[[1]])
    Best.model.all[[i]] <- cbind.data.frame(ntrees = best_model@allparameters[["ntrees"]],
                                            max_depth = best_model@allparameters[["max_depth"]],
                                            min_rows = best_model@allparameters[["min_rows"]],
                                            mtries = best_model@allparameters[["mtries"]],
                                            sample_rate = best_model@allparameters[["sample_rate"]])
    
    # Perform best Random Forest model: 
    best_model_h2o_rf <- h2o.randomForest(
      x = predictors, 
      y = response,
      training_frame = Data_train, 
      ntrees = best_model@allparameters[["ntrees"]],
      max_depth = best_model@allparameters[["max_depth"]],
      min_rows = best_model@allparameters[["min_rows"]],
      mtries = best_model@allparameters[["mtries"]],
      sample_rate = best_model@allparameters[["sample_rate"]],
      seed = 123,
      nfolds = 4
    )
    
    # Extract metrics from the best random forest 
    metrics <- best_model_h2o_rf@model[["cross_validation_metrics_summary"]]
    metrics <- data.frame(t(metrics[-nrow(metrics), c(-1,-2)]))
    metrics <- metrics %>% gather(Metric, Value)
    Metrics.RF.all[[i]] <- metrics
    
    # Variable importance
    Variable.importance.RF.all[[i]] <- best_model_h2o_rf@model[["variable_importances"]]
  
    # Once we’ve identified our preferred model we can use the traditional predict function to make predictions on a new data set
    Environment <- lapply(Environment, as.vector)
    Environment.h2o <- as.h2o(Environment)
    pred_h2o <- as.data.frame(predict(best_model_h2o_rf, Environment.h2o))
    Prediction.all[[i]] <- pred_h2o
    
  }
  
  # Plot results expected versus observed
  Compare.Results <- data.frame(Sp[, i], pred_h2o)
  colnames(Compare.Results)<- c("Observations","Predictions")
  
  
  install.packages("Matrix")
  library(Matrix)
  install.packages("targz/ggpmisc_0.5.5.tar.gz",
                   repos = NULL, type = "source")
  
  library(ggpmisc)
  
  Compare <- ggplot(data = Compare.Results, aes(x = Observations, y = Predictions)) +
    geom_point(shape = 21, alpha = 0.9, fill = "dodgerblue", size = 3, colour = "black") + 
    geom_smooth(method='lm', formula= y~x,  colour = "firebrick") +
    stat_poly_eq(use_label(c("eq", "R2")), size = 5) +
    theme_hc() + 
    theme(axis.text.x = element_text(face="bold", size = 12), 
          axis.text.y = element_text(face="bold", size = 12))
  Compare
  
  # Correlation between observations and predictions
  res <- cor.test(Compare.Results$Observations, Compare.Results$Predictions, method = "pearson")
  res
  
  # Clean environment
  
  rm(train_h2o, response, predictors, n_features, h2o_rf1, mtries, hyper_grid, search_criteria, random_grid, random_grid_perf, best_grid, best_model, best_model_h2o_rf, metrics, Metric_result, Environment.h2o, pred_h2o, Results_PC, Raster_PC, Raster_PC.res, Plot_PC, data_train, data_PCA, Compare_PC, Compare.PC.model)
  
  
  
  
  #### GPT try ####
  
  
  
  h2o.init(max_mem_size = "5g")
  
  # Create empty lists to store grid objects and other results
  Grids <- list()
  BestModels <- list()
  MetricsList <- list()
  VariableImportanceList <- list()
  PredictionsList <- list()
  
  for (i in 1:ncol(Sp)) {
    if (i > 1) {
      # Clear objects to prevent conflicts
      rm(Data.RF, h2o_rf1, hyper_grid, response, predictors, n_features, search_criteria, random_grid, random_grid_perf, best_grid, best_model_h2o_rf, best_model, metrics, Environment.h2o, pred_h2o)
    }
    
    
    Species <- as.vector(Sp[, i])
    df <- cbind.data.frame(Species, Environment)
    df <- lapply(df, as.vector)
    Data.RF  <- as.h2o(df)
    # Start up h2o
    # Data.RF <- as.h2o(cbind.data.frame(Species = Sp[, i], Environment))
    
    # Split the dataset into a train and valid set
    Data.split <- h2o.splitFrame(data = Data.RF, ratios = 0.8, seed = 123)
    Data_train <- Data.split[[1]]
    
    # Set response and predictors
    response <- names(Data_train[1])
    predictors <- setdiff(colnames(Data_train), response)
    n_features <- length(setdiff(names(Data_train), response))
    
    # Fit default random forest model with h2o
    h2o_rf1 <- h2o.randomForest(
      x = predictors, 
      y = response,
      training_frame = Data_train, 
      ntrees = n_features * 10,
      seed = 123,
      nfolds = 4
    )
    
    # Hyperparameter grid
    hyper_grid <- list(
      ntrees = seq(50, 1000, by = 50),
      mtries = seq(2, 9, by = 1),
      min_rows = c(1, 3, 5, 10),
      max_depth = c(5, 10, 20, 30, 40, 50),
      sample_rate = c(.55, .632, .70, .80)
    )
    
    # Random grid search strategy
    search_criteria <- list(
      strategy = "RandomDiscrete",
      stopping_metric = "RMSE",
      stopping_tolerance = 0.001,
      stopping_rounds = 10,
      max_runtime_secs = 60 * 10
    )
    
    # Perform grid search 
    random_grid <- h2o.grid(
      algorithm = "randomForest",
      grid_id = paste0("rf_random_grid_", i),
      x = predictors, 
      y = response, 
      training_frame = Data_train,
      hyper_params = hyper_grid,
      seed = 123,
      nfolds = 4,
      stopping_metric = "RMSE",
      stopping_rounds = 10,
      stopping_tolerance = 0.001,
      search_criteria = search_criteria
    )
    
    # Store grid object in list
    Grids[[i]] <- random_grid
    
    # Retrieve best model and its parameters
    random_grid_perf <- h2o.getGrid(grid_id = paste0("rf_random_grid_", i), sort_by = "mse", decreasing = FALSE)
    best_model <- h2o.getModel(random_grid_perf@model_ids[[1]])
    BestModels[[i]] <- cbind.data.frame(
      ntrees = best_model@allparameters[["ntrees"]],
      max_depth = best_model@allparameters[["max_depth"]],
      min_rows = best_model@allparameters[["min_rows"]],
      mtries = best_model@allparameters[["mtries"]],
      sample_rate = best_model@allparameters[["sample_rate"]]
    )
    
    # Perform best Random Forest model
    best_model_h2o_rf <- h2o.randomForest(
      x = predictors, 
      y = response,
      training_frame = Data_train, 
      ntrees = best_model@allparameters[["ntrees"]],
      max_depth = best_model@allparameters[["max_depth"]],
      min_rows = best_model@allparameters[["min_rows"]],
      mtries = best_model@allparameters[["mtries"]],
      sample_rate = best_model@allparameters[["sample_rate"]],
      seed = 123,
      nfolds = 4
    )
    
    # Extract metrics from the best random forest 
    metrics <- best_model_h2o_rf@model[["cross_validation_metrics_summary"]]
    metrics <- data.frame(t(metrics[-nrow(metrics), c(-1,-2)]))
    metrics <- metrics %>% gather(Metric, Value)
    MetricsList[[i]] <- metrics
    
    # Variable importance
    VariableImportanceList[[i]] <- best_model_h2o_rf@model[["variable_importances"]]
    
    # Make predictions on Environment
    Environment.h2o <- as.h2o(Environment)
    pred_h2o <- as.data.frame(predict(best_model_h2o_rf, Environment.h2o))
    PredictionsList[[i]] <- pred_h2o
  }
  
  # save.image(file = "outputs/RDATA/my_work_space_RF.RData")
  
  names(MetricsList) <- colnames(Sp)
  names(VariableImportanceList) <- colnames(Sp)
  names(PredictionsList) <- colnames(Sp)
  names(BestModels) <- colnames(Sp)
  
  # Transformer la liste en un tableau de données
  tab_RF_vide <- as.data.frame(matrix(NA, nrow = ncol(Sp), ncol = length(colnames(Environment))))
  colnames(tab_RF_vide) <- colnames(Environment)
  rownames(tab_RF_vide) <- colnames(Sp)
  tab_RF_vide$formula <- NA
  tab_RF_vide$R2 <- NA
  tab_RF_vide$TreesNum <- NA
  tab_RF_vide$SampleSize <- NA
  
  
  
  for (x in names(Sp)) {
    #x=1
      VarImpL <- as.data.frame(VariableImportanceList[[x]])
      rownames(VarImpL) <- VarImpL$variable
      tab_RF_vide[x,"div_c"] <- VarImpL["div_c", 4]
      tab_RF_vide[x,"Pocillo"] <- VarImpL["Pocillo", 4]
      tab_RF_vide[x,"Acropora"] <- VarImpL["Acropora", 4]
      tab_RF_vide[x,"Astreopora"] <- VarImpL["Astreopora", 4]
      tab_RF_vide[x,"Porites"] <- VarImpL["Porites", 4]
      tab_RF_vide[x,"algal_turf"] <- VarImpL["algal_turf", 4]
      tab_RF_vide[x,"Soft_coral"] <- VarImpL["Soft_coral", 4]
      tab_RF_vide[x,"flow_veloc_magnitude"] <- VarImpL["flow_veloc_magnitude", 4]
      tab_RF_vide[x,"wave_height_dev"] <- VarImpL["wave_height_dev", 4]
  
      Compare.Results <- data.frame(Sp[, x], PredictionsList[[x]])
      colnames(Compare.Results) <- c("Observations","Predictions")
      lm <- lm(Compare.Results$Predictions ~ Compare.Results$Observations)
      formula <- paste0("y = ", round(lm$coefficients[2], 2),"*x + ", round(lm$coefficients[1], 2))
      lm <- summary(lm)
      R2 <- lm$adj.r.squared
      tab_RF_vide[x,"formula"] <- formula
      tab_RF_vide[x,"R2"] <- R2
      
      tab_RF_vide[x, "TreesNum"] <- BestModels[[x]]$ntrees
      tab_RF_vide[x, "SampleSize"] <- BestModels[[x]]$sample_rate
      
  }
  
  tab_RF <- tab_RF_vide

  
  path_to_tab <- paste0("outputs/Results_RF.csv")
  
  
  write.table(tab_RF, 
              file = path_to_tab, 
              dec = ",", 
              sep = ";",
              row.names = T,
              col.names = T)
 
  
  # save.image(file = "outputs/RDATA/my_work_space_RF.RData")
  
  # Shutdown h2o cluster after using models
  h2o.shutdown()
  
  rm(train_h2o, response, predictors, n_features, h2o_rf1, mtries, hyper_grid, search_criteria, random_grid, random_grid_perf, best_grid, best_model, best_model_h2o_rf, metrics, Metric_result, Environment.h2o, pred_h2o, Results_PC, Raster_PC, Raster_PC.res, Plot_PC, data_train, data_PCA, Compare_PC, Compare.PC.model)
  
  
  return(path_to_tab)
  
  
  
}
