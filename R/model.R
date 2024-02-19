#' Modelling 
#'
#' @param meta_and_data the path to the raw data file
#' @param arms_id the ID of the arms to subset for
#' @param data_temp temperature data from copernicus
#' @return the path to the mean value of temperature for each sites
#' @export
#'

model <- function(arms_id, 
                  meta_and_data, 
                  data_crypt, 
                  temperature, 
                  wave_height, 
                  path_to_data_trans, 
                  flow_E, 
                  flow_N) {
  library(knitr)
  library(kableExtra)
  library(gt)
  library(htmlTable)
  library(dplyr)
  library(tidyverse)
  # meta_and_data <- targets::tar_read("metadata_data")
  # arms_id <- targets::tar_read("campain_id")
  # data_crypt <- targets::tar_read("data_cryptique")
  # temperature <- targets::tar_read("temp")
  # wave_height <- targets::tar_read("wave_h")
  # path_to_data_trans <- targets::tar_read("data_trans_paths")
  # flow_E <- targets::tar_read("flowveloc")
  # flow_N <- targets::tar_read("flowveloc_N")


  #### load response variables ####
  data_cryptique <-  read.table(file = data_crypt, header=TRUE, sep=";", dec=",")
  row.names(data_cryptique) <- data_cryptique$arms
  data_cryptique <- data_cryptique[,-1]
  data_cryptique[] <- lapply(data_cryptique, as.numeric)
  matrix.hel.crypt = vegan::decostand(data_cryptique, "hellinger")
  # Add LCBD to response variable
  spe.beta <- adespatial::beta.div(data_cryptique, method = "hellinger", nperm = 9999)
  alpha_div <- vegan::specnumber(data_cryptique)
  data_cryptique$LCBD <- spe.beta$LCBD
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
  diversité_c <- rep(tapply(data_trans_mean_DIV_HC$div_HC, site, mean), each = 3)
  
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
  
  for (col in 2:ncol(env)) {
    env[, col] <- as.numeric(env[, col])
  }
  
  
  
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
                     ordre = "original",
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
  
  #correl var expicative et var reponse
  # corr <- cor(data_cryptique, env[3:ncol(env)])
  # corr_pool <- as.matrix(corr) 
  # corr_pool[corr_pool > -0.7 & corr_pool < 0.7] <- NA   # Remplace les valeurs entre -0.7 et 0.7 par NA
  # data_corr_pool <- as.data.frame(corr_pool)
  # data_corr_pool[is.na(data_corr_pool)] <- 0
  # data_corr_pool <- data_corr_pool[, colSums(data_corr_pool) != 0]
  # data_corr_pool <- data_corr_pool[ rowSums(data_corr_pool) != 0,]
  # corrplot::corrplot(as.matrix(data_corr_pool), method = "circle")

  
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
      tab_vide[x, "R"] <- rest_fin[[x]]$sum$adj.r.squared
      } else {
        tab_vide[x, "shap"] <- "Not OK"}
    }
  }
  
  
  tab_vide_OK <- subset(tab_vide, shap == "OK")
  # tab_vide_OK <- data.frame(Species = rownames(tab_vide_OK), tab_vide_OK)

  path_to_tab <- paste0("outputs/tab_abiot.csv")
  
  
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

  # data_comb = data.frame(cbind(data_cryptique, env))
  # 
  # ggplot(data_comb, aes(x = alpha_div, y = Acropora)) +
  #   geom_point() +            # Add scatterplot points
  #   geom_smooth(se = FALSE, method = "lm")
  # 
  # # 
  
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
 
  
  path_to_tab <- paste0("outputs/tab_biot.csv")
  
  
  write.table(tab_vide_OK, 
              file = path_to_tab, 
              dec = ",", 
              sep = ";",
              row.names = T)
  
  
  # biot_table <- tab_vide_OK %>%
  #   kable("html") %>%
  #   kable_styling("striped", full_width = FALSE)
  
  # htmltools::save_html(biot_table, file = "biot_table_27_01.html")
  
  
  return(path_to_tab)
  
  
 
}

####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  ####  
# 
# 
# 
# 
# #### ^^^^^ #####
# #### WORK #####
# 
# #### Computing expl var order - abiot ####  
# 
# sort_variable <-  function(y, env_abiot) {
# 
#   mod_mat <- data.frame(y, env_abiot)
#   
#   mod <- lm(y ~ ., data = mod_mat)
#   
#   #if (shapiro.test(residuals(mod))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
# 
#   Rtot <- summary(mod)$r.squared
# 
#   deltas <- sapply(names(env_abiot), function(v) {
#     
#     #v = "wave_height"
#     
#     mod_mat_v <- mod_mat[, names(mod_mat) != v]
#     
#     mod_v <- lm(y ~ ., data = mod_mat_v)
#     
#     Rtot - summary(mod_v)$r.squared
#     
#   })
#   
#   deltas <- sort(deltas, decreasing = TRUE)
#   
#   names(deltas)
# 
# }
# 
# 
# var_list <- sort_variable(y = data_cryptique[, "CCA"], env_abiot = env_abiot)
# 
# #### Reduction du modèle step by step ####
# 
# reduce_mod <- function(y, env) {
#   
#   mod_mat_sort <- data.frame(y = y, env)
#   
#   #if(length(var_list) == 0) stop("No more variables")
#   
#   mod_full <- lm(y ~ ., data = mod_mat_sort)
# 
#   #if (shapiro.test(residuals(mod_full))$p.value < 0.05) stop("les résidus du modèle ne suivent pas une loi normale") # -> OK, je peux utiliser le summary
#     
#   coef_mod      <- summary(mod_full)$coefficients
#   
#   max_row_index <- which.max(coef_mod[,4])
#   
#   var_supp      <- row.names(coef_mod)[max_row_index]
#   
#   max_p_value   <- coef_mod[max_row_index, 4]
#   
#   if (max_p_value < 0.05) {
#     message("Toutes les variables sont significatives !")
#     return(list(sum = summary(mod_full), shap = shapiro.test(residuals(mod_full))))
#   } else {
#     
#     env <- env[, names(env) != var_supp]
#     
#     Recall(y = y, env = env)
#     
#   }
#   
# }
# 
# res <- reduce_mod(y = data_cryptique[, "CCA"], env = env_abiot[, var_list])
# 
# shapiro.test(residuals(mod_final)) # 
# 
# summary(mod)
# 
# 
# 

#### trying to obtain normality with response variables ####

# data_cryptique$Polysyncraton_rostrum_ascc
# library(bestNormalize)
# MASS::truehist(data_cryptique$Polysyncraton_rostrum_ascc, nbins = 12)
# arcsinh_obj <- bestNormalize::arcsinh_x(data_cryptique$Polysyncraton_rostrum_ascc)
# boxcox_obj <- bestNormalize::boxcox(data_cryptique$Polysyncraton_rostrum_ascc)
# yeojohnson_obj <- yeojohnson(data_cryptique$Polysyncraton_rostrum_ascc)
# orderNorm_obj <- orderNorm(data_cryptique$Polysyncraton_rostrum_ascc)
# 
# BNobject <- bestNormalize(data_cryptique$Polysyncraton_rostrum_ascc)
# binarize_obj <- binarize(data_cryptique$Polysyncraton_rostrum_ascc)
# 
# par(mfrow = c(2,2))
# MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = 12)
# MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
# MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = 12)
# MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)



#### Model
# Selection des variable via VIF #
# env.cont <- env[,c(2,3,6,7,8,13,14)]
# env <- na.omit(env)
# 
# usdm::vif(env[,-c(1)])
# 
# ####
# library(vegan)
# data_hel <- vegan::decostand(data_cryptique, "hellinger")
# 
# rda <- vegan::rda(data_hel ~ ., env.cont)
# adespatial::forward.sel(data_hel, env.cont)
# 
# mod0 <- rda(data_hel ~ 1, data = env.cont)
# step.forward <- ordistep(mod0,
#                          scope = formula(rda),
#                          direction = "forward",
#                          permutations = how(nperm = 499))
# RsquareAdj(step.forward)
# 
# mod.fin <- rda(data_hel ~ data_wave.mean +  Branching_Corals + data_temp.mean, data = env.cont)
# plot(mod.fin)
# 
# 
# data_cryptique <- data_cryptique /100
# data_cryptique <- mvabund::mvabund(data_cryptique$Polysyncraton_rostrum_ascc)
# env.glm <- mvabund::mvabund(env[,-c(7,8,1)])
# mod1 <- mvabund::manyglm(data_cryptique ~., data = as.data.frame(env.glm), family = "gamma")
# any(is.na(env.glm))
# ?manyglm
# data_cryptique
# plot(mod1)
# #data_cryptique <- vegan::decostand(data_cryptique, "hellinger")
# mod <- vegan::capscale(data_cryptique ~ , data = env, na.action = na.exclude)
# plot(mod)
# 
# 
# return()


# Generalised linear models are designed for non-normal data for which a distribution can be specified
# that offers a reasonable model for data, as specified using the argument family. The manyglm
# function currently handles count and binary data, and accepts either a character argument or a family
# argument for common choices of family. For binary (presence/absence) data, family=binomial()
# can be used for logistic regression (logit link, "logistic regression"), or the complementary log-log
# link can be used family=binomial("cloglog"), arguably a better choice for presence-absence
# data. Poisson regression family=poisson() can be used for counts that are not "overdispersed"
# (that is, if the variance is not larger than the mean), although for multivariate abundance data it has
# been shown that the negative binomial distribution (family="negative.binomial") is usually a
# better choice (Warton 2005). In both cases, a log-link is used. If another link function or family is
# desired, this can be specified using the manyany function, which accepts regular family arguments.