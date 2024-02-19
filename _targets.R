library(targets)

targets::tar_source()

list(
   tar_target(raw_data, "data/raw-data/Data_sans_UNAV-NR-OROS_rename(2).csv", format = "file")
  
  ,tar_target(data_flowvelo, "data/raw-data/NC/cmems_mod_glo_phy-cur_anfc_0.083deg_P1D-m_1679926640974.nc", format = "file")
  
  ,tar_target(data_salinity, "data/raw-data/NC/cmems_mod_glo_phy_anfc_0.083deg_P1D-m_1681219768937.nc", format = "file")
   
  ,tar_target(data_wave_height, "data/raw-data/NC/cmems_mod_glo_wav_anfc_0.083deg_PT3H-i_1679910272535.nc", format = "file")
  
  ,tar_target(data_temperature, "data/raw-data/NC/cmems_mod_glo_phy-thetao_anfc_0.083deg_P1D-m_1681462171103.nc", format = "file")
  
  ,tar_target(in_situ_temperature, "data/derived-data/temperature_in_situ.csv")
  
  ,tar_target(data_transect, "data/raw-data/New_Data_R8/Data_RUNTRANS_Full_23_10_23.csv", format = "file")
  
  ,tar_target(labels, "data/raw-data/New_Data_R8/Label_classif_23_10_23.csv", format = "file")
  
  ,tar_target(data_trans_paths, fun_data_runtrans(data_trans = data_transect, 
                                                  path_labels = labels))
  
  # ,tar_target(data_transect_pool, "data/derived-data/data_trans_pool.csv", format = "file")
  
  ,tar_target(data_cryptique, "data/raw-data/data_for_tanglegram.csv", format = "file")
  
  ,tar_target(data_cryptique_msp_pool, "data/derived-data/data_msp_pool.csv", format = "file")
  
  ,tar_target(data_msp_pool, "data/raw-data/data_msp_pool.csv", format = "file")
  
  ,tar_target(campain_id, "RUNA")
  
  ,tar_target(metadata_data, data_arms(raw_data = raw_data, 
                                       arms_id = campain_id), format = "file")
  
  ,tar_target(wave_h, wave_height(arms_id = campain_id,
                                  meta_and_data = metadata_data,
                                  data_w_h = data_wave_height))
  
  ,tar_target(sal, salinity(arms_id = campain_id,
                            meta_and_data = metadata_data,
                            data_salin = data_salinity))
  
  ,tar_target(temp, temperature(arms_id = campain_id,
                                meta_and_data = metadata_data,
                                data_temp = data_temperature,
                                data_crypt = data_cryptique))
  
  ,tar_target(flowveloc, flow(arms_id = campain_id,
                                meta_and_data = metadata_data,
                                data_flow = data_flowvelo))
  
  ,tar_target(flowveloc_N, flow_N(arms_id = campain_id,
                              meta_and_data = metadata_data,
                              data_flow = data_flowvelo))
  
  ,tar_target(magnitude, magnitude_flow(flow_E = flowveloc,
                                        flow_N = flowveloc_N))
  
  ,tar_target(boxplot, boxplot_environment(arms_id = campain_id,
                                           stat_salinity = sal,
                                           stat_temperature = temp, 
                                           stat_w_h = wave_h,
                                           in_situ_t = in_situ_temperature))
  
  # ,tar_target(comparaison_cluster, comp_clust(data_trans = data_transect,
  #                                             data_crypt = data_cryptique))
 
  # ,tar_target(comparaison_cluster_env, comp_clust_env(temperature = temp,
  #                                                     salinity = sal,
  #                                                     wave_height = wave_h,
  #                                                     data_crypt = data_cryptique))
  # 
  # ,tar_target(corellplot, correl(data_trans = data_transect,
  #                                meta_and_data = metadata_data, 
  #                                arms_id = campain_id,
  #                                dat_msp_pool = data_msp_pool,
  #                                dat_transect_pool = data_transect_pool))
  
  ,tar_target(boxplot_transect, box_trans(path_to_data_trans = data_trans_paths))
  
  ,tar_target(modelisation, model(meta_and_data = metadata_data,
                                  arms_id = campain_id,
                                  data_crypt = data_cryptique,
                                  temperature = temp,
                                  wave_height = wave_h,
                                  path_to_data_trans = data_trans_paths,
                                  flow_E = flowveloc,
                                  flow_N = flowveloc_N))
  
  # ,tar_target(modelisation_pool, model_msp_pool(meta_and_data = metadata_data,
  #                                               arms_id = campain_id,
  #                                               data_crypt_pool = data_cryptique_msp_pool,
  #                                               temperature = temp,
  #                                               wave_height = wave_h,
  #                                               path_to_data_trans = data_trans_paths,
  #                                               flow_E = flowveloc,
  #                                               flow_N = flowveloc_N))
)
