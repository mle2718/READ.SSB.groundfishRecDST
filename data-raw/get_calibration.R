library(magrittr)

for(k in 1:150){
  for(i in 0:1){
  calibration_fh<- calibrate_rec_catch(select_mode = "fh",
                                       k = k, select_season = i,
                                       directed_trips_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv",
                                       catch_draws_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws",
                                       MRIP_comparison = "C:/Users/kimberly.bastille/Desktop/codhad_data/simulated_catch_totals_open_season.csv",
                                       pstar_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars")

  pds_new_all_fh<- data.table::data.table(calibration_fh[1][[1]])
  costs_new_all_fh<-  data.table::data.table(calibration_fh[2][[1]])

  calibration_pr<- calibrate_rec_catch(select_mode = "pr",
                                       k = k, select_season = i,
                                       directed_trips_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv",
                                       catch_draws_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws",
                                       MRIP_comparison = "C:/Users/kimberly.bastille/Desktop/codhad_data/simulated_catch_totals_open_season.csv",
                                       pstar_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars")
  pds_new_all_pr<- data.table::data.table(calibration_pr[1][[1]])
  costs_new_all_pr<-  data.table::data.table(calibration_pr[2][[1]])

  calibration_sh<- calibrate_rec_catch(select_mode = "sh",
                                       k = k, select_season = i,
                                       directed_trips_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv",
                                       catch_draws_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws",
                                       MRIP_comparison = "C:/Users/kimberly.bastille/Desktop/codhad_data/simulated_catch_totals_open_season.csv",
                                       pstar_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars")
  pds_new_all_sh<- data.table::data.table(calibration_sh[1][[1]])
  costs_new_all_sh<-  data.table::data.table(calibration_sh[2][[1]])

  pds_new_all <- rbind(pds_new_all_fh, pds_new_all_pr, pds_new_all_sh, fill = TRUE)
  costs_new_all <- rbind(costs_new_all_fh, costs_new_all_pr, costs_new_all_sh, fill = TRUE)

  saveRDS(pds_new_all, file = paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/pds_new_all_",i,"_",k,".rds"))
  saveRDS(costs_new_all, file = paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/cost_new_all_",i,"_",k,".rds"))
  }
  }
