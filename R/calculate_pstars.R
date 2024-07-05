## Calculate p_stars
### Use incremental steps to calculate harvest difference under different pstars until model harvest
### falls within 2% or 500 fish of MRIP harvest.
###
### Not all states have values for all spp so that is when there is ifesle combos of spp == "NaN"

library(magrittr)

calculate_pstars<- function(select_mode, select_month){

  # start_est <-  read.csv(here::here("data-raw/total AB1B2 by state mode_pstar.csv")) %>% #Starting point values for pstars
  #   dplyr::group_by(mode) %>%
  #   dplyr::mutate(cod_start = 1 - cod_harvest/cod_tot_cat,
  #                 had_start = 1 - had_harvest/had_tot_cat) %>%
  #   dplyr::select(mode, cod_start, had_start)
  # start_est<- start_est %>% dplyr::filter(mode == select_mode)

  # p_star_cod <- start_est[[3]]
  # p_star_had <- start_est[[4]]
  p_star_cod <- .9
  p_star_had <- .5
  #print(p_star_had)

  pstar_out <- data.frame()

  for(k in 1:150){
    repeat{

      pstar <- calibrate_pstars( p_star_cod, p_star_had,
                                   select_mode = select_mode,
                                 select_month = select_month,
                                   k = k,
                                 directed_trips_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv",
                                 catch_draws_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws",
                                 MRIP_comparison = "C:/Users/kimberly.bastille/Desktop/codhad_data/simulated_catch_totals_month.csv")

      cod <- pstar %>% dplyr::filter(species == "COD")
      cod_harvest_harv_diff <- cod[[10]]
      cod_model_mrip_diff <- abs(cod$tot_keep_model - cod$harvest_MRIP)

      had <- pstar %>%dplyr::filter(species == "HAD")
      p_star_had <- had[[2]]
      had_harvest_harv_diff <- had[[10]]
      had_model_mrip_diff <- abs(had$tot_keep_model - had$harvest_MRIP)

      if(cod_harvest_harv_diff %in% c("NaN", "NA", NA) & had_harvest_harv_diff %in% c("NaN", "NA", NA)){
        break

      }else{
      if(cod_harvest_harv_diff %in% c("NaN", "NA", NA)){
        if(had_harvest_harv_diff<0 & abs(had_harvest_harv_diff)>1){
          p_star_had<-p_star_had +.005 # some values smaller step because the code kept finding valleys at high incremental change.
        }

        if(had_harvest_harv_diff>0 & abs(had_harvest_harv_diff)>1){
          p_star_had<-p_star_had -.005
        }

        if(p_star_had < 0) break
        if(((abs(had_harvest_harv_diff)<2)|(had_model_mrip_diff < 100))) break
      }else{

        if(had_harvest_harv_diff %in% c("NaN", "NA", NA)){
          if(cod_harvest_harv_diff<0 & abs(cod_harvest_harv_diff)>1){
            p_star_cod<-p_star_cod +.005
          }

          if(cod_harvest_harv_diff>0 & abs(cod_harvest_harv_diff)>1){
            p_star_cod<-p_star_cod -.005
          }
          if(p_star_cod < 0) break
          if(((abs(cod_harvest_harv_diff)<2)|(cod_model_mrip_diff < 100))) break
        }else{



            if(cod_harvest_harv_diff<0 & abs(cod_harvest_harv_diff)>1){
              p_star_cod<-p_star_cod +.005
            }

            if(cod_harvest_harv_diff>0 & abs(cod_harvest_harv_diff)>1){
              p_star_cod<-p_star_cod -.005
            }

            if(had_harvest_harv_diff<0 & abs(had_harvest_harv_diff)>1){
              p_star_had<-p_star_had +.005
            }

            if(had_harvest_harv_diff>0 & abs(had_harvest_harv_diff)>1){
              p_star_had<-p_star_had -.005
            }


            if(((abs(cod_harvest_harv_diff)<2)|(cod_model_mrip_diff < 100)) &
               ((abs(had_harvest_harv_diff)<2)|(had_model_mrip_diff < 100))) break
            print("out of breaks")
            if(p_star_cod < 0) break
            print("cod break")
            if(p_star_had < 0) break
            print("had break")

          }}}
      print(pstar)
  }
    print(pstar) #printed outside megaforloop
    pstar_out <- pstar_out %>%
      rbind(pstar)
  }
  return(pstar_out)

}












