## calibrate_pstars
library(magrittr)

select_mode = "sh"
select_season = 0
k = 1
directed_trips_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/directed_trips/directed_trips_calib_150draws.csv"
catch_draws_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/catch_draws/catch_draws"
MRIP_comparison = "C:/Users/kimberly.bastille/Desktop/codhad_data/simulated_catch_totals_open_season.csv"
pstar_file_path = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars"

pds_test <-   readRDS(file.path(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/pds_new_all_1.rds")))
costs_test <-   readRDS(file.path(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/calibration/cost_new_all_1.rds")))

calibrate_rec_catch <- function(p_star_cod, p_star_had, select_mode, k, select_season,
                             directed_trips_file_path, catch_draws_file_path, MRIP_comparison, pstar_file_path){

  print(select_mode)
  print(select_season)
  MRIP_data <-   read.csv(file.path(paste0(MRIP_comparison)))%>%
    dplyr::filter(mode == select_mode,
                  open == select_season,
                  draw == k )

  if (MRIP_data$dtrip == 0) {
    pds_new_all<- data.frame(period2 = NA, tot_keep_cod = 0, tot_keep_had = 0,
                             tot_rel_cod = 0, tot_rel_had = 0, tot_cod_catch = 0,
                             tot_had_catch = 0, estimated_trips = 0, n_choice_occasions = 0,
                             n_cal_draw = NA, mode = NA)

    costs_new_all<- data.frame(tripid = NA, cost = NA, catch_draw = NA, tot_keep_cod_base = NA,
                               tot_rel_cod_base = NA, age = NA,days_fished = NA,beta_opt_out_age = NA,
                               beta_opt_out_likely = NA,beta_opt_out_prefer = NA,tot_keep_had_base = NA,tot_rel_had_base = NA,
                               beta_cost = NA,beta_opt_out = NA,beta_sqrt_hadd_keep = NA,beta_sqrt_hadd_release = NA,
                               beta_sqrt_cod_hadd_keep = NA,beta_sqrt_cod_keep = NA,beta_sqrt_cod_release = NA,period2 = NA,
                               n_cal_draw = NA,mode = select_mode, open = select_season)
    output<-list(pds_new_all, costs_new_all)
  } else {
    print(k)

    n_drawz = 50
    n_catch_draws = 30
    set.seed(k)
    directed_trips<-read.csv(directed_trips_file_path) %>%
      tibble::tibble() %>%
      dplyr::filter(draw == k,
                    mode == select_mode) %>%
      dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0))

    # New_DTRIP <- read.csv(directed_trips_file_path)  %>%
    #   dplyr::mutate(open = dplyr::case_when(cod_bag > 0 ~ 1, TRUE ~ 0)) %>%
    #   dplyr::reframe(dtrip= sum(dtrip), .by = c(mode, open, draw))


    open<- directed_trips %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::select(period2, open) %>%
      dplyr::filter(open == select_season)


    directed_trips<- directed_trips %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, '\\d{2}')),
                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::filter(open == select_season)
    ######################################
    ##   Begin simulating trip outcomes ##
    ######################################

    # Set up an output file for the separately simulated within-season regulatory periods
    directed_trips_p <- directed_trips %>%
      dplyr::mutate(month = as.numeric(month)) %>%
      #dplyr::mutate(period2 = as.character(paste0(month, "_", day, "_", mode))) %>% #make day of year and mode combo
      #group_by(period) %>%
      dplyr::mutate(#n_trips = floor(mean(dtrip_2019)),
        n_trips = floor(dtrip),
        n_draws = n_drawz)%>%
      dplyr::select(!c(month, mode))

    regs <- directed_trips_p %>%
      dplyr::select(period2,
                    cod_bag,
                    cod_min,
                    hadd_bag,
                    hadd_min)

    param_draws <- directed_trips_p %>%
      dplyr::select(period2, n_draws, open) %>%
      tidyr::uncount(n_draws) # %>% mutate(sample_id=1:nrow(period_vec))

    cod_catch_data <- read.csv(file.path(paste0(catch_draws_file_path, k, ".csv"))) %>%
      dplyr::mutate(month = stringr::str_sub(day, 3, 5),
                    month = dplyr::recode(month, jan = 1, feb = 2, mar = 3, apr = 4,
                                          may = 5, jun = 6, jul = 7, aug = 8,
                                          sep = 9, oct = 10, nov = 11, dec = 12),
                    day = as.numeric(stringr::str_extract(day, '\\d{2}')) ,

                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::left_join(open, by = "period2") %>%
      dplyr::filter(open == select_season) %>%
      dplyr::select(!open, !day) %>%
      dplyr::rename(tot_cod_catch = cod_catch,
                    tot_had_catch = hadd_catch,
                    keep_cod =  cod_keep,
                    keep_had =  hadd_keep)  %>%
      dplyr::select(mode,month,tot_cod_catch,keep_cod,cod_rel,tot_had_catch,keep_had,hadd_rel,
                    tripid,catch_draw,day, draw, age, days_fished, cost)

    trip_costs<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(cost)

    age<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(age)

    avidity<-cod_catch_data  %>%
      dplyr::filter(mode == select_mode) %>%
      dplyr::select(days_fished)

    cod_catch_data <- cod_catch_data %>%
      dplyr::mutate(day = as.numeric(stringr::str_extract(day, "\\d+")),
                    period2 = paste0(month, "_", day, "_", mode)) %>%
      dplyr::group_by(period2) %>%
      dplyr::slice_sample(n = n_drawz*n_catch_draws, replace = TRUE)   %>%
      dplyr::mutate(#period = rep(period_vec$period2, each = nsamp),
        catch_draw = rep(1:n_catch_draws, length.out = n_drawz*n_catch_draws),
        tripid = rep(1:n_drawz, each=n_catch_draws)) %>%
      dplyr::ungroup()%>%
      dplyr::select(!c(age, days_fished, cost))%>%
      dplyr::select(!c(month, mode))
    print("postmutate")


    if(select_season == 1){
    seas = "open"
    }
    if(select_season == 0){
      seas = "closed"
    }
    pstar<- read.csv(file.path(here::here(paste0(pstar_file_path, "/pstar_", select_mode, "_",seas, ".csv")))) %>%
      dplyr::filter(run_number == k)
    p_star_cod <- pstar %>%
      dplyr::filter(species == "COD")
    p_star_cod <- p_star_cod$p_star_value

    p_star_had <- pstar %>%
      dplyr::filter(species == "HAD")
    p_star_had <- p_star_had$p_star_value




    #Need 1,000xn_catch_draws(per trip) random draws of catch for each period, with catch-per-trip rates that vary by month.

    # check <- directed_trips_p %>%
    #     dplyr::select(period2,month, mode1) %>%
    #     distinct(period2,month, mode1) %>%
    #     mutate(period2=as.factor(period2)) %>%
    #     mutate(month=as.numeric(month))%>%
    #     #mutate(month=sprintf("%02d", month)) %>%
    #     rename(month1=month)
    #
    #
    # sf_catch_data_pd<-list()
    # sf_catch_data_check_draw<-list()
    #
    #
    # levels(check$period2)
    #   for(p in levels(check$period2)){
    #     #p<-"10_bt"
    #       check1<-subset(check, period2==p)
    #       month_val<-unique(check1$month1)
    #       md_val<-unique(check1$mode1)
    #
    #       for(i in 1:n_catch_draws){
    #        # i=1
    #         sf_catch_data_check<- sf_catch_data %>% dplyr::filter(month == month_val & mode == md_val) # %>%
    #         #sf_catch_data_check <- sf_catch_data_check %>%  slice_sample(n = 1000)
    #         #sf_catch_data_check<-sf_catch_data_check[sample(nrow(sf_catch_data_check), size=n_drawz), ]
    #         sf_catch_data_check<- sample_n(sf_catch_data_check,n_drawz)
    #
    #         #[sample(nrow(sf_catch_data_check), size=n_draws), ]
    #
    #
    #         sf_catch_data_check <- sf_catch_data_check  %>%
    #         mutate(catch_draw=i) %>%
    #         mutate(tripid=1:n_drawz)
    #
    #         sf_catch_data_check_draw[[i]]<-sf_catch_data_check
    #       }
    #
    #       sf_catch_data_check_draw_all<- list.stack(sf_catch_data_check_draw)
    #       sf_catch_data_check_draw_all<- sf_catch_data_check_draw_all %>% mutate(period2=p)
    #       sf_catch_data_pd[[p]]=sf_catch_data_check_draw_all
    #
    #       }
    #
    #       sf_catch_data<- list.stack(sf_catch_data_pd)
    #       rm(sf_catch_data_pd)
    #       rm(sf_catch_data_check_draw_all, sf_catch_data_check_draw)

    #sf_catch_data<- sf_catch_data %>%   #dplyr::arrange(period2, tripid, catch_draw) %>% plyr::select(-wp_int)


    # sf_catch_data$tab<-1
    # agg_tbl <- sf_catch_data %>% group_by(period2) %>%
    #   summarise(sum_tab=sum(tab), .groups = 'drop')

    cod_had_catch_data <- cod_catch_data


    # subset trips with zero catch, as no size draws are required
    cod_zero_catch <- dplyr::filter(cod_catch_data, tot_cod_catch == 0)


    #Check to see if there is no catch for either species and if so, pipe code around keep/release determination
    cod_catch_check<-base::sum(cod_catch_data$tot_cod_catch)
    had_catch_check<-base::sum(cod_catch_data$tot_had_catch)

    if(cod_catch_check !=0){
      #remove trips with zero summer flounder catch
      cod_catch_data <- dplyr::filter(cod_catch_data, tot_cod_catch > 0)


      #expand the sf_catch_data so that each row represents a fish
      row_inds <- seq_len(nrow(cod_catch_data))
      #sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
      cod_catch_data<- cod_catch_data %>%
        dplyr::slice(rep(row_inds,tot_cod_catch))   %>%
        dplyr::mutate(fishid=dplyr::row_number())



      # sf_size_data<- sf_size_data %>%
      #   dplyr::filter(state == state1)
      # # generate lengths for each fish
      # catch_size_data <- sf_catch_data %>%
      #   dplyr::mutate(fitted_length = sample(sf_size_data$length,
      #                                        nrow(.),
      #                                        prob = sf_size_data$fitted_prob,
      #                                        replace = TRUE)) #%>%    dplyr::arrange(period2, tripid, catch_draw)
      #
      ##I()

      # Impose regulations, calculate keep and release per trip
      # For summer flounder, retain keep- and release-at-length
      ####### Start Here #################

      # ################### P_Star #############################
      catch_data2 <-  cod_catch_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(cod_catch_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_cod, 1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw)   %>%
        dplyr::mutate(cod_bag_total = cod_bag) %>%
        # keep = case_when(
        # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
        # TRUE ~ 0),
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj =dplyr:: case_when(
            cod_bag_total > 0 ~ ifelse(csum_keep<=cod_bag_total & posskeep==1,1,0),
            TRUE ~ 0)) #%>%
      #dplyr::mutate(posskeep2 = ifelse(uniform>=p_star_sf, 1,0)) %>%
      #dplyr::group_by(tripid, period2, catch_draw) #%>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),


      #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
      #catch_size_data[is.na(catch_size_data)] <- 0
      catch_data <- catch_data2 %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

      catch_data <- catch_data %>%
        dplyr::mutate(keep_tot = keep_adj,
                      release = ifelse(keep_adj==0, 1,0))

      catch_data<- catch_data %>%
        dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw)) %>%
        dplyr::rename(keep = keep_tot)

      summed_catch_data <- catch_data %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_cod = sum(keep),
                         tot_rel_cod = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      ############# Length #####################################
      # catch_size_data <- catch_size_data %>%
      #   dplyr::left_join(regs, by = "period2") %>%
      #   dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<fluke_max1,1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw)   %>%
      #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>% #,
      #   # keep = dplyr::case_when(
      #   #   fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   #   TRUE ~ 0)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj = dplyr::case_when(
      #       fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
      #       TRUE ~ 0))  %>%
      #
      #   dplyr::mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2,1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw) %>%
      #   # keep = case_when(
      #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   # TRUE ~ 0),
      #   dplyr::mutate(csum_keep2 = cumsum(posskeep2)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj2 = dplyr::case_when(
      #       fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0),
      #       TRUE ~ 0))
      #
      # #catch_size_data[is.na(catch_size_data)] <- 0
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      #
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate(keep_tot = keep_adj+keep_adj2,
      #                 release = ifelse(keep_adj==0 & keep_adj2==0,1,0))
      #
      # ###### ANDREWS CODE #@######
      # catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw)) %>%
      #   dplyr::rename(keep = keep_tot)
      #
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop") %>%
      #   dplyr::ungroup()
      #
      # summed_catch_data <- catch_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid) %>%
      #   dplyr::summarize(tot_keep_sf = sum(keep),
      #                    tot_rel_sf = sum(release),
      #                    .groups = "drop") %>%
      #   dplyr::ungroup()
      #
      # keep_size_data <- new_size_data %>%
      #   dplyr::select(-release) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "keep_sf_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = keep,
      #                      values_fill = 0)
      #
      # release_size_data <- new_size_data %>%
      #   dplyr::select(-keep) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "release_sf_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = release,
      #                      values_fill = 0)
      #
      # keep_release_sf <- keep_size_data %>%
      #   dplyr::full_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
      #
      # sf_zero_catch<- sf_zero_catch %>% ## ADD back zero catches
      #   dplyr::select(period2, tripid, catch_draw)
      #
      # keep_release_sf <- keep_release_sf %>%
      #   dplyr::full_join(sf_zero_catch, by = c("period2", "catch_draw", "tripid")) %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      #

      trip_data <- summed_catch_data

      # trip_test<- trip_data %>%
      #   dplyr::filter(period2 == "6_6_fh",
      #                 tripid == 1)
      #
      # sf_zero_test <- sf_zero_catch %>%
      #   dplyr::filter(period2 == "6_6_fh",
      #                 tripid == 1)
      trip_data <- dplyr::bind_rows(trip_data, cod_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(tot_cod_catch = tot_keep_cod + tot_rel_cod) %>%
        dplyr::select(c("period2", "catch_draw","tripid",
                        "tot_keep_cod","tot_rel_cod", "tot_cod_catch"))

    }
    if (cod_catch_check==0){
      trip_data<-cod_catch_data %>%
        dplyr::mutate(tot_keep_cod=0,
                      tot_rel_cod=0,
                      tot_cod_catch = tot_keep_cod+tot_rel_cod)
      #subset(dplyr::select(-c(tot_bsb_catch, tot_scup_catch)))

    }


    #########################
    ###  Haddock  ####
    #########################


    if (had_catch_check!=0){
      # subset trips with zero catch, as no size draws are required
      had_zero_catch <- dplyr::filter(cod_had_catch_data, tot_had_catch == 0)

      #remove trips with zero summer flounder catch
      #sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
      had_catch_data <- dplyr::filter(cod_had_catch_data, tot_had_catch > 0)

      #expand the sf_catch_data so that each row represents a fish
      row_inds <- seq_len(nrow(had_catch_data))
      #bsb_catch_data <- bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]

      had_catch_data<- had_catch_data %>%
        dplyr::slice(rep(row_inds,tot_had_catch))

      rownames(had_catch_data) <- NULL
      had_catch_data$fishid <- 1:nrow(had_catch_data)


      # # ###### P_star ####################
      # catch_size_data2 <- bsb_catch_data %>%
      #   dplyr::left_join(regs, by = "period2") %>%
      #   dplyr::mutate(uniform=runif(nrow(bsb_catch_data))) %>%
      #   dplyr::mutate(posskeep = ifelse(uniform>=p_star_bsb, 1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw)   %>%
      #   # keep = case_when(
      #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   # TRUE ~ 0),
      #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj =dplyr:: case_when(
      #       bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
      #       TRUE ~ 0))
      # #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
      # #catch_size_data[is.na(catch_size_data)] <- 0
      # catch_size_data2 <- catch_size_data2 %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      #
      # catch_size_data <- catch_size_data2 %>%
      #   dplyr::mutate(keep_tot = keep_adj,
      #                 release = ifelse(keep_adj==0,1,0))
      #
      # catch_size_data<- catch_size_data %>%
      #   dplyr::select(c(fishid, tripid, keep_adj, release, period2, catch_draw, mode1, month)) %>%
      #   dplyr::rename(keep = keep_adj)

      #
      # # generate lengths for each fish
      had_data<- had_catch_data

      # ################### P_Star #############################
      catch_data2 <- had_data %>%
        dplyr::left_join(regs, by = "period2") %>%
        dplyr::mutate(uniform=runif(nrow(had_catch_data))) %>%
        dplyr::mutate(posskeep = ifelse(uniform>=p_star_had, 1,0)) %>%
        dplyr::group_by(tripid, period2, catch_draw)   %>%
        #dplyr::mutate(sf_bag_total = bsb_bag1+fluke_bag2) %>%
        # keep = case_when(
        # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
        # TRUE ~ 0),
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj =dplyr:: case_when(
            hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
            TRUE ~ 0)) #%>%
      #dplyr::mutate(posskeep2 = ifelse(uniform>=p_star_sf, 1,0)) %>%
      #dplyr::group_by(tripid, period2, catch_draw) #%>%
      # keep = case_when(
      # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      # TRUE ~ 0),


      #sf_catch_data <- sf_catch_data %>% dplyr::arrange(period2, tripid, catch_draw)
      #catch_size_data[is.na(catch_size_data)] <- 0
      catch_data <- catch_data2 %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

      catch_data <- catch_data %>%
        dplyr::mutate(keep_tot = keep_adj,
                      release = ifelse(keep_adj==0, 1,0))

      catch_data<- catch_data %>%
        dplyr::select(c(fishid, tripid, keep_tot, release, period2, catch_draw)) %>%
        dplyr::rename(keep = keep_tot)

      summed_catch_data <- catch_data %>%
        dplyr::group_by(period2, catch_draw, tripid) %>%
        dplyr::summarize(tot_keep_had = sum(keep),
                         tot_rel_had = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()


      # catch_size_data <- bsb_catch_data %>%
      #   dplyr::mutate(fitted_length = sample(bsb_size_data$length,
      #                                        nrow(.),
      #                                        prob = bsb_size_data$fitted_prob,
      #                                        replace = TRUE)) #%>%
      #
      #
      # catch_size_data <- catch_size_data %>%
      #   dplyr::left_join(regs, by = "period2") %>%
      #   dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>%
      #   dplyr::group_by(tripid, period2, catch_draw) %>%
      #   # keep = case_when(
      #   # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
      #   # TRUE ~ 0),
      #   dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::mutate(
      #     keep_adj = dplyr::case_when(
      #       bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
      #       TRUE ~ 0))
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      #
      # catch_size_data <- catch_size_data %>%
      #   dplyr::mutate(keep_tot = keep_adj,
      #                 release = ifelse(keep_adj==0,1,0))
      #
      # catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period2, catch_draw)) %>%
      #   dplyr::rename(keep = keep_tot)
      #
      # new_size_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid, fitted_length) %>%
      #   dplyr::summarize(keep = sum(keep),
      #                    release = sum(release), .groups = "drop") %>%
      #   dplyr::ungroup()
      #
      # summed_catch_data <- catch_size_data %>%
      #   dplyr::group_by(period2, catch_draw, tripid) %>%
      #   dplyr::summarize(tot_keep_bsb = sum(keep),
      #                    tot_rel_bsb = sum(release),
      #                    .groups = "drop") %>%
      #   dplyr::ungroup()
      #
      # keep_size_data <- new_size_data %>%
      #   dplyr::select(-release) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "keep_bsb_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = keep,
      #                      values_fill = 0)
      #
      # release_size_data <- new_size_data %>%
      #   dplyr::select(-keep) %>%
      #   tidyr::pivot_wider(names_from = fitted_length, #_length,
      #                      names_glue = "release_bsb_{fitted_length}",
      #                      names_sort = TRUE,
      #                      values_from = release,
      #                      values_fill = 0)
      #
      # keep_release_bsb <- keep_size_data %>%
      #   dplyr::full_join(release_size_data, by = c("period2",  "tripid", "catch_draw"))
      #

      # bsb_zero_catch2<- bsb_zero_catch %>% ## ADD back zero catches
      #   dplyr::select(period2, tripid, catch_draw)
      #
      # keep_release_bsb <- keep_release_bsb %>%
      #   dplyr::full_join(bsb_zero_catch2, by = c("period2", "catch_draw", "tripid")) %>%
      #   dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      #

      trip_data_had <- summed_catch_data


      trip_data_had <- dplyr::bind_rows(trip_data_had, had_zero_catch) %>%
        #arrange(period, catch_draw, tripid) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::mutate(tot_had_catch = tot_keep_had + tot_rel_had) %>%
        dplyr::select(c("period2", "catch_draw","tripid",
                        "tot_keep_had","tot_rel_had", "tot_had_catch"))


      # print(setdiff(trip_data_bsb$period2, keep_release_bsb$period2))
      # print(setdiff(keep_release_bsb$period2, trip_data_bsb$period2))
      #

      # merge the bsb trip data with the rest of the trip data
      #trip_data <-  merge(trip_data,trip_data_bsb,by=c("period2", "catch_draw", "tripid", "state", "mode", "month" ))

      trip_data <- trip_data %>%
        dplyr::left_join(trip_data_had, by = c("period2", "catch_draw", "tripid"))#%>%  select(-decade.x, -decade.y)

      # %>%

    }

    if (had_catch_check==0){
      trip_data$tot_had_catch<-0
      trip_data$tot_keep_had<-0
      trip_data$tot_rel_had<-0
    }



    period_vec1 <- param_draws %>%
      dplyr::mutate(beta_sqrt_cod_keep = rnorm(nrow(param_draws), mean = 1.594, sd = .615),
                    beta_sqrt_cod_release = rnorm(nrow(param_draws), mean = 0.162 , sd = 0.445),
                    beta_sqrt_hadd_keep = rnorm(nrow(param_draws), mean = 1.156, sd = 0.603 ),
                    beta_sqrt_hadd_release = rnorm(nrow(param_draws), mean = 0.094 , sd = 0 ),
                    beta_sqrt_cod_hadd_keep = rnorm(nrow(param_draws), mean =-0.314  , sd = 0.778 ),
                    beta_cost = rnorm(nrow(param_draws), mean =-0.015 , sd =0 ),
                    beta_opt_out = rnorm(nrow(param_draws), mean =-1.871 , sd = 3.208 ),
                    beta_opt_out_age = rnorm(nrow(param_draws), mean =0.047 , sd = 0 ),
                    beta_opt_out_likely = rnorm(nrow(param_draws), mean =-1.272 , sd = 0 ),
                    beta_opt_out_prefer = rnorm(nrow(param_draws), mean =-1.079 , sd = 0 ))%>%
      dplyr::group_by(period2) %>% dplyr::mutate(tripid = dplyr::row_number(period2))


    trip_data3<- trip_data %>%
      dplyr::left_join(period_vec1, by = c("period2","tripid"))


    trip_data2 <- trip_data3 %>%
      cbind(trip_costs)

    # trip_data <- trip_data %>%
    #   dplyr::mutate(cost=rnorm(nrow(trip_data), mean=trip_data$mean, sd=trip_data$st_error))

    # pmax takes max of each row instead of max which takes the max of column
    trip_data <- trip_data2 %>%
      dplyr::mutate(cost=pmax(0,cost))


    #import age and avidity distribution and assign each trip an age and avidity

    period_vec2<- period_vec1 %>%
      dplyr::mutate(period2=as.factor(period2))

    demographics0<-list()

    levels(period_vec2$period2)
    for(p in levels(period_vec2$period2)){
      #p<-"10_bt"

      #Ages
      age_distn <- age

      #next two commands ensure there are enough observations  per period
      expand_rows=ceiling((n_drawz/nrow(age_distn)))+2

      age_distn <- age_distn %>%
        dplyr::slice(rep(1:dplyr::n(), each = expand_rows))

      #now need to assign tripid in order to merge to trip data
      age_distn <- age_distn %>%  dplyr::slice_sample(n = n_drawz) %>%
        dplyr::mutate(period2=p,
                      tripid = 1:n_drawz)

      #Avidities
      avid_distn <- avidity

      #next two commands ensure there are enough observations per period
      expand_rows=ceiling((n_drawz/nrow(avid_distn)))+2

      avid_distn <- avid_distn %>%
        dplyr::slice(rep(1:dplyr::n(), each = expand_rows))

      #now need to assign tripid in order to merge to trip data
      avid_distn <- avid_distn %>%  dplyr::slice_sample(n = n_drawz) %>%
        dplyr::mutate(period2=p,
                      tripid = 1:n_drawz)

      avid_distn<- avid_distn %>%
        dplyr::left_join(age_distn, by = c("tripid", "period2"))


      demographics0[[p]]=avid_distn

    }

    demographics<- as.data.frame(do.call(rbind, demographics0))

    #now merge the ages and avidities to the trip data

    trip_data<- trip_data %>%
      dplyr::left_join(demographics, by = c("tripid", "period2"))


    # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario.
    # We will merge these data to the prediction year outcomes to calculate changes in CS.
    costs_new_all <- trip_data %>%
      dplyr::select(c(tripid, cost, catch_draw, tot_keep_cod, tot_rel_cod,
                      age, days_fished, beta_opt_out_age,  beta_opt_out_likely,  beta_opt_out_prefer,
                      tot_keep_had,tot_rel_had,
                      beta_cost, beta_opt_out, beta_sqrt_hadd_keep,
                      beta_sqrt_hadd_release, beta_sqrt_cod_hadd_keep,
                      beta_sqrt_cod_keep, beta_sqrt_cod_release, period2)) %>%
      dplyr::rename(tot_keep_cod_base = tot_keep_cod,
                    tot_rel_cod_base = tot_rel_cod,
                    tot_keep_had_base = tot_keep_had,
                    tot_rel_had_base = tot_rel_had)%>%
      dplyr::mutate(n_cal_draw = k,
                    mode = select_mode,
                    open = select_season)


    #  utility (prediction year)
    trip_data <-trip_data %>%
      dplyr::mutate(
        vA = beta_sqrt_cod_keep*sqrt(tot_keep_cod) +
          beta_sqrt_cod_release*sqrt(tot_rel_cod) +
          beta_sqrt_hadd_keep*sqrt(tot_keep_had) +
          beta_sqrt_hadd_release*sqrt(tot_rel_had) +
          beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod)*sqrt(tot_keep_had)) +
          beta_cost*cost)

    trip_data <- trip_data %>%
      dplyr::mutate(period = as.numeric(as.factor(period2)))

    period_names<-subset(trip_data, select=c("period", "period2"))
    period_names <- period_names[!duplicated(period_names), ]


    mean_trip_data <- trip_data %>% data.table::data.table() #%>% dplyr::arrange(period, tripid, catch_draw)

    #mean_trip_data<-mean_trip_data %>% dplyr::arrange(period, tripid, catch_draw)

    #New code to calculate probability of each choice occasion

    # Now expand the data to create two alternatives, representing the alternatives available in choice survey
    #mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
      tidyr::uncount(n_alt) %>%
      dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                    opt_out = ifelse(alt == 2, 1, 0))


    #Caluculate the expected utility of alts 2 parameters of the utility function,
    #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
    # mean_trip_data<-mean_trip_data %>%
    #   mutate(vA_optout= beta_opt_out*opt_out,
    #          v0_optout= beta_opt_out*opt_out,
    #          expon_vA= case_when(alt==1 ~ exp(vA),
    #                                alt==2 ~ exp(vA_optout)),
    #          expon_v0= case_when(alt==1 ~ exp(v0),
    #                                alt==2 ~ exp(v0_optout)))

    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, vA_optout := beta_opt_out*opt_out+
          beta_opt_out_age*age + beta_opt_out_likely*days_fished] %>%
      .[alt==1, expon_vA := exp(vA)] %>%
      .[alt==2, expon_vA := exp(vA_optout)]


    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, vA_col_sum := sum(expon_vA), by=list(period, catch_draw, tripid)]


    #
    # mean_trip_data1 <- mean_trip_data %>%
    #   mutate(change_CS =(1/beta_cost)*(log(vA_col_sum)-log(v0_col_sum)),
    #          probA = expon_vA/vA_col_sum,
    #          prob0 = expon_v0/v0_col_sum)
    #

    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[, probA :=expon_vA/vA_col_sum]


    mean_trip_data<- subset(mean_trip_data, alt==1)

    # mean_trip_data1<-mean_trip_data %>% group_by(period,tripid) %>% summarise(across(everything(), mean), .groups = 'drop') %>%
    #   tibble()

    all_vars<-c()
    all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("period2","tripid")]




    mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2","tripid"), .SDcols = all_vars]


    # Get rid of things we don't need.
    mean_trip_data <- subset(mean_trip_data, alt==1,select=-c(alt, beta_cost,catch_draw, expon_vA,
                                                              opt_out, vA, vA_optout, vA_col_sum))

    # Multiply the average trip probability by each of the catch variables (not the variables below) to get probability-weighted catch
    list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="tripid"
                                           & colnames(mean_trip_data) !="period2"
                                           & colnames(mean_trip_data) !="probA"]

    # for (l in list_names){
    #   mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
    # }




    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
      .[]

    # all_vars<-c()
    # all_vars <- names(length_data)[!names(length_data) %in% c("period2","tripid", "catch_draw" )]
    # length_data<- length_data %>%
    #   data.table::data.table() %>%
    #   .[,lapply(.SD, base::mean), by = c("period2","tripid"), .SDcols = all_vars]
    #
    # length_data[is.na(length_data)] <- 0
    #
    # length_data2<- mean_trip_data %>%
    #   dplyr::select(period2, tripid, probA) %>%
    #   dplyr::full_join(length_data, by = c("period2", "tripid")) #%>%
    # #dplyr::select(!c(month.x.x, month.y.x, month.x.y, month.y.y))
    # length_data2[is.na(length_data2)] <- 0
    # all_vars<-c()
    # all_vars <- names(length_data2)[!names(length_data2) %in% c("period2","tripid", "mode1" )]
    #
    # length_data3 <- length_data2 %>% ## ADD mean_kr_total for each species by tripid and perdiod2
    #   data.table::as.data.table()  %>%
    #   .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(probA)), .SDcols = all_vars] %>%
    #   .[]
    #
    #

    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate( n_choice_occasions = rep(1,nrow(.))) %>%
      dplyr::left_join(period_names, by = c("period2"))

    # mean_trip_data <- mean_trip_data %>%
    #   dplyr::select(-c("period"))
    #

    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    #calibration_data <- calibration_data  %>%   rename(period2 = period)

    sims <- directed_trips_p %>%
      dplyr::select(c(dtrip, period2))

    mean_trip_data <- mean_trip_data %>%
      dplyr::left_join(sims, by="period2")


    mean_probs<-mean_trip_data  %>% data.table::as.data.table() %>%
      .[,lapply(.SD, mean), by = c("period2"), .SDcols = "probA"]

    mean_probs<-mean_probs  %>%
      dplyr::rename(mean_prob=probA)


    mean_trip_data <- mean_trip_data %>%
      dplyr::left_join(mean_probs, by="period2")


    mean_trip_data <- mean_trip_data %>%
      dplyr::mutate(sims=dtrip/mean_prob,
                    expand=sims/n_drawz)



    list_names = c("tot_keep_cod","tot_keep_had",
                   "tot_rel_cod","tot_rel_had",
                   "tot_cod_catch","tot_had_catch",
                   "cost" ,"probA", "n_choice_occasions")


    mean_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,as.vector(list_names) := lapply(.SD, function(x) x * expand), .SDcols = list_names] %>%
      .[]

    # length_expand <- mean_trip_data %>%
    #   dplyr::select(period2, tripid, expand) %>%
    #   dplyr::full_join(length_data3, by = c("period2", "tripid")) %>%
    #   dplyr::select(-probA)
    #
    #
    # all_vars<-c()
    # all_vars <- names(length_expand)[!names(length_expand) %in% c("period2","tripid", "mode1", "expand")]
    #
    # ## Move to outside function
    # length_expand <- length_expand %>%
    #   data.table::as.data.table() %>%
    #   .[,as.vector(all_vars) := lapply(.SD, function(x) x * as.numeric(expand)), .SDcols = all_vars] %>%
    #   .[]


    # list_names = names(length_expand)[!names(length_expand) %in% c("period2","tripid", "mode1", "expand")]
    #
    # aggragate_length_data<- length_expand %>% ### Sum across tirpid
    #   data.table::as.data.table() %>%
    #   .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]
    #
    list_names = c( "tot_keep_cod","tot_keep_had",
                    "tot_rel_cod","tot_rel_had",
                    "tot_cod_catch", "tot_had_catch",
                    "probA", "n_choice_occasions")



    aggregate_trip_data <- mean_trip_data %>%
      data.table::as.data.table() %>%
      .[,lapply(.SD, sum), by =c("period2"), .SDcols = list_names]

    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "estimated_trips"
    pds_new_all<-aggregate_trip_data %>%
      #dplyr::left_join(aggragate_length_data, by = "period2") %>%
      dplyr::mutate(n_cal_draw = k,
                    mode = select_mode,
                    open = select_season)

    output<-list(pds_new_all, costs_new_all)
    #write.csv(pds_new_all, file = here::here(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/out/pds_new_all_", select_mode, "_", k, ".csv")))
    #write.csv(costs_new_all, file = here::here(paste0("C:/Users/kimberly.bastille/Desktop/codhad_data/out/costs_new_all_", select_mode, "_", k, ".csv")))
    #return(output)
  }
  return(output)

}





