## Get P_stars to be used in the calibration of the model
### This script calculates and saves the pstar values that are used to correct for harvested and
# ### discarded fish in the model calibration.
#
# #### Run pstars by mode and month
#





#### pstars by open vs closed season

# fh_0<- calculate_pstars("fh", 0,1, .01, .5, .5)
# write.csv(fh_0, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_closed.csv")
# sh_0<- calculate_pstars("sh", 0,1, .01, .5, .5)
# write.csv(sh_0, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_closed.csv")
# pr_0<- calculate_pstars("pr", 0, 1, .01,.5, .5)
# write.csv(pr_0, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_closed.csv")
#
# fh_1<- calculate_pstars("fh", 1, 1, .005,.92, .5)
# write.csv(fh_1, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_open.csv")
# sh_1<- calculate_pstars("sh", 1, 1, .005,.999, .999)
# write.csv(sh_1, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_open.csv")
pr_1<- calculate_pstars("pr", 1, 1, .002, .91, .71)
write.csv(pr_1, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_open.csv")




# pstar_46_150<- read.csv("C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_open.csv")
# write.csv(pstar_46_150, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_open_46_150.csv")





