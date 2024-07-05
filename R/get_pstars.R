## Get P_stars to be used in the calibration of the model
### This script calculates and saves the pstar values that are used to correct for harvested and
### discarded fish in the model calibration.

#### Run pstars by mode and month


fh_4<- calculate_pstars("fh", 4)
write.csv(fh_4, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_4.csv")
fh_5<- calculate_pstars("fh", 5)
write.csv(fh_5, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_5.csv")
fh_6<- calculate_pstars("fh", 6)
write.csv(fh_6, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_6.csv")
fh_7<- calculate_pstars("fh", 7)
write.csv(fh_7, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_7.csv")
fh_8<- calculate_pstars("fh", 8)
write.csv(fh_8, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_8.csv")
fh_9<- calculate_pstars("fh", 9)
write.csv(fh_9, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_9.csv")
fh_10<- calculate_pstars("fh", 10)
write.csv(fh_10, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_fh_10.csv")


pr_4<- calculate_pstars("pr", 4)
write.csv(pr_4, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_4.csv")
pr_5<- calculate_pstars("pr", 5)
write.csv(pr_5, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_5.csv")
pr_6<- calculate_pstars("pr", 6)
write.csv(pr_6, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_6.csv")
pr_7<- calculate_pstars("pr", 7)
write.csv(pr_7, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_7.csv")
pr_8<- calculate_pstars("pr", 8)
write.csv(pr_8, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_8.csv")
pr_9<- calculate_pstars("pr", 9)
write.csv(pr_9, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_9.csv")
pr_10<- calculate_pstars("pr", 10)
write.csv(pr_10, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_pr_10.csv")


sh_4<- calculate_pstars("sh", 4)
write.csv(sh_4, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_4.csv")
sh_5<- calculate_pstars("sh", 5)
write.csv(sh_5, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_5.csv")
sh_6<- calculate_pstars("sh", 6)
write.csv(sh_6, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_6.csv")
sh_7<- calculate_pstars("sh", 7)
write.csv(sh_7, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_7.csv")
sh_8<- calculate_pstars("sh", 8)
write.csv(sh_8, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_8.csv")
sh_9<- calculate_pstars("sh", 9)
write.csv(sh_9, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_9.csv")
sh_10<- calculate_pstars("sh", 10)
write.csv(sh_10, file = "C:/Users/kimberly.bastille/Desktop/codhad_data/pstars/pstar_sh_10.csv")
