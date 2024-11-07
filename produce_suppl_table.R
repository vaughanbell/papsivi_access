############################################################################
#
# Script to produce supplementary tables 2 to 6 from results files
#
############################################################################

library(dplyr)

options(scipen = 999)

data_dir <- "./Output/"
output_dir <- "./Output/"

mod1_filename <- paste(data_dir, "mod1_results.csv", sep = "")
mod2_filename <- paste(data_dir, "mod2_results.csv", sep = "")
mod3_filename <- paste(data_dir, "mod3_results.csv", sep = "")

mod1_sens_filename <- paste(data_dir, "mod1_sens_results.csv", sep = "")
mod2_sens_filename <- paste(data_dir, "mod2_sens_results.csv", sep = "")
mod3_sens_filename <- paste(data_dir, "mod3_sens_results.csv", sep = "")

mod3_strat_sex_filename <- paste(data_dir, "mod3_strat_sex_results.csv", sep = "")
mod3_strat_reg_filename <- paste(data_dir, "mod3_strat_reg_results.csv", sep = "")
mod3_strat_etn_filename <- paste(data_dir, "mod3_strat_etn_results.csv", sep = "")

mod3_int_sex_p_filename <- paste(data_dir, "mod3_int_sex_pvalues.csv", sep = "")
mod3_int_reg_p_filename <- paste(data_dir, "mod3_int_reg_pvalues.csv", sep = "")
mod3_int_etn_p_filename <- paste(data_dir, "mod3_int_etn_pvalues.csv", sep = "")

#
# Generate supplementary table 2
#

file_list <- c(mod1_filename, mod2_filename, mod3_filename)

for (i in seq_along(file_list)) {
  mod_df <- read.csv(file_list[i], header = TRUE)
  
  mod_df <- mod_df %>%
    mutate(exposure = case_match(exposure,
                                 "despojo_tierras" ~ "Forced land abandonment / dispossession",
                                 "hostigamientos" ~ "Witness to terrorism or combat",
                                 "amenaza" ~ "Threats",
                                 "confinamiento" ~ "Confinement",
                                 "violenciasexual" ~ "Sexual violence",
                                 "desparacion" ~ "Forced disappearance",
                                 "desplazamiento" ~ "Forced displacement",
                                 "homocidio" ~ "Homicide",
                                 "lesion_fis" ~ "Physical injury",
                                 "lesion_psic" ~ "Psychological injury",
                                 "minas" ~ "Mines, improvised explosives",
                                 "perdida_bienes" ~ "Loss of personal belongings",
                                 "secuestro" ~ "Kidnapping",
                                 "tortura" ~ "Torture",
                                 "reclut_ninos" ~ "Child recruitment to armed groups"))
  
  mod_df <- mod_df %>%
    mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

  if (i == 1) {
    table_df <- mod_df %>%
      select(exposure, N, mod1_str = est_str)
  }
  if (i == 2) {
    table_df$mod2_str <- mod_df$est_str
  }
  if (i == 3) {
    table_df$mod3_str <- mod_df$est_str
  }
}

write.csv(table_df, paste(output_dir, "suppl_table_s2.csv", sep=""), row.names = FALSE)


#
# Generate supplementary table 3
#

file_list <- c(mod1_sens_filename, mod2_sens_filename, mod3_sens_filename)

for (i in seq_along(file_list)) {
  mod_df <- read.csv(file_list[i], header = TRUE)
  
  mod_df <- mod_df %>%
    mutate(exposure = case_match(exposure,
                                 "despojo_tierras" ~ "Forced land abandonment / dispossession",
                                 "hostigamientos" ~ "Witness to terrorism or combat",
                                 "amenaza" ~ "Threats",
                                 "confinamiento" ~ "Confinement",
                                 "violenciasexual" ~ "Sexual violence",
                                 "desparacion" ~ "Forced disappearance",
                                 "desplazamiento" ~ "Forced displacement",
                                 "homocidio" ~ "Homicide",
                                 "lesion_fis" ~ "Physical injury",
                                 "lesion_psic" ~ "Psychological injury",
                                 "minas" ~ "Mines, improvised explosives",
                                 "perdida_bienes" ~ "Loss of personal belongings",
                                 "secuestro" ~ "Kidnapping",
                                 "tortura" ~ "Torture",
                                 "reclut_ninos" ~ "Child recruitment to armed groups"))
  
  mod_df <- mod_df %>%
    mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))
  
  if (i == 1) {
    table_df <- mod_df %>%
      select(exposure, N, mod1_str = est_str)
  }
  if (i == 2) {
    table_df$mod2_str <- mod_df$est_str
  }
  if (i == 3) {
    table_df$mod3_str <- mod_df$est_str
  }
}

write.csv(table_df, paste(output_dir, "suppl_table_s3.csv", sep=""), row.names = FALSE)


#
# Generate supplementary table S4
#

sex_strat_df <- read.csv(mod3_strat_sex_filename, header = TRUE)
sex_int_p_df <- read.csv(mod3_strat_sex_p_filename, header = TRUE)

# Round p values to 5 decimal places and replace values below 0.00001 with "p<0.00001"
sex_int_p_df$p_value <- ifelse(sex_int_p_df$p_value < 0.00001, "<0.00001", round(sex_int_p_df$p_value, 5))

# Merge p values with stratified estimates, keeping on the p value for the top line of each exposure
sex_strat_df <- sex_strat_df %>%
  left_join(sex_int_p_df, by = "exposure") %>%
  mutate(p_value = ifelse(duplicated(exposure), NA, p_value))  # Set p to NA for all but the first occurrence of each exposure

sex_strat_df <- sex_strat_df %>%
  mutate(exposure = case_match(exposure,
                               "despojo_tierras" ~ "Forced land abandonment / dispossession",
                               "hostigamientos" ~ "Witness to terrorism or combat",
                               "amenaza" ~ "Threats",
                               "confinamiento" ~ "Confinement",
                               "violenciasexual" ~ "Sexual violence",
                               "desparacion" ~ "Forced disappearance",
                               "desplazamiento" ~ "Forced displacement",
                               "homocidio" ~ "Homicide",
                               "lesion_fis" ~ "Physical injury",
                               "lesion_psic" ~ "Psychological injury",
                               "minas" ~ "Mines, improvised explosives",
                               "perdida_bienes" ~ "Loss of personal belongings",
                               "secuestro" ~ "Kidnapping",
                               "tortura" ~ "Torture",
                               "reclut_ninos" ~ "Child recruitment to armed groups"))



sex_strat_df <- sex_strat_df %>%
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub)) %>%
  select(exposure, sexo, est_str, p_value) %>%
  arrange(exposure, sexo)

write.csv(sex_strat_df, paste(output_dir, "suppl_table_s4.csv", sep=""), row.names = FALSE)


#
# Generate supplementary table S5
#

reg_strat_df <- read.csv(mod3_strat_reg_filename, header = TRUE)
reg_int_p_df <- read.csv(mod3_strat_reg_p_filename, header = TRUE)

# Round p values to 5 decimal places and replace values below 0.00001 with "p<0.00001"
reg_int_p_df$p_value <- ifelse(reg_int_p_df$p_value < 0.00001, "<0.00001", round(reg_int_p_df$p_value, 5))

# Merge p values with stratified estimates, keeping on the p value for the top line of each exposure
reg_strat_df <- reg_strat_df %>%
  left_join(reg_int_p_df, by = "exposure") %>%
  mutate(p_value = ifelse(duplicated(exposure), NA, p_value))  # Set p to NA for all but the first occurrence of each exposure

reg_strat_df <- reg_strat_df %>%
  mutate(exposure = case_match(exposure,
                               "despojo_tierras" ~ "Forced land abandonment / dispossession",
                               "hostigamientos" ~ "Witness to terrorism or combat",
                               "amenaza" ~ "Threats",
                               "confinamiento" ~ "Confinement",
                               "violenciasexual" ~ "Sexual violence",
                               "desparacion" ~ "Forced disappearance",
                               "desplazamiento" ~ "Forced displacement",
                               "homocidio" ~ "Homicide",
                               "lesion_fis" ~ "Physical injury",
                               "lesion_psic" ~ "Psychological injury",
                               "minas" ~ "Mines, improvised explosives",
                               "perdida_bienes" ~ "Loss of personal belongings",
                               "secuestro" ~ "Kidnapping",
                               "tortura" ~ "Torture",
                               "reclut_ninos" ~ "Child recruitment to armed groups"))



reg_strat_df <- reg_strat_df %>%
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub)) %>%
  select(exposure, tipo_regimen, est_str, p_value) %>%
  arrange(exposure, tipo_regimen)

write.csv(reg_strat_df, paste(output_dir, "suppl_table_s5.csv", sep=""), row.names = FALSE)

#
# Generate supplementary table S6
#

etn_strat_df <- read.csv(mod3_strat_etn_filename, header = TRUE)
etn_int_p_df <- read.csv(mod3_strat_etn_p_filename, header = TRUE)

# Round p values to 5 decimal places and replace values below 0.00001 with "p<0.00001"
etn_int_p_df$p_value <- ifelse(etn_int_p_df$p_value < 0.00001, "<0.00001", round(etn_int_p_df$p_value, 5))

# Merge p values with stratified estimates, keeping on the p value for the top line of each exposure
etn_strat_df <- etn_strat_df %>%
  left_join(etn_int_p_df, by = "exposure") %>%
  mutate(p_value = ifelse(duplicated(exposure), NA, p_value))  # Set p to NA for all but the first occurrence of each exposure

etn_strat_df <- etn_strat_df %>%
  mutate(exposure = case_match(exposure,
                               "despojo_tierras" ~ "Forced land abandonment / dispossession",
                               "hostigamientos" ~ "Witness to terrorism or combat",
                               "amenaza" ~ "Threats",
                               "confinamiento" ~ "Confinement",
                               "violenciasexual" ~ "Sexual violence",
                               "desparacion" ~ "Forced disappearance",
                               "desplazamiento" ~ "Forced displacement",
                               "homocidio" ~ "Homicide",
                               "lesion_fis" ~ "Physical injury",
                               "lesion_psic" ~ "Psychological injury",
                               "minas" ~ "Mines, improvised explosives",
                               "perdida_bienes" ~ "Loss of personal belongings",
                               "secuestro" ~ "Kidnapping",
                               "tortura" ~ "Torture",
                               "reclut_ninos" ~ "Child recruitment to armed groups"))



etn_strat_df <- etn_strat_df %>%
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub)) %>%
  select(exposure, etnia_min, est_str, p_value) %>%
  arrange(exposure, etnia_min)

write.csv(etn_strat_df, paste(output_dir, "suppl_table_s6.csv", sep=""), row.names = FALSE)
