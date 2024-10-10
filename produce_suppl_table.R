############################################################################
#
# Script to produce supplementary tables 2 and 3 from results files
#
############################################################################

library(dplyr)

data_dir <- "./Output/"
output_dir <- "./Output/"

mod1_filename <- paste(data_dir, "mod1_results.csv", sep = "")
mod2_filename <- paste(data_dir, "mod2_results.csv", sep = "")
mod3_filename <- paste(data_dir, "mod3_results.csv", sep = "")

mod1_sens_filename <- paste(data_dir, "mod1_sens_results.csv", sep = "")
mod2_sens_filename <- paste(data_dir, "mod2_sens_results.csv", sep = "")
mod3_sens_filename <- paste(data_dir, "mod3_sens_results.csv", sep = "")

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
