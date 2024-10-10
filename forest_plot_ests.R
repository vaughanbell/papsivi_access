library(dplyr)
library(forester)   # from https://github.com/rdboyes/forester; requires ggplot 3.4.4 for compatibility

data_dir <- "./Output/"
output_dir <- "./ForestPlots/"

all_vics_est_data_filename <- paste(data_dir, "mod3_results.csv", sep = "")
mod3_all_vics_estimate_df <- read.csv(all_vics_est_data_filename, header = TRUE)

mod3_all_vics_estimate_df <- mod3_all_vics_estimate_df %>%
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


first_vic_est_data_filename <- paste(data_dir, "mod3_sens_results.csv", sep = "")
mod3_first_vic_estimate_df <- read.csv(first_vic_est_data_filename, header = TRUE)

mod3_first_vic_estimate_df <- mod3_first_vic_estimate_df %>%
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

#
# forester_plot: Function to generate a forest plot from the dataframe of theme prevalences outputted by run_meta using the forester package
#
# Parameters:
#  results_df: dataframe with themes, prevalences, and confidence intervals from run_meta
#  max_ci_ub: the upper limit of the prevalence scale for the plot
#  render_as: filetype to write image file as
#  file_path: the path to write the PNG file
#  sparse_option: switch for sparse option for the rma.mv meta-analysis.
#                 It massively speeds up the calculation but prevents I2 being
#                 calculated because var.comp can't run on sparse matrices. Therefore,
#                 this needs to be switched off for main meta-analysis where I2 is needed
# Returns:
#  nothing
#
forester_plot <- function(results_df, max_ci_ub, render_as, file_path) {
  forest_df <- results_df %>%
    arrange(desc(estimate))
  
  # Extract column for the left side column needed for forester
  forest_df_lsd <- forest_df %>%
    select("Exposure" = exposure, "Total cases" = N)
  
  # Extract columns for the right side column needed for forester (and round Q to 2 decimals places)
  forest_df_rsd <- forest_df %>%
    select("OR (95% CI)" = est_str)
  
  # Plot as forest plot
  if (plot == 1) {
    forester(left_side_data = forest_df_lsd,
             right_side_data = forest_df_rsd,
             estimate = forest_df$estimate,
             ci_low = forest_df$ci_lb,
             ci_high = forest_df$ci_ub,
             null_line_at = 1,
             stripe_colour = "#ffffff",
             estimate_precision = 2,
             font_family = "sans",
             arrows = TRUE, 
             arrow_labels = c("Less likely", "More likely"),
             ggplot_width = 30,
             nudge_x = 1,
             xlim = c(-0.05, max_ci_ub),
             render_as = render_as,
             file_path = file_path,
             display = TRUE)
  }
}

plot <- 1

# Add convenient summary string of OR and 95% CI for plot
mod3_all_vics_estimate_df <- mod3_all_vics_estimate_df %>%
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# Set filename and plot
filename <- paste(output_dir, "all_victimizations_assocs.png", sep = "")
forest_plot <- forester_plot(mod3_all_vics_estimate_df, 4, render_as = "png", filename)

# Add convenient summary string of OR and 95% CI for plot
mod3_first_vic_estimate_df <- mod3_first_vic_estimate_df %>%
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# Set filename and plot
filename <- paste(output_dir, "first_victimization_assocs.png", sep = "")
forest_plot <- forester_plot(mod3_first_vic_estimate_df, 4, render_as = "png", filename)

