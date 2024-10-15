library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(fastDummies)
library(glmmTMB)
library(parallel)
library(jtools)
library(table1)

# This removes all objects from memory. Useful to check reproducibility from a 'cleanb start'
# rm(list=ls())

# Directories
# Place to save the output files
output_dir <- "S:/PAPSIVI_Data/VaughanR/PAPSIVI/output/"

# Set number of cores for glmmTMB parallel processing (1 - total core count)
n_cores <- detectCores() - 1

############################################################################
#
# Load and merge datafiles
#
############################################################################

# read_in_minsalud_file: function to read in file using UTF-8 encoding to accept Spanish characters,
#                  autocorrect the case id variable, and change variable names to lower case
# Parameters:
#  filename: name of file to read in
# Returns:
#  dataframe of file with corrected name of case id variable to 'personid'
#
read_in_minsalud_file <- function(filename) {
  # Read in datafile
  flat_df <- read.csv(filename, header = TRUE, sep = '|', stringsAsFactors = TRUE, encoding = "UTF-8")
  
  # Rename miscoded PersonaID variable label
  flat_df <- flat_df %>%
    rename(personid = X.U.FEFF.PersonaID)
  
  # Convert all the variable names to lowercase
  names(flat_df) <- tolower(names(flat_df))
  
  return(flat_df)
}

#
# Load and recode Registro de victimas file
#

ruv_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV20211201.txt"
ruv_df <- read_in_minsalud_file(ruv_filename)

# Remove last row (it's empty)
ruv_df <- ruv_df |>
  filter(row_number() <= n() -1)

# Remove ZonaResidencia variable (has 'SIN INFORMACION' for everything)
ruv_df$zonaresidencia <- NULL

# Extract the municipio code from the mpioresidencia and put it into a separate variable municipio_id
ruv_df <- ruv_df |>
  mutate(municipio_id = str_split(mpioresidencia, " - ", simplify = TRUE)[,1])

# Recode hechovictimizante variable to more convenient level names
ruv_df <- ruv_df |>
  mutate(hechovictimizante = case_match(hechovictimizante,
                                        "ABANDONO O DESPOJO FORZADO DE TIERRAS" ~ "despojo_tierras",
                                        "ACTO TERRORISTA / ATENTADOS / COMBATES / ENFRENTAMIENTOS / HOSTIGAMIENTOS" ~ "hostigamientos",
                                        "AMENAZA" ~ "amenaza",
                                        "CONFIMANIENTO" ~ "confinamiento",
                                        "DELITOS CONTRA LA LIBERTAD Y LA INTEGRIDAD SEXUAL EN DESARROLLO DEL CONFLICTO ARMADO" ~ "violenciasexual",
                                        "DESAPARICIÓN FORZADA" ~ "desparacion",
                                        "DESPLAZAMIENTO FORZADO" ~ "desplazamiento",
                                        "HOMICIDIO" ~ "homocidio",
                                        "LESIONES PERSONALES FISICAS" ~ "lesion_fis",
                                        "LESIONES PERSONALES PSICOLOGICAS" ~ "lesion_psic",
                                        "MINAS ANTIPERSONAL, MUNICIÓN SIN EXPLOTAR Y ARTEFACTO EXPLOSIVO IMPROVISADO" ~ "minas",
                                        "PERDIDA DE BIENES MUEBLES O INMUEBLES" ~ "perdida_bienes",
                                        "SECUESTRO" ~ "secuestro",
                                        "TORTURA" ~ "tortura",
                                        "VINCULACIÓN DE NIÑOS NIÑAS Y ADOLESCENTES A ACTIVIDADES RELACIONADAS CON GRUPOS ARMADOS" ~ "reclut_ninos",
                                        "SIN INFORMACIÓN" ~ "no_info"))

# Convert hechovictimizante to dummy variables
ruv_df <- dummy_cols(ruv_df, select_columns = c("hechovictimizante"), omit_colname_prefix = TRUE)

# Recode anyone 'older' than 110 to missing
ruv_df <- ruv_df %>%
  mutate(edad = ifelse(edad > 110, NA, edad)) %>%
  mutate(edad = ifelse(edad == 0, NA, edad))

# Recode sex
ruv_df <- ruv_df |>
  mutate(sexo = case_match(sexo,
                           "HOMBRE" ~ "Male",
                           "MUJER" ~ "Female",
                           "LGBTI" ~ "Other",
                           "NO DEFINIDO" ~ NA))

ruv_df$sexo <- factor(ruv_df$sexo, ordered = FALSE)
ruv_df$sexo <- relevel(ruv_df$sexo, ref = "Male")

# Recode ethnicity
ruv_df <- ruv_df |>
  mutate(etnia = case_match(etnia,
                            "1 - INDÍGENA" ~ "Indigenous",
                            "2 - ROM (GITANO)" ~ "Roma",
                            "3 - RAIZAL (SAN ANDRES Y PROVIDENCIA)" ~ "Raizal",
                            "4 - PALENQUERO DE SAN BASILIO" ~ "Palenquero de San Basilio",
                            "5 - NEGRO, MULATO, AFROCOLOMBIANO O AFRODESCENCIENTE" ~ "Afrocolombian",
                            "NO DEFINIDO" ~ "White or Mestiza"))

# Create ethnic minority variable
ruv_df <- ruv_df |>
  mutate(etnia_min = ifelse(etnia == "White or Mestiza", "No", "Yes"))

ruv_df$etnia_min <- factor(ruv_df$etnia_min, ordered = FALSE)
ruv_df$etnia_min <- relevel(ruv_df$etnia_min, ref = "No")

# Recode indicador PAPSIVI to from YES / NO to 1 / 0 and label
ruv_df <- ruv_df |>
  mutate(indicadorpapsivi = ifelse(indicadorpapsivi == "NO", 0, 1))

ruv_df$indicadorpapsivi <- factor(ruv_df$indicadorpapsivi,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes"))

# Recode indicador Discapacidad to from YES / NO to 1 / 0 and label
ruv_df <- ruv_df |>
  mutate(indicadordiscapacidad = ifelse(indicadordiscapacidad == "NO", 0, 1))
  
ruv_df$indicadordiscapacidad <- factor(ruv_df$indicadordiscapacidad,
                                       levels = c(0, 1),
                                       labels = c("No", "Yes"))

#
# Load and recode Regimen de salud file
#

regimensal_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV_RUAFSalud.txt"
regimensal_df <- read_in_minsalud_file(regimensal_filename)

# Recode healthcare regime
regimensal_df <- regimensal_df |>
  mutate(tipo_regimen = case_match(tipo_regimen,
                                   "S - SUBSIDIADO" ~ "Subsidised",
                                   "C - CONTRIBUTIVO" ~ "Contributive"))

regimensal_df$tipo_regimen <- factor(regimensal_df$tipo_regimen, ordered = FALSE)
regimensal_df$tipo_regimen <- relevel(regimensal_df$tipo_regimen, ref = "Contributive")

# Merge Registro de victimas and Regimen de salud files into new dataframe df
df <- ruv_df %>%
  left_join(regimensal_df, by = "personid", multiple = "first")

#
# Load and recode CERAC data
#

cerac_filename = "S:/PAPSIVI_Data/data_files/CERAC_Data/CERAC_Data_2000_2012.csv"
cerac_df <- read.csv(cerac_filename, header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")

# Change variable names
cerac_df <- cerac_df %>%
  rename(municipio = Municipio) %>%
  rename(exp = Grupo.de.categoría) %>%
  rename(municipio_id = X.U.FEFF.Divipola)

# Label the cat variable conflict exposure
attr(cerac_df$exp, "label") <- "Conflict exposure"

# Quick function to convert municipio_id to the correct format (string with leading zeros)
convert_to_municipio_id_str <- function(num) {
  if (num < 10000) {
    return(sprintf("%05d", num))  # Add leading zeros if less than 1000
  } else {
    return(as.character(num))  # Convert to string without leading zeros
  }
}

# Convert all the municipio ids to the correct format using the above function
cerac_df$municipio_id <- sapply(cerac_df$municipio_id, convert_to_municipio_id_str)

# Recode the conflict exposure category so the highest number is highest exposure
cerac_df <- cerac_df |>
  mutate(exp = case_when(
    exp == "1" ~ as.factor(7),
    exp == "2" ~ as.factor(6),
    exp == "3" ~ as.factor(5),
    exp == "4" ~ as.factor(4),
    exp == "5" ~ as.factor(3),
    exp == "6" ~ as.factor(2),
    exp == "7" ~ as.factor(1)
  ))

# Make sure the conflict exposure category is an ordered factor
cerac_df$exp <- as.ordered(cerac_df$exp)

# Merge CERAC data 
df <- df %>%
  left_join(cerac_df, by = "municipio_id", multiple = "first")


############################################################################
#
# Create 'first registration' and add registration order variable to
# 'all registrations' datasets
#
############################################################################

# Create a dataset with only the first registration per participant
first_reg_df <- df |>
  distinct(personid, .keep_all = TRUE)

# Add a reg_count variable to main dataset indicating the count for entries where the same
# personid appears multiple times to use as repeated measures variable in multi-level regression
df <- df |>
  group_by(personid) |>
  mutate(df_reg_count = row_number())

# Create counts of person by exposure for future reference (for when saving the regression results)

# For all registrations dataset, we can calculate this using ruv_df rather than df here
# because it's a smaller dataset and therefore faster
N_per_exp <- ruv_df %>%
  group_by(hechovictimizante) %>%
  summarise(N = n_distinct(personid)) %>%
  rename(exposure = hechovictimizante)

# Calculate the same for the 1st registration of exposure only dataset for sensitivity analyses
N_per_exp_1st_reg <- first_reg_df %>%
  group_by(hechovictimizante) %>%
  summarise(N = n_distinct(personid)) %>%
  rename(exposure = hechovictimizante)

############################################################################
#
# Descriptives table
#
############################################################################

label(first_reg_df$sexo) <- "Sex"
label(first_reg_df$edad) <- "Age"
label(first_reg_df$etnia) <- "Ethnicity"
label(first_reg_df$tipo_regimen) <- "Healthcare regime"
label(first_reg_df$indicadordiscapacidad) <- "Disability"

# Generate descriptive statistics tables and write to file
table1(~ sexo + etnia + tipo_regimen + indicadordiscapacidad + edad | indicadorpapsivi, data = first_reg_df)
descrip_table <- table1(~ sexo + etnia + tipo_regimen + indicadordiscapacidad + edad | indicadorpapsivi, data = first_reg_df)
write(descrip_table, file = paste(output_dir, "descrip_table.html", sep = ""))

############################################################################
#
# Logistic regression analyses
#
############################################################################

#
# Unadjusted analyses for demographics
#

# Sex
#
sex_model_unadj <- glm(indicadorpapsivi ~ sexo,
                       data = first_reg_df,
                       family = binomial(link = "logit"))
summ(sex_model_unadj, exp = TRUE, digits = 3)

# Age
#
age_model_unadj <- glm(indicadorpapsivi ~ edad,
                       data = first_reg_df,
                       family = binomial(link = "logit"))
summ(age_model_unadj, exp = TRUE, digits = 4)

# Ethnicity
#
ethnicity_model_unadj <- glm(indicadorpapsivi ~ etnia_min,
                             data = first_reg_df,
                             family = binomial(link = "logit"))
summ(ethnicity_model_unadj, exp = TRUE, digits = 3)

# Healthcare Regime
#
regime_model_unadj <- glm(indicadorpapsivi ~ tipo_regimen,
                          data = first_reg_df,
                          family = binomial(link = "logit"))
summ(regime_model_unadj, exp = TRUE, digits = 3)

# Disability
#
disability_model_unadj <- glm(indicadorpapsivi ~ indicadordiscapacidad,
                              data = first_reg_df,
                              family = binomial(link = "logit"))
summ(disability_model_unadj, exp = TRUE, digits = 3)


############################################################################
#
# Multi-level models
#
############################################################################

# Function to take a regression model definition as model_str, run a multi-level model using the df dataframe and
# return a single line dataframe with results from the main predictor
#
# Parameters:
#  model_str: a string that defines a regression model to be passed to glmmTMB
# Returns:
#  A single line dataframe with exposure (the main predictor name), estimate (the odds ratio), 95% CIs and cl_lb and cl_ub
#

run_reg <- function(model_str) {
  # Run logistic regression model, time and print duration
  print(paste("Running: ", model_str, sep = ""))
  start.time <- Sys.time()
  reg_model <- glmmTMB(as.formula(model_str), data = df, family = binomial(link = "logit"), control = glmmTMBControl(parallel = n_cores))
  end.time <- Sys.time()
  print(end.time - start.time)
  
  # Calculate ORs and CIs from model
  coef_table <- summary(reg_model)$coefficients$cond 
  odds_ratio_info <- exp(coef_table[,"Estimate"]) 
  conf_int <- exp(confint(reg_model, parm = "beta_", level = 0.95))
  
  # Extract estimates and CIs
  conf_int_df <- as.data.frame(conf_int)
  conf_int_df <- rownames_to_column(conf_int_df, "name")
  
  # Filter only for main predictor
  pred_name <- names(odds_ratio_info[2])
  results_df <- conf_int_df |>
    filter(name == pred_name)
  
  # Change order of columns and rename
  results_df <- results_df |>
    select(exposure = name, estimate = Estimate, ci_lb = "2.5 %", ci_ub = "97.5 %")
  
  # and return
  return(results_df)
}

# Vector of all of the exposure predictor variables
# Used to create regression model strings to pass to run_reg
exp_vec <- c("despojo_tierras", "hostigamientos", "amenaza", "confinamiento", "violenciasexual", "desparacion",
                "desplazamiento", "homocidio", "lesion_fis", "lesion_psic", "minas", "perdida_bienes", "secuestro",
                "tortura", "reclut_ninos")

############################################################################
#
# Run Model 1 for all exposures
#
############################################################################

# Create vector of regression model strings, each with the name of a conflict exposure variable in the appropriate place:
# e.g. indicadorpapsivi ~ despojo_tierras + (1|df_reg_count) etc
model1_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Define empty dataframe to store results in
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model1_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model1_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model1_results_df <- model1_results_df %>%
  left_join(N_per_exp, by = "exposure")

# Reorder columns for writing file
model1_results_df <- model1_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model1_results_df <- model1_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model1_results_df, paste(output_dir, "mod1_results.csv", sep = ""), row.names = FALSE)


############################################################################
#
# Run Model 2 for all exposures
#
############################################################################

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model2_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + edad + sexo + etnia_min + tipo_regimen + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Define empty dataframe to append results to
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model2_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model2_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model2_results_df <- model2_results_df %>%
  left_join(N_per_exp, by = "exposure")

# Reorder columns for writing file
model2_results_df <- model2_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model2_results_df <- model2_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model2_results_df, paste(output_dir, "mod2_results.csv", sep = ""), row.names = FALSE)


############################################################################
#
# Run Model 3 for all exposures
#
############################################################################

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model3_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + edad + sexo + etnia_min + tipo_regimen + exp + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Define empty dataframe to append results to
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model3_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model3_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model3_results_df <- model3_results_df %>%
  left_join(N_per_exp, by = "exposure")

# Reorder columns for writing file
model3_results_df <- model3_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model3_results_df <- model3_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model3_results_df, paste(output_dir, "mod3_results.csv", sep = ""), row.names = FALSE)


############################################################################
#
# Interactions (using Model 3)
#
############################################################################

#
# Sex
#

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model3_int_sex_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " * sexo + edad + etnia_min + tipo_regimen + exp + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Iterate through every armed conflict exposure regression model, run it, and write the results
# Iterating using i so we can track the model_str regression equation and name of the exposure at the same 
for (i in seq_along(model3_int_sex_str_vec)) {
  
  # Pull the regression equation string and exposure name string into separate variables for clarity
  model_str <- model3_int_sex_str_vec[i]
  pred_str <- exp_vec[i]

  # Run the regression
  print(paste("Running: ", model_str, sep = ""))
  start.time <- Sys.time()
  reg_model <- glmmTMB(as.formula(model_str), data = df, family = binomial(link = "logit"), control = glmmTMBControl(parallel = n_cores))
  end.time <- Sys.time()
  print(end.time - start.time)
  
  # Calculate ORs and CIs from model
  coef_table <- summary(reg_model)$coefficients$cond 
  odds_ratio_info <- exp(coef_table[,"Estimate"]) 
  conf_int <- exp(confint(reg_model, parm = "beta_", level = 0.95))
  
  # Extract estimates and CIs
  conf_int_df <- as.data.frame(conf_int)
  conf_int_df <- rownames_to_column(conf_int_df, "name")
  
  # Change order of columns and rename
  conf_int_df <- conf_int_df |>
    select(exposure = name, estimate = Estimate, ci_lb = "2.5 %", ci_ub = "97.5 %")

  write.csv(conf_int_df, paste(output_dir, "model3_int_sex_", pred_str, "_results.csv", sep = ""), row.names = FALSE)
}

#
# Healthcare regime
#

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model3_int_reg_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " * tipo_regimen + edad + etnia_min + sexo + exp + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Iterate through every armed conflict exposure regression model, run it, and write the results
# Iterating using i so we can track the model_str regression equation and name of the exposure at the same 
for (i in seq_along(model3_int_reg_str_vec)) {
  # Pull the regression equation string and exposure name string into separate variables for clarity
  model_str <- model3_int_reg_str_vec[i]
  pred_str <- exp_vec[i]
  
  # Run the regression
  print(paste("Running: ", model_str, sep = ""))
  start.time <- Sys.time()
  reg_model <- glmmTMB(as.formula(model_str), data = df, family = binomial(link = "logit"), control = glmmTMBControl(parallel = n_cores))
  end.time <- Sys.time()
  print(end.time - start.time)
  
  # Calculate ORs and CIs from model
  coef_table <- summary(reg_model)$coefficients$cond 
  odds_ratio_info <- exp(coef_table[,"Estimate"]) 
  conf_int <- exp(confint(reg_model, parm = "beta_", level = 0.95))
  
  # Extract estimates and CIs
  conf_int_df <- as.data.frame(conf_int)
  conf_int_df <- rownames_to_column(conf_int_df, "name")

  conf_int_df <- conf_int_df |>
    select(exposure = name, estimate = Estimate, ci_lb = "2.5 %", ci_ub = "97.5 %")

  write.csv(conf_int_df, paste(output_dir, "model3_int_reg_", pred_str, "_results.csv", sep = ""), row.names = FALSE)
}


#
# Ethnic minority
#

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model3_int_etn_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " * etnia_min + tipo_regimen + edad + sexo + exp + (1|df_reg_count) + (1|municipio_id)", sep = "")

# Iterate through every armed conflict exposure regression model, run it, and write the results
# Iterating using i so we can track the model_str regression equation and name of the exposure at the same 
for (i in seq_along(model3_int_etn_str_vec)) {
  # Pull the regression equation string and exposure name string into separate variables for clarity
  model_str <- model3_int_etn_str_vec[i]
  pred_str <- exp_vec[i]
  
  # Run the regression
  print(paste("Running: ", model_str, sep = ""))
  start.time <- Sys.time()
  reg_model <- glmmTMB(as.formula(model_str), data = df, family = binomial(link = "logit"), control = glmmTMBControl(parallel = n_cores))
  end.time <- Sys.time()
  print(end.time - start.time)
  
  # Calculate ORs and CIs from model
  coef_table <- summary(reg_model)$coefficients$cond 
  odds_ratio_info <- exp(coef_table[,"Estimate"]) 
  conf_int <- exp(confint(reg_model, parm = "beta_", level = 0.95))
  
  # Extract estimates and CIs
  conf_int_df <- as.data.frame(conf_int)
  conf_int_df <- rownames_to_column(conf_int_df, "name")

  conf_int_df <- conf_int_df |>
    select(exposure = name, estimate = Estimate, ci_lb = "2.5 %", ci_ub = "97.5 %")
  
  write.csv(conf_int_df, paste(output_dir, "model3_int_etn_", pred_str, "_results.csv", sep = ""), row.names = FALSE)
}


############################################################################
#
# Sensitivity analyses
#
############################################################################

# Swap dataframes to make first_reg_df the one we use
all_reg_df <- df
df <- first_reg_df


############################################################################
#
# Run Model 1 sensitivity analyses (1st reg only) for all exposures
#
############################################################################

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model1_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + (1|municipio_id)", sep = "")

# Define empty dataframe to store results in
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model1_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model1_sens_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model1_sens_results_df <- model1_sens_results_df %>%
  left_join(N_per_exp_1st_reg, by = "exposure")

# Reorder columns for writing file
model1_sens_results_df <- model1_sens_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model1_sens_results_df <- model1_sens_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model1_sens_results_df, paste(output_dir, "mod1_sens_results.csv", sep = ""), row.names = FALSE)


############################################################################
#
# Run Model 2 sensitivity analyses (1st reg only) for all exposures
#
############################################################################

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model2_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + edad + sexo + etnia_min + tipo_regimen + (1|municipio_id)", sep = "")

# Define empty dataframe to append results to
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model2_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model2_sens_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model2_sens_results_df <- model2_sens_results_df %>%
  left_join(N_per_exp_1st_reg, by = "exposure")

# Reorder columns for writing file
model2_sens_results_df <- model2_sens_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model2_sens_results_df <- model2_sens_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model2_sens_results_df, paste(output_dir, "mod2_sens_results.csv", sep = ""), row.names = FALSE)


############################################################################
#
# Run Model 3 sensitivity analyses (1st reg only) for all exposures
#
############################################################################

# Create list of regression model strings, each with the name of the conflict exposure in the right place
model3_str_vec <- paste("indicadorpapsivi ~ ", exp_vec, " + edad + sexo + etnia_min + tipo_regimen + exp + (1|municipio_id)", sep = "")

# Define empty dataframe to append results to
results_df <- data.frame(exposure = character(),
                         estimate = double(),
                         ci_lb = double(),
                         ci_ub = double())

# Iterate through every armed conflict regression model, run it, and append the results to results_df
for (model_str in model3_str_vec) {
  model_result_df <- run_reg(model_str)
  colnames(model_result_df) <- colnames(results_df)
  results_df <- rbind(results_df, model_result_df)
}

# Store results in specifically named dataframe
model3_sens_results_df <- results_df

# Include N of people per exposure by merging with N_per_exp
model3_sens_results_df <- model3_sens_results_df %>%
  left_join(N_per_exp_1st_reg, by = "exposure")

# Reorder columns for writing file
model3_sens_results_df <- model3_sens_results_df |>
  select(exposure, N, estimate, ci_lb, ci_ub)

# Add handy summary str column
model3_sens_results_df <- model3_sens_results_df |>
  mutate(est_str = sprintf("%.2f (%.2f - %.2f)", estimate, ci_lb, ci_ub))

# ...and write
write.csv(model3_sens_results_df, paste(output_dir, "mod3_sens_results.csv", sep = ""), row.names = FALSE)

