#scripts/00_run_full_pipeline.R

scripts_to_run <- c(
  "scripts/02_prepare_analysis_data.R",
  "scripts/04_detect_peaks.R",
  "scripts/05_generate_peak_pairs.R",
  "scripts/06_calculate_degree_days.R",
  "scripts/07_create_train_test_split.R",
  "scripts/08_select_generation_pairs.R",
  "scripts/09_fit_generation_model.R",
  "scripts/10_validate_generation_model.R",
  "scripts/11_refit_final_model.R",
  "scripts/12_estimate_peak_uncertainty.R",
  "scripts/13_create_final_algorithm_parameters.R",
  "scripts/14_test_final_workflow.R"
)

for (script in scripts_to_run) {
  cat("\n============================\n")
  cat("Running:", script, "\n")
  cat("============================\n")
  
  source(script)
}

# paste "source("scripts/00_run_full_pipeline.R")" to the console to use this script.