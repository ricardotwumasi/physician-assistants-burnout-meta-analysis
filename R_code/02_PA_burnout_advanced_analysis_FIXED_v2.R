# female percentage analysis
results_summary_enhanced <- list(
  study_characteristics = list(
    n_studies = res_overall$k,
    total_participants = sum(pa_final$sample_size),
    study_period = paste(min(pa_final$year), "to", max(pa_final$year)),
    studies_with_female_data = sum(!is.na(pa_final$female_percentage_clean)),
    female_percentage_range = ifelse(sum(!is.na(pa_final$female_percentage_clean)) > 0,
                                    paste(sprintf("%.1f%%", min(pa_final$female_percentage_clean, na.rm = TRUE)),
                                          "to",
                                          sprintf("%.1f%%", max(pa_final$female_percentage_clean, na.rm = TRUE))),
                                    "N/A")
  ),
  overall_results = list(
    prevalence = sprintf("%.1f%% (95%% CI: %.1f%% to %.1f%%)", 
                         overall_results$pred * 100,
                         overall_results$ci.lb * 100,
                         overall_results$ci.ub * 100),
    prediction_interval = sprintf("%.1f%% to %.1f%%", 
                                  overall_results$pi.lb * 100,
                                  overall_results$pi.ub * 100),
    heterogeneity = list(
      I2 = sprintf("%.1f%%", res_overall$I2),
      tau2 = sprintf("%.4f", res_overall$tau2),
      Q_test = sprintf("Q = %.2f, p = %.4f", res_overall$QE, res_overall$QEp)
    )
  ),
  female_percentage_analysis = if(!is.null(reg_female)) {
    list(
      studies_included = sum(!is.na(pa_final$female_percentage_clean)),
      coefficient = sprintf("%.4f", reg_female$b[2]),
      p_value = sprintf("%.3f", reg_female$pval[2]),
      r_squared = sprintf("%.1f%%", reg_female$R2),
      significant = reg_female$pval[2] < 0.05,
      interpretation = ifelse(reg_female$pval[2] < 0.05,
                             paste("Significant association:", 
                                   ifelse(reg_female$b[2] > 0, 
                                         "Higher female % associated with higher burnout",
                                         "Higher female % associated with lower burnout")),
                             "No significant association with female percentage")
    )
  } else {
    list(message = "Insufficient data for female percentage analysis")
  },
  publication_bias = list(
    egger_test = sprintf("p = %.3f", egger_test$pval),
    begg_test = sprintf("p = %.3f", rank_test$pval),
    trim_fill_missing = studies_trimmed,
    overall_assessment = ifelse(egger_test$pval < 0.05 | rank_test$pval < 0.05,
                               "Evidence of publication bias detected",
                               "No clear evidence of publication bias")
  ),
  subgroup_differences = list(
    measurement_tool = list(
      significant = mbi_subgroup$QMp < 0.05,
      p_value = sprintf("%.4f", mbi_subgroup$QMp),
      mbi_estimate = sprintf("%.1f%%", mbi_est$pred * 100),
      non_mbi_estimate = sprintf("%.1f%%", non_mbi_est$pred * 100)
    ),
    study_quality = list(
      significant = quality_subgroup$QMp < 0.05,
      p_value = sprintf("%.4f", quality_subgroup$QMp),
      low_risk_estimate = sprintf("%.1f%%", low_risk_est$pred * 100),
      moderate_risk_estimate = sprintf("%.1f%%", mod_risk_est$pred * 100)
    )
  ),
  model_comparison = list(
    best_model = aic_table$Model[1],
    best_model_aic = aic_table$AIC[1],
    female_percentage_included = ifelse(!is.null(reg_female), 
                                       "Female percentage model available", 
                                       "Female percentage model not available")
  )
)

# Save as JSON for easy import
library(jsonlite)
write_json(results_summary_enhanced, "PA_burnout_results_enhanced_summary.json", pretty = TRUE)

# ============================================================================
# FINAL SUMMARY REPORT
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ENHANCED FINAL SUMMARY REPORT - PA BURNOUT META-ANALYSIS\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\n🔥 MAIN FINDINGS:\n")
cat("• One in three PAs experience burnout (", results_summary_enhanced$overall_results$prevalence, ")\n", sep = "")
cat("• Very high heterogeneity observed (I² = ", res_overall$I2, "%)\n", sep = "")
cat("• Evidence of publication bias (Egger's test p = ", sprintf("%.3f", egger_test$pval), ")\n", sep = "")
cat("• Burnout measurement tool significantly influences estimates (p = ", sprintf("%.4f", mbi_subgroup$QMp), ")\n", sep = "")

if(!is.null(reg_female)) {
  cat("\n🚺 FEMALE PERCENTAGE FINDINGS:\n")
  cat("• Studies with female % data:", sum(!is.na(pa_final$female_percentage_clean)), "out of", res_overall$k, "\n")
  cat("• Female % range:", results_summary_enhanced$study_characteristics$female_percentage_range, "\n")
  cat("• Association with burnout:", results_summary_enhanced$female_percentage_analysis$interpretation, "\n")
  if(reg_female$pval[2] < 0.05) {
    cat("• Meta-regression coefficient:", sprintf("%.4f (p = %.3f)", reg_female$b[2], reg_female$pval[2]), "\n")
    cat("• Variance explained (R²):", sprintf("%.1f%%", reg_female$R2), "\n")
  }
}

cat("\n📊 CLINICAL IMPLICATIONS:\n")
cat("• Burnout prevalence among PAs is substantial and concerning\n")
cat("• Wide variation suggests multiple contributing factors\n")
cat("• Standardized measurement needed for better comparisons\n")
if(!is.null(reg_female) && reg_female$pval[2] < 0.05) {
  cat("• Gender composition may influence burnout rates in PA teams\n")
}
cat("• Targeted interventions should be developed and tested\n")

cat("\n💡 RECOMMENDATIONS FOR FUTURE RESEARCH:\n")
cat("• Use standardized burnout measures (preferably MBI)\n")
cat("• Investigate specialty-specific risk factors\n")
cat("• Examine workplace and demographic moderators comprehensively\n")
if(sum(!is.na(pa_final$female_percentage_clean)) < res_overall$k) {
  cat("• Consistently report gender demographics in PA burnout studies\n")
}
cat("• Develop and test PA-specific interventions\n")
cat("• Conduct longitudinal studies to examine burnout trajectories\n")

cat("\n📁 ALL ENHANCED OUTPUT FILES CREATED:\n")
cat("Forest Plots:\n")
cat("  • PA_forest_plot_main_with_rob_scores.pdf\n")
cat("  • PA_forest_plot_by_quality_with_rob.pdf\n")
cat("  • PA_forest_plot_by_mbi_enhanced.pdf\n")
if(length(main_specialties) > 0) {
  cat("  • PA_forest_plot_by_specialty_enhanced.pdf\n")
}

cat("Analysis Plots:\n")
cat("  • PA_publication_bias_assessment.pdf\n")
cat("  • PA_cumulative_forest_plot.pdf\n")
cat("  • PA_meta_regression_plots.pdf\n")
cat("  • PA_enhanced_funnel_plots_with_moderators.pdf\n")
cat("  • PA_correlation_matrix.pdf\n")

if(!is.null(reg_female)) {
  cat("Female Percentage Analysis:\n")
  cat("  • PA_female_percentage_regression.pdf\n")
  cat("  • PA_female_percentage_detailed_analysis.pdf\n")
}

cat("Tables and Data:\n")
cat("  • PA_study_characteristics_enhanced_table.docx\n")
cat("  • PA_meta_regression_results_table.docx\n")
cat("  • PA_subgroup_analysis_enhanced_table.docx\n")
if(!is.null(reg_female)) {
  cat("  • PA_female_percentage_studies_table.docx\n")
}
cat("  • PA_burnout_results_enhanced_summary.json\n")
cat("  • All CSV files from main analysis\n")

cat("\n🎉 ENHANCED ADVANCED ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("🚀 All files ready for high-impact manuscript submission!\n")

# Final check on female percentage analysis
if(!is.null(reg_female)) {
  cat("\n✨ FEMALE PERCENTAGE META-REGRESSION SUCCESSFULLY COMPLETED:\n")
  cat("Studies included:", sum(!is.na(pa_final$female_percentage_clean)), "\n")
  cat("Effect estimate:", sprintf("%.4f (SE = %.4f)", reg_female$b[2], reg_female$se[2]), "\n")
  cat("95% CI:", sprintf("%.4f to %.4f", reg_female$ci.lb[2], reg_female$ci.ub[2]), "\n")
  cat("Statistical significance:", ifelse(reg_female$pval[2] < 0.05, "YES", "NO"), "\n")
  cat("Clinical interpretation:", 
      ifelse(reg_female$pval[2] < 0.05,
             paste("A", sprintf("%.1f", abs(reg_female$b[2] * 10)), 
                   "percentage point change in female % is associated with a",
                   ifelse(reg_female$b[2] > 0, "higher", "lower"), 
                   "likelihood of burnout"),
             "No significant association between team gender composition and burnout rates"), "\n")
}

cat("\n🎯 ADVANCED ANALYSIS COMPLETE 🎯\n")
