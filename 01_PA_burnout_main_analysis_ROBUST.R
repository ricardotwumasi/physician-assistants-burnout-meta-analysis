# Create study labels for forest plots
    study_label = paste(author, year, sep = ", "),
    
    # Clean author names (remove "et al." for cleaner labels)
    clean_authors = gsub(" et al\\.?\\s*$", "", author),
    study_label_clean = paste(clean_authors, year, sep = " "),
    
    # Create risk of bias categories
    rob_quality = case_when(
      risk_of_bias >= 7 ~ "Low Risk",
      risk_of_bias >= 4 ~ "Moderate Risk",
      risk_of_bias < 4 ~ "High Risk",
      TRUE ~ "Unknown"
    ),
    
    # Determine if MBI was used (more robust detection)
    mbi_binary = case_when(
      grepl("MBI", measures_used, ignore.case = TRUE) ~ "MBI",
      grepl("Maslach", measures_used, ignore.case = TRUE) ~ "MBI",
      TRUE ~ "Non-MBI"
    ),
    
    # Create specialty grouping
    specialty_group = case_when(
      specialty == "Any" ~ "General",
      grepl("Any", specialty) ~ "General",
      specialty %in% c("Oncology", "Primary Care", "Dermatology", 
                       "Orthopedics", "Plastic Surgery", "Psychiatry", 
                       "Rheumatology") ~ specialty,
      TRUE ~ "Other"
    )
  ) %>%
  # Remove any rows with missing critical data
  filter(!is.na(sample_size) & !is.na(burnout_proportion) & 
         !is.na(events) & !is.na(risk_of_bias) & 
         burnout_proportion > 0 & burnout_proportion < 1)

# Calculate total participants
total_participants <- sum(pa_clean$sample_size, na.rm = TRUE)
cat("📈 Dataset contains:", nrow(pa_clean), "studies with", 
    format(total_participants, big.mark = ","), 
    "total participants\n")

# Data validation
cat("\n=== DATA VALIDATION ===\n")
cat("Studies retained after cleaning:", nrow(pa_clean), "\n")
cat("Burnout proportions range:", 
    sprintf("%.1f%%", min(pa_clean$burnout_proportion * 100)), "to",
    sprintf("%.1f%%", max(pa_clean$burnout_proportion * 100)), "\n")
cat("Sample sizes range:", min(pa_clean$sample_size), "to", 
    format(max(pa_clean$sample_size), big.mark = ","), "\n")

# Check for female percentage data
gender_available <- sum(!is.na(pa_clean$female_percentage_clean))
cat("Studies with female percentage data:", gender_available, "out of", nrow(pa_clean), "\n")

if(gender_available > 0) {
  cat("Female percentage range:", 
      sprintf("%.1f%%", min(pa_clean$female_percentage_clean, na.rm = TRUE)), "to",
      sprintf("%.1f%%", max(pa_clean$female_percentage_clean, na.rm = TRUE)), "\n")
}

# Display cleaned data summary
cat("\n=== CLEANED DATA SUMMARY ===\n")
summary_table <- pa_clean %>%
  select(study_label_clean, year, sample_size, events, burnout_proportion, 
         female_percentage_clean, mbi_binary, risk_of_bias) %>%
  mutate(
    burnout_percent = sprintf("%.1f%%", burnout_proportion * 100),
    female_percent = ifelse(is.na(female_percentage_clean), "Missing", 
                           paste0(female_percentage_clean, "%"))
  )

print(summary_table[, c("study_label_clean", "year", "sample_size", "events", 
                        "burnout_percent", "female_percent", "mbi_binary", "risk_of_bias")])

# ============================================================================
# EFFECT SIZE CALCULATION
# ============================================================================

cat("\n=== EFFECT SIZE CALCULATION ===\n")

# Calculate effect sizes using escalc with proper error handling
tryCatch({
  pa_es <- escalc(
    measure = "PLO",                    # Proportion logit transformation
    xi = pa_clean$events,               # Number of events
    ni = pa_clean$sample_size,          # Sample size
    data = pa_clean,
    slab = pa_clean$study_label_clean,
    digits = 4
  )
  
  cat("✅ Effect size calculation successful\n")
}, error = function(e) {
  cat("❌ Error in effect size calculation:", e$message, "\n")
  stop("Effect size calculation failed")
})

# Add effect size data to main dataframe
pa_final <- pa_clean %>%
  bind_cols(
    yi = pa_es$yi,                    
    vi = pa_es$vi,                    
    sei = sqrt(pa_es$vi)              
  ) %>%
  mutate(
    # Back-transform logit to proportion
    prop_est = transf.ilogit(yi),
    # Calculate 95% CI bounds
    ci_lb = transf.ilogit(yi - 1.96 * sei),
    ci_ub = transf.ilogit(yi + 1.96 * sei),
    # Compute SD using escalc results
    sd_computed = sqrt(vi * sample_size)
  )

cat("Effect size calculations completed for", nrow(pa_final), "studies\n")

# ============================================================================
# MAIN RANDOM EFFECTS META-ANALYSIS
# ============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("MAIN RANDOM EFFECTS META-ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Fit random-effects model
res_overall <- rma(yi = yi, 
                   vi = vi, 
                   data = pa_final, 
                   method = "REML",
                   test = "knha")

print(res_overall)

# Calculate overall proportion
overall_results <- predict(res_overall, transf = transf.ilogit, digits = 3)

cat("\n=== OVERALL BURNOUT PREVALENCE ===\n")
cat("🔥 Pooled Proportion:", sprintf("%.1f%% (95%% CI: %.1f%% to %.1f%%)", 
                                     overall_results$pred * 100, 
                                     overall_results$ci.lb * 100, 
                                     overall_results$ci.ub * 100), "\n")

# ============================================================================
# ENHANCED FOREST PLOT WITH ROBUST ERROR HANDLING
# ============================================================================

cat("\n=== CREATING ENHANCED FOREST PLOT ===\n")

tryCatch({
  # Calculate weights for display
  weights_pct <- paste0(formatC(weights(res_overall), format = "f", digits = 1), "%")
  
  # Create enhanced forest plot
  pdf("PA_forest_plot_main_with_rob_scores.pdf", width = 16, height = 12)
  
  par(mar = c(10, 0, 4, 2))
  
  # Create forest plot with enhanced information
  forest(res_overall,
         transf = transf.ilogit,
         at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
         xlim = c(-5.5, 3),
         alim = c(0, 1),
         xlab = "Proportion of Burnout in Physician Assistants/Associates",
         header = c("Study", "Proportion [95% CI]"),
         mlab = paste("Random Effects Model (I² = ", sprintf("%.1f%%", res_overall$I2), ")", sep = ""),
         ilab = cbind(pa_final$events, 
                      pa_final$sample_size,
                      weights_pct,
                      pa_final$risk_of_bias,
                      ifelse(is.na(pa_final$female_percentage_clean), 
                             "N/A", 
                             paste0(pa_final$female_percentage_clean, "%"))),
         ilab.xpos = c(-4.8, -4.2, -3.6, -3.0, -2.2),
         ilab.pos = 2,
         cex = 0.75)
  
  # Add column headers
  text(c(-4.8, -4.2, -3.6, -3.0, -2.2), 
       res_overall$k + 2, 
       c("Events", "Total", "Weight", "RoB", "Female %"), 
       pos = 2, cex = 0.75, font = 2)
  
  # Add RoB legend
  text(-5.2, res_overall$k + 4, "Risk of Bias (NOS):", pos = 4, cex = 0.7, font = 2)
  text(-5.2, res_overall$k + 3.5, "7-9: Low Risk", pos = 4, cex = 0.65)
  text(-5.2, res_overall$k + 3, "4-6: Moderate Risk", pos = 4, cex = 0.65)
  
  dev.off()
  cat("✅ Forest plot created successfully\n")
  
}, error = function(e) {
  cat("❌ Error creating forest plot:", e$message, "\n")
  dev.off()  # Close any open graphics device
})

# ============================================================================
# PUBLICATION BIAS ASSESSMENT
# ============================================================================

cat("\n=== PUBLICATION BIAS ASSESSMENT ===\n")

tryCatch({
  # Egger's test
  egger_test <- regtest(res_overall, model = "lm")
  cat("Egger's test p-value:", sprintf("%.3f", egger_test$pval), "\n")
  
  # Trim-and-fill
  trim_fill <- trimfill(res_overall)
  studies_trimmed <- max(0, trim_fill$k - res_overall$k)
  cat("Trim-and-fill estimated missing studies:", studies_trimmed, "\n")
  
  # Create publication bias plots
  pdf("PA_publication_bias_assessment.pdf", width = 12, height = 8)
  par(mfrow = c(2, 2))
  
  funnel(res_overall, main = "Funnel Plot")
  funnel(res_overall, level = c(90, 95, 99), shade = c("white", "gray75", "gray50"),
         main = "Contour-Enhanced Funnel Plot")
  funnel(trim_fill, main = "Trim-and-Fill Analysis")
  
  # Add a summary plot
  plot.new()
  text(0.5, 0.7, "Publication Bias Summary", cex = 1.5, font = 2)
  text(0.1, 0.5, paste("Egger's test: p =", sprintf("%.3f", egger_test$pval)), adj = 0)
  text(0.1, 0.4, paste("Missing studies:", studies_trimmed), adj = 0)
  
  dev.off()
  cat("✅ Publication bias plots created\n")
  
}, error = function(e) {
  cat("❌ Error in publication bias assessment:", e$message, "\n")
  dev.off()
})

# ============================================================================
# SUBGROUP ANALYSES
# ============================================================================

cat("\n=== SUBGROUP ANALYSES ===\n")

# Function for robust subgroup analysis
perform_subgroup_safe <- function(data, var, var_name) {
  cat("\n--- Subgroup Analysis by", var_name, "---\n")
  
  tryCatch({
    # Convert to factor and remove any missing levels
    data[[var]] <- factor(data[[var]])
    data <- data[!is.na(data[[var]]), ]
    
    if(nrow(data) < 3) {
      cat("Insufficient data for subgroup analysis\n")
      return(NULL)
    }
    
    # Fit mixed-effects model
    formula_str <- paste("~", var)
    sub_model <- rma(yi, vi, mods = as.formula(formula_str), 
                     data = data, method = "REML", test = "knha")
    
    print(sub_model)
    
    # Calculate estimates for each subgroup
    levels <- levels(data[[var]])
    for(level in levels) {
      subset_data <- data[data[[var]] == level, ]
      if(nrow(subset_data) > 1) {
        sub_res <- rma(yi, vi, data = subset_data, method = "REML", test = "knha")
        sub_est <- predict(sub_res, transf = transf.ilogit)
        
        cat(sprintf("%s (k=%d): %.1f%% (95%% CI: %.1f%% to %.1f%%)\n", 
                    level, nrow(subset_data),
                    sub_est$pred * 100, 
                    sub_est$ci.lb * 100, 
                    sub_est$ci.ub * 100))
      }
    }
    
    return(sub_model)
    
  }, error = function(e) {
    cat("Error in subgroup analysis:", e$message, "\n")
    return(NULL)
  })
}

# Perform subgroup analyses
mbi_subgroup <- perform_subgroup_safe(pa_final, "mbi_binary", "Burnout Measurement Tool")
quality_subgroup <- perform_subgroup_safe(pa_final, "rob_quality", "Study Quality")

# ============================================================================
# META-REGRESSION INCLUDING FEMALE PERCENTAGE
# ============================================================================

cat("\n=== META-REGRESSION ANALYSES ===\n")

# Function for safe meta-regression
perform_metareg_safe <- function(data, predictor, pred_name) {
  cat("\n--- Meta-Regression:", pred_name, "---\n")
  
  tryCatch({
    # Remove missing data
    data_subset <- data[!is.na(data[[predictor]]), ]
    
    if(nrow(data_subset) < 3) {
      cat("Insufficient data for", pred_name, "meta-regression\n")
      return(NULL)
    }
    
    formula_str <- paste("~", predictor)
    meta_reg <- rma(yi, vi, mods = as.formula(formula_str), 
                    data = data_subset, method = "REML", test = "knha")
    
    print(meta_reg)
    
    if(!is.null(meta_reg$R2)) {
      cat("R² =", sprintf("%.1f%%", meta_reg$R2), "\n")
    }
    
    if(length(meta_reg$pval) > 1) {
      p_val <- meta_reg$pval[2]
      cat("Significance:", ifelse(p_val < 0.05, "YES", "NO"), 
          "(p =", sprintf("%.3f", p_val), ")\n")
    }
    
    return(meta_reg)
    
  }, error = function(e) {
    cat("Error in meta-regression:", e$message, "\n")
    return(NULL)
  })
}

# Perform meta-regressions
reg_year <- perform_metareg_safe(pa_final, "year", "Publication Year")
reg_size <- perform_metareg_safe(pa_final, "sample_size", "Sample Size")
reg_rob <- perform_metareg_safe(pa_final, "risk_of_bias", "Risk of Bias Score")

# Female percentage meta-regression (key analysis)
reg_female <- perform_metareg_safe(pa_final, "female_percentage_clean", "Female Percentage")

if(!is.null(reg_female)) {
  cat("\n🚀 FEMALE PERCENTAGE META-REGRESSION RESULTS:\n")
  cat("Studies included:", sum(!is.na(pa_final$female_percentage_clean)), "\n")
  cat("Coefficient:", sprintf("%.4f", reg_female$b[2]), "\n")
  cat("P-value:", sprintf("%.3f", reg_female$pval[2]), "\n")
  cat("Significant:", ifelse(reg_female$pval[2] < 0.05, "YES", "NO"), "\n")
  
  # Create regression plot
  tryCatch({
    pdf("PA_female_percentage_regression.pdf", width = 10, height = 8)
    
    female_data <- pa_final[!is.na(pa_final$female_percentage_clean), ]
    
    regplot(reg_female, 
            xlab = "Female Percentage (%)", 
            ylab = "Logit Proportion of Burnout",
            main = "Meta-Regression: Female Percentage vs Burnout Rate",
            pch = 19, col = "darkblue", cex = 1.2)
    
    # Add study labels
    text(female_data$female_percentage_clean, female_data$yi, 
         labels = female_data$study_label_clean, pos = 3, cex = 0.6)
    
    dev.off()
    cat("✅ Female percentage regression plot created\n")
    
  }, error = function(e) {
    cat("❌ Error creating regression plot:", e$message, "\n")
    dev.off()
  })
}

# ============================================================================
# SENSITIVITY ANALYSIS WITH ROBUST ERROR HANDLING
# ============================================================================

cat("\n=== SENSITIVITY ANALYSIS ===\n")

tryCatch({
  # Leave-one-out analysis
  loo_results <- leave1out(res_overall)
  
  # Find most influential study
  original_est <- transf.ilogit(res_overall$b[1]) * 100
  loo_ests <- transf.ilogit(loo_results$estimate) * 100
  changes <- abs(loo_ests - original_est)
  
  max_change_idx <- which.max(changes)
  cat("Most influential study:", pa_final$study_label_clean[max_change_idx], "\n")
  cat("Change in estimate:", sprintf("%.2f%%", changes[max_change_idx]), "\n")
  
  # Save sensitivity results
  sensitivity_table <- data.frame(
    Study_Removed = pa_final$study_label_clean,
    Estimate_Without = sprintf("%.1f%%", loo_ests),
    Change = sprintf("%.2f%%", changes)
  )
  
  write.csv(sensitivity_table, "PA_sensitivity_analysis.csv", row.names = FALSE)
  cat("✅ Sensitivity analysis completed\n")
  
}, error = function(e) {
  cat("❌ Error in sensitivity analysis:", e$message, "\n")
})

# ============================================================================
# INFLUENCE DIAGNOSTICS WITH SAFE ERROR HANDLING
# ============================================================================

cat("\n=== INFLUENCE DIAGNOSTICS ===\n")

tryCatch({
  # Calculate influence measures
  inf <- influence(res_overall)
  
  # Check if Cook's distance is available
  if(!is.null(inf$cook.d) && is.numeric(inf$cook.d)) {
    cooks_d <- inf$cook.d
    
    influence_table <- data.frame(
      Study = pa_final$study_label_clean,
      Cooks_D = round(cooks_d, 3),
      Hat = round(inf$hat, 3),
      Weight = round(weights(res_overall), 1)
    )
    
    # Sort by Cook's D
    influence_table <- influence_table[order(-influence_table$Cooks_D), ]
    
    cat("Top 3 most influential studies:\n")
    print(head(influence_table, 3))
    
    write.csv(influence_table, "PA_influence_analysis.csv", row.names = FALSE)
    cat("✅ Influence analysis completed\n")
    
  } else {
    cat("⚠️ Cook's distance not available, using alternative influence measures\n")
    
    # Alternative: use hat values and dfbetas
    influence_table <- data.frame(
      Study = pa_final$study_label_clean,
      Hat = round(inf$hat, 3),
      Weight = round(weights(res_overall), 1)
    )
    
    print(head(influence_table[order(-influence_table$Hat), ], 3))
  }
  
}, error = function(e) {
  cat("⚠️ Influence diagnostics not available:", e$message, "\n")
  cat("This is often due to software version compatibility issues\n")
})

# ============================================================================
# SUMMARY AND EXPORT
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Create summary statistics
summary_stats <- data.frame(
  Statistic = c("Number of Studies", "Total Participants", "Pooled Prevalence", 
                "95% CI Lower", "95% CI Upper", "I² Heterogeneity", 
                "Studies with Female %", "Egger's Test p-value"),
  Value = c(res_overall$k,
            format(sum(pa_final$sample_size), big.mark = ","),
            sprintf("%.1f%%", overall_results$pred * 100),
            sprintf("%.1f%%", overall_results$ci.lb * 100),
            sprintf("%.1f%%", overall_results$ci.ub * 100),
            sprintf("%.1f%%", res_overall$I2),
            sum(!is.na(pa_final$female_percentage_clean)),
            if(exists("egger_test")) sprintf("%.3f", egger_test$pval) else "N/A")
)

print(summary_stats)

# Save final dataset and results
write.csv(pa_final, "PA_burnout_final_dataset.csv", row.names = FALSE)
write.csv(summary_stats, "PA_burnout_summary_stats.csv", row.names = FALSE)
save.image("PA_burnout_meta_analysis_workspace.RData")

cat("\n🎉 ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("📁 Files created:\n")
cat("  • PA_forest_plot_main_with_rob_scores.pdf\n")
cat("  • PA_publication_bias_assessment.pdf\n")
if(!is.null(reg_female)) {
  cat("  • PA_female_percentage_regression.pdf\n")
}
cat("  • PA_sensitivity_analysis.csv\n")
cat("  • PA_influence_analysis.csv (if available)\n")
cat("  • PA_burnout_final_dataset.csv\n")
cat("  • PA_burnout_summary_stats.csv\n")
cat("  • PA_burnout_meta_analysis_workspace.RData\n")

# Print key findings
cat("\n🔑 KEY FINDINGS:\n")
cat("• Pooled burnout prevalence:", sprintf("%.1f%% (95%% CI: %.1f%% to %.1f%%)", 
                                            overall_results$pred * 100,
                                            overall_results$ci.lb * 100,
                                            overall_results$ci.ub * 100), "\n")
cat("• Heterogeneity: I² =", sprintf("%.1f%%", res_overall$I2), "\n")

if(!is.null(reg_female)) {
  cat("• Female percentage analysis: Studies =", sum(!is.na(pa_final$female_percentage_clean)), "\n")
  cat("• Female % significance:", ifelse(reg_female$pval[2] < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), 
      "(p =", sprintf("%.3f", reg_female$pval[2]), ")\n")
}

if(!is.null(mbi_subgroup)) {
  cat("• MBI vs Non-MBI difference:", ifelse(mbi_subgroup$QMp < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), 
      "(p =", sprintf("%.3f", mbi_subgroup$QMp), ")\n")
}

cat("\n✅ Analysis ready for manuscript preparation!\n")
