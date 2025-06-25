# ============================================================================
# BURNOUT AMONG PHYSICIAN ASSISTANTS/ASSOCIATES: FIXED META-ANALYSIS
# Version: 1.0.6 - Clean and Corrected
# Authors: Anna Bock, Alec Knight, Ricardo Twumasi
# Last Updated: June 2025
# ============================================================================

cat("🔥 PHYSICIAN ASSISTANTS/ASSOCIATES BURNOUT META-ANALYSIS\n")
cat("=======================================================\n")
cat("Version 1.0.6 - Clean and Corrected - Optimised Forest Plot\n")
cat("Following methods as specified in manuscript\n\n")

# ============================================================================
# PACKAGE INSTALLATION AND LOADING
# ============================================================================

required_packages <- c("metafor", "tidyverse", "meta", "dplyr", "ggplot2", 
                       "viridis", "scales", "gridExtra", "knitr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    cat("📦 Installed package:", pkg, "\n")
  }
  library(pkg, character.only = TRUE)
}

cat("✅ All required packages loaded successfully!\n\n")

# ============================================================================
# DATA LOADING AND PREPARATION
# ============================================================================

cat("📊 Loading PA burnout meta-analysis data...\n")

# Read the CSV file
pa_data <- read.csv("PA Meta Extraction.csv", stringsAsFactors = FALSE)

cat("✅ Data loaded successfully!\n")

# ============================================================================
# COMPREHENSIVE DATA CLEANING
# ============================================================================

cat("\n=== COMPREHENSIVE DATA CLEANING ===\n")

# Clean data using the same approach that worked in quick test
pa_clean <- pa_data %>%
  mutate(
    # Clean sample size
    sample_size = as.numeric(gsub(",", "", sample_size)),
    
    # Manual fixes for specific studies with data issues (from quick test)
    events = case_when(
      grepl("Bautista", author) ~ 295,  # Use 24.9% of 1186
      grepl("Blackstone", author) ~ round(0.343 * 880),  # Use 34.3% of 880
      !is.na(proportion_count) & grepl("/", proportion_count) ~ 
        as.numeric(str_extract(proportion_count, "^[0-9]+")),
      TRUE ~ round((burnout_prevalence / 100) * sample_size)
    ),
    
    # Clean female percentage
    female_percentage_clean = as.numeric(female_percentage),
    
    # Create study labels
    study_label_clean = paste(gsub(" et al.*", "", author), year),
    
    # Risk of bias categories  
    rob_quality = case_when(
      risk_of_bias >= 7 ~ "Low Risk",
      risk_of_bias >= 4 ~ "Moderate Risk", 
      TRUE ~ "High Risk"
    ),
    
    # MBI binary
    mbi_binary = ifelse(grepl("MBI", measures_used, ignore.case = TRUE), "MBI", "Non-MBI"),
    
    # Calculate burnout proportion
    burnout_proportion = events / sample_size,
    
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
  # Keep only complete cases
  filter(!is.na(sample_size) & !is.na(events) & !is.na(risk_of_bias) &
         burnout_proportion > 0 & burnout_proportion < 1)

# Data validation
cat("Studies after cleaning:", nrow(pa_clean), "\n")
cat("Sample size range:", min(pa_clean$sample_size), "to", 
    format(max(pa_clean$sample_size), big.mark = ","), "\n")
cat("Burnout rate range:", 
    sprintf("%.2f%% to %.2f%%", 
            min(pa_clean$burnout_proportion) * 100,
            max(pa_clean$burnout_proportion) * 100), "\n")
cat("Female % studies:", sum(!is.na(pa_clean$female_percentage_clean)), "\n")

# Display cleaned data
print(pa_clean[, c("study_label_clean", "sample_size", "events", "burnout_proportion", 
                   "female_percentage_clean", "mbi_binary", "risk_of_bias")])

# ============================================================================
# EFFECT SIZE CALCULATION
# ============================================================================

cat("\n=== EFFECT SIZE CALCULATION ===\n")

# Calculate effect sizes
pa_es <- escalc(measure = "PLO", 
                xi = pa_clean$events, 
                ni = pa_clean$sample_size, 
                data = pa_clean,
                slab = pa_clean$study_label_clean)

# Add to main dataframe
pa_final <- pa_clean %>%
  bind_cols(yi = pa_es$yi, vi = pa_es$vi, sei = sqrt(pa_es$vi)) %>%
  mutate(
    prop_est = transf.ilogit(yi),
    ci_lb = transf.ilogit(yi - 1.96 * sei),
    ci_ub = transf.ilogit(yi + 1.96 * sei),
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
res_overall <- rma(yi, vi, data = pa_final, method = "REML", test = "knha")

print(res_overall)

# Calculate overall proportion with prediction intervals
overall_results <- predict(res_overall, transf = transf.ilogit, digits = 3)

cat("\n=== OVERALL BURNOUT PREVALENCE ===\n")
cat("🔥 Pooled Proportion:", sprintf("%.2f%% (95%% CI: %.2f%% to %.2f%%)", 
                                     overall_results$pred * 100, 
                                     overall_results$ci.lb * 100, 
                                     overall_results$ci.ub * 100), "\n")
cat("📊 Prediction Interval:", sprintf("%.2f%% to %.2f%%", 
                                       overall_results$pi.lb * 100, 
                                       overall_results$pi.ub * 100), "\n")

# Store prediction interval values for use in summary
pi_lower <- overall_results$pi.lb * 100
pi_upper <- overall_results$pi.ub * 100

# Heterogeneity
cat("\n=== HETEROGENEITY ===\n")
cat("I² =", sprintf("%.2f%%", res_overall$I2), "\n")
cat("τ² =", sprintf("%.4f", res_overall$tau2), "\n")

# ============================================================================
# ENHANCED FOREST PLOT WITH RISK OF BIAS SCORES - USING WORKING CODE
# ============================================================================

cat("\n=== CREATING ENHANCED FOREST PLOT ===\n")

tryCatch({
  # Calculate weights
  weights_pct <- paste0(formatC(weights(res_overall), format = "f", digits = 1), "%")
  
  # Create enhanced forest plot using your working code structure
  pdf("PA_forest_plot_main_with_rob_scores.pdf", width = 14, height = 10)
  
  # Create enhanced forest plot with risk of bias
  forest_plot <- forest(res_overall,
                        transf = transf.ilogit,
                        at = c(0, 2, 0.4, 0.6, 0.8, 1.0),
                        xlim = c(-4.2, 3.0),
                        alim = c(-0, 1),
                        xlab = "Proportion of Burnout in Physician Assistants/Associates",
                        header = c("Study", "Proportion [95% CI]"),
                        mlab = paste("Random Effects Model (I² = ", sprintf("%.2f%%", res_overall$I2), ")", sep = ""),
                        ilab = cbind(weights_pct,
                                     pa_final$risk_of_bias,
                                     ifelse(is.na(pa_final$female_percentage_clean), 
                                            "N/A", 
                                            paste0(pa_final$female_percentage_clean, "%"))),
                        ilab.xpos = c(-2.8, -2.3, -1.8),
                        ilab.pos = 2,
                        cex = .9,
                        refline = 0.33)
  
  # Add column headers
  text(c(-2.8, -2.3, -1.8), 
       res_overall$k + 2, 
       c("Weight", "RoB", "Female %"), 
       pos = 2, cex = 0.8, font = 2)
  
  # Add RoB legend at bottom
  mtext("Risk of Bias (Newcastle-Ottawa Scale): 7-9 Low Risk | 4-6 Moderate Risk | 0-3 High Risk", 
        side = 1, line = 4, cex = 0.7)
  
  dev.off()
  cat("✅ Enhanced forest plot created successfully using working code structure\n")
  
}, error = function(e) {
  cat("❌ Error creating forest plot:", e$message, "\n")
  dev.off()
})

# ============================================================================
# PUBLICATION BIAS ASSESSMENT
# ============================================================================

cat("\n=== PUBLICATION BIAS ASSESSMENT ===\n")

tryCatch({
  # Tests
  egger_test <- regtest(res_overall, model = "lm")
  rank_test <- ranktest(res_overall)
  trim_fill <- trimfill(res_overall)
  studies_trimmed <- max(0, trim_fill$k - res_overall$k)
  
  cat("Egger's test p-value:", sprintf("%.2f", egger_test$pval), "\n")
  cat("Begg's test p-value:", sprintf("%.2f", rank_test$pval), "\n")
  cat("Trim-and-fill missing studies:", studies_trimmed, "\n")
  
  # Create plots
  pdf("PA_publication_bias_assessment.pdf", width = 12, height = 8)
  par(mfrow = c(2, 2))
  
  funnel(res_overall, main = "Funnel Plot")
  funnel(res_overall, level = c(90, 95, 99), shade = c("white", "gray75", "gray50"),
         main = "Contour-Enhanced Funnel Plot")
  funnel(trim_fill, main = "Trim-and-Fill Analysis")
  
  # Summary
  plot.new()
  text(0.5, 0.8, "Publication Bias Summary", cex = 1.2, font = 2)
  text(0.1, 0.6, paste("Egger's test: p =", sprintf("%.2f", egger_test$pval)), adj = 0)
  text(0.1, 0.5, paste("Begg's test: p =", sprintf("%.2f", rank_test$pval)), adj = 0)
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

# Function for safe subgroup analysis
perform_subgroup_safe <- function(data, var, var_name) {
  cat("\n--- Subgroup Analysis by", var_name, "---\n")
  
  tryCatch({
    data[[var]] <- factor(data[[var]])
    data <- data[!is.na(data[[var]]), ]
    
    if(nrow(data) < 3) {
      cat("Insufficient data\n")
      return(NULL)
    }
    
    formula_str <- paste("~", var)
    sub_model <- rma(yi, vi, mods = as.formula(formula_str), 
                     data = data, method = "REML", test = "knha")
    
    print(sub_model)
    
    # Calculate subgroup estimates
    levels <- levels(data[[var]])
    for(level in levels) {
      subset_data <- data[data[[var]] == level, ]
      if(nrow(subset_data) > 1) {
        sub_res <- rma(yi, vi, data = subset_data, method = "REML", test = "knha")
        sub_est <- predict(sub_res, transf = transf.ilogit)
        
        cat(sprintf("%s (k=%d): %.2f%% (95%% CI: %.2f%% to %.2f%%)\n", 
                    level, nrow(subset_data),
                    sub_est$pred * 100, 
                    sub_est$ci.lb * 100, 
                    sub_est$ci.ub * 100))
      }
    }
    
    return(sub_model)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# Perform subgroup analyses
mbi_subgroup <- perform_subgroup_safe(pa_final, "mbi_binary", "Burnout Measurement Tool")
quality_subgroup <- perform_subgroup_safe(pa_final, "rob_quality", "Study Quality")

# ============================================================================
# META-REGRESSION ANALYSES
# ============================================================================

cat("\n=== META-REGRESSION ANALYSES ===\n")

# Function for safe meta-regression
perform_metareg_safe <- function(data, predictor, pred_name) {
  cat("\n--- Meta-Regression:", pred_name, "---\n")
  
  tryCatch({
    data_subset <- data[!is.na(data[[predictor]]), ]
    
    if(nrow(data_subset) < 3) {
      cat("Insufficient data\n")
      return(NULL)
    }
    
    formula_str <- paste("~", predictor)
    meta_reg <- rma(yi, vi, mods = as.formula(formula_str), 
                    data = data_subset, method = "REML", test = "knha")
    
    print(meta_reg)
    
    if(!is.null(meta_reg$R2)) {
      cat("R² =", sprintf("%.2f%%", meta_reg$R2), "\n")
    }
    
    if(length(meta_reg$pval) > 1) {
      p_val <- meta_reg$pval[2]
      cat("Significance:", ifelse(p_val < 0.05, "YES", "NO"), 
          "(p =", sprintf("%.2f", p_val), ")\n")
    }
    
    return(meta_reg)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

# Perform meta-regressions
reg_year <- perform_metareg_safe(pa_final, "year", "Publication Year")
reg_size <- perform_metareg_safe(pa_final, "sample_size", "Sample Size")
reg_rob <- perform_metareg_safe(pa_final, "risk_of_bias", "Risk of Bias Score")

# FEMALE PERCENTAGE META-REGRESSION (KEY ANALYSIS) - FIXED
reg_female <- perform_metareg_safe(pa_final, "female_percentage_clean", "Female Percentage")

if(!is.null(reg_female)) {
  cat("\n🚀 FEMALE PERCENTAGE META-REGRESSION RESULTS:\n")
  cat("Studies included:", sum(!is.na(pa_final$female_percentage_clean)), "\n")
  cat("Female % range:", sprintf("%.2f%% to %.2f%%", 
                               min(pa_final$female_percentage_clean, na.rm = TRUE),
                               max(pa_final$female_percentage_clean, na.rm = TRUE)), "\n")
  cat("Coefficient:", sprintf("%.4f", reg_female$b[2]), "\n")
  cat("P-value:", sprintf("%.2f", reg_female$pval[2]), "\n")
  cat("R²:", sprintf("%.2f%%", reg_female$R2), "\n")
  cat("Significant:", ifelse(reg_female$pval[2] < 0.05, "YES", "NO"), "\n")
  
  # FIXED: Create regression plot without cex conflicts
  tryCatch({
    pdf("PA_female_percentage_regression.pdf", width = 10, height = 8)
    
    female_data <- pa_final[!is.na(pa_final$female_percentage_clean), ]
    
    # FIXED: Use regplot without conflicting cex parameters
    regplot(reg_female, 
            xlab = "Female Percentage (%)", 
            ylab = "Logit Proportion of Burnout",
            main = "Meta-Regression: Female Percentage vs Burnout Rate",
            pch = 19, 
            col = "darkblue", 
            ci = TRUE, 
            shade = TRUE)  # REMOVED: cex parameter that was causing conflicts
    
    # Add study labels with specific cex
    text(female_data$female_percentage_clean, female_data$yi, 
         labels = female_data$study_label_clean, pos = 3, cex = 0.6)
    
    # Add correlation info
    cor_val <- cor(female_data$female_percentage_clean, female_data$yi, use = "complete.obs")
    text(min(female_data$female_percentage_clean), max(female_data$yi),
         paste("r =", sprintf("%.2f", cor_val)), pos = 4, cex = 1, font = 2)
    
    dev.off()
    cat("✅ Female percentage regression plot created successfully\n")
    
  }, error = function(e) {
    cat("❌ Error creating regression plot:", e$message, "\n")
    dev.off()
  })
}

# ============================================================================
# SENSITIVITY ANALYSIS
# ============================================================================

cat("\n=== SENSITIVITY ANALYSIS ===\n")

tryCatch({
  loo_results <- leave1out(res_overall)
  
  original_est <- transf.ilogit(res_overall$b[1]) * 100
  loo_ests <- transf.ilogit(loo_results$estimate) * 100
  changes <- abs(loo_ests - original_est)
  
  max_change_idx <- which.max(changes)
  cat("Most influential study:", pa_final$study_label_clean[max_change_idx], "\n")
  cat("Change in estimate:", sprintf("%.2f%%", changes[max_change_idx]), "\n")
  
  # Save results
  sensitivity_table <- data.frame(
    Study_Removed = pa_final$study_label_clean,
    Estimate_Without = sprintf("%.2f%%", loo_ests),
    Change = sprintf("%.2f%%", changes)
  )
  
  write.csv(sensitivity_table, "PA_sensitivity_analysis.csv", row.names = FALSE)
  cat("✅ Sensitivity analysis completed\n")
  
}, error = function(e) {
  cat("❌ Error in sensitivity analysis:", e$message, "\n")
})

# ============================================================================
# INFLUENCE DIAGNOSTICS (SAFE)
# ============================================================================

cat("\n=== INFLUENCE DIAGNOSTICS ===\n")

tryCatch({
  inf <- influence(res_overall)
  
  if(!is.null(inf$cook.d) && is.numeric(inf$cook.d)) {
    influence_table <- data.frame(
      Study = pa_final$study_label_clean,
      Cooks_D = round(inf$cook.d, 3),
      Hat = round(inf$hat, 3),
      Weight = round(weights(res_overall), 1)
    )
    
    influence_table <- influence_table[order(-influence_table$Cooks_D), ]
    cat("Top 3 most influential studies:\n")
    print(head(influence_table, 3))
    
    write.csv(influence_table, "PA_influence_analysis.csv", row.names = FALSE)
    cat("✅ Influence analysis completed\n")
  } else {
    cat("⚠️ Cook's distance not available, using hat values\n")
    influence_table <- data.frame(
      Study = pa_final$study_label_clean,
      Hat = round(inf$hat, 3),
      Weight = round(weights(res_overall), 1)
    )
    print(head(influence_table[order(-influence_table$Hat), ], 3))
  }
  
}, error = function(e) {
  cat("⚠️ Influence diagnostics skipped:", e$message, "\n")
})

# ============================================================================
# EXPORT RESULTS
# ============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Summary statistics
summary_stats <- data.frame(
  Statistic = c("Number of Studies", "Total Participants", "Pooled Prevalence", 
                "95% CI Lower", "95% CI Upper", "Prediction Interval Lower", "Prediction Interval Upper",
                "I² Heterogeneity", "τ² Heterogeneity",
                "Studies with Female %", "Female % Range", "Egger's Test p-value"),
  Value = c(res_overall$k,
            format(sum(pa_final$sample_size), big.mark = ","),
            sprintf("%.2f%%", overall_results$pred * 100),
            sprintf("%.2f%%", overall_results$ci.lb * 100),
            sprintf("%.2f%%", overall_results$ci.ub * 100),
            sprintf("%.2f%%", pi_lower),
            sprintf("%.2f%%", pi_upper),
            sprintf("%.2f%%", res_overall$I2),
            sprintf("%.4f", res_overall$tau2),
            sum(!is.na(pa_final$female_percentage_clean)),
            sprintf("%.2f%%-%.2f%%", 
                    min(pa_final$female_percentage_clean, na.rm = TRUE),
                    max(pa_final$female_percentage_clean, na.rm = TRUE)),
            if(exists("egger_test")) sprintf("%.2f", egger_test$pval) else "N/A")
)

print(summary_stats)

# Save datasets and results
write.csv(pa_final, "PA_burnout_final_dataset.csv", row.names = FALSE)
write.csv(summary_stats, "PA_burnout_summary_stats.csv", row.names = FALSE)
save.image("PA_burnout_meta_analysis_workspace.RData")

cat("\n🎉 ENHANCED ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("\n📁 FILES CREATED:\n")
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

cat("\n🔑 KEY FINDINGS:\n")
cat("• Pooled burnout prevalence:", sprintf("%.2f%% (95%% CI: %.2f%% to %.2f%%)", 
                                            overall_results$pred * 100,
                                            overall_results$ci.lb * 100,
                                            overall_results$ci.ub * 100), "\n")
cat("• Heterogeneity: I² =", sprintf("%.2f%%", res_overall$I2), "\n")

if(!is.null(reg_female)) {
  cat("• Female percentage analysis:\n")
  cat("  - Studies included:", sum(!is.na(pa_final$female_percentage_clean)), "\n")
  cat("  - Coefficient:", sprintf("%.4f", reg_female$b[2]), "\n")
  cat("  - Significance:", ifelse(reg_female$pval[2] < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), 
      "(p =", sprintf("%.2f", reg_female$pval[2]), ")\n")
  cat("  - R²:", sprintf("%.2f%%", reg_female$R2), "\n")
}

if(!is.null(mbi_subgroup)) {
  cat("• MBI vs Non-MBI difference:", ifelse(mbi_subgroup$QMp < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), 
      "(p =", sprintf("%.2f", mbi_subgroup$QMp), ")\n")
}

if(exists("egger_test")) {
  cat("• Publication bias:", ifelse(egger_test$pval < 0.05, "DETECTED", "NOT DETECTED"), 
      "(Egger's p =", sprintf("%.2f", egger_test$pval), ")\n")
}

cat("\n✅ Analysis ready for manuscript preparation!\n")
cat("🎯 Enhanced forest plot includes numerical RoB scores and female percentages\n")
cat("📊 Female percentage meta-regression completed with visualization\n")
cat("🚀 All outputs ready!\n")
