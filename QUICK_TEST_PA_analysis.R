# ============================================================================
# QUICK FIX PA BURNOUT META-ANALYSIS - IMMEDIATE TESTING
# ============================================================================

cat("🔧 QUICK FIX PA BURNOUT ANALYSIS\n")
cat("================================\n\n")

# Load packages
library(metafor)
library(dplyr)

# Read data
pa_data <- read.csv("PA Meta Extraction.csv", stringsAsFactors = FALSE)

# Quick data cleaning with manual fixes for problematic rows
pa_clean <- pa_data %>%
  mutate(
    # Clean sample size
    sample_size = as.numeric(gsub(",", "", sample_size)),
    
    # Manual fixes for specific studies with data issues
    events = case_when(
      # Bautista: proportion_count says 286/1186, but denominator matches sample_size
      grepl("Bautista", author) ~ 295,  # Use 24.9% of 1186
      
      # Blackstone: proportion_count says 295/860, but sample_size is 880
      grepl("Blackstone", author) ~ round(0.343 * 880),  # Use 34.3% of 880
      
      # For studies with proportion_count, extract numerator
      !is.na(proportion_count) & grepl("/", proportion_count) ~ 
        as.numeric(str_extract(proportion_count, "^[0-9]+")),
      
      # For studies with only burnout_prevalence, calculate events
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
    mbi_binary = ifelse(grepl("MBI", measures_used, ignore.case = TRUE), "MBI", "Non-MBI")
  ) %>%
  # Keep only complete cases
  filter(!is.na(sample_size) & !is.na(events) & !is.na(risk_of_bias))

# Quick validation
cat("Studies after cleaning:", nrow(pa_clean), "\n")
cat("Sample size range:", min(pa_clean$sample_size), "to", max(pa_clean$sample_size), "\n")
cat("Events range:", min(pa_clean$events), "to", max(pa_clean$events), "\n")

# Calculate proportions
pa_clean$burnout_proportion <- pa_clean$events / pa_clean$sample_size

cat("Burnout rate range:", 
    sprintf("%.1f%% to %.1f%%", 
            min(pa_clean$burnout_proportion) * 100,
            max(pa_clean$burnout_proportion) * 100), "\n")

# Calculate effect sizes
pa_es <- escalc(measure = "PLO", 
                xi = pa_clean$events, 
                ni = pa_clean$sample_size, 
                data = pa_clean)

pa_final <- pa_clean %>%
  bind_cols(yi = pa_es$yi, vi = pa_es$vi, sei = sqrt(pa_es$vi))

# Main meta-analysis
res_overall <- rma(yi, vi, data = pa_final, method = "REML", test = "knha")

# Results
overall_results <- predict(res_overall, transf = transf.ilogit)

cat("\n🔥 RESULTS:\n")
cat("Pooled prevalence:", sprintf("%.1f%% (95%% CI: %.1f%% to %.1f%%)", 
                                 overall_results$pred * 100, 
                                 overall_results$ci.lb * 100, 
                                 overall_results$ci.ub * 100), "\n")
cat("I² =", sprintf("%.1f%%", res_overall$I2), "\n")

# Create basic forest plot
cat("\n📊 Creating forest plot...\n")
pdf("PA_forest_plot_QUICK.pdf", width = 14, height = 10)

par(mar = c(8, 0, 4, 2))

forest(res_overall,
       transf = transf.ilogit,
       xlim = c(-4, 2.5),
       xlab = "Proportion of Burnout",
       header = c("Study", "Proportion [95% CI]"),
       ilab = cbind(pa_final$events, 
                    pa_final$sample_size, 
                    pa_final$risk_of_bias,
                    ifelse(is.na(pa_final$female_percentage_clean), 
                           "N/A", 
                           paste0(pa_final$female_percentage_clean, "%"))),
       ilab.xpos = c(-3.2, -2.6, -2.0, -1.4),
       ilab.pos = 2,
       cex = 0.7)

text(c(-3.2, -2.6, -2.0, -1.4), res_overall$k + 2, 
     c("Events", "Total", "RoB", "Female%"), pos = 2, cex = 0.7, font = 2)

dev.off()
cat("✅ Forest plot saved as PA_forest_plot_QUICK.pdf\n")

# Female percentage meta-regression
if(sum(!is.na(pa_final$female_percentage_clean)) >= 3) {
  cat("\n🚺 Female percentage meta-regression...\n")
  reg_female <- rma(yi, vi, mods = ~ female_percentage_clean, 
                    data = pa_final, method = "REML", test = "knha")
  
  cat("Coefficient:", sprintf("%.4f", reg_female$b[2]), "\n")
  cat("P-value:", sprintf("%.3f", reg_female$pval[2]), "\n")
  cat("Significant:", ifelse(reg_female$pval[2] < 0.05, "YES", "NO"), "\n")
  
  # Quick regression plot
  pdf("PA_female_regression_QUICK.pdf", width = 10, height = 8)
  regplot(reg_female, 
          xlab = "Female Percentage (%)", 
          ylab = "Logit Proportion",
          main = "Female % vs Burnout")
  dev.off()
  cat("✅ Female regression plot saved as PA_female_regression_QUICK.pdf\n")
}

# MBI subgroup analysis
cat("\n📊 MBI vs Non-MBI subgroup analysis...\n")
mbi_subgroup <- rma(yi, vi, mods = ~ mbi_binary, 
                    data = pa_final, method = "REML", test = "knha")

cat("MBI difference p-value:", sprintf("%.3f", mbi_subgroup$pval[2]), "\n")
cat("Significant:", ifelse(mbi_subgroup$pval[2] < 0.05, "YES", "NO"), "\n")

# Quick publication bias check
cat("\n📈 Publication bias check...\n")
egger_test <- regtest(res_overall)
cat("Egger's test p-value:", sprintf("%.3f", egger_test$pval), "\n")

# Create basic funnel plot
pdf("PA_funnel_plot_QUICK.pdf", width = 8, height = 6)
funnel(res_overall, main = "Funnel Plot")
dev.off()
cat("✅ Funnel plot saved as PA_funnel_plot_QUICK.pdf\n")

# Summary
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("🎉 QUICK ANALYSIS COMPLETED!\n")
cat("Files created:\n")
cat("• PA_forest_plot_QUICK.pdf\n")
cat("• PA_female_regression_QUICK.pdf\n") 
cat("• PA_funnel_plot_QUICK.pdf\n")

cat("\n🔑 KEY FINDINGS:\n")
cat("• Studies:", res_overall$k, "\n")
cat("• Participants:", format(sum(pa_final$sample_size), big.mark = ","), "\n")
cat("• Pooled prevalence:", sprintf("%.1f%%", overall_results$pred * 100), "\n")
cat("• I² heterogeneity:", sprintf("%.1f%%", res_overall$I2), "\n")
cat("• Female % studies:", sum(!is.na(pa_final$female_percentage_clean)), "\n")

if(exists("reg_female")) {
  cat("• Female % association:", ifelse(reg_female$pval[2] < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), "\n")
}

cat("• MBI difference:", ifelse(mbi_subgroup$pval[2] < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"), "\n")
cat("• Publication bias:", ifelse(egger_test$pval < 0.05, "DETECTED", "NOT DETECTED"), "\n")

cat("\n✅ Basic analysis working! You can now run the full analysis.\n")
