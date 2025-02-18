#################################################|
## Project: ENAR DATAFEST 2025
## Script: ExploratoryPlots.R
## Last Header Update: 2025-02-16
## Author: Chris Xie
## R-version: R version 4.4.1 
#################################################|

rm(list=ls())
options(stringsAsFactors=F,digits=10, scipen=100)
###############################################################################
# Load Libraries
###############################################################################
library(readr)   
library(dplyr)   
library(survey) 
library(ggplot2) 

###############################################################################
# 1) Read Data
###############################################################################
file_path <- "C:/Users/ENAR DATAFEST 2025/Datafest_cleaned/hypertension_subpopulation_test.csv"
datafest_data <- read_csv(file_path)

###############################################################################
# 2) Define NHANES Survey Design
###############################################################################
nhanes_design <- svydesign(
  id      = ~svy_psu,
  strata  = ~svy_strata,
  weights = ~svy_weight_mec,
  nest    = TRUE,
  data    = datafest_data
)

###############################################################################
# 3) Calculate Weighted Proportions for BP Control by YEAR
###############################################################################
bp_control_svy <- svyby(
  ~I(bp_control_accaha == "Yes"),  # Logical expression for controlled BP
  by = ~YEAR,                      # Group by YEAR
  design = nhanes_design,
  FUN = svymean,
  na.rm = TRUE,
  vartype = "ci"                   # Get confidence intervals
)

# Check column names to see the "TRUE" and "FALSE" columns
# colnames(bp_control_svy)

###############################################################################
# 4) Rename Columns & Convert to Percent
###############################################################################
bp_control_svy <- bp_control_svy %>%
  rename(
    prop_yes = `I(bp_control_accaha == "Yes")TRUE`,   # Weighted proportion for "Yes"
    ci_l     = `ci_l.I(bp_control_accaha == "Yes")TRUE`, 
    ci_u     = `ci_u.I(bp_control_accaha == "Yes")TRUE`
  ) %>%
  mutate(
    prop_yes = prop_yes * 100,
    ci_l     = ci_l * 100,
    ci_u     = ci_u * 100
  )

###############################################################################
# 5) Plot bp control overall trend 
###############################################################################
ggplot(bp_control_svy, aes(x = factor(YEAR), y = prop_yes)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2, color = "black") +
  geom_text(aes(label = sprintf("%.1f", prop_yes)),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "BP Control (ACC/AHA) Over Time (Weighted)",
    x = "NHANES Survey Year",
    y = "Percentage with Controlled BP (95% CI)"
  ) +
  theme_minimal()


###############################################################################
# 6) Identify Eligible Categorical Variables (≤5 levels)
###############################################################################
top_vars <- c(
  "bp_med_use", "htn_resistant_accaha", "ldl_corrected", "demo_race", 
  "BMXBMI", "htn_aware", "demo_age_years", "cc_smoke", "cc_diabetes",
  "cc_ckd", "demo_gender", "cc_cvd_stroke", "cc_cvd_any", "cc_cvd_hf",
  "cc_cvd_mi", "cc_cvd_chd", "BMXWAIST", "INDMPIR", "DMDHRGND", 
  "chol_med_use_sr",
  "demo_race_Non-Hispanic", "BPACSZ", "htn_resistant_jnc7", "LBXSCH",
  "cc_cvd_nh", "cc_acr", "svy_strata", "LBXBSU", "LBBDANO", "BMXWT"
)
top_vars <- unique(top_vars)

eligible_vars <- c()
for (v in top_vars) {
  if (!v %in% names(datafest_data)) next
  if (is.factor(datafest_data[[v]]) || is.character(datafest_data[[v]])) {
    lvls <- unique(na.omit(datafest_data[[v]]))
    if (length(lvls) <= 5) {
      eligible_vars <- c(eligible_vars, v)
    }
  }
}
cat("Eligible categorical variables (≤ 5 levels):\n")
print(eligible_vars)


###############################################################################
# 7) For Each Eligible Variable, Make a Two-Panel Plot
###############################################################################

pdf("Distribution_and_BPControl_WithOverallLine.pdf", width = 8, height = 8)
for (rf in eligible_vars) {
  
  #-----------------------
  # 7A) PROPORTION OF EACH LEVEL
  #-----------------------
  levels_rf <- unique(na.omit(datafest_data[[rf]]))
  df_levels <- data.frame()
  
  for (lvl in levels_rf) {
    formula_str <- paste0("~ I(", rf, " == '", lvl, "')")
    svy_res_lvl <- svyby(
      as.formula(formula_str),
      by = ~ Survey_Year,
      design = nhanes_design,
      FUN = svymean,
      na.rm = TRUE
    )
    
    col_true <- grep("TRUE", names(svy_res_lvl), value = TRUE)[1]
    if (is.null(col_true)) next
    
    svy_res_lvl <- svy_res_lvl %>%
      rename(proportion = !!col_true)
    svy_res_lvl$proportion <- svy_res_lvl$proportion * 100
    svy_res_lvl$level <- lvl
    svy_res_lvl$measure <- "Proportion of level"
    
    df_levels <- bind_rows(df_levels, svy_res_lvl)
  }
  
  #-----------------------
  # 7B) PROPORTION WITH BP CONTROL
  #-----------------------
  df_control <- svyby(
    ~ I(bp_control_accaha == "Yes"),
    by = ~ Survey_Year + get(rf),
    design = nhanes_design,
    FUN = svymean,
    na.rm = TRUE
  )
  
  col_true2 <- "I(bp_control_accaha == \"Yes\")TRUE"
  if (!col_true2 %in% names(df_control)) {
    cat(paste("Skipping", rf, "- missing proportion column for control.\n"))
    next
  }
  
  df_control <- df_control %>%
    rename(proportion = !!col_true2)
  df_control$proportion <- df_control$proportion * 100
  
  grouping_col <- "get(rf)"
  if (!grouping_col %in% names(df_control)) {
    cat(paste("Skipping", rf, "- grouping_col not found.\n"))
    next
  }
  df_control <- df_control %>%
    rename(level = !!grouping_col)
  
  df_control$measure <- "BP Control"
  
  #-----------------------
  # 7C) COMBINE
  #-----------------------
  df_plot <- bind_rows(df_levels, df_control)
  
  # Ensure numeric
  df_plot$Survey_Year <- as.numeric(df_plot$Survey_Year)
  bp_control_svy$Survey_Year <- as.numeric(bp_control_svy$Survey_Year)
  
  # Force measure order: "BP Control" first (top), "Proportion of level" second (bottom)
  df_plot$measure <- factor(df_plot$measure, levels = c("BP Control", "Proportion of level"))
  
  #-----------------------
  # 7D) DUMMY POINTS for the BOTTOM panel only (Proportion = 0–100)
  #-----------------------
  # We'll keep the top panel flexible (no dummy points for "BP Control"),
  
  df_dummy_bottom <- data.frame(
    Survey_Year = c(1999, 1999),
    proportion  = c(0, 100),
    measure     = c("Proportion of level", "Proportion of level"),
    level       = NA  # no color mapping
  )
  
  #-----------------------
  # 7E) PLOT
  #-----------------------
  p <- ggplot() +
    # 1) geom_blank for bottom panel dummy points (0 & 100)
    geom_blank(
      data = df_dummy_bottom,
      aes(x = Survey_Year, y = proportion),
      inherit.aes = FALSE
    ) +
    # 2) The real data lines/points
    geom_line(
      data = df_plot,
      aes(x = Survey_Year, y = proportion, color = factor(level), group = factor(level)),
      size = 1
    ) +
    geom_point(
      data = df_plot,
      aes(x = Survey_Year, y = proportion, color = factor(level), group = factor(level)),
      size = 2
    ) +
    # 3) Two-panel facet with free_y, so top is flexible, bottom is forced by the dummy
    facet_wrap(~measure, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = seq(1999, 2017, by = 2)) +
    labs(
      title = paste("BP Control Over Time by", rf),
      x = "Survey Year",
      y = "Percentage (%)",
      color = "Level"
    ) +
    theme_minimal()
  
  #-----------------------
  # 7F) ADD BLACK DOTTED LINE WITH A LEGEND KEY
  #-----------------------
  p2 <- p +
    geom_line(
      data = bp_control_svy,
      aes(x = Survey_Year, y = prop_yes, linetype = "Overall Trend"),
      color = "black",
      size = 1,
      inherit.aes = FALSE
    ) +
    scale_linetype_manual(
      name = "", 
      values = c("Overall Trend" = "dotted")
    )
  
  # Print final figure
  print(p2)
}
dev.off()






###############################################################################
# 8) Plot BP Medication use by Year & Gender
###############################################################################

#-----------------------
# 8a) Compute Weighted Proportion of BP Medication Users ("Yes") by Year & Gender
#-----------------------
df_yes <- svyby(
  ~ I(bp_med_use == "Yes"), 
  by = ~ Survey_Year + demo_gender,
  design = nhanes_design,
  FUN = svymean,
  na.rm = TRUE,
  vartype = "ci"
) %>%
  rename(
    prop = `I(bp_med_use == "Yes")TRUE`,
    ci_lower = `ci_l.I(bp_med_use == "Yes")TRUE`,
    ci_upper = `ci_u.I(bp_med_use == "Yes")TRUE`
  ) %>%
  mutate(
    prop = prop * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100,
    measure = "BP Medication Users"
  )


#-----------------------
# 8b) Compute Weighted Proportion of BP Medication Non-Users ("No") by Year & Gender
#-----------------------
df_no <- svyby(
  ~ I(bp_med_use == "No"), 
  by = ~ Survey_Year + demo_gender,
  design = nhanes_design,
  FUN = svymean,
  na.rm = TRUE,
  vartype = "ci"
) %>%
  rename(
    prop = `I(bp_med_use == "No")TRUE`,
    ci_lower = `ci_l.I(bp_med_use == "No")TRUE`,
    ci_upper = `ci_u.I(bp_med_use == "No")TRUE`
  ) %>%
  mutate(
    prop = prop * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100,
    measure = "BP Medication Non-Users"
  )

#-----------------------
# 8c) Plot:Two-Panel Figure with Facets (Top: Users; Bottom: Non-Users)
#-----------------------

df_combined <- bind_rows(df_yes, df_no)
df_combined <- df_combined %>% mutate(Survey_Year = as.numeric(as.character(Survey_Year)))
ggplot(df_combined, aes(
  x = Survey_Year,
  y = prop,
  color = demo_gender,
  group = demo_gender
)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  facet_wrap(~ measure, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(1999, 2017, by = 2)) +
  labs(
    title = "Weighted Proportion of BP Medication Use and Non-Use\nby Survey Year and Gender",
    x = "Survey Year",
    y = "Percentage (%)",
    color = "Gender"
  ) +
  theme_minimal()


