
library(tidyverse) 
library(data.table)
library(readxl)
library(svglite)



data_PRS_badge_simple <- read_excel(path = "../data/data_PRS_badge_simplifié_18_PRS.xlsx")

names(data_PRS_badge_simple)

df_pers_gene <- data_PRS_badge_simple %>% select(SUBJID, GROUPE, 
                                 ns, ha, rd, ps, sd, c, st,
                                 PGS000903,	PGS002738,	PGS002115,	PGS001901,	PGS002037,	
                                 PGS003057,	PGS002098,	PGS001118,	PGS002124,	PGS002746,
                                 PGS003753,	PGS004230,	PGS000205,	PGS002222,	PGS004451,
                                 PGS000907,	PGS001016,	PGS002342)

sum(is.na(df_pers_gene)) # 252

unique(df_pers_gene$GROUPE)

df_pers_gene <- df_pers_gene %>% mutate(GROUPE=ifelse(GROUPE=="PATIENT TEMOIN", 0, 1))

df_pers_gene <- na.omit(df_pers_gene) # Since these patients have no genetic data at all, at least for PGS vars

personality_vars <- c("ns", "ha", "rd", "ps", "sd", "c", "st")

pgs_vars <- colnames(df_pers_gene)[grepl("^PGS", colnames(df_pers_gene))]

unique(data_PRS_badge_simple$GROUPE)



# Personality summary distributions

# Loop over each personality variable and plot them, save to .SVG
for (trait in personality_vars) {
  p <- data_PRS_badge_simple %>%
    mutate(GROUPE = ifelse(GROUPE == "PATIENT TEMOIN", "Control", "AC")) %>%
    select(GROUPE, !!sym(trait)) %>%
    mutate(GROUPE = as.factor(GROUPE)) %>%
    ggplot(aes(x = GROUPE, y = .data[[trait]], colour = GROUPE, fill = GROUPE)) +
    geom_boxplot(alpha = 0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
    geom_jitter(shape = 1, size = 2, stroke = 2.0, width = 0.3) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 10, vjust = -0.5),
      axis.title.y = element_text(size = 10, vjust = -0.5),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    scale_color_manual(values = c("#bf4438", "#283f60")) +
    scale_fill_manual(values = c("#bf4438", "#283f60")) +
    xlab("\nGroup") +
    ylab(paste0("'", trait, "'", " Score\n")) +
    ggtitle(paste0("Distribution of '", trait, "' by Group"))

  # Save plot as SVG
  ggsave(filename = paste0("../out/plots/boxplot_", trait, ".svg"), plot = p, width = 5, height = 5, device = "svg")
}


# Summary stats 

summary_personality_var <- data.frame(
  data_PRS_badge_simple %>%
  mutate(GROUP = ifelse(GROUPE == "PATIENT TEMOIN", "Control", "AC")) %>%
  select(GROUP, all_of(personality_vars)) %>%
  pivot_longer(-GROUP, names_to = "PGS", values_to = "value") %>%
  group_by(GROUP, PGS) %>%
  summarise(
    mean_sd = sprintf("%.2f ± %.2f", mean(value, na.rm = TRUE), sd(value, na.rm = TRUE)),
    median_iqr = {
      q <- quantile(value, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      sprintf("%.2f [%.2f–%.2f]", q[2], q[1], q[3])
    },
    .groups = "drop"
  ) %>%
  mutate(summary=paste(mean_sd, median_iqr, sep=" | ")) %>%
  select(-mean_sd, -median_iqr) %>%
  spread(key=GROUP, value=summary)
  )

fwrite(summary_personality_var, "../out/summaries/summary_personality_var.txt")






# PGS variables summary distributions

# Loop over each PGS variable and plot
for (pgs in pgs_vars) {
  p <- data_PRS_badge_simple %>%
    mutate(GROUPE = ifelse(GROUPE == "PATIENT TEMOIN", "Control", "AC")) %>%
    select(GROUPE, !!sym(pgs)) %>% filter(!is.na(!!sym(pgs))) %>%
    mutate(GROUPE = as.factor(GROUPE)) %>%
    ggplot(aes(x = GROUPE, y = .data[[pgs]], colour = GROUPE, fill = GROUPE)) +
    geom_boxplot(alpha = 0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
    geom_jitter(shape = 1, size = 2, stroke = 2.0, width = 0.3) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 10, vjust = -0.5),
      axis.title.y = element_text(size = 10, vjust = -0.5),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    scale_color_manual(values = c("#bf4438", "#283f60")) +
    scale_fill_manual(values = c("#bf4438", "#283f60")) +
    xlab("\nGroup") +
    ylab(paste0("'", pgs, "'", " Score\n")) +
    ggtitle(paste0("Distribution of '", pgs, "' by Group"))

  # Save the plot as an SVG file
  ggsave(filename = paste0("../out/plots/boxplot_", pgs, ".svg"), plot = p, width = 5, height = 6, device = "svg")
}



# Summary stats 

summary_pgs_var <- data.frame(
  data_PRS_badge_simple %>%
  mutate(GROUP = ifelse(GROUPE == "PATIENT TEMOIN", "Control", "AC")) %>%
  select(GROUP, all_of(pgs_vars)) %>%
  pivot_longer(-GROUP, names_to = "PGS", values_to = "value") %>%
  group_by(GROUP, PGS) %>%
  summarise(
    mean_sd = sprintf("%.2f ± %.2f", mean(value, na.rm = TRUE), sd(value, na.rm = TRUE)),
    median_iqr = {
      q <- quantile(value, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      sprintf("%.2f [%.2f–%.2f]", q[2], q[1], q[3])
    },
    .groups = "drop"
  ) %>%
  mutate(summary=paste(mean_sd, median_iqr, sep=" | ")) %>%
  select(-mean_sd, -median_iqr) %>%
  spread(key=GROUP, value=summary)
  )

fwrite(summary_pgs_var, "../out/summaries/summary_pgs_var.txt")










# Logisitc regressions for interactions - NEED TO SCALE DATA

df_scaled <- df_pers_gene %>%
  mutate(across(where(is.numeric) & !GROUPE, scale))



# Correlation between personality vars

# Correlation matrix Spearman (pairwise complete to handle NAs)

cor_matrix <- df_pers_gene %>%
  select(all_of(personality_vars)) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")



corr_plot <- ggplot(data.frame(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#283f60", name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 2.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(x = NULL, y = NULL, title = "Correlation Heatmap\nPersonality Variables")

ggsave(filename = paste0("../out/plots/corr_plot_personality", ".svg"), plot = corr_plot, width = 4, height = 4, device = "svg")






# Correlation between PGS vars

# Compute correlation matrix (use pairwise complete to handle NAs )
cor_matrix <- df_pers_gene %>%
  select(all_of(pgs_vars)) %>%
  cor(use = "pairwise.complete.obs", method = "spearman") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")


corr_plot <- ggplot(data.frame(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#283f60", name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 2.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(x = NULL, y = NULL, title = "Correlation Heatmap\nPGS Variables")

ggsave(filename = paste0("../out/plots/corr_plot_pgs", ".svg"), plot = corr_plot, width = 8, height = 8, device = "svg")





# Correlation between personality AND PGS vars

# Correlation matrix (pairwise complete to handle NAs)


cor_matrix_cross <- cor(
  df_pers_gene[personality_vars],
  df_pers_gene[pgs_vars],
  use = "pairwise.complete.obs",
  method = "spearman"
)


cor_long <- as.data.frame(cor_matrix_cross) %>%
  rownames_to_column("Personality") %>%
  pivot_longer(-Personality, names_to = "PGS", values_to = "Correlation")


corr_plot <- ggplot(cor_long, aes(x = Personality, y = PGS, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#283f60", name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), size = 2.5, color = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Correlation Heatmap: PGS × Personality",
    x = "Personality Trait",
    y = "Polygenic Score"
  )


ggsave(filename = paste0("../out/plots/corr_plot_pgs_personality", ".svg"), plot = corr_plot, width = 5, height = 8, device = "svg")





# Compute correlation matrix with correct variable order

cor_matrix_all <- df_pers_gene %>%
  select(all_of(c(personality_vars, pgs_vars))) %>%
  cor(use = "pairwise.complete.obs", method = "pearson") %>%
  as.data.frame() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = c(personality_vars, pgs_vars)),
    Var2 = factor(Var2, levels = c(personality_vars, pgs_vars))
  )


all_vars_heat_map <- ggplot(cor_matrix_all, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#283f60", name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 2.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank()
  ) +
  labs(x = NULL, y = NULL, title = "Correlation Heatmap: Personality and PGS Variables")


ggsave(filename = paste0("../out/plots/all_vars_heat_map", ".svg"), plot = all_vars_heat_map, width = 8, height = 8, device = "svg")




# Logistic regressions: each 2 pairs of personality x polygenic trait 

# Empty results data frame

results <- data.frame()

# Loop through all combinations
for (pers in personality_vars) {
  for (pgs in pgs_vars) {
    # Model formula
    form <- as.formula(paste("GROUPE ~", pers, "+", pgs, "+", paste0(pers, ":", pgs)))
    
    # Fit logistic regression model
    model <- glm(form, data = df_scaled, family = "binomial")
    
    # Get coefficient table
    coefs <- summary(model)$coefficients
    
    # Create safe names for each term
    pers_term <- pers
    pgs_term <- pgs
    interaction_term <- paste0(pers, ":", pgs)
    
    # Add row to results with NA fallback if term is missing
    results <- rbind(results, data.frame(
      personality = pers,
      genetic = pgs,
      
      coef_pers = ifelse(pers_term %in% rownames(coefs), coefs[pers_term, "Estimate"], NA),
      se_pers = ifelse(pers_term %in% rownames(coefs), coefs[pers_term, "Std. Error"], NA),
      pval_pers = ifelse(pers_term %in% rownames(coefs), coefs[pers_term, "Pr(>|z|)"], NA),
      
      coef_pgs = ifelse(pgs_term %in% rownames(coefs), coefs[pgs_term, "Estimate"], NA),
      se_pgs = ifelse(pgs_term %in% rownames(coefs), coefs[pgs_term, "Std. Error"], NA),
      pval_pgs = ifelse(pgs_term %in% rownames(coefs), coefs[pgs_term, "Pr(>|z|)"], NA),
      
      coef_interaction = ifelse(interaction_term %in% rownames(coefs), coefs[interaction_term, "Estimate"], NA),
      se_interaction = ifelse(interaction_term %in% rownames(coefs), coefs[interaction_term, "Std. Error"], NA),
      pval_interaction = ifelse(interaction_term %in% rownames(coefs), coefs[interaction_term, "Pr(>|z|)"], NA)
    ))
  }
}

results

fwrite(results, "../out/summaries/results_log_odds_logisitc_regression.txt")


# Pivot for interaction p-values heatmap
heat_data <- results %>%
  select(personality, genetic, coef_interaction, pval_interaction) %>%
  mutate(sig = ifelse(pval_interaction < 0.001, " *** ", 
                      ifelse(pval_interaction < 0.01, " ** ", 
                             ifelse(pval_interaction < 0.05, " * ", "")))) # Optional stars


interactions_heat_map <- ggplot(heat_data, aes(x = personality, y = genetic, fill = coef_interaction)) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = paste0(round(coef_interaction, 2), sig),
      fontface = ifelse(sig != "", "bold", "plain")
    ),size = 2.5) +  
  scale_fill_gradient2(
    low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
    name = "Log-odds\nInteraction\nCoefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Logistic Regression\nLog-odds\nInteraction Effects",
    x = "Personality Trait", y = "PGS Variable"
  )

ggsave(filename = paste0("../out/plots/interactions_heat_map_log_odds", ".svg"), plot = interactions_heat_map, width = 5, height = 6, device = "svg")




results

results_exp <- results %>%
  mutate(
    # Personality OR + 95% CI
    OR_pers = exp(coef_pers),
    Lower_pers = exp(coef_pers - 1.96 * se_pers),
    Upper_pers = exp(coef_pers + 1.96 * se_pers),
    
    # PGS OR + 95% CI
    OR_pgs = exp(coef_pgs),
    Lower_pgs = exp(coef_pgs - 1.96 * se_pgs),
    Upper_pgs = exp(coef_pgs + 1.96 * se_pgs),
    
    # Interaction OR + 95% CI
    OR_inter = exp(coef_interaction),
    Lower_inter = exp(coef_interaction - 1.96 * se_interaction),
    Upper_inter = exp(coef_interaction + 1.96 * se_interaction),
    
    
    # FINAL Interaction OR + 95% CI
    sum_coef = coef_pers + coef_pgs + coef_interaction,
    sum_se = sqrt(se_pers^2 + se_pgs^2 + se_interaction^2),
    
    OR_interaction = exp(sum_coef),
    Lower_interaction = exp(sum_coef - 1.96 * sum_se),
    Upper_interaction = exp(sum_coef + 1.96 * sum_se)
  ) %>%
  select(-sum_coef, -sum_se)  # remove intermediate calculation columns if you want


results_exp <- results_exp %>% select(personality, genetic, OR_pers:Upper_interaction)

results_exp %>% filter(personality=="ha" & grepl("907", genetic))
results_exp %>% filter(personality=="ha" & grepl("3753", genetic))
results_exp %>% filter(personality=="rd" & grepl("2115", genetic))
results_exp %>% filter(personality=="sd" & grepl("2231", genetic))

fwrite(results_exp, "../out/summaries/results_exp_probability_exp_logisitc_regression.txt")



# Reshape to long format for OR and CIs
results_long <- results_exp %>%
  mutate(pair = paste(personality, genetic, sep = " × ")) %>%
  select(pair,
         OR_pers, Lower_pers, Upper_pers,
         OR_pgs, Lower_pgs, Upper_pgs,
         OR_interaction, Lower_interaction, Upper_interaction) %>%
  pivot_longer(
    cols = -pair,
    names_to = c(".value", "type"),
    names_pattern = "(OR|Lower|Upper)_(.*)"
  )



# Plot
forest_plot_odds_ratios <- results_long %>%
  mutate(type=ifelse(type=="pers", "1- Personality Positive only",
                     ifelse(type=="pgs", "2- PGS Positive only", "Personality & PGS Positive"))) %>%
ggplot(aes(x = OR, y = fct_rev(factor(pair)), color = type)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), position = position_dodge(width = 0.7), width = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +   # reference line at OR = 1
  scale_x_log10() +  # log scale for odds ratios
  labs(x = "Odds Ratio (log scale)", y = "Personality × PGS Pair", color = "Effect Type") +
  facet_wrap(~type, scales="free_x") +
  scale_colour_manual(values=c("#bf4438", "#283f60", "black")) +
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 10, vjust = -0.5),
    axis.title.y = element_text(size = 10, vjust = -0.5),
    plot.margin = margin(5, 5, 5, 5, "pt"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_blank()   
  )

ggsave(filename = paste0("../out/plots/forest_plot_odds_ratios", ".svg"), plot = forest_plot_odds_ratios, width = 8, height = 18, device = "svg")

results_long





# Create new column for interaction direction
results$interaction_direction <- ifelse(
  results$coef_interaction > 0,
  "Positive (↑ odds)",
  "Negative (↓ odds)"
)

# Create new column for interaction type (multiplying or attenuating)
results$interaction_type <- ifelse(
  sign(results$coef_interaction) == sign(results$coef_pers) & 
  sign(results$coef_interaction) == sign(results$coef_pgs),
  "Multiplying",
  "Attenuating"
)

interactins_types_classification <- results %>% select(personality, genetic, interaction_direction, interaction_type)

fwrite(interactins_types_classification, "../out/summaries/interactins_types_classification.txt")


interactins_types_classification %>% group_by(interaction_type) %>% count()
interactins_types_classification %>% group_by(interaction_direction) %>% count()

# interaction_type     n
# <chr>            <int>
#   1 Attenuating         77
# 2 Multiplying         49

# interaction_direction     n
# <chr>                 <int>
#   1 Negative (↓ odds)        53
# 2 Positive (↑ odds)        73






# Logistic regressions: each 2 pairs of personality traits


# Empty results data frame

results <- data.frame()

# Loop through all combinations
for (i in 1:(length(personality_vars)-1)) {
  for (j in (i+1):length(personality_vars)) {
    var1 <- personality_vars[i]
    var2 <- personality_vars[j]
    form <- as.formula(paste("GROUPE ~", var1, "+", var2, "+", paste0(var1, ":", var2)))
    model <- glm(form, data = df_scaled, family = "binomial")
    coefs <- summary(model)$coefficients

    results <- rbind(results, data.frame(
      var1 = var1,
      var2 = var2,
      
      coef_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Estimate"], NA),
      se_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Std. Error"], NA),
      pval_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Pr(>|z|)"], NA),

      coef_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Estimate"], NA),
      se_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Std. Error"], NA),
      pval_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Pr(>|z|)"], NA),

      coef_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Estimate"], NA),
      se_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Std. Error"], NA),
      pval_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Pr(>|z|)"], NA)
    ))
  }
}

results

fwrite(results, "../out/summaries/results_log_odds_logisitc_regression_personality_only.txt")




results

results_exp <- results %>%
  mutate(
    # var1 OR + 95% CI
    OR_var1 = exp(coef_var1   ),
    Lower_var1 = exp(coef_var1    - 1.96 * se_var1    ),
    Upper_var1 = exp(coef_var1    + 1.96 * se_var1    ),
    
    # var2 OR + 95% CI
    OR_var2 = exp(coef_var2   ),
    Lower_var2 = exp(coef_var2 - 1.96 * se_var2    ),
    Upper_var2 = exp(coef_var2 + 1.96 * se_var2    ),
    
    # Interaction OR + 95% CI
    OR_inter = exp(coef_interaction),
    Lower_inter = exp(coef_interaction - 1.96 * se_interaction),
    Upper_inter = exp(coef_interaction + 1.96 * se_interaction),
    
    
    # FINAL Interaction OR + 95% CI
    sum_coef = coef_var1 + coef_var2 + coef_interaction,
    sum_se = sqrt(se_var1^2 + se_var2^2 + se_interaction^2),
    
    OR_interaction = exp(sum_coef),
    Lower_interaction = exp(sum_coef - 1.96 * sum_se),
    Upper_interaction = exp(sum_coef + 1.96 * sum_se)
  ) %>%
  select(-sum_coef, -sum_se)  # remove intermediate calculation columns if you want


results_exp <- results_exp %>% select(var1, var2, OR_var1:Upper_interaction)

results_exp %>% filter(var1=="ha" & var2=="st")

fwrite(results_exp, "../out/summaries/results_exp_probability_exp_logisitc_regression.txt")








# Pivot for interaction p-values heatmap
heat_data <- results %>%
  select(var1, var2, coef_interaction, pval_interaction) %>%
  mutate(sig = ifelse(pval_interaction < 0.001, " *** ", 
                      ifelse(pval_interaction < 0.01, " ** ", 
                             ifelse(pval_interaction < 0.05, " * ", "")))) # Optional stars


interactions_heat_map <- ggplot(heat_data, aes(x = var1, y = var2, fill = coef_interaction)) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = paste0(round(coef_interaction, 2), sig),
      fontface = ifelse(sig != "", "bold", "plain")
    ),size = 2.5) +  
  scale_fill_gradient2(
    low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
    name = "Log-odds\nInteraction\nCoefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Logistic Regression\nLog-odds\nInteraction Effects",
    x = "Personality Trait", y = "Personality Trait"
  )

interactions_heat_map
ggsave(filename = paste0("../out/plots/interactions_heat_map_log_odds", ".svg"), plot = interactions_heat_map, width = 5, height = 5, device = "svg")












# Logistic regressions: each 2 pairs of polygenic traits


# Empty results data frame

results <- data.frame()

# Loop through all combinations
for (i in 1:(length(pgs_vars)-1)) {
  for (j in (i+1):length(pgs_vars)) {
    var1 <- pgs_vars[i]
    var2 <- pgs_vars[j]
    form <- as.formula(paste("GROUPE ~", var1, "+", var2, "+", paste0(var1, ":", var2)))
    model <- glm(form, data = df_scaled, family = "binomial")
    coefs <- summary(model)$coefficients

    results <- rbind(results, data.frame(
      var1 = var1,
      var2 = var2,
      
      coef_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Estimate"], NA),
      se_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Std. Error"], NA),
      pval_var1 = ifelse(var1 %in% rownames(coefs), coefs[var1, "Pr(>|z|)"], NA),

      coef_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Estimate"], NA),
      se_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Std. Error"], NA),
      pval_var2 = ifelse(var2 %in% rownames(coefs), coefs[var2, "Pr(>|z|)"], NA),

      coef_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Estimate"], NA),
      se_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Std. Error"], NA),
      pval_interaction = ifelse(paste0(var1, ":", var2) %in% rownames(coefs), coefs[paste0(var1, ":", var2), "Pr(>|z|)"], NA)
    ))
  }
}

results

fwrite(results, "../out/summaries/results_log_odds_logisitc_regression_polygenic_only.txt")





results

results_exp <- results %>%
  mutate(
    # var1 OR + 95% CI
    OR_var1 = exp(coef_var1   ),
    Lower_var1 = exp(coef_var1    - 1.96 * se_var1    ),
    Upper_var1 = exp(coef_var1    + 1.96 * se_var1    ),
    
    # var2 OR + 95% CI
    OR_var2 = exp(coef_var2   ),
    Lower_var2 = exp(coef_var2 - 1.96 * se_var2    ),
    Upper_var2 = exp(coef_var2 + 1.96 * se_var2    ),
    
    # Interaction OR + 95% CI
    OR_inter = exp(coef_interaction),
    Lower_inter = exp(coef_interaction - 1.96 * se_interaction),
    Upper_inter = exp(coef_interaction + 1.96 * se_interaction),
    
    
    # FINAL Interaction OR + 95% CI
    sum_coef = coef_var1 + coef_var2 + coef_interaction,
    sum_se = sqrt(se_var1^2 + se_var2^2 + se_interaction^2),
    
    OR_interaction = exp(sum_coef),
    Lower_interaction = exp(sum_coef - 1.96 * sum_se),
    Upper_interaction = exp(sum_coef + 1.96 * sum_se)
  ) %>%
  select(-sum_coef, -sum_se)  # remove intermediate calculation columns if you want


results_exp <- results_exp %>% select(var1, var2, OR_var1:Upper_interaction)


fwrite(results_exp, "../out/summaries/results_log_odds_logisitc_regression_polygenic_only_exp.txt")





# Pivot for interaction p-values heatmap
heat_data <- results %>%
  select(var1, var2, coef_interaction, pval_interaction) %>%
  mutate(sig = ifelse(pval_interaction < 0.001, " *** ", 
                      ifelse(pval_interaction < 0.01, " ** ", 
                             ifelse(pval_interaction < 0.05, " * ", "")))) # Optional stars


interactions_heat_map <- ggplot(heat_data, aes(x = var1, y = var2, fill = coef_interaction)) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = paste0(round(coef_interaction, 2), sig),
      fontface = ifelse(sig != "", "bold", "plain")
    ),size = 2.5) +  
  scale_fill_gradient2(
    low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
    name = "Log-odds\nInteraction\nCoefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Logistic Regression\nLog-odds\nInteraction Effects",
    x = "Polygenic Trait", y = "Polygenic Trait"
  )

interactions_heat_map
ggsave(filename = paste0("../out/plots/interactions_heat_map_log_odds", ".svg"), plot = interactions_heat_map, width = 9, height = 9, device = "svg")










# Plot 2D and 3D probabilities

library(dplyr)
library(plotly)
library(htmlwidgets)



# Output folders
dir.create("../out/plots/heatmaps_raw", showWarnings = FALSE, recursive = TRUE)
dir.create("../out/plots/plotly_raw", showWarnings = FALSE, recursive = TRUE)




# Personality and PGS variable names
pers_vars <- c("ns", "ha", "rd", "ps", "sd", "c", "st")
pgs_vars <- names(df_scaled)[grepl("^PGS", names(df_scaled))]

# Loop through all pairs
for (pers in pers_vars) {
  for (pgs in pgs_vars) {
    
    # Clean variable names for saving
    pers_clean <- gsub("[\\[\\],]", "", pers)
    filename_base <- paste0(pers_clean, "_x_", pgs)

    # Fit logistic model with interaction
    formula_str <- paste0("GROUPE ~ `", pers, "` + `", pgs, "` + `", pers, "`:`", pgs, "`")
    model <- glm(as.formula(formula_str), data = df_scaled, family = "binomial")
    coefs <- summary(model)$coefficients
    
    # Skip if model doesn't return full coefficient set
    if (nrow(coefs) < 4 || any(is.na(coefs[1:4, 1]))) next

    # Extract coefficients
    intercept <- coefs[1, 1]
    coef_pers <- coefs[2, 1]
    coef_pgs <- coefs[3, 1]
    coef_inter <- coefs[4, 1]
    
    # Create prediction grid
    grid <- expand.grid(
      x = seq(-3, 3, length.out = 100),
      y = seq(-3, 3, length.out = 100)
    ) %>%
      mutate(
        log_odds = intercept + coef_pers * x + coef_pgs * y + coef_inter * x * y,
        probability = plogis(log_odds)
      )
    
    # ---------- 2D HEATMAP ----------
    p2d <- ggplot(grid, aes(x = x, y = y, fill = probability)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "#283f60", mid = "white", high = "#bf4438", midpoint = 0.5
      ) +
      labs(
        title = paste0("Interaction: ", pers_clean, " × ", pgs),
        x = pers_clean,
        y = pgs,
        fill = "Probability"
      ) +
      theme_minimal()
    
    ggsave(
      filename = paste0("../out/plots/heatmaps_raw/", filename_base, ".svg"),
      plot = p2d,
      width = 6, height = 5
    )
    
    # ---------- 3D PLOTLY SURFACE ----------
    z_matrix <- matrix(grid$probability, nrow = 100, ncol = 100)

    p3d <- plot_ly(
      x = unique(grid$x),
      y = unique(grid$y),
      z = z_matrix,
      type = "surface",
      colorscale = list(
        c(0, 'rgb(40,63,96)'), 
        c(0.5, 'rgb(255,255,255)'), 
        c(1, 'rgb(191,68,56)')
      )
    ) %>%
      layout(
        title = paste0("3D Surface: ", pers_clean, " × ", pgs),
        scene = list(
          xaxis = list(title = pers_clean),
          yaxis = list(title = pgs),
          zaxis = list(title = "Predicted Probability")
        )
      )
    
    htmlwidgets::saveWidget(
      p3d,
      file = paste0("../out/plots/plotly_raw/", filename_base, ".html"),
      selfcontained = TRUE
    )
  }
}








# Tests Logistic regression interactions after PCA using orthogonal PCs

# Plot 2D and 3D probabilities


personality_pca <- prcomp(df_pers_gene[, personality_vars], scale. = TRUE)

# Scree plot (to choose number of PCs)
plot(personality_pca, type = "l")

df_pca <- as.data.frame(personality_pca$x[, 1:7])
colnames(df_pca) <- paste0("PC_PERS_", 1:7)



pgs_pca <- prcomp(df_pers_gene[, pgs_vars], scale. = TRUE)
summary(pgs_pca)
plot(pgs_pca, type = "l")

df_pca_pgs <- as.data.frame(pgs_pca$x[, 1:23])
colnames(df_pca_pgs) <- paste0("PC_PGS_", 1:23)

df_combined <- cbind(df_pers_gene["GROUPE"], df_pca, df_pca_pgs)
df_combined_scaled <- as.data.frame(scale(df_combined[, -1]))
df_combined_scaled$GROUPE <- df_combined$GROUPE


results_pca <- data.frame()

for (pc_pers in colnames(df_pca)) {
  for (pc_pgs in colnames(df_pca_pgs)) {
    form <- as.formula(paste("GROUPE ~", pc_pers, "+", pc_pgs, "+", paste0(pc_pers, ":", pc_pgs)))
    model <- glm(form, data = df_combined_scaled, family = "binomial")
    coefs <- summary(model)$coefficients
    
    results_pca <- rbind(results_pca, data.frame(
      PC_personality = pc_pers,
      PC_genetic = pc_pgs,
      coef_pers = coefs[pc_pers, "Estimate"],
      se_pers = coefs[pc_pers, "Std. Error"],
      pval_pers = coefs[pc_pers, "Pr(>|z|)"],
      coef_pgs = coefs[pc_pgs, "Estimate"],
      se_pgs = coefs[pc_pgs, "Std. Error"],
      pval_pgs = coefs[pc_pgs, "Pr(>|z|)"],
      coef_interaction = coefs[paste0(pc_pers, ":", pc_pgs), "Estimate"],
      se_interaction = coefs[paste0(pc_pers, ":", pc_pgs), "Std. Error"],
      pval_interaction = coefs[paste0(pc_pers, ":", pc_pgs), "Pr(>|z|)"]
    ))
  }
}
results_pca %>% filter( (PC_personality=="PC_PERS_1" | PC_personality  =="PC_PERS_2") & 
                          (PC_genetic=="PC_PGS_1"|PC_genetic=="PC_PGS_2"))

fwrite(results_pca, "../out/summaries/results_pca.txt")




library(psych)
rotation_personality <- psych::principal(df_pers_gene[, personality_vars], nfactors = 7, rotate = "varimax", scores = TRUE)
rotation_pgs <- psych::principal(df_pers_gene[, pgs_vars], nfactors = 23, rotate = "varimax", scores = TRUE)

# For personality
loadings_personality <- personality_pca$rotation

# For PGS
loadings_pgs <-pgs_pca$rotation

# Convert to data frame for easy sorting
load_df_personality <- as.data.frame(loadings_personality[, 1:7])  # Adjust number of components
load_df_pgs <- as.data.frame(loadings_pgs[, 1:23])  # Adjust as needed



# Personality PCA loadings
load_df_personality_long <- load_df_personality %>%
  rownames_to_column("Trait") %>%
  pivot_longer(
    cols = -Trait,
    names_to = "Component",
    values_to = "Loading"
  )

loadings_pers <- ggplot(load_df_personality_long, aes(x = Component, y = Trait, fill = Loading)) +
  geom_tile(alpha=0.7) +
  scale_fill_gradient2(low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
                       name = "Loading") +
   geom_text(
    aes(
      label = paste0(round(Loading, 2))
    ),size = 2.5) +
  labs(title = "Personality PCA Loadings",
       x = "Principal Component",
       y = "Personality Trait") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0("../out/plots/loadings_pers", ".svg"), plot = loadings_pers, width = 4, height =4, device = "svg")



# PGS PCA loadings
load_df_pgs_long <- load_df_pgs %>%
  rownames_to_column("Trait") %>%
  pivot_longer(
    cols = -Trait,
    names_to = "Component",
    values_to = "Loading"
  )

loadings_pgs <- ggplot(load_df_pgs_long, aes(x = Component, y = Trait, fill = Loading)) +
  geom_tile(alpha=0.7) +
  scale_fill_gradient2(low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
                       name = "Loading") +
     geom_text(
    aes(
      label = paste0(round(Loading, 2))
    ),size = 2.5) +
  labs(title = "Polygenic Score PCA Loadings",
       x = "Principal Component",
       y = "PGS Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = paste0("../out/plots/loadings_pgs", ".svg"), plot = loadings_pgs, width = 8, height = 8, device = "svg")





# Pivot for interaction p-values heatmap
heat_data <- results_pca %>%
  select(PC_personality , PC_genetic   , coef_interaction, pval_interaction) %>%
  mutate(sig = ifelse(pval_interaction < 0.001, " *** ", 
                      ifelse(pval_interaction < 0.01, " ** ", 
                             ifelse(pval_interaction < 0.05, " * ", "")))) # Optional stars


interactions_heat_map <- ggplot(heat_data, aes(x = PC_personality, y = PC_genetic, fill = coef_interaction)) +
  geom_tile(color = "white") +
  geom_text(
    aes(
      label = paste0(round(coef_interaction, 2), sig),
      fontface = ifelse(sig != "", "bold", "plain")
    ),size = 2.5) +  
  scale_fill_gradient2(
    low = "#bf4438", mid = "white", high = "#283f60", midpoint = 0,
    name = "Log-odds\nInteraction\nCoefficient"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Logistic Regression\nLog-odds\nInteraction Effects",
    x = "Personality Principal Component", y = "PGS Principal Component"
  )

ggsave(filename = paste0("../out/plots/interactions_heat_map_log_odds", ".svg"), plot = interactions_heat_map, width = 5, height = 6, device = "svg")

names(df_combined_scaled)


# Extract variable names
pers_vars <- names(df_combined_scaled)[str_detect(names(df_combined_scaled), "PC_PERS_")]
pgs_vars <- names(df_combined_scaled)[str_detect(names(df_combined_scaled), "PC_PGS_")]


# Loop through all pairs
for (pers in pers_vars) {
  for (pgs in pgs_vars) {
    
    # Fit model
    formula <- as.formula(paste("GROUPE ~", pers, "+", pgs, "+", paste0(pers, ":", pgs)))
    model <- glm(formula, data = df_combined_scaled, family = "binomial")
    
    # Extract coefficients
    coefs <- summary(model)$coefficients
    if (nrow(coefs) < 4) next  # skip if model didn't converge properly
    
    intercept <- coefs[1, 1]
    coef_pers <- coefs[2, 1]
    coef_pgs <- coefs[3, 1]
    coef_interaction <- coefs[4, 1]
    
    # Create grid
    grid <- expand.grid(
      PERS = seq(-3, 3, length.out = 100),
      PGS = seq(-3, 3, length.out = 100)
    ) %>%
      mutate(
        log_odds = intercept + coef_pers * PERS + coef_pgs * PGS + coef_interaction * PERS * PGS,
        probability = plogis(log_odds)
      )
    
    # Plot
    p <- ggplot(grid, aes(x = PERS, y = PGS, fill = probability)) +
      geom_tile() +
      scale_fill_gradient2(low = "#283f60", mid = "white", high = "#bf4438", midpoint = 0.5) +
      labs(
        title = paste("Predicted Probability of 'AC' Parkinson's Disease Group\nInteraction:", pers, "x", pgs),
        x = paste(pers, "(Personality Component)"),
        y = paste(pgs, "(Polygenic Component)"),
        fill = "Probability"
      ) +
      theme_minimal()
    
    # Save as SVG
    filename <- paste0("../out/plots/interaction_", pers, "_", pgs, ".svg")
    ggsave(filename, p, width = 6, height = 5, device = "svg")
  }
}


library(plotly)
library(dplyr)
library(htmlwidgets)


output_folder <- "../out/plots/plots_plotly/"

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Extract variable names
pers_vars <- names(df_combined_scaled)[grepl("PC_PERS_", names(df_combined_scaled))]
pgs_vars  <- names(df_combined_scaled)[grepl("PC_PGS_", names(df_combined_scaled))]


# Loop through all pairs
for (pers in pers_vars) {
  for (pgs in pgs_vars) {
    
    # Fit model
    formula_str <- paste0("GROUPE ~ ", pers, " + ", pgs, " + ", pers, ":", pgs)
    model <- glm(as.formula(formula_str), data = df_combined_scaled, family = "binomial")
    coefs <- summary(model)$coefficients
    
    # Skip if interaction term not estimated (e.g., NA due to separation)
    if (nrow(coefs) < 4 || any(is.na(coefs[1:4,1]))) next
    
    # Extract coefficients
    intercept <- coefs[1,1]
    coef_pers <- coefs[2,1]
    coef_pgs <- coefs[3,1]
    coef_interaction <- coefs[4,1]
    
    # Create grid
    grid <- expand.grid(
      x = seq(-3, 3, length.out = 100),
      y = seq(-3, 3, length.out = 100)
    ) %>%
      mutate(
        log_odds = intercept + coef_pers * x + coef_pgs * y + coef_interaction * x * y,
        probability = plogis(log_odds)
      )
    
    # Create matrix for plotly
    z_matrix <- matrix(grid$probability, nrow = 100, ncol = 100)
    
    # Create plot
    p <- plot_ly(
      x = unique(grid$x),
      y = unique(grid$y),
      z = z_matrix,
      type = "surface",
      colorscale = list(
        c(0, 'rgb(40,63,96)'),    # dark blue
        c(0.5, 'rgb(255,255,255)'), # white
        c(1, 'rgb(191,68,56)')    # dark red
      )
    ) %>%
      layout(
        title = paste0("Interaction: ", pers, " × ", pgs),
        scene = list(
          xaxis = list(title = pers),
          yaxis = list(title = pgs),
          zaxis = list(title = "Predicted Probability")
        )
      )
    
    # Save as HTML
    filename <- paste0(output_folder, "3D_", pers, "_x_", pgs, ".html")
    htmlwidgets::saveWidget(p, filename, selfcontained = TRUE)
  }
}











# Cluster-Based Interaction Analysis 
# Cluster patients and do logistic regression with those to rpedict group = 'AC'


library(mclust)
library(dplyr)
library(ggplot2)
library(tidyr)

personality_vars <- c("ns", "ha", "rd", "ps", "sd", "c", "st")

pgs_vars <- colnames(df_pers_gene)[grepl("^PGS", colnames(df_pers_gene))]


# Assume these are the correct column patterns
personality_data <- df_scaled %>%
  select(all_of(personality_vars))

pgs_data <- df_scaled %>%
  select(matches("^PGS"))

# Personality cluster model
mclust_pers <- Mclust(personality_data, G = 1:6)
pers_clusters <- mclust_pers$classification

# PGS cluster model
mclust_pgs <- Mclust(pgs_data, G = 1:6)
pgs_clusters <- mclust_pgs$classification


summary(mclust_pers)
mclust_pers$BIC

summary(mclust_pgs)
mclust_pgs$BIC

mclust_pers$G  # Number of personality clusters selected
mclust_pgs$G   # Number of PGS clusters selected


plot(mclust_pers, what = "BIC")
plot(mclust_pgs, what = "BIC")



df_clustered <- df_scaled %>%
  mutate(
    pers_cluster = factor(pers_clusters),
    pgs_cluster = factor(pgs_clusters)
  )


interaction_model <- glm(GROUPE ~ pers_cluster * pgs_cluster, 
                         data = df_clustered, family = "binomial")

summary(interaction_model)

library(ggeffects)

plot_data <- ggpredict(interaction_model, terms = c("pers_cluster", "pgs_cluster"))

plot(plot_data) +
  labs(
    title = "Interaction of Personality and PGS Cluster Memberships",
    x = "Personality Cluster",
    y = "Predicted Probability of Parkinson's",
    color = "PGS Cluster"
  )




# Generate prediction grid with all combinations of clusters
newdata <- expand.grid(
  pers_cluster = factor(c(1, 2), levels = c(1, 2)),
  pgs_cluster = factor(c(1, 2), levels = c(1, 2))
)

# Predict probabilities using the model
newdata$predicted_prob <- predict(interaction_model, newdata = newdata, type = "response")

# Create readable labels for the plot
newdata$group_label <- paste0("Pers ", newdata$pers_cluster, " × PGS ", newdata$pgs_cluster)

# Plot
plot <- ggplot(newdata, aes(x = group_label, y = predicted_prob, fill = group_label, colour = group_label)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values=c("#677c99", "#283f60", "#c28680", "#bf4438")) +
  scale_colour_manual(values=c("#677c99", "#283f60", "#c28680", "#bf4438")) +
  labs(
    title = "Predicted Probability of 'AC' Parkinson's Group\nby patient 'Cluster' Combination",
    x = "Cluster Combination",
    y = "Predicted Probability of 'AC' group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14)


ggsave(filename = paste0("../out/plots/interactions_clusters_probability", ".svg"), plot = plot, width = 6, height = 6, device = "svg")


# Personality clusters
df_pers_gene$pers_cluster <- factor(pers_clusters)

# PGS clusters
df_pers_gene$pgs_cluster <- factor(pgs_clusters)


# Mean personality scores by cluster
df_pers_gene %>%
  group_by(pers_cluster) %>%
  summarise(across(all_of(personality_vars), mean, na.rm = TRUE))


# Mean PGS scores by cluster
df_pers_gene %>%
  group_by(pgs_cluster) %>%
  summarise(across(all_of(pgs_vars), mean, na.rm = TRUE)) %>%
  transpose()


pers <- df_pers_gene %>%
  group_by(pers_cluster) %>%
  summarise(across(
    all_of(personality_vars),
    ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x, na.rm = TRUE)
      q <- quantile(.x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      sprintf("%.2f ± %.2f | %.2f [%.2f – %.2f]", m, s, q[2], q[1], q[3])
    }
  ))


library(dplyr)
library(purrr)

# Get list of variables and group names
personality_tests <- personality_vars
group_var <- "pers_cluster"

# Perform Wilcoxon tests
wilcox_results <- map_dfr(personality_tests, function(var) {
  formula <- as.formula(paste(var, "~", group_var))
  test <- wilcox.test(formula, data = df_pers_gene)
  tibble(
    variable = var,
    statistic = test$statistic,
    p_value = test$p.value
  )
})

wilcox_results <- wilcox_results %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))  # Benjamini-Hochberg



pgs <- df_pers_gene %>%
  group_by(pgs_cluster) %>%
  summarise(across(
    all_of(pgs_vars),
    ~{
      m <- mean(.x, na.rm = TRUE)
      s <- sd(.x, na.rm = TRUE)
      q <- quantile(.x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      sprintf("%.2f ± %.2f | %.2f [%.2f – %.2f]", m, s, q[2], q[1], q[3])
    }
  ))



# Get list of variables and group names
pgs_tests <- pgs_vars
group_var <- "pgs_cluster"

# Perform Wilcoxon tests
wilcox_results <- map_dfr(pgs_tests, function(var) {
  formula <- as.formula(paste(var, "~", group_var))
  test <- wilcox.test(formula, data = df_pers_gene)
  tibble(
    variable = var,
    statistic = test$statistic,
    p_value = test$p.value
  )
})

options(scipen = 999)

wilcox_results <- wilcox_results %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))  # Benjamini-Hochberg




fwrite(pers, "pers_cluster_vars_summary.csv")

fwrite(pgs, "pgs_cluster_vars_summary.csv")




# Bayseian Logistic regression 

library(brms)



# Create interaction model with all relevant priors (flat or weakly informative)
bayes_model_ha_PGS003753 <- brm(
  formula = GROUPE ~ ha * PGS003753,
  data = df_scaled,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4,
  seed = 42
)


# Summarize
summary(bayes_model_ha_PGS003753)
plot_bayes_model_ha_PGS003753 <- plot(bayes_model_ha_PGS003753)

plot_bayes_model_ha_PGS003753 <- plot_bayes_model_ha_PGS003753[[1]]

ggsave(filename = paste0("../out/plots/plot_bayes_model_ha_PGS003753", ".svg"), plot = plot_bayes_model_ha_PGS003753, width = 5, height = 5, device = "svg")





# Create interaction model with all relevant priors (flat or weakly informative)
bayes_model_sd_PGS000777 <- brm(
  formula = GROUPE ~ sd * PGS000777,
  data = df_scaled,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4,
  seed = 42
)

# Summarize
summary(bayes_model_sd_PGS000777)
plot_bayes_model_sd_PGS000777 <- plot(bayes_model_sd_PGS000777)

plot_bayes_model_sd_PGS000777 <- plot(bayes_model_sd_PGS000777)[[1]]

ggsave(filename = paste0("../out/plots/plot_bayes_model_sd_PGS000777", ".svg"), plot = plot_bayes_model_sd_PGS000777, width = 5, height = 5, device = "svg")





# Create interaction model with all relevant priors (flat or weakly informative)
bayes_model_ns_PGS000907 <- brm(
  formula = GROUPE ~ ns * PGS000907,
  data = df_scaled,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4,
  seed = 42
)

# Summarize
summary(bayes_model_ns_PGS000907)
plot_bayes_model_ns_PGS000907 <- plot(bayes_model_ns_PGS000907)

plot_bayes_model_ns_PGS000907 <- plot_bayes_model_ns_PGS000907[[1]]

ggsave(filename = paste0("../out/plots/plot_bayes_model_ns_PGS000907", ".svg"), plot = plot_bayes_model_ns_PGS000907, width = 5, height = 5, device = "svg")






# Create interaction model with all relevant priors (flat or weakly informative)
bayes_model_c_PGS003753 <- brm(
  formula = GROUPE ~ c * PGS003753,
  data = df_scaled,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4,
  seed = 42
)

# Summarize
summary(bayes_model_c_PGS003753)
plot_bayes_model_c_PGS003753 <- plot(bayes_model_c_PGS003753)

plot_bayes_model_c_PGS003753 <- plot(bayes_model_c_PGS003753)[[1]]

ggsave(filename = paste0("../out/plots/plot_bayes_model_c_PGS003753", ".svg"), plot = plot_bayes_model_c_PGS003753, width = 5, height = 5, device = "svg")




# Create interaction model with all relevant priors (flat or weakly informative)
bayes_model_sd_PGS000903 <- brm(
  formula = GROUPE ~ sd * PGS000903,
  data = df_scaled,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4, iter = 2000, cores = 4,
  seed = 42
)

# Summarize
summary(bayes_model_sd_PGS000903)
plot_bayes_model_sd_PGS000903 <- plot(bayes_model_sd_PGS000903)

plot_bayes_model_sd_PGS000903 <- plot_bayes_model_sd_PGS000903[[1]]

ggsave(filename = paste0("../out/plots/plot_bayes_model_sd_PGS000903", ".svg"), plot = plot_bayes_model_sd_PGS000903, width = 5, height = 5, device = "svg")

