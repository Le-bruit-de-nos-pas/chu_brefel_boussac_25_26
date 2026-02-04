
library(tidyverse) 
library(data.table)
library(readxl)
library(purrr)


# LDA: 3 PD groups, only TCI variables --------------

Dataset_PSYCHO_PAIN_2_Feuil1 <- read_xlsx(path="../data/Dataset PSYCHO-PAIN_2.xlsx", trim_ws = TRUE, sheet="Feuil1")

unique(Dataset_PSYCHO_PAIN_2_Feuil1$Disease)
unique(Dataset_PSYCHO_PAIN_2_Feuil1$groupes)

Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(grepl("PD", groupes))  %>%
  group_by(groupes) %>% count()

# 1 NO_pain_PD         25
# 2 PD_central_pain     9
# 3 PD_musculo_pain    12

names(Dataset_PSYCHO_PAIN_2_Feuil1)


pd_groups_tci_only <- Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(grepl("PD", groupes))  %>%
  select(Subjects , Number, groupes, NS, HA, RD, P, SD, C, ST)


# Keep only numeric vars
df_num <- pd_groups_tci_only %>%
  select(where(is.numeric)) %>%
  drop_na()  # optional

# Keep groups separately
groups <- pd_groups_tci_only$groupes


library(MASS)

lda_model <- lda(groupes ~ ., data = cbind(df_num, groupes = groups))

lda_scores <- as.data.frame(predict(lda_model)$x) %>%
  mutate(groupes = groups)

lda_model

# Call:
# lda(groupes ~ ., data = cbind(df_num, groupes = groups))
# 
# Prior probabilities of groups:
#      NO_pain_PD PD_central_pain PD_musculo_pain 
#       0.5434783       0.1956522       0.2608696 
# 
# Group means:
#                       NS       HA       RD        P       SD        C       ST
# NO_pain_PD      19.12000 17.08000 16.36000 5.040000 34.92000 34.44000 13.40000
# PD_central_pain 18.33333 16.44444 16.11111 4.666667 33.11111 31.44444 15.00000
# PD_musculo_pain 16.25000 19.91667 16.58333 5.916667 33.75000 36.75000 15.83333
# 
# Coefficients of linear discriminants:
#             LD1         LD2
# NS -0.004818516  0.14118930
# HA  0.108803919  0.03309719
# RD -0.054415567 -0.01362393
# P   0.274148120  0.02446726
# SD  0.051252522  0.06872828
# C   0.212198064  0.05408476
# ST -0.003733522 -0.08564405
# 
# Proportion of trace:
#    LD1    LD2 
# 0.8611 0.1389 


lda_model$scaling


#          LD1      LD2
# NS -0.004819  0.14119
# HA  0.108804  0.03310
# RD -0.054416 -0.01362
# P   0.274148  0.02447
# SD  0.051253  0.06873
# C   0.212198  0.05408
# ST -0.003734 -0.08564



library(tidyverse)

# Create the loadings table
loadings <- tribble(
  ~Variable,               ~LD1,           ~LD2,       
  "NS",        -0.004819,  0.14119,
  "HA",     0.108804,   0.03310,
  "RD",      -0.054416,  -0.01362,
  "P",           0.274148,   0.02447,
  "SD",       0.051253,   0.06873,
  "C",       0.212198,  0.05408,
  "ST",         -0.003734,   -0.08564
)

# Convert to long format
loadings_long <- loadings %>%
  pivot_longer(cols = starts_with("LD"),
               names_to = "LD",
               values_to = "Loading")

# Plot
plot <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~LD, scales = "free_x") +
  scale_fill_gradient2(low = "#C597C9", high = "#335D87", mid = "grey90") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Loadings for LD1 and LD2",
    x = "Variable \n",
    y = "\n Loading"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 



ggsave(file="test.svg", plot=plot, width=6, height=3)





centroids <- lda_scores %>%
  group_by(groupes) %>%
  summarise(across(c(LD1, LD2), mean))

dist_matrix <- dist(centroids[, c("LD1", "LD2")])
as.matrix(dist_matrix)

centroids

#  groupes             LD1    LD2
# 1 NO_pain_PD      -0.0797  0.229
# 2 PD_central_pain -0.968  -0.332
# 3 PD_musculo_pain  0.892  -0.228


# Predicted class memberships
lda_pred <- predict(lda_model)$class

# Confusion matrix
conf_mat <- table(Predicted = lda_pred, Actual = groups)
conf_mat

# Classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy # 0.63










library(pROC)
library(tidyverse)

# Predict class probabilities
lda_pred <- predict(lda_model)
post_probs <- lda_pred$posterior  # posterior probabilities for each class

# Create ROC data frame for plotting
roc_df <- map_dfr(colnames(post_probs), function(cls) {
  roc_obj <- roc(response = groups == cls,
                 predictor = post_probs[, cls])
  data.frame(
    LD_class = cls,
    sensitivity = rev(roc_obj$sensitivities),
    specificity = rev(1 - roc_obj$specificities)
  )
})


# Compute AUCs separately
auc_df <- map_dfr(colnames(post_probs), function(cls) {
  data.frame(
    LD_class = cls,
    auc = as.numeric(auc(roc(groups == cls, post_probs[, cls])))
  )
})

# Plot
plot <- ggplot(roc_df, aes(x = specificity, y = sensitivity, color = LD_class)) +
  geom_line(size = 2.2, alpha=0.85) +
  geom_abline(linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(title = "ROC Curves for LDA Classes",
       x = "\n 1 - Specificity",
       y = "Sensitivity \n",
       color = "Group") +
  scale_colour_manual(values=c("gray", "#A32121", "#335D87")) +
  geom_text(
    data = auc_df,
    aes(x = 0.75, y = 0.25 - 0.05*as.numeric(factor(LD_class)),
        label = paste0("AUC ", LD_class, ": ", round(auc, 2))),
    inherit.aes = FALSE
  )


ggsave(file="test.svg", plot=plot, width=7, height=7)











lda_scores <- lda_scores %>%
  mutate(groupes=ifelse(groupes=="PD_central_pain", "PD_Central",
                        ifelse(groupes=="PD_musculo_pain", "PD_Musculo",
                               ifelse(groupes=="NO_pain_PD", "NO_pain", ""))))



quadrants <- data.frame(
  xmin = c(-Inf, 0.1841, -Inf, 0.1841),
  xmax = c(0.1841, Inf, 0.1841, Inf),
  ymin = c(-Inf, -Inf, -0.0255, -0.0255),
  ymax = c(-0.0255, -0.0255, Inf, Inf),
  fill = factor(c("Q1", "Q2", "Q3", "Q4"))
)


quad_colors <- c("Q1" = "#A32121", "Q2" = "#335D87", "Q3" = "#DBC44D", "Q4" = "#DBC44D")

group_colors <- c("PD_Central" = "#A32121",
                  "NO_pain" = "#DBC44D",
                  "PD_Musculo" = "#335D87")

# Plot
plot <- ggplot() +
 # geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
  #          alpha = 0.2, inherit.aes = FALSE) +
  geom_point(data = lda_scores, aes(LD1, LD2, color = groupes, shape = groupes), size = 3, stroke = 2) +
  stat_ellipse(data = lda_scores, aes(LD1, LD2, color = groupes), level = 0.68, size = 2, alpha = 0.6) +
    geom_text(data = centroids, aes(LD1, LD2, label = groupes), size = 4, fontface = "bold", vjust = -1) +
 geom_vline(xintercept = 0.1841) +
  geom_hline(yintercept = -0.0255) +
 # scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +  # different shapes per group
   theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(title = "LDA — Group Separation",
       subtitle = "Ellipses 68% confidence regions") +
  theme(legend.position = "none")


ggsave(file="test.svg", plot=plot, width=6, height=6)








library(dplyr)
library(tidyr)
library(purrr)


groups <- unique(lda_scores$groupes)

# Generate all pairwise combinations
pairs <- t(combn(groups, 2)) %>% as.data.frame()
colnames(pairs) <- c("g1", "g2")

run_tests <- function(dim) {
  pairs %>%
    rowwise() %>%
    mutate(
      stat = list(wilcox.test(
        lda_scores[[dim]][lda_scores$groupes == g1],
        lda_scores[[dim]][lda_scores$groupes == g2]
      )),
      p_value = stat$p.value
    ) %>%
    dplyr::select(g1, g2, p_value)
}

LD1_results <- run_tests("LD1")
LD2_results <- run_tests("LD2")


p_to_sig <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}


make_heatmap_df <- function(results) {
  
  groups <- sort(unique(c(results$g1, results$g2)))
  
  expand_grid(g1 = groups, g2 = groups) %>%
    left_join(results, by = c("g1", "g2")) %>%
    left_join(results, by = c("g1" = "g2", "g2" = "g1"),
              suffix = c("", "_rev")) %>%
    mutate(
      p = coalesce(p_value, p_value_rev),
      sig = p_to_sig(p),
      p = ifelse(g1 == g2, NA, p),
      sig = ifelse(g1 == g2, "", sig)
    )
}


LD1_hm <- make_heatmap_df(LD1_results)
LD2_hm <- make_heatmap_df(LD2_results)



plot_sig_heatmap <- function(df, title) {
  
  ggplot(df, aes(g1, g2, fill = sig)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig), size = 6, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "ns"  = "#CFE1E8",
        "*"   = "#72B9D6",
        "**"  = "#347F9E",
        "***" = "#125470"
      ),
      drop = FALSE
    ) +
    coord_equal() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


p_LD1 <- plot_sig_heatmap(LD1_hm, "LD1 – Pairwise Wilcox")
p_LD2 <- plot_sig_heatmap(LD2_hm, "LD2 – Pairwise Wilcox")

p_LD1
p_LD2

ggsave(file="test2.svg", plot=p_LD2, width=5, height=5)






# ----------------
# LDA: 3 PD groups, extended variables --------------

Dataset_PSYCHO_PAIN_2_Feuil1 <- read_xlsx(path="../data/Dataset PSYCHO-PAIN_2.xlsx", trim_ws = TRUE, sheet="Feuil1")

df_complete <- Dataset_PSYCHO_PAIN_2_Feuil1 %>%
  filter(grepl("PD", groupes)) %>%
  select(groupes, NS, HA, RD, P, SD, C, ST, Moca_total_score, Age, Sexe,
         duree_maladie, HAD_total, ORT_total, MDS_UPDRS_tot1, MDS_UPDRS_tot2,
         MDS_UPDRS_tot3, HOEHN_YAHR = `HOEHN&YAHR`, MDS_UPDRS_tot4, DEL_total) %>%
  drop_na()


df_complete <- df_complete %>%
  mutate(Sexe = ifelse(Sexe == "Homme", 1, 0))

predictor_cols <- setdiff(names(df_complete), "groupes")

df_num <- as.data.frame(scale(df_complete %>% select(all_of(predictor_cols))))

names(df_num) <- predictor_cols

var_check <- apply(df_num, 2, var, na.rm = TRUE)

zero_var_cols <- names(var_check)[var_check == 0 | is.na(var_check)]

if(length(zero_var_cols) > 0) {
  message("Dropping zero-variance columns: ", paste(zero_var_cols, collapse = ", "))
  df_num <- df_num %>% select(-all_of(zero_var_cols))
}


groups <- factor(df_complete$groupes)

lda_data <- data.frame(df_num, groupes = groups)

lda_data %>% group_by(groupes) %>% count()

#  groupes             n
# 1 NO_pain_PD         20
# 2 PD_central_pain     6
# 3 PD_musculo_pain    10

library(MASS)
lda_model <- lda(groupes ~ ., data = lda_data)


# Call:
# lda(groupes ~ ., data = lda_data)
# 
# Prior probabilities of groups:
#      NO_pain_PD PD_central_pain PD_musculo_pain 
#       0.5581395       0.1860465       0.2558140 
# 
# Group means:
#                          NS          HA           RD           P         SD
# NO_pain_PD       0.13935326 -0.08376801 -0.003597663 -0.03951051  0.1199811
# PD_central_pain  0.03603964 -0.22230740 -0.158297182 -0.26741870 -0.1826106
# PD_musculo_pain -0.33025412  0.34444467  0.122974670  0.28069108 -0.1289693
#                          C          ST Moca_total_score         Age
# NO_pain_PD       0.0126115 -0.07893267      -0.09863573  0.09609794
# PD_central_pain -0.7737153 -0.11233922       0.25480896 -0.24330340
# PD_musculo_pain  0.5351861  0.25391800       0.02988961 -0.03272030
#                        Sexe duree_maladie  HAD_total   ORT_total
# NO_pain_PD       0.01634956   -0.03372410 -0.2227242 -0.06868637
# PD_central_pain  0.89513857    0.05834805  0.2569064  0.02014112
# PD_musculo_pain -0.68668164    0.03114491  0.2991026  0.13521308
#                 MDS_UPDRS_tot1 MDS_UPDRS_tot2 MDS_UPDRS_tot3  HOEHN_YAHR
# NO_pain_PD          -0.4347488     -0.2281680    -0.14380964 -0.08735453
# PD_central_pain      0.8933333      0.2630851     0.05362116  0.19088583
# PD_musculo_pain      0.2988459      0.3064864     0.27476929  0.05176565
#                 MDS_UPDRS_tot4  DEL_total
# NO_pain_PD         -0.27523350 -0.1716285
# PD_central_pain     0.70896946  0.9291696
# PD_musculo_pain     0.08489531 -0.3012976
# 
# Coefficients of linear discriminants:
#                          LD1         LD2
# NS               -0.34256032 -0.31533549
# HA               -0.76178727 -0.39692622
# RD                0.17239656 -0.06729200
# P                -0.31992580  0.26137862
# SD                0.16895384 -0.14313514
# C                 0.07133565  0.54156847
# ST               -0.46038758 -0.50848084
# Moca_total_score -0.15307626  0.16694215
# Age              -0.20098797  0.06135965
# Sexe              1.05839337 -0.51256000
# duree_maladie     0.42149255 -0.35257531
# HAD_total         0.06422445  0.02950620
# ORT_total        -0.09128542  0.07033752
# MDS_UPDRS_tot1    1.88033760  0.81669859
# MDS_UPDRS_tot2   -0.32235570 -0.06050026
# MDS_UPDRS_tot3   -0.59766115  0.09456563
# HOEHN_YAHR        0.17004475  0.21314086
# MDS_UPDRS_tot4   -0.98646887  1.17918874
# DEL_total         1.15103630 -0.61564139
# 
# Proportion of trace:
#    LD1    LD2 
# 0.8124 0.1876 

print(lda_model$scaling)


#                           LD1         LD2
# NS               -0.035150773  0.17254184
# HA                0.593341620  0.52304756
# RD               -0.332333198  0.16218380
# P                 0.556173981 -0.80116087
# SD               -1.063926954  0.62598400
# C                -0.563159007  0.28114569
# ST                0.818926732  0.26338414
# Moca_total_score  0.717529170 -0.55096431
# Age               0.678797827 -0.47357939
# Sexe             -2.624834107  0.64241579
# duree_maladie    -1.852716730  0.71727715
# HAD_total        -0.002167688  0.62808989
# ORT_total        -0.683886777  0.03906981
# MDS_UPDRS_tot1   -2.817355886 -1.03168460
# MDS_UPDRS_tot2    0.217800562  0.55529244
# MDS_UPDRS_tot3    1.377318222 -0.08136543
# HOEHN_YAHR       -0.177333234 -0.58083810
# MDS_UPDRS_tot4    2.399844928 -1.88545673
# DEL_total        -1.526210514  1.04000199





library(tidyverse)

# Create the loadings table (NEW DATA)
loadings <- tribble(
  ~Variable,               ~LD1,           ~LD2,
  "NS",                -0.035150773,   0.17254184,
  "HA",                 0.593341620,   0.52304756,
  "RD",                -0.332333198,   0.16218380,
  "P",                  0.556173981,  -0.80116087,
  "SD",                -1.063926954,   0.62598400,
  "C",                 -0.563159007,   0.28114569,
  "ST",                 0.818926732,   0.26338414,
  "Moca_total_score",   0.717529170,  -0.55096431,
  "Age",                0.678797827,  -0.47357939,
  "Sexe",              -2.624834107,   0.64241579,
  "duree_maladie",     -1.852716730,   0.71727715,
  "HAD_total",         -0.002167688,   0.62808989,
  "ORT_total",         -0.683886777,   0.03906981,
  "MDS_UPDRS_tot1",    -2.817355886,  -1.03168460,
  "MDS_UPDRS_tot2",     0.217800562,   0.55529244,
  "MDS_UPDRS_tot3",     1.377318222,  -0.08136543,
  "HOEHN_YAHR",        -0.177333234,  -0.58083810,
  "MDS_UPDRS_tot4",     2.399844928,  -1.88545673,
  "DEL_total",         -1.526210514,   1.04000199
)

# Convert to long format
loadings_long <- loadings %>%
  pivot_longer(cols = c(LD1, LD2),
               names_to = "LD",
               values_to = "Loading")

# Plot
plot <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~LD, scales = "free_x") +
  scale_fill_gradient2(low = "#C597C9", high = "#335D87", mid = "grey90") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Loadings for LD1 and LD2",
    x = "Variable \n",
    y = "\n Loading"
  ) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
theme(panel.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      strip.background = element_blank(), 
      #strip.text = element_blank(), 
      axis.line = element_blank(), 
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10), 
      axis.title.x = element_text(size = 12, vjust = -0.5),
      axis.title.y = element_text(size = 12, vjust = -0.5), 
      plot.margin = margin(5, 5, 5, 5, "pt"))



ggsave(file="test.svg", plot=plot, width=6, height=4)


lda_scores <- as.data.frame(predict(lda_model)$x) %>%
  mutate(groupes = groups)


centroids <- lda_scores %>%
  group_by(groupes) %>%
  summarise(across(everything(), mean))  


#   groupes             LD1    LD2
#   <fct>             <dbl>  <dbl>
# 1 NO_pain_PD      -0.522 -0.602
# 2 PD_central_pain  3.14   0.262
# 3 PD_musculo_pain -1.15   1.12 

unique(lda_scores$groupes)

# Predicted class memberships
lda_pred <- predict(lda_model)$class

# Confusion matrix
conf_mat <- table(Predicted = lda_pred, Actual = groups)
conf_mat

# Classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy # 0.88










library(pROC)
library(tidyverse)

# Predict class probabilities
lda_pred <- predict(lda_model)
post_probs <- lda_pred$posterior  # posterior probabilities for each class

# Create ROC data frame for plotting
roc_df <- map_dfr(colnames(post_probs), function(cls) {
  roc_obj <- roc(response = groups == cls,
                 predictor = post_probs[, cls])
  data.frame(
    LD_class = cls,
    sensitivity = rev(roc_obj$sensitivities),
    specificity = rev(1 - roc_obj$specificities)
  )
})


# Compute AUCs separately
auc_df <- map_dfr(colnames(post_probs), function(cls) {
  data.frame(
    LD_class = cls,
    auc = as.numeric(auc(roc(groups == cls, post_probs[, cls])))
  )
})

# Plot
plot <- ggplot(roc_df, aes(x = specificity, y = sensitivity, color = LD_class)) +
  geom_line(size = 2.2, alpha=0.85) +
  geom_abline(linetype = "dashed") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(title = "ROC Curves for LDA Classes",
       x = "\n 1 - Specificity",
       y = "Sensitivity \n",
       color = "Group") +
  scale_colour_manual(values=c("gray", "#A32121", "#335D87")) +
  geom_text(
    data = auc_df,
    aes(x = 0.75, y = 0.25 - 0.05*as.numeric(factor(LD_class)),
        label = paste0("AUC ", LD_class, ": ", round(auc, 2))),
    inherit.aes = FALSE
  )


ggsave(file="test.svg", plot=plot, width=7, height=7)










# Fix group names in lda_scores
lda_scores <- lda_scores %>%
  mutate(groupes = case_when(
    groupes == "PD_central_pain"  ~ "PD_Central",
    groupes == "PD_musculo_pain"  ~ "PD_Musculo",
    TRUE                          ~ "No_pain"
  ))

# Fix group names in centroids (if needed)
centroids <- centroids %>%
  mutate(groupes = case_when(
    groupes == "PD_central_pain"  ~ "PD_Central",
    groupes == "PD_musculo_pain"  ~ "PD_Musculo",
    TRUE                          ~ "No_pain"
  ))


# Quadrants
quadrants <- data.frame(
  xmin = c(-Inf, 1.152, -Inf, 1.152),
  xmax = c(1.152, Inf, 1.152, Inf),
  ymin = c(-Inf, -Inf, 0.0445, 0.0445),
  ymax = c(0.0445, 0.0445, Inf, Inf),
  fill = factor(c("Q1", "Q2", "Q3", "Q4"))
)



quad_colors <- c(
  "Q1" = "#DBC44D",
  "Q2" = "#335D87",
  "Q2" = "#A32121",
  "Q4" = "#335D87"
)

# Updated colors for groups (matching the corrected names)
group_colors <- c(
  "PD_Central" = "#A32121",
  "PD_Musculo" = "#335D87",
  "No_pain"    = "#DBC44D"
)

# Updated shapes for groups
group_shapes <- c(
  "PD_Central" = 16,
  "PD_Musculo" = 17,
  "No_pain"    = 15
)

# Plot
plot <- ggplot() +
#  geom_rect(data = quadrants,
#            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
#            alpha = 0.2,
#            inherit.aes = FALSE) +
  
  geom_point(data = lda_scores,
             aes(LD1, LD2, color = groupes, shape = groupes),
             size = 3, stroke = 2) +
  
  stat_ellipse(data = lda_scores,
               aes(LD1, LD2, color = groupes),
               level = 0.68, size = 2, alpha = 0.6) +
  
  geom_text(data = centroids,
            aes(LD1, LD2, label = groupes),
            size = 4, fontface = "bold", vjust = -1) +
  
  geom_vline(xintercept = 1.152) +
  geom_hline(yintercept = 0.0445) +
  
  scale_fill_manual(values = quad_colors, drop = FALSE) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = group_shapes) +
  
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  
  labs(title = "LDA — Group Separation",
       subtitle = "Ellipses 68% confidence regions")



ggsave(file="test.svg", plot=plot, width=6, height=5)






library(dplyr)
library(tidyr)
library(purrr)


groups <- unique(lda_scores$groupes)

# Generate all pairwise combinations
pairs <- t(combn(groups, 2)) %>% as.data.frame()
colnames(pairs) <- c("g1", "g2")

run_tests <- function(dim) {
  pairs %>%
    rowwise() %>%
    mutate(
      stat = list(wilcox.test(
        lda_scores[[dim]][lda_scores$groupes == g1],
        lda_scores[[dim]][lda_scores$groupes == g2]
      )),
      p_value = stat$p.value
    ) %>%
    dplyr::select(g1, g2, p_value)
}

LD1_results <- run_tests("LD1")
LD2_results <- run_tests("LD2")


p_to_sig <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}


make_heatmap_df <- function(results) {
  
  groups <- sort(unique(c(results$g1, results$g2)))
  
  expand_grid(g1 = groups, g2 = groups) %>%
    left_join(results, by = c("g1", "g2")) %>%
    left_join(results, by = c("g1" = "g2", "g2" = "g1"),
              suffix = c("", "_rev")) %>%
    mutate(
      p = coalesce(p_value, p_value_rev),
      sig = p_to_sig(p),
      p = ifelse(g1 == g2, NA, p),
      sig = ifelse(g1 == g2, "", sig)
    )
}


LD1_hm <- make_heatmap_df(LD1_results)
LD2_hm <- make_heatmap_df(LD2_results)



plot_sig_heatmap <- function(df, title) {
  
  ggplot(df, aes(g1, g2, fill = sig)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig), size = 6, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "ns"  = "#CFE1E8",
        "*"   = "#72B9D6",
        "**"  = "#347F9E",
        "***" = "#125470"
      ),
      drop = FALSE
    ) +
    coord_equal() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


p_LD1 <- plot_sig_heatmap(LD1_hm, "LD1 – Pairwise Wilcox")
p_LD2 <- plot_sig_heatmap(LD2_hm, "LD2 – Pairwise Wilcox")

p_LD1
p_LD2

ggsave(file="test2.svg", plot=p_LD2, width=5, height=5)





# ----------------

# LDA: 4 pain groups, TCI variables  --------------

Dataset_PSYCHO_PAIN_2_Feuil1 <- read_xlsx(path="../data/Dataset PSYCHO-PAIN_2.xlsx", trim_ws = TRUE, sheet="Feuil1")

unique(Dataset_PSYCHO_PAIN_2_Feuil1$Disease)
unique(Dataset_PSYCHO_PAIN_2_Feuil1$groupes)

Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(groupes!="NO_pain_PD")  %>%
  group_by(groupes) %>% count()



pd_groups_extended <- Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(groupes!="NO_pain_PD") %>%
  select(Subjects , Number, groupes, NS, HA, RD, P, SD, C, ST) 
  


# Keep only numeric vars
df_num <- pd_groups_extended %>%
  select(where(is.numeric)) %>%
  drop_na()  # optional

# Keep groups separately
groups <- pd_groups_extended$groupes



library(MASS)

lda_model <- lda(groupes ~ ., data = cbind(df_num, groupes = groups))

# Call:
# lda(groupes ~ ., data = cbind(df_num, groupes = groups))
# 
# Prior probabilities of groups:
#           fibro        migraine PD_central_pain PD_musculo_pain 
#       0.3521127       0.3521127       0.1267606       0.1690141 
# 
# Group means:
#                       NS       HA       RD        P       SD        C       ST
# fibro           15.40000 25.56000 17.40000 6.160000 29.52000 34.84000 14.56000
# migraine        18.76000 19.44000 15.96000 5.480000 32.84000 32.72000 14.32000
# PD_central_pain 18.33333 16.44444 16.11111 4.666667 33.11111 31.44444 15.00000
# PD_musculo_pain 16.25000 19.91667 16.58333 5.916667 33.75000 36.75000 15.83333
# 
# Coefficients of linear discriminants:
#            LD1         LD2          LD3
# NS  0.01988770 -0.01742850  0.192585621
# HA -0.12024283 -0.01747998  0.112308738
# RD -0.03399000 -0.09222402 -0.196531360
# P  -0.23604459 -0.16655504  0.366227775
# SD -0.01487281  0.05659304  0.114143916
# C  -0.11605090  0.17279144  0.007358172
# ST  0.01771421  0.06045375 -0.018810490
# 
# Proportion of trace:
#    LD1    LD2    LD3 
# 0.7549 0.1500 0.0951 




lda_scores <- as.data.frame(predict(lda_model)$x) %>%
  mutate(groupes = groups)


lda_model$scaling

# 
#            LD1         LD2          LD3
# NS  0.01988770 -0.01742850  0.192585621
# HA -0.12024283 -0.01747998  0.112308738
# RD -0.03399000 -0.09222402 -0.196531360
# P  -0.23604459 -0.16655504  0.366227775
# SD -0.01487281  0.05659304  0.114143916
# C  -0.11605090  0.17279144  0.007358172
# ST  0.01771421  0.06045375 -0.018810490





library(tidyverse)

# Create the loadings table
loadings <- tribble(
  ~Variable,               ~LD1,           ~LD2,       
  "NS",        0.01988770,  -0.01742850,
  "HA",     -0.12024283,   -0.01747998,
  "RD",      -0.03399000,  -0.09222402,
  "P",           -0.23604459,   -0.16655504,
  "SD",       -0.01487281,   0.05659304,
  "C",       -0.11605090,  0.17279144,
  "ST",         0.01771421,   0.06045375
)

# Convert to long format
loadings_long <- loadings %>%
  pivot_longer(cols = starts_with("LD"),
               names_to = "LD",
               values_to = "Loading")

# Plot
plot <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~LD, scales = "free_x") +
  scale_fill_gradient2(low = "#C597C9", high = "#335D87", mid = "grey90") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Loadings for LD1 and LD2",
    x = "Variable \n",
    y = "\n Loading"
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) 



ggsave(file="test.svg", plot=plot, width=6, height=3)






centroids <- lda_scores %>%
  group_by(groupes) %>%
  summarise(across(c(LD1, LD2), mean))

dist_matrix <- dist(centroids[, c("LD1", "LD2")])
as.matrix(dist_matrix)

centroids

#   groupes            LD1     LD2
# 1 PD_central_pain  1.15  -0.0749
# 2 PD_musculo_pain -0.234  0.652 
# 3 fibro           -0.753 -0.194 
# 4 migraine         0.452 -0.0923

# Predicted class memberships
lda_pred <- predict(lda_model)$class

# Confusion matrix
conf_mat <- table(Predicted = lda_pred, Actual = groups)
conf_mat

# Classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy # 0.45













quadrants <- data.frame(
  xmin = c(-Inf, 0.15325, -Inf, 0.15325),
  xmax = c(0.15325, Inf, 0.15325, Inf),
  ymin = c(-Inf, -Inf, 0.2658, 0.2658),
  ymax = c(0.2658, 0.2658, Inf, Inf),
  fill = factor(c("Q1", "Q2", "Q3", "Q4"))
)

quad_colors <- c("Q1" = "#A32121", "Q2" = "#DBC44D", "Q3" = "#335D87", "Q4" = "#C597C9")


plot <- ggplot(lda_scores, aes(LD1, LD2, color = groupes,  shape=groupes)) +
  geom_point(size = 3, alpha = 0.9, stroke=2) +
  stat_ellipse(level = 0.68, size=2, alpha=0.6) +
  geom_vline(xintercept = 0.15325) +
  geom_hline(yintercept = 0.2658) +
#  geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
 #           alpha = 0.2, inherit.aes = FALSE) +
  geom_text(data = centroids, aes(LD1, LD2, label = groupes),
            size = 4, fontface = "bold", vjust = -1) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(title = "LDA — Group Separation",
       subtitle = "Ellipses show 68% confidence regions for each group") +
#  scale_fill_manual(values=c("#A32121", "#C597C9", "#335D87", "gray")) +
  scale_colour_manual(values=c("#C597C9", "gray", "#A32121", "#335D87")) 

ggsave(file="test.svg", plot=plot, width=6, height=6)





library(dplyr)
library(tidyr)
library(purrr)


groups <- unique(lda_scores$groupes)

# Generate all pairwise combinations
pairs <- t(combn(groups, 2)) %>% as.data.frame()
colnames(pairs) <- c("g1", "g2")

run_tests <- function(dim) {
  pairs %>%
    rowwise() %>%
    mutate(
      stat = list(wilcox.test(
        lda_scores[[dim]][lda_scores$groupes == g1],
        lda_scores[[dim]][lda_scores$groupes == g2]
      )),
      p_value = stat$p.value
    ) %>%
    dplyr::select(g1, g2, p_value)
}

LD1_results <- run_tests("LD1")
LD2_results <- run_tests("LD2")


p_to_sig <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}


make_heatmap_df <- function(results) {
  
  groups <- sort(unique(c(results$g1, results$g2)))
  
  expand_grid(g1 = groups, g2 = groups) %>%
    left_join(results, by = c("g1", "g2")) %>%
    left_join(results, by = c("g1" = "g2", "g2" = "g1"),
              suffix = c("", "_rev")) %>%
    mutate(
      p = coalesce(p_value, p_value_rev),
      sig = p_to_sig(p),
      p = ifelse(g1 == g2, NA, p),
      sig = ifelse(g1 == g2, "", sig)
    )
}


LD1_hm <- make_heatmap_df(LD1_results)
LD2_hm <- make_heatmap_df(LD2_results)



plot_sig_heatmap <- function(df, title) {
  
  ggplot(df, aes(g1, g2, fill = sig)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig), size = 6, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "ns"  = "#CFE1E8",
        "*"   = "#72B9D6",
        "**"  = "#347F9E",
        "***" = "#125470"
      ),
      drop = FALSE
    ) +
    coord_equal() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


p_LD1 <- plot_sig_heatmap(LD1_hm, "LD1 – Pairwise Wilcox")
p_LD2 <- plot_sig_heatmap(LD2_hm, "LD2 – Pairwise Wilcox")

p_LD1
p_LD2

ggsave(file="test2.svg", plot=p_LD2, width=5, height=5)



# ----------------

# LDA: 4 pain groups, TCI + extended variables  --------------

Dataset_PSYCHO_PAIN_2_Feuil1 <- read_xlsx(path="../data/Dataset PSYCHO-PAIN_2.xlsx", trim_ws = TRUE, sheet="Feuil1")

unique(Dataset_PSYCHO_PAIN_2_Feuil1$Disease)
unique(Dataset_PSYCHO_PAIN_2_Feuil1$groupes)

Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(groupes!="NO_pain_PD")  %>%
  group_by(groupes) %>% count()


names(Dataset_PSYCHO_PAIN_2_Feuil1)



pd_groups_extended <- Dataset_PSYCHO_PAIN_2_Feuil1 %>% filter(groupes!="NO_pain_PD") %>%
  select(Subjects , Number, groupes, NS, HA, RD, P, SD, C, ST, EVA_moy, BPI_score_tot, 
         CPAQ8_acceptation, CPAQ8_engagement, CPAQ8_total, McGill_sensori, McGill_affectif, McGill_total,
         HAD_A, HAD_D, HAD_total, PCS_total, ORT_total) 
  


# Keep only numeric vars
df_num <- pd_groups_extended %>%
  select(where(is.numeric)) %>%
  drop_na()  # optional

# Keep groups separately
groups <- pd_groups_extended$groupes



library(MASS)

lda_model <- lda(groupes ~ ., data = cbind(df_num, groupes = groups))

# Prior probabilities of groups:
#           fibro        migraine PD_central_pain PD_musculo_pain 
#       0.3521127       0.3521127       0.1267606       0.1690141 
# 
# Group means:
#                       NS       HA       RD        P       SD        C       ST  EVA_moy BPI_score_tot
# fibro           15.40000 25.56000 17.40000 6.160000 29.52000 34.84000 14.56000 64.16000      38.24000
# migraine        18.76000 19.44000 15.96000 5.480000 32.84000 32.72000 14.32000 62.96000      33.72000
# PD_central_pain 18.33333 16.44444 16.11111 4.666667 33.11111 31.44444 15.00000 57.33333      31.88889
# PD_musculo_pain 16.25000 19.91667 16.58333 5.916667 33.75000 36.75000 15.83333 55.33333      20.66667
#                 CPAQ8_acceptation CPAQ8_engagement CPAQ8_total McGill_sensori McGill_affectif McGill_total
# fibro                    7.120000         11.08000    18.20000       16.88000        5.280000     22.16000
# migraine                 5.000000         12.56000    17.56000       12.48000        5.880000     18.36000
# PD_central_pain          8.333333         17.44444    25.77778       12.33333        5.111111     17.44444
# PD_musculo_pain          8.583333         16.25000    24.83333       12.91667        4.000000     16.91667
#                     HAD_A    HAD_D HAD_total PCS_total ORT_total
# fibro           10.840000 8.400000  19.24000  28.40000 4.4400000
# migraine         8.640000 6.400000  15.04000  26.76000 2.2000000
# PD_central_pain  8.555556 5.000000  13.55556  19.22222 0.7777778
# PD_musculo_pain  8.166667 5.166667  13.33333  18.83333 1.0000000
# 
# Coefficients of linear discriminants:
#                            LD1          LD2          LD3
# NS                -0.005853672 -0.049406666  0.148161711
# HA                -0.041026216  0.070781715  0.072538912
# RD                -0.044584325 -0.006147083 -0.155620692
# P                 -0.180958355  0.212523025  0.363244367
# SD                -0.030283909 -0.006983668  0.081636984
# C                 -0.117218094  0.036335449  0.068534880
# ST                 0.032279637  0.015397575  0.011468546
# EVA_moy           -0.018728550 -0.012838401  0.007847165
# BPI_score_tot     -0.015633428 -0.080916427 -0.062132431
# CPAQ8_acceptation -0.017568071  0.103071308 -0.077079010
# CPAQ8_engagement   0.050262774 -0.004095125 -0.015688656
# CPAQ8_total        0.023049443  0.031647018 -0.034426952
# McGill_sensori    -0.073289122  0.041750983 -0.009210875
# McGill_affectif    0.223328291 -0.144032305  0.065893796
# McGill_total      -0.015742828  0.006626596  0.003291509
# HAD_A              0.056490151 -0.023079841 -0.127509697
# HAD_D             -0.087127025  0.084880659  0.111735120
# HAD_total         -0.009012031  0.019994122 -0.007767928
# PCS_total         -0.029323816 -0.018882536  0.016891955
# ORT_total         -0.169389513 -0.130638853 -0.007679716
# 
# Proportion of trace:
#    LD1    LD2    LD3 
# 0.5316 0.3491 0.1193 

lda_scores <- as.data.frame(predict(lda_model)$x) %>%
  mutate(groupes = groups)


lda_model$scaling


#                            LD1          LD2          LD3
# NS                -0.005853672 -0.049406666  0.148161711
# HA                -0.041026216  0.070781715  0.072538912
# RD                -0.044584325 -0.006147083 -0.155620692
# P                 -0.180958355  0.212523025  0.363244367
# SD                -0.030283909 -0.006983668  0.081636984
# C                 -0.117218094  0.036335449  0.068534880
# ST                 0.032279637  0.015397575  0.011468546
# EVA_moy           -0.018728550 -0.012838401  0.007847165
# BPI_score_tot     -0.015633428 -0.080916427 -0.062132431
# CPAQ8_acceptation -0.017568071  0.103071308 -0.077079010
# CPAQ8_engagement   0.050262774 -0.004095125 -0.015688656
# CPAQ8_total        0.023049443  0.031647018 -0.034426952
# McGill_sensori    -0.073289122  0.041750983 -0.009210875
# McGill_affectif    0.223328291 -0.144032305  0.065893796
# McGill_total      -0.015742828  0.006626596  0.003291509
# HAD_A              0.056490151 -0.023079841 -0.127509697
# HAD_D             -0.087127025  0.084880659  0.111735120
# HAD_total         -0.009012031  0.019994122 -0.007767928
# PCS_total         -0.029323816 -0.018882536  0.016891955
# ORT_total         -0.169389513 -0.130638853 -0.007679716



# Create the loadings table (NEW DATA – LD1 & LD2 only)
loadings <- tribble(
  ~Variable,               ~LD1,            ~LD2,
  "NS",                -0.005853672,  -0.049406666,
  "HA",                -0.041026216,   0.070781715,
  "RD",                -0.044584325,  -0.006147083,
  "P",                 -0.180958355,   0.212523025,
  "SD",                -0.030283909,  -0.006983668,
  "C",                 -0.117218094,   0.036335449,
  "ST",                 0.032279637,   0.015397575,
  "EVA_moy",           -0.018728550,  -0.012838401,
  "BPI_score_tot",     -0.015633428,  -0.080916427,
  "CPAQ8_acceptation", -0.017568071,   0.103071308,
  "CPAQ8_engagement",   0.050262774,  -0.004095125,
  "CPAQ8_total",        0.023049443,   0.031647018,
  "McGill_sensori",    -0.073289122,   0.041750983,
  "McGill_affectif",    0.223328291,  -0.144032305,
  "McGill_total",      -0.015742828,   0.006626596,
  "HAD_A",              0.056490151,  -0.023079841,
  "HAD_D",             -0.087127025,   0.084880659,
  "HAD_total",         -0.009012031,   0.019994122,
  "PCS_total",         -0.029323816,  -0.018882536,
  "ORT_total",         -0.169389513,  -0.130638853
)

# Convert to long format
loadings_long <- loadings %>%
  pivot_longer(cols = c(LD1, LD2),
               names_to = "LD",
               values_to = "Loading")

# Plot
plot <- ggplot(loadings_long, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~LD, scales = "free_x") +
  scale_fill_gradient2(low = "#C597C9", high = "#335D87", mid = "grey90") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Loadings for LD1 and LD2",
    x = "Variable \n",
    y = "\n Loading"
  ) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        #strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12, 
                                    vjust = -0.5), 
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt"))


ggsave(file="test.svg", plot=plot, width=6, height=4)




centroids <- lda_scores %>%
  group_by(groupes) %>%
  summarise(across(c(LD1, LD2), mean))

dist_matrix <- dist(centroids[, c("LD1", "LD2")])
as.matrix(dist_matrix)

centroids


# Predicted class memberships
lda_pred <- predict(lda_model)$class

# Confusion matrix
conf_mat <- table(Predicted = lda_pred, Actual = groups)
conf_mat

# Classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy # 0.82









plot <- ggplot(lda_scores, aes(LD1, LD2, color = groupes, fill=groupes, shape=groupes)) +
  geom_point(size = 3, alpha = 0.9, stroke=2) +
  stat_ellipse(level = 0.68, size=2, alpha=0.6) +
 geom_vline(xintercept = 0) +
 geom_hline(yintercept = 0) +
  geom_text(data = centroids, aes(LD1, LD2, label = groupes),
            size = 4, fontface = "bold", vjust = -1) +
    theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(title = "LDA — Group Separation",
       subtitle = "Ellipses show 68% confidence regions for each group") +
  scale_fill_manual(values=c("#C597C9", "gray", "#A32121", "#335D87")) +
  scale_colour_manual(values=c("#C597C9", "gray", "#A32121", "#335D87")) 

ggsave(file="test.svg", plot=plot, width=6, height=5)





library(dplyr)
library(purrr)

groups <- unique(lda_scores$groupes)

# All pairwise combinations
pairs <- t(combn(groups, 2)) %>% as.data.frame()
colnames(pairs) <- c("g1", "g2")

run_tests <- function(dim) {

  pairs %>%
    rowwise() %>%
    mutate(
      stat = list(
        wilcox.test(
          lda_scores[[dim]][lda_scores$groupes == g1],
          lda_scores[[dim]][lda_scores$groupes == g2]
        )
      ),
      p_value = stat$p.value
    ) %>%
    dplyr::select(g1, g2, p_value) %>%   # explicitly use dplyr
    ungroup()
}

LD1_results <- run_tests("LD1")
LD2_results <- run_tests("LD2")


p_to_sig <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}


make_heatmap_df <- function(results) {
  
  groups <- sort(unique(c(results$g1, results$g2)))
  
  expand_grid(g1 = groups, g2 = groups) %>%
    left_join(results, by = c("g1", "g2")) %>%
    left_join(results, by = c("g1" = "g2", "g2" = "g1"),
              suffix = c("", "_rev")) %>%
    mutate(
      p = coalesce(p_value, p_value_rev),
      sig = p_to_sig(p),
      p = ifelse(g1 == g2, NA, p),
      sig = ifelse(g1 == g2, "", sig)
    )
}


LD1_hm <- make_heatmap_df(LD1_results)
LD2_hm <- make_heatmap_df(LD2_results)



plot_sig_heatmap <- function(df, title) {
  
  ggplot(df, aes(g1, g2, fill = sig)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig), size = 6, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "ns"  = "#CFE1E8",
        "*"   = "#72B9D6",
        "**"  = "#347F9E",
        "***" = "#125470"
      ),
      drop = FALSE
    ) +
    coord_equal() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}


p_LD1 <- plot_sig_heatmap(LD1_hm, "LD1 – Pairwise Wilcox")
p_LD2 <- plot_sig_heatmap(LD2_hm, "LD2 – Pairwise Wilcox")

p_LD1
p_LD2

ggsave(file="test.svg", plot=p_LD2, width=5, height=5)


# ----------------
