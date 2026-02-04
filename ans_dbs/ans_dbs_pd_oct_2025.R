library(tidyverse) 
library(data.table)
library(readxl)

library(FactoMineR)
library(missMDA)

# SCOPA -----------------------

SCOPA_PREOP <- read_excel(path = "../data/SCOPA.xlsx", sheet="PREOP")

# Select only numeric SCOPA columns (exclude names and dates)
scopa_vars <- SCOPA_PREOP %>%
  select(`SO-digestif`, `SO-urinaire`, `SO-orthostatisme`,
         `SO-sensitif`, `SO-photosensibilité`, `SO-gynéco`)

# Step 1: Estimate optimal number of dimensions for PCA imputation
nb <- estim_ncpPCA(scopa_vars, ncp.max = 5)  # tries 1–5 dimensions

# Step 2: Perform PCA-based imputation
imputed <- imputePCA(scopa_vars, ncp = nb$ncp)

SCOPA_PREOP[,4:9] <- imputed$completeObs

SCOPA_PREOP$TOTAL <- rowSums(SCOPA_PREOP[,4:9], na.rm = TRUE)



SCOPA_OFF <- read_excel(path = "../data/SCOPA.xlsx", sheet="POST OP STIM OFF MED ON")

# Select only numeric SCOPA columns (exclude names and dates)
scopa_vars <- SCOPA_OFF %>%
  select(`SO-digestif OFF`, `SO-urinaire OFF`, `SO-ortho OFF`,
         `SO-Transpiration OFF`, `SO-photo OFF`, `SO-gynéco OFF`)

# Step 1: Estimate optimal number of dimensions for PCA imputation
nb <- estim_ncpPCA(scopa_vars, ncp.max = 5)  # tries 1–5 dimensions

# Step 2: Perform PCA-based imputation
imputed <- imputePCA(scopa_vars, ncp = nb$ncp)

SCOPA_OFF[,4:9] <- imputed$completeObs

SCOPA_OFF$TOTAL <- rowSums(SCOPA_OFF[,4:9], na.rm = TRUE)





SCOPA_ON <- read_excel(path = "../data/SCOPA.xlsx", sheet="POST OP STIM ON MED ON")

# Select only numeric SCOPA columns (exclude names and dates)
scopa_vars <- SCOPA_ON %>%
  select(`SO-digestif ON`, `SO-urinaire ON`, `SO-ortho ON`,
         `SO-transpi ON`, `SO-photo ON`, `SO-gynéco ON`)

# Step 1: Estimate optimal number of dimensions for PCA imputation
nb <- estim_ncpPCA(scopa_vars, ncp.max = 5)  # tries 1–5 dimensions

# Step 2: Perform PCA-based imputation
imputed <- imputePCA(scopa_vars, ncp = nb$ncp)

SCOPA_ON[,4:9] <- imputed$completeObs

SCOPA_ON$TOTAL <- rowSums(SCOPA_ON[,4:9], na.rm = TRUE)






library(rstatix)


SCOPA_DIGESTIVE <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-digestif`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-digestif OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-digestif ON`)
  ) %>%
  gather(Eval, Score,  `SO-digestif`:`SO-digestif ON`)


SCOPA_DIGESTIVE %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval             mean    sd median    q1    q3
# <fct>           <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 PREOP            4.33  2.64    4       3  5.25
# 2 STIM-OFF MED-ON  3.77  2.31    3.5     2  4.71
# 3 ON/ON            2.92  2.75    2       1  4   


res.fried <- SCOPA_DIGESTIVE %>% friedman_test(Score ~ Eval |NOM)
res.fried

# .y.       n statistic    df     p method       
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>        
#   1 Score    12      3.67     2 0.159 Friedman test

# pairwise comparisons
pwc <- SCOPA_DIGESTIVE %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# .y.   group1          group2             n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>           <chr>           <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-digestif     SO-digestif OFF    12    12      39.5 0.59  1     ns          
# 2 Score SO-digestif     SO-digestif ON     12    12      57.5 0.156 0.468 ns          
# 3 Score SO-digestif OFF SO-digestif ON     12    12      28.5 0.158 0.474 ns    


unique(SCOPA_DIGESTIVE$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_DIGESTIVE <- SCOPA_DIGESTIVE %>%
  mutate(Eval=ifelse(Eval=="SO-digestif", "PREOP",
                     ifelse(Eval=="SO-digestif OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_DIGESTIVE <- SCOPA_DIGESTIVE %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_DIGESTIVE, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Digestif",
    x = "\n Evaluation",
    y = "SCOPA Digestive Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_dig.svg", plot=plot, width=5, height=5)








SCOPA_URINARY <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-urinaire`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-urinaire OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-urinaire ON`)
  ) %>%
  gather(Eval, Score,  `SO-urinaire`:`SO-urinaire ON`)


SCOPA_URINARY %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval             mean    sd median    q1    q3
# <chr>           <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 SO-urinaire      4.33  3.06      4  2.75  4.25
# 2 SO-urinaire OFF  4.97  3.97      3  2.75  6.22
# 3 SO-urinaire ON   4     3.22      3  2     4   


res.fried <- SCOPA_URINARY %>% friedman_test(Score ~ Eval |NOM)
res.fried

# .y.       n statistic    df     p method
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>
#   1 Score    12      3.67     2 0.159 Friedman test

# pairwise comparisons
pwc <- SCOPA_URINARY %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# .y.   group1          group2             n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>           <chr>           <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-urinaire     SO-urinaire OFF    12    12      31   0.553  1    ns          
# 2 Score SO-urinaire     SO-urinaire ON     12    12      27   0.632  1    ns          
# 3 Score SO-urinaire OFF SO-urinaire ON     12    12      26.5 0.26   0.78 ns      


unique(SCOPA_URINARY$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_URINARY <- SCOPA_URINARY %>%
  mutate(Eval=ifelse(Eval=="SO-urinaire", "PREOP",
                     ifelse(Eval=="SO-urinaire OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_URINARY <- SCOPA_URINARY %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_URINARY, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Urinary",
    x = "\n Evaluation",
    y = "SCOPA Urinary Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_uri.svg", plot=plot, width=5, height=5)











SCOPA_ortho <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-orthostatisme`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-ortho OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-ortho ON`)
  ) %>%
  gather(Eval, Score,  `SO-orthostatisme`:`SO-ortho ON`)


SCOPA_ortho %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval              mean    sd median    q1    q3
# <chr>            <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 SO-ortho OFF     0.909 0.996  0.952     0  1.25
# 2 SO-ortho ON      0.5   0.798  0         0  1   
# 3 SO-orthostatisme 0.917 1.24   0.5       0  1.25


res.fried <- SCOPA_ortho %>% friedman_test(Score ~ Eval |NOM)
res.fried

# .y.       n statistic    df     p method       
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>        
#   1 Score    12      1.75     2 0.417 Friedman test

# pairwise comparisons
pwc <- SCOPA_ortho %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# 
# .y.   group1       group2              n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>        <chr>            <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-ortho OFF SO-ortho ON         12    12      17.5 0.161 0.483 ns          
# 2 Score SO-ortho OFF SO-orthostatisme    12    12      10   1     1     ns          
# 3 Score SO-ortho ON  SO-orthostatisme    12    12       4   0.41  1     ns  

unique(SCOPA_ortho$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_ortho <- SCOPA_ortho %>%
  mutate(Eval=ifelse(Eval=="SO-orthostatisme", "PREOP",
                     ifelse(Eval=="SO-ortho OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_ortho <- SCOPA_ortho %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_ortho, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Ortho",
    x = "\n Evaluation",
    y = "SCOPA Ortho Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_ortho.svg", plot=plot, width=5, height=5)





SCOPA_sensitive <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-sensitif`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-Transpiration OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-transpi ON`)
  ) %>%
  gather(Eval, Score,  `SO-sensitif`:`SO-transpi ON`)


SCOPA_sensitive %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval                  mean    sd median    q1    q3
# <chr>                <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 SO-Transpiration OFF  2.33  2.46    2    0     3.25
# 2 SO-sensitif           3.42  3.09    3    0.75  5.5 
# 3 SO-transpi ON         2.08  2.54    1.5  0     3.25

res.fried <- SCOPA_sensitive %>% friedman_test(Score ~ Eval |NOM)
res.fried
# 
# .y.       n statistic    df     p method       
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>        
#   1 Score    12      2.18     2 0.337 Friedman test

# pairwise comparisons
pwc <- SCOPA_sensitive %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# 
# .y.   group1        group2                  n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>         <chr>                <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-sensitif   SO-transpi ON           12    12      32.5 0.26  0.78  ns          
# 2 Score SO-sensitif   SO-Transpiration OFF    12    12      41.5 0.165 0.495 ns          
# 3 Score SO-transpi ON SO-Transpiration OFF    12    12       4.5 0.48  1     ns 


unique(SCOPA_sensitive$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_sensitive <- SCOPA_sensitive %>%
  mutate(Eval=ifelse(Eval=="SO-sensitif", "PREOP",
                     ifelse(Eval=="SO-Transpiration OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_sensitive <- SCOPA_sensitive %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_sensitive, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Sensitive",
    x = "\n Evaluation",
    y = "SCOPA Sensitive Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_sensitive.svg", plot=plot, width=5, height=5)










SCOPA_photo <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-photosensibilité`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-photo OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-photo ON`)
  ) %>%
  gather(Eval, Score,  `SO-photosensibilité`:`SO-photo ON`)


SCOPA_photo %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval                 mean    sd median    q1    q3
# <chr>               <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 SO-photo OFF        0.5   0.798      0     0  1   
# 2 SO-photo ON         0.333 0.651      0     0  0.25
# 3 SO-photosensibilité 0.556 0.891      0     0  1 

res.fried <- SCOPA_photo %>% friedman_test(Score ~ Eval |NOM)
res.fried
# 
# .y.       n statistic    df     p method       
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>        
#   1 Score    12      1.14     2 0.565 Friedman test

# pairwise comparisons
pwc <- SCOPA_photo %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# .y.   group1       group2                 n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>        <chr>               <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-photo OFF SO-photo ON            12    12         3 0.346     1 ns          
# 2 Score SO-photo OFF SO-photosensibilité    12    12        11 1         1 ns          
# 3 Score SO-photo ON  SO-photosensibilité    12    12         8 0.672     1 ns  


unique(SCOPA_photo$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_photo <- SCOPA_photo %>%
  mutate(Eval=ifelse(Eval=="SO-photosensibilité", "PREOP",
                     ifelse(Eval=="SO-photo OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_photo <- SCOPA_photo %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_photo, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Photo",
    x = "\n Evaluation",
    y = "SCOPA Photo Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_photo.svg", plot=plot, width=5, height=5)










SCOPA_gyneco <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `SO-gynéco`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `SO-gynéco OFF`) 
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `SO-gynéco ON`)
  ) %>%
  gather(Eval, Score,  `SO-gynéco`:`SO-gynéco ON`)


SCOPA_gyneco %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval           mean    sd median    q1    q3
# <chr>         <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 SO-gynéco     0.861 1.34    0        0  1.17
# 2 SO-gynéco OFF 1.63  1.77    1.28     0  3   
# 3 SO-gynéco ON  0.598 0.679   0.5      0  1.06

res.fried <- SCOPA_gyneco %>% friedman_test(Score ~ Eval |NOM)
res.fried
# 
# .y.       n statistic    df      p method       
# * <chr> <int>     <dbl> <dbl>  <dbl> <chr>        
#   1 Score    12         5     2 0.0821 Friedman test

# pairwise comparisons
pwc <- SCOPA_gyneco %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# .y.   group1        group2           n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>         <chr>         <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score SO-gynéco     SO-gynéco OFF    12    12       6   0.203 0.609 ns          
# 2 Score SO-gynéco     SO-gynéco ON     12    12      18   1     1     ns          
# 3 Score SO-gynéco OFF SO-gynéco ON     12    12      25.5 0.062 0.186 ns    

unique(SCOPA_gyneco$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_gyneco <- SCOPA_gyneco %>%
  mutate(Eval=ifelse(Eval=="SO-gynéco", "PREOP",
                     ifelse(Eval=="SO-gynéco OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_gyneco <- SCOPA_gyneco %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_gyneco, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Gyneco",
    x = "\n Evaluation",
    y = "SCOPA Gyneco Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_gyneco.svg", plot=plot, width=5, height=5)











SCOPA_TOTAL <- SCOPA_PREOP %>%
  select(NOM, PRENOM, `TOTAL`) %>%
  inner_join(
    SCOPA_OFF %>%
      select(NOM, PRENOM, `TOTAL`) %>% rename("TOTAL OFF"="TOTAL")  
  ) %>%
  inner_join(
    SCOPA_ON %>%
      select(NOM, PRENOM, `TOTAL`) %>% rename("TOTAL ON"="TOTAL")
  ) %>%
  gather(Eval, Score,  `TOTAL`:`TOTAL ON`)


SCOPA_TOTAL %>%
  group_by(Eval) %>%
  summarise(mean=mean(Score), sd=sd(Score),
            median=median(Score), q1=quantile(Score, 0.25), q3=quantile(Score, 0.75))

# Eval       mean    sd median    q1    q3
# <chr>     <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 TOTAL      14.4  8.36  12.5   8.38  22.2
# 2 TOTAL OFF  14.1  8.78  11.6   7.25  22  
# 3 TOTAL ON   10.4  8.48   8.03  4.04  12.8

res.fried <- SCOPA_TOTAL %>% friedman_test(Score ~ Eval |NOM)
res.fried
# 
# .y.       n statistic    df     p method       
# * <chr> <int>     <dbl> <dbl> <dbl> <chr>        
# 1 Score    12      4.04     2 0.132 Friedman test

# pairwise comparisons
pwc <- SCOPA_TOTAL %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc
# .y.   group1    group2       n1    n2 statistic     p p.adj p.adj.signif
# * <chr> <chr>     <chr>     <int> <int>     <dbl> <dbl> <dbl> <chr>       
#   1 Score TOTAL     TOTAL OFF    12    12      41   0.91  1     ns          
# 2 Score TOTAL     TOTAL ON     12    12      48.5 0.182 0.546 ns          
# 3 Score TOTAL OFF TOTAL ON     12    12      46   0.066 0.199 ns  

unique(SCOPA_TOTAL$Eval)


# Make sure Eval is an ordered factor (if you have timepoints)
SCOPA_TOTAL <- SCOPA_TOTAL %>%
  mutate(Eval=ifelse(Eval=="TOTAL", "PREOP",
                     ifelse(Eval=="TOTAL OFF", "STIM-OFF MED-ON", "ON/ON"))) %>%
  mutate(Eval = factor(Eval, levels = c("PREOP", "STIM-OFF MED-ON", "ON/ON")))

# Create a unique patient ID
SCOPA_TOTAL <- SCOPA_TOTAL %>%
  mutate(ID = paste(NOM, PRENOM))

# Plot
plot <- ggplot(SCOPA_TOTAL, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "SCOPA Total",
    x = "\n Evaluation",
    y = "SCOPA Total Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA", "#730909")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA", "#730909")) 

ggsave(file="scopa_total.svg", plot=plot, width=5, height=5)



names(SCOPA_PREOP) <- c("NOM", "PRENOM", "Date finapress", "Digestive_Pre",
                        "Urinary_Pre","Orthos_Pre","Sensitive_Pre",
                        "Photo_Pre", "Gyneco_Pre","TOTAL_Pre") 


names(SCOPA_OFF) <- c("NOM", "PRENOM", "Date finapress", "Digestive_OFF",
                      "Urinary_OFF","Orthos_OFF","Sensitive_OFF",
                      "Photo_OFF", "Gyneco_OFF","TOTAL_OFF") 

 
names(SCOPA_ON) <- c("NOM", "PRENOM", "Date finapress", "Digestive_ON",
                      "Urinary_ON","Orthos_ON","Sensitive_ON",
                      "Photo_ON", "Gyneco_ON","TOTAL_ON") 



SCOPA_df <- SCOPA_PREOP %>% select(-`Date finapress`) %>%
  full_join(SCOPA_OFF %>% select(-`Date finapress`)) %>%
  full_join(SCOPA_ON %>% select(-`Date finapress`)) 
  
  
fwrite(SCOPA_df, "../data/SCOPA_df.txt")


# ------------------

# Baseline Demographics ---------

bdd09072025_demo <- read_excel(path = "../data/bdd09072025.xlsx", sheet="données démo")


bdd09072025_demo <- bdd09072025_demo %>%
  mutate(`DATE XIE` = case_when(
    # If it's a number stored as text, convert from Excel date serial
    grepl("^[0-9]+$", `DATE XIE`) ~ 
      as.character(as.Date(as.numeric(`DATE XIE`), origin = "1899-12-30")),
    
    # Otherwise, parse as a text date (day/month/year)
    TRUE ~ as.character(dmy(`DATE XIE`))
  )) %>%
  mutate(`DATE XIE` = as.Date(`DATE XIE`))

str(bdd09072025_demo$`DATE XIE`)

bdd09072025_demo$`DATE DE NAISSANCE` <- as.Date(bdd09072025_demo$`DATE DE NAISSANCE`)
bdd09072025_demo$`LEED POST OP 3 MOIS` <- as.numeric(bdd09072025_demo$`LEED POST OP 3 MOIS`)

# 32

bdd09072025_demo %>%
  summarise(mean=mean(AGE),
            sd=sd(AGE),
            median=median(AGE),
            q1=quantile(AGE, 0.25),
            q3=quantile(AGE, 0.75))

# mean    sd median    q1    q3
# <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  63.4  6.50   65.5  59.8    69


bdd09072025_demo %>%
  summarise(mean=mean(`DUREE MALADIE`),
            sd=sd(`DUREE MALADIE`),
            median=median(`DUREE MALADIE`),
            q1=quantile(`DUREE MALADIE`, 0.25),
            q3=quantile(`DUREE MALADIE`, 0.75))

# mean    sd median    q1    q3
# <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1  13.3  4.22   12.5    10    16



bdd09072025_demo %>%
  summarise(mean=mean(`DOSE DOPAMINE pre`),
            sd=sd(`DOSE DOPAMINE pre`),
            median=median(`DOSE DOPAMINE pre`),
            q1=quantile(`DOSE DOPAMINE pre`, 0.25),
            q3=quantile(`DOSE DOPAMINE pre`, 0.75))

# mean    sd median    q1    q3
# <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1 1635.  446.  1652. 1278. 1992.


bdd09072025_demo %>%
  summarise(mean=mean(`LEED POST OP 3 MOIS`),
            sd=sd(`LEED POST OP 3 MOIS`),
            median=median(`LEED POST OP 3 MOIS`),
            q1=quantile(`LEED POST OP 3 MOIS`, 0.25),
            q3=quantile(`LEED POST OP 3 MOIS`, 0.75))


# mean    sd median    q1    q3
# <dbl> <dbl>  <dbl> <dbl> <dbl>
#   1  452.  189.    425  340.  558.


wilcox.test(
  bdd09072025_demo$`DOSE DOPAMINE pre`,
  bdd09072025_demo$`LEED POST OP 3 MOIS`,
  paired = TRUE,
  exact = FALSE
)

# Wilcoxon signed rank test with continuity correction
# 
# data:  bdd09072025_demo$`DOSE DOPAMINE pre` and bdd09072025_demo$`LEED POST OP 3 MOIS`
# V = 528, p-value = 8.343e-07
# alternative hypothesis: true location shift is not equal to 0


# -------------


# Clinical demographics OFFs vs ONs -- OLD VERSION --------------

bdd09072025_PREOP <- read_excel(path = "../data/bdd09072025.xlsx", sheet="PREOP", skip=1)
bdd09072025_OFFON <- read_excel(path = "../data/bdd09072025.xlsx", sheet="POSTOP STIM OFF MED ON", skip=1)
bdd09072025_ONON <- read_excel(path = "../data/bdd09072025.xlsx", sheet="POST OP STIM ON MED ON", skip=1)

data.frame(names(bdd09072025_PREOP))
data.frame(names(bdd09072025_OFFON))

data.frame(names(bdd09072025_ONON))

library(dplyr)
library(rstatix)
library(tidyr)


bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS I`) %>% rename("UPDRS_I_PRE"="UPDRS I") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS I`) %>% rename("UPDRS_I_POST"="UPDRS I")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(UPDRS_I_PRE),
            sd_PRE=sd(UPDRS_I_PRE),
            median_PRE=median(UPDRS_I_PRE),
            q1_PRE=quantile(UPDRS_I_PRE, 0.25),
            q3_PRE=quantile(UPDRS_I_PRE, 0.75),
            mean_POST=mean(UPDRS_I_POST),
            sd_POST=sd(UPDRS_I_POST),
            median_POST=median(UPDRS_I_POST),
            q1_POST=quantile(UPDRS_I_POST, 0.25),
            q3_POST=quantile(UPDRS_I_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS I`) %>% rename("UPDRS_I_PRE"="UPDRS I") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS I`) %>% rename("UPDRS_I_POST"="UPDRS I")) %>%
  drop_na() 

wilcox.test(df$UPDRS_I_PRE, df$UPDRS_I_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `UPDRS_I_PRE`:`UPDRS_I_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="UPDRS_I_POST", "UPDRS I POST", "UPDRS I PRE")) %>%
    mutate(Eval = factor(Eval, levels = c("UPDRS I PRE", "UPDRS I POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "UPDRS I",
    x = "\n Evaluation",
    y = "UPDRS I Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="updrs_I.svg", plot=plot, width=5, height=5)









bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS II`) %>% rename("UPDRS_II_PRE"="UPDRS II") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS II`) %>% rename("UPDRS_II_POST"="UPDRS II")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(UPDRS_II_PRE),
            sd_PRE=sd(UPDRS_II_PRE),
            median_PRE=median(UPDRS_II_PRE),
            q1_PRE=quantile(UPDRS_II_PRE, 0.25),
            q3_PRE=quantile(UPDRS_II_PRE, 0.75),
            mean_POST=mean(UPDRS_II_POST),
            sd_POST=sd(UPDRS_II_POST),
            median_POST=median(UPDRS_II_POST),
            q1_POST=quantile(UPDRS_II_POST, 0.25),
            q3_POST=quantile(UPDRS_II_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS II`) %>% rename("UPDRS_II_PRE"="UPDRS II") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS II`) %>% rename("UPDRS_II_POST"="UPDRS II")) %>%
  drop_na() 

wilcox.test(df$UPDRS_II_PRE, df$UPDRS_II_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `UPDRS_II_PRE`:`UPDRS_II_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="UPDRS_II_POST", "UPDRS II POST", "UPDRS II PRE")) %>%
    mutate(Eval = factor(Eval, levels = c("UPDRS II PRE", "UPDRS II POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "UPDRS II",
    x = "\n Evaluation",
    y = "UPDRS II Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="updrs_II.svg", plot=plot, width=5, height=5)









bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS IV`) %>% rename("UPDRS_IV_PRE"="UPDRS IV") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS IV`) %>% rename("UPDRS_IV_POST"="UPDRS IV")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(UPDRS_IV_PRE),
            sd_PRE=sd(UPDRS_IV_PRE),
            median_PRE=median(UPDRS_IV_PRE),
            q1_PRE=quantile(UPDRS_IV_PRE, 0.25),
            q3_PRE=quantile(UPDRS_IV_PRE, 0.75),
            mean_POST=mean(UPDRS_IV_POST),
            sd_POST=sd(UPDRS_IV_POST),
            median_POST=median(UPDRS_IV_POST),
            q1_POST=quantile(UPDRS_IV_POST, 0.25),
            q3_POST=quantile(UPDRS_IV_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS IV`) %>% rename("UPDRS_IV_PRE"="UPDRS IV") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS IV`) %>% rename("UPDRS_IV_POST"="UPDRS IV")) %>%
  drop_na() 

wilcox.test(df$UPDRS_IV_PRE, df$UPDRS_IV_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `UPDRS_IV_PRE`:`UPDRS_IV_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="UPDRS_IV_POST", "UPDRS IV POST", "UPDRS IV PRE")) %>%
    mutate(Eval = factor(Eval, levels = c("UPDRS IV PRE", "UPDRS IV POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "UPDRS IV",
    x = "\n Evaluation",
    y = "UPDRS IV Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="updrs_IV.svg", plot=plot, width=5, height=5)











bdd09072025_PREOP %>% select(NOM, PRENOM, `HY OFF`) %>% rename("H&Y_OFF_PRE"="HY OFF") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `HY ST-OFF`) %>% rename("H&Y_OFF_POST"="HY ST-OFF")) %>%
  drop_na() %>%
  mutate(`H&Y_OFF_PRE`=round(`H&Y_OFF_PRE`), `H&Y_OFF_POST`=round(`H&Y_OFF_POST`)) %>%
  group_by(`H&Y_OFF_POST`) %>% count() %>% mutate(n=n/28)


df <-  bdd09072025_PREOP %>% select(NOM, PRENOM, `HY OFF`) %>% rename("H&Y_OFF_PRE"="HY OFF") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `HY ST-OFF`) %>% rename("H&Y_OFF_POST"="HY ST-OFF")) %>%
  drop_na() %>%
  mutate(`H&Y_OFF_PRE`=round(`H&Y_OFF_PRE`), `H&Y_OFF_POST`=round(`H&Y_OFF_POST`))

wilcox.test(df$`H&Y_OFF_PRE`, df$`H&Y_OFF_POST`, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `H&Y_OFF_PRE`:`H&Y_OFF_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="H&Y_OFF_PRE", "H&Y OFF [PRE]", "H&Y OFF [POST]")) %>%
    mutate(Eval = factor(Eval, levels = c("H&Y OFF [PRE]", "H&Y OFF [POST]")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "H&Y OFF",
    x = "\n Evaluation",
    y = "H&Y OFF Stage \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="Hy_off.svg", plot=plot, width=5, height=5)













bdd09072025_PREOP %>% select(NOM, PRENOM, `HY ON`) %>% rename("H&Y_ON_PRE"="HY ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `HY ST-ON`) %>% rename("H&Y_ON_POST"="HY ST-ON")) %>%
  drop_na() %>%
  mutate(`H&Y_ON_PRE`=round(`H&Y_ON_PRE`), `H&Y_ON_POST`=round(`H&Y_ON_POST`)) %>%
  group_by(`H&Y_ON_POST`) %>% count() %>% mutate(n=n/27)


df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `HY ON`) %>% rename("H&Y_ON_PRE"="HY ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `HY ST-ON`) %>% rename("H&Y_ON_POST"="HY ST-ON")) %>%
  drop_na() %>%
  mutate(`H&Y_ON_PRE`=round(`H&Y_ON_PRE`), `H&Y_ON_POST`=round(`H&Y_ON_POST`)) 

wilcox.test(df$`H&Y_ON_PRE`, df$`H&Y_ON_POST`, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `H&Y_ON_PRE`:`H&Y_ON_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="H&Y_ON_PRE", "H&Y ON [PRE]", "H&Y ON [POST]")) %>%
    mutate(Eval = factor(Eval, levels = c("H&Y ON [PRE]", "H&Y ON [POST]")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "H&Y ON",
    x = "\n Evaluation",
    y = "H&Y ON Stage \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="Hy_on.svg", plot=plot, width=5, height=5)








df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS III OFF`, `UPDRS III ON`) %>% rename("UPDRS_III_OFF_PRE"="UPDRS III OFF", "UPDRS_III_ON_PRE"="UPDRS III ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS III MED ON ST-OFF`, `UPDRS III MED OFF ST-OFF`) %>% rename("UPDRS_III_MedON_StimOFF_POST"="UPDRS III MED ON ST-OFF", "UPDRS_III_MedOFF_StimOFF_POST"="UPDRS III MED OFF ST-OFF")) %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `UPDRS III MED OFF ST-ON`, `UPDRS III MED ON ST-ON`) %>% rename("UPDRS_III_MedOFF_StimON_POST"="UPDRS III MED OFF ST-ON", "UPDRS_III_MedON_StimON_POST"="UPDRS III MED ON ST-ON")) %>%
    drop_na()


df %>%
  select(-NOM, -PRENOM) %>%   # remove ID columns
  summarise(across(
    everything(),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
      Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") # separate column name and stat





df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS III OFF`, `UPDRS III ON`) %>% rename("UPDRS_III_OFF_PRE"="UPDRS III OFF", "UPDRS_III_ON_PRE"="UPDRS III ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS III MED ON ST-OFF`, `UPDRS III MED OFF ST-OFF`) %>% rename("UPDRS_III_MedON_StimOFF_POST"="UPDRS III MED ON ST-OFF", "UPDRS_III_MedOFF_StimOFF_POST"="UPDRS III MED OFF ST-OFF")) %>%
    select(NOM, PRENOM, UPDRS_III_OFF_PRE, UPDRS_III_MedOFF_StimOFF_POST) %>% drop_na()


wilcox.test(df$UPDRS_III_OFF_PRE, df$UPDRS_III_MedOFF_StimOFF_POST, paired = TRUE) # 0.1742


df <- df %>%
  gather(Eval, Score,  `UPDRS_III_OFF_PRE`:`UPDRS_III_MedOFF_StimOFF_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="UPDRS_III_OFF_PRE", "UPDRS III OFF [PRE]", "UPDRS III OFF [POST]")) %>%
    mutate(Eval = factor(Eval, levels = c("UPDRS III OFF [PRE]", "UPDRS III OFF [POST]")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "UPDRS III OFF",
    x = "\n Evaluation",
    y = "UPDRS III OFF Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="updrs_IIIOFF.svg", plot=plot, width=5, height=5)











df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS III OFF`, `UPDRS III ON`) %>% rename("UPDRS_III_OFF_PRE"="UPDRS III OFF", "UPDRS_III_ON_PRE"="UPDRS III ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS III MED ON ST-OFF`, `UPDRS III MED OFF ST-OFF`) %>% rename("UPDRS_III_MedON_StimOFF_POST"="UPDRS III MED ON ST-OFF", "UPDRS_III_MedOFF_StimOFF_POST"="UPDRS III MED OFF ST-OFF")) %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `UPDRS III MED OFF ST-ON`, `UPDRS III MED ON ST-ON`) %>% rename("UPDRS_III_MedOFF_StimON_POST"="UPDRS III MED OFF ST-ON", "UPDRS_III_MedON_StimON_POST"="UPDRS III MED ON ST-ON")) %>%
    drop_na()



df %>%
  select(-NOM, -PRENOM) %>%   # remove ID columns
  summarise(across(
    everything(),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
      Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") # separate column name and stat





df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `UPDRS III OFF`, `UPDRS III ON`) %>% rename("UPDRS_III_OFF_PRE"="UPDRS III OFF", "UPDRS_III_ON_PRE"="UPDRS III ON") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, `UPDRS III MED ON ST-OFF`, `UPDRS III MED OFF ST-OFF`) %>% rename("UPDRS_III_MedON_StimOFF_POST"="UPDRS III MED ON ST-OFF", "UPDRS_III_MedOFF_StimOFF_POST"="UPDRS III MED OFF ST-OFF")) %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `UPDRS III MED OFF ST-ON`, `UPDRS III MED ON ST-ON`) %>% rename("UPDRS_III_MedOFF_StimON_POST"="UPDRS III MED OFF ST-ON", "UPDRS_III_MedON_StimON_POST"="UPDRS III MED ON ST-ON")) %>%
  select(NOM, PRENOM, UPDRS_III_ON_PRE, UPDRS_III_MedON_StimOFF_POST, UPDRS_III_MedOFF_StimON_POST, UPDRS_III_MedON_StimON_POST) %>% drop_na()

df <- df %>%   gather(Eval, Score,  `UPDRS_III_ON_PRE`:`UPDRS_III_MedON_StimON_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM))

res.fried <- df %>% friedman_test(Score ~ Eval |ID)
res.fried



pwc <- df %>%
  wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc

#  .y.   group1                       group2                          n1    n2 statistic         p    p.adj p.adj.signif
# * <chr> <chr>                        <chr>                        <int> <int>     <dbl>     <dbl>    <dbl> <chr>       
# 1 Score UPDRS_III_MedOFF_StimON_POST UPDRS_III_MedON_StimOFF_POST    23    23      75.5 0.279     1        ns          
# 2 Score UPDRS_III_MedOFF_StimON_POST UPDRS_III_MedON_StimON_POST     23    23     276   0.0000285 0.000171 ***         
# 3 Score UPDRS_III_MedOFF_StimON_POST UPDRS_III_ON_PRE                23    23     236.  0.003     0.019    *           
# 4 Score UPDRS_III_MedON_StimOFF_POST UPDRS_III_MedON_StimON_POST     23    23     253   0.0000424 0.000254 ***         
# 5 Score UPDRS_III_MedON_StimOFF_POST UPDRS_III_ON_PRE                23    23     211   0.006     0.038    *           
# 6 Score UPDRS_III_MedON_StimON_POST  UPDRS_III_ON_PRE                23    23      48.5 0.012     0.07     ns   

unique(df$Eval)

df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="UPDRS_III_ON_PRE", "UPDRS III ON [PRE]", 
                     ifelse(Eval=="UPDRS_III_MedON_StimOFF_POST", "UPDRS III Med-ON Stim-OFF [POST]",
                            ifelse(Eval=="UPDRS_III_MedOFF_StimON_POST", "UPDRS III Med-OFF Stim-ON [POST]", "UPDRS III Med-ON Stim-ON [POST]")))) %>%
    mutate(Eval = factor(Eval, levels = c("UPDRS III ON [PRE]", "UPDRS III Med-ON Stim-OFF [POST]", "UPDRS III Med-OFF Stim-ON [POST]", "UPDRS III Med-ON Stim-ON [POST]")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "UPDRS III ON",
    x = "\n Evaluation",
    y = "UPDRS III ON Score \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
     axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  scale_fill_manual(values=c("#DEBABA", "#95BBC9", "#29687F", "#942F4B")) +
  scale_colour_manual(values=c("#DEBABA", "#95BBC9", "#29687F", "#942F4B")) 

ggsave(file="updrs_IIION.svg", plot=plot, width=7, height=5)







bdd09072025_PREOP %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_PRE"="Dopasensibilité") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_OFFON"="Dopasensibilité")) %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_ONON"="Dopasensibilité")) %>%
    drop_na() %>%
  summarise(mean_PRE=mean(Dopa_PercentResponse_PRE),
            sd_PRE=sd(Dopa_PercentResponse_PRE),
            median_PRE=median(Dopa_PercentResponse_PRE),
            q1_PRE=quantile(Dopa_PercentResponse_PRE, 0.25),
            q3_PRE=quantile(Dopa_PercentResponse_PRE, 0.75),
            mean_OFFON=mean(Dopa_PercentResponse_OFFON),
            sd_OFFON=sd(Dopa_PercentResponse_OFFON),
            median_OFFON=median(Dopa_PercentResponse_OFFON),
            q1_OFFON=quantile(Dopa_PercentResponse_OFFON, 0.25),
            q3_OFFON=quantile(Dopa_PercentResponse_OFFON, 0.75),
            mean_ONON=mean(Dopa_PercentResponse_ONON),
            sd_ONON=sd(Dopa_PercentResponse_ONON),
            median_ONON=median(Dopa_PercentResponse_ONON),
            q1_ONON=quantile(Dopa_PercentResponse_ONON, 0.25),
            q3_ONON=quantile(Dopa_PercentResponse_ONON, 0.75))

df <-  bdd09072025_PREOP %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_PRE"="Dopasensibilité") %>%
  inner_join(bdd09072025_OFFON %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_OFFON"="Dopasensibilité")) %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, Dopasensibilité) %>% rename("Dopa_PercentResponse_ONON"="Dopasensibilité")) %>%
    drop_na()



df <- df %>%   gather(Eval, Score,  `Dopa_PercentResponse_PRE`:`Dopa_PercentResponse_ONON`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM))

res.fried <- df %>% rstatix::friedman_test(Score ~ Eval |ID)
res.fried


pwc <- df %>%
  rstatix ::wilcox_test(Score ~ Eval, paired = TRUE, p.adjust.method = "bonferroni")
pwc

#   .y.   group1                     group2                       n1    n2 statistic        p    p.adj p.adj.signif
# * <chr> <chr>                      <chr>                     <int> <int>     <dbl>    <dbl>    <dbl> <chr>       
# 1 Score Dopa_PercentResponse_OFFON Dopa_PercentResponse_ONON    23    23        82 0.092    0.275    ns          
# 2 Score Dopa_PercentResponse_OFFON Dopa_PercentResponse_PRE     23    23        22 0.000128 0.000384 ***         
# 3 Score Dopa_PercentResponse_ONON  Dopa_PercentResponse_PRE     23    23        36 0.002    0.006    **    

df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="Dopa_PercentResponse_PRE", "L-Dopa % Response [PRE]", 
                     ifelse(Eval=="Dopa_PercentResponse_OFFON", "L-Dopa % Response [Med-ON Stim-OFF]", "L-Dopa % Response [Med-ON Stim-ON]"))) %>%
    mutate(Eval = factor(Eval, levels = c("L-Dopa % Response [PRE]", "L-Dopa % Response [Med-ON Stim-OFF]", "L-Dopa % Response [Med-ON Stim-ON]")))


plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "L-Dopa % Response",
    x = "\n Evaluation",
    y = "L-Dopa % Response \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#DEBABA", "#95BBC9", "#29687F" )) +
  scale_colour_manual(values=c("#DEBABA", "#95BBC9", "#29687F" )) 

ggsave(file="doparesp.svg", plot=plot, width=7, height=5)









bdd09072025_PREOP <- read_excel(path = "../data/bdd09072025.xlsx", sheet="PREOP", skip=1)
bdd09072025_OFFON <- read_excel(path = "../data/bdd09072025.xlsx", sheet="POSTOP STIM OFF MED ON", skip=1)
bdd09072025_ONON <- read_excel(path = "../data/bdd09072025.xlsx", sheet="POST OP STIM ON MED ON", skip=1)

data.frame(names(bdd09072025_PREOP))
data.frame(names(bdd09072025_OFFON))

data.frame(names(bdd09072025_ONON))

library(dplyr)
library(rstatix)
library(tidyr)






bdd09072025_PREOP %>% select(NOM, PRENOM, `DL PRE`) %>% rename("QSART_DL_PRE"="DL PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `DL PRE`) %>% rename("QSART_DL_POST"="DL PRE")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(QSART_DL_PRE),
            sd_PRE=sd(QSART_DL_PRE),
            median_PRE=median(QSART_DL_PRE),
            q1_PRE=quantile(QSART_DL_PRE, 0.25),
            q3_PRE=quantile(QSART_DL_PRE, 0.75),
            mean_POST=mean(QSART_DL_POST),
            sd_POST=sd(QSART_DL_POST),
            median_POST=median(QSART_DL_POST),
            q1_POST=quantile(QSART_DL_POST, 0.25),
            q3_POST=quantile(QSART_DL_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `DL PRE`) %>% rename("QSART_DL_PRE"="DL PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `DL PRE`) %>% rename("QSART_DL_POST"="DL PRE")) %>%
  drop_na() 

wilcox.test(df$QSART_DL_PRE, df$QSART_DL_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `QSART_DL_PRE`:`QSART_DL_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="QSART_DL_PRE", "QSART DL PRE", "QSART DL POST")) %>%
  mutate(Eval = factor(Eval, levels = c("QSART DL PRE", "QSART DL POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "QSART Distal Leg",
    x = "\n Evaluation",
    y = "QSART Distal Leg \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="qsartLD.svg", plot=plot, width=5, height=5)









bdd09072025_PREOP %>% select(NOM, PRENOM, `PL PRE`) %>% rename("QSART_PL_PRE"="PL PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `PL PRE`) %>% rename("QSART_PL_POST"="PL PRE")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(QSART_PL_PRE),
            sd_PRE=sd(QSART_PL_PRE),
            median_PRE=median(QSART_PL_PRE),
            q1_PRE=quantile(QSART_PL_PRE, 0.25),
            q3_PRE=quantile(QSART_PL_PRE, 0.75),
            mean_POST=mean(QSART_PL_POST),
            sd_POST=sd(QSART_PL_POST),
            median_POST=median(QSART_PL_POST),
            q1_POST=quantile(QSART_PL_POST, 0.25),
            q3_POST=quantile(QSART_PL_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `PL PRE`) %>% rename("QSART_PL_PRE"="PL PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `PL PRE`) %>% rename("QSART_PL_POST"="PL PRE")) %>%
  drop_na() 

wilcox.test(df$QSART_PL_PRE, df$QSART_PL_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `QSART_PL_PRE`:`QSART_PL_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="QSART_PL_PRE", "QSART PL PRE", "QSART PL POST")) %>%
  mutate(Eval = factor(Eval, levels = c("QSART PL PRE", "QSART PL POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # Boxplots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "QSART Proximal Leg",
    x = "\n Evaluation",
    y = "QSART Proximal Leg \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="qsartPL.svg", plot=plot, width=5, height=5)





bdd09072025_PREOP %>% select(NOM, PRENOM, `F PRE`) %>% rename("QSART_F_PRE"="F PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `F PRE`) %>% rename("QSART_F_POST"="F PRE")) %>%
  drop_na() %>%
  summarise(mean_PRE=mean(QSART_F_PRE),
            sd_PRE=sd(QSART_F_PRE),
            median_PRE=median(QSART_F_PRE),
            q1_PRE=quantile(QSART_F_PRE, 0.25),
            q3_PRE=quantile(QSART_F_PRE, 0.75),
            mean_POST=mean(QSART_F_POST),
            sd_POST=sd(QSART_F_POST),
            median_POST=median(QSART_F_POST),
            q1_POST=quantile(QSART_F_POST, 0.25),
            q3_POST=quantile(QSART_F_POST, 0.75),)

df <- bdd09072025_PREOP %>% select(NOM, PRENOM, `F PRE`) %>% rename("QSART_F_PRE"="F PRE") %>%
  inner_join(bdd09072025_ONON %>% select(NOM, PRENOM, `F PRE`) %>% rename("QSART_F_POST"="F PRE")) %>%
  drop_na() 

wilcox.test(df$QSART_F_PRE, df$QSART_F_POST, paired = TRUE)


df <- df %>%
  gather(Eval, Score,  `QSART_F_PRE`:`QSART_F_POST`)


df <- df %>%
  mutate(ID = paste(NOM, PRENOM)) %>%
  mutate(Eval=ifelse(Eval=="QSART_F_PRE", "QSART F PRE", "QSART F POST")) %>%
  mutate(Eval = factor(Eval, levels = c("QSART F PRE", "QSART F POST")))



plot <- ggplot(df, aes(x = Eval, y = Score, colour=Eval, fill=Eval)) +
  # BoxFots for the 3 evaluations
  geom_boxplot(outlier.shape = NA, notch = TRUE, alpha = 0.6, ) +
  # Lines connecting the same patient's scores
  geom_line(aes(group = ID), color = "black", alpha = 0.8) +
  # Points (jittered slightly to avoid overlap)
  geom_point(stroke=2, size = 2.2, aes(color = Eval), alpha = 1, shape=1) +
  # Labels and theme
  labs(
    title = "QSART Forearm",
    x = "\n Evaluation",
    y = "QSART Forearm \n"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values=c("#95BBC9", "#DEBABA")) +
  scale_colour_manual(values=c("#95BBC9", "#DEBABA")) 

ggsave(file="forearm.svg", plot=plot, width=5, height=5)








# -----------------------
# NEW VERSION MERGED CLEANED -----------


library(FactoMineR)
library(missMDA)


merged_df <- read_excel(path = "../data/merged_df.xlsx", sheet="merged")

merged_df[5:64] <- lapply(merged_df[5:64], function(x) as.numeric(as.character(x)))

length(unique(merged_df$NOM))

merged_df_imputed <- merged_df

merged_df_vars <- merged_df_imputed %>%
  select(-NOM, -PRENOM, -Date_finapress, -Eval)

nb <- estim_ncpPCA(merged_df_vars, ncp.max = 50)  # tries 1–10 dimensions

nb$ncp

imputed <- imputePCA(merged_df_vars, ncp = nb$ncp)

merged_df_imputed[,5:64] <- imputed$completeObs

fwrite(merged_df_imputed, "../data/merged_df_imputed.txt")




library(dplyr)

results_list <- list()  # store results for each variable

for (i in 5:64) {
  varname <- names(merged_df)[i]
  
  df_sub <- merged_df %>%
    select(NOM, PRENOM, Eval, all_of(varname)) %>%
    filter(!is.na(.data[[varname]]))
  
  # Which Eval levels exist for this variable
  eval_levels <- unique(df_sub$Eval)
  
  # Patients who have *all* these Evals
  patient_summary <- df_sub %>%
    group_by(NOM, PRENOM) %>%
    summarize(n_evals = n_distinct(Eval), .groups = "drop")
  
  complete_patients <- patient_summary %>%
    filter(n_evals == length(eval_levels))
  
  # Filter to only those complete patients
  df_balanced <- df_sub %>%
    semi_join(complete_patients, by = c("NOM", "PRENOM"))
  
  # Stats: total, kept, excluded
  total_patients <- n_distinct(df_sub$NOM, df_sub$PRENOM)
  kept_patients  <- n_distinct(df_balanced$NOM, df_balanced$PRENOM)
  excluded_patients <- total_patients - kept_patients
  
  # If we have any balanced patients, calculate stats
  if (nrow(df_balanced) > 0) {
    summary_stats <- df_balanced %>%
      group_by(Eval) %>%
      summarize(
        mean = mean(.data[[varname]], na.rm = TRUE),
        sd = sd(.data[[varname]], na.rm = TRUE),
        median = median(.data[[varname]], na.rm = TRUE),
        q1 = quantile(.data[[varname]], 0.25, na.rm = TRUE),
        q3 = quantile(.data[[varname]], 0.75, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(
        variable = varname,
        total_patients = total_patients,
        kept_patients = kept_patients,
        excluded_patients = excluded_patients
      )
    
    results_list[[varname]] <- summary_stats
  }
}

# Combine all results
results_df <- bind_rows(results_list)

data.frame(results_df %>%
             mutate(mean = paste0(round(mean,1), 
                                  paste0(" ± ", 
                                         paste0(round(sd,1), 
                                                paste0( " | " , 
                                                        paste0(round(median,1), 
                                                               paste0(" [", 
                                                                      paste0(round(q1,1), 
                                                                             paste0("-", 
                                                                                    paste0(round(q3,1), "]")))))) ) ))) %>%
             select(-sd, -median, -q1, -q3, -total_patients, -kept_patients, -excluded_patients, -n)  %>%
             spread(key=Eval, value=mean))




library(dplyr)

friedman_results <- list()

for (i in 5:64) {
  varname <- names(merged_df)[i]
  
  df_sub <- merged_df %>%
    select(NOM, PRENOM, Eval, all_of(varname)) %>%
    filter(!is.na(.data[[varname]]))
  
  # Which Eval levels exist for this variable
  eval_levels <- unique(df_sub$Eval)
  
  # Patients who have all these Evals
  patient_summary <- df_sub %>%
    group_by(NOM, PRENOM) %>%
    summarize(n_evals = n_distinct(Eval), .groups = "drop")
  
  complete_patients <- patient_summary %>%
    filter(n_evals == length(eval_levels))
  
  # Keep only balanced patients
  df_balanced <- df_sub %>%
    semi_join(complete_patients, by = c("NOM", "PRENOM"))
  
  total_patients <- n_distinct(df_sub$NOM, df_sub$PRENOM)
  kept_patients  <- n_distinct(df_balanced$NOM, df_balanced$PRENOM)
  excluded_patients <- total_patients - kept_patients
  
  # Run Friedman test only if at least 2 Evals and >1 patient
  if (nrow(df_balanced) > 0 && length(eval_levels) > 1 && kept_patients > 1) {
    # Reshape to wide for the Friedman test
    df_wide <- tidyr::pivot_wider(
      df_balanced,
      id_cols = c(NOM, PRENOM),
      names_from = Eval,
      values_from = all_of(varname)
    )
    
    # Only keep patients with complete data across all Eval columns
    df_wide <- df_wide %>%
      filter(if_all(all_of(eval_levels), ~ !is.na(.)))
    
    if (nrow(df_wide) > 1) {
      test <- tryCatch({
        friedman.test(as.matrix(df_wide[eval_levels]))$p.value
      }, error = function(e) NA)
      
      friedman_results[[varname]] <- tibble(
        variable = varname,
        p_value = test,
        n_patients = nrow(df_wide),
        n_evals = length(eval_levels),
        total_patients = total_patients,
        kept_patients = kept_patients,
        excluded_patients = excluded_patients
      )
    }
  }
}

friedman_df <- bind_rows(friedman_results)








library(dplyr)
library(tidyr)
library(rstatix)   # clean wilcox_test + p.adjust helpers

posthoc_results <- list()

for (i in 5:64) {
  varname <- names(merged_df)[i]
  
  df_sub <- merged_df %>%
    select(NOM, PRENOM, Eval, all_of(varname)) %>%
    filter(!is.na(.data[[varname]]))
  
  eval_levels <- unique(df_sub$Eval)
  
  # --- identify patients with complete data for all Evals ---
  patient_summary <- df_sub %>%
    group_by(NOM, PRENOM) %>%
    summarise(n_evals = n_distinct(Eval), .groups = "drop")
  
  complete_patients <- patient_summary %>%
    filter(n_evals == length(eval_levels))
  
  df_balanced <- df_sub %>%
    semi_join(complete_patients, by = c("NOM", "PRENOM"))
  
  total_patients <- n_distinct(df_sub$NOM, df_sub$PRENOM)
  kept_patients  <- n_distinct(df_balanced$NOM, df_balanced$PRENOM)
  excluded_patients <- total_patients - kept_patients
  
  # --- run tests only if we have enough data ---
  if (nrow(df_balanced) > 0 && length(eval_levels) > 1 && kept_patients > 1) {
    # reshape to wide format
    df_wide <- pivot_wider(
      df_balanced,
      id_cols = c(NOM, PRENOM),
      names_from = Eval,
      values_from = all_of(varname)
    ) %>%
      filter(if_all(all_of(eval_levels), ~ !is.na(.)))
    
    if (nrow(df_wide) > 1) {
      # Friedman test
      friedman_p <- tryCatch({
        friedman.test(as.matrix(df_wide[eval_levels]))$p.value
      }, error = function(e) NA)
      
      # ---- Post-hoc pairwise Wilcoxon (only if >2 levels) ----
      if (length(eval_levels) > 2) {
        df_long <- df_balanced %>%
          filter(NOM %in% df_wide$NOM & PRENOM %in% df_wide$PRENOM)
        
        posthoc <- df_long %>%
          pairwise_wilcox_test(
            formula = as.formula(paste(varname, "~ Eval")),
            paired = TRUE,
            p.adjust.method = "holm"
          ) %>%
          mutate(variable = varname,
                 friedman_p = friedman_p,
                 n_patients = nrow(df_wide),
                 n_evals = length(eval_levels),
                 total_patients = total_patients,
                 kept_patients = kept_patients,
                 excluded_patients = excluded_patients)
        
        posthoc_results[[varname]] <- posthoc
      } else {
        # only 2 levels: just one Wilcoxon test
        df_long <- df_balanced %>%
          filter(NOM %in% df_wide$NOM & PRENOM %in% df_wide$PRENOM)
        
        w_test <- tryCatch({
          wilcox.test(
            x = df_long[df_long$Eval == eval_levels[1], varname, drop=TRUE],
            y = df_long[df_long$Eval == eval_levels[2], varname, drop=TRUE],
            paired = TRUE
          )$p.value
        }, error = function(e) NA)
        
        posthoc_results[[varname]] <- tibble(
          variable = varname,
          friedman_p = friedman_p,
          group1 = eval_levels[1],
          group2 = eval_levels[2],
          p = w_test,
          p.adj = w_test,
          p.adj.signif = rstatix::p_format(w_test),
          n_patients = nrow(df_wide),
          n_evals = length(eval_levels),
          total_patients = total_patients,
          kept_patients = kept_patients,
          excluded_patients = excluded_patients
        )
      }
    }
  }
}


posthoc_df <- bind_rows(posthoc_results)


fwrite(posthoc_df, "posthoc_df.csv")

posthoc_df <- posthoc_df %>% select(variable, friedman_p, group1, group2, p, p.adj)

posthoc_df <- posthoc_df %>% filter( (group1=="STIM_OFF_Med_ON" & group2 == "STIM_ON_Med_ON")|(group1=="STIM_ON_Med_ON" & group2 == "STIM_OFF_Med_ON") )


# ------------
# PCA ----------------

merged_df_imputed <- fread("../data/merged_df_imputed.txt")

names(merged_df_imputed)

predictor_cols <- setdiff(names(merged_df_imputed), c("NOM","PRENOM","Date_finapress","Eval") )

df_num <- as.data.frame(scale(merged_df_imputed %>% select(all_of(predictor_cols))))


names(df_num) <- predictor_cols

var_check <- apply(df_num, 2, var, na.rm = TRUE)

zero_var_cols <- names(var_check)[var_check == 0 | is.na(var_check)]

if(length(zero_var_cols) > 0) {
  message("Dropping zero-variance columns: ", paste(zero_var_cols, collapse = ", "))
  df_num <- df_num %>% select(-all_of(zero_var_cols))
}


groups <- factor(merged_df_imputed$Eval)

pca <- prcomp(df_num, center = TRUE, scale. = TRUE)

summary(pca)

pca_df <- pca$rotation

pca_df <- data.frame(pca_df)

pca_df$var <- row.names(pca_df) 


fwrite(pca_df, "pca_df.csv")

library(FSA)


loadings_df <- data.frame(pca$x)

loadings_df <- loadings_df %>% bind_cols(merged_df_imputed$Eval)

fwrite(loadings_df, "loadings_df.csv")

names(loadings_df)[61] <- "Eval"


FSA::dunnTest(PC1 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC2 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC3 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC4 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC5 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC6 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC7 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC8 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC9 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC10 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC11 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC12 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC13 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC14 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC15 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC16 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC17 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC18 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC19 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC20 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC21 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC22 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC23 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC24 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC25 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC26 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC27 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC28 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC29 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC30 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC31 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC32 ~ Eval, data = loadings_df, method = "bonferroni")
FSA::dunnTest(PC33 ~ Eval, data = loadings_df, method = "bonferroni")


loadings_df %>%
  ggplot(aes(PC1, PC3, fill=Eval, color=Eval)) +
  geom_jitter()






# ------------


# LDA overall ----------------

merged_df_imputed <- fread("../data/merged_df_imputed.txt")

names(merged_df_imputed)

predictor_cols <- setdiff(names(merged_df_imputed), c("NOM","PRENOM","Date_finapress","Eval") )

df_num <- as.data.frame(scale(merged_df_imputed %>% select(all_of(predictor_cols))))

names(df_num) <- predictor_cols

var_check <- apply(df_num, 2, var, na.rm = TRUE)

zero_var_cols <- names(var_check)[var_check == 0 | is.na(var_check)]

if(length(zero_var_cols) > 0) {
  message("Dropping zero-variance columns: ", paste(zero_var_cols, collapse = ", "))
  df_num <- df_num %>% select(-all_of(zero_var_cols))
}


groups <- factor(merged_df_imputed$Eval)

lda_data <- data.frame(df_num, groupes = groups)

library(MASS)

lda_model <- lda(groupes ~ ., data = lda_data)

print(lda_model$scaling)

rownames(lda_model$scaling)

# LD1          LD2
# UPDRS_I                               0.501367596  0.282009299
# UPDRS_II                              0.003024331  0.056592440
# UPDRS_IV                             -0.756343710  1.591324135
# UPDRS_III_OFF                        -1.975544195 -0.793118651
# UPDRS_III_ON                          0.241777277 -2.193416317
# L_Dopa_Percent_Resp                  -1.439132550 -1.445254726
# HY_OFF                                0.912712887 -0.020401825
# HY_ON                                 0.047589638  0.758080725
# QSART_DL_PRE                         -1.206329702 -0.388519455
# QSART_PL_PRE                         -0.050746457 -0.762129301
# QSART_F_PRE                           0.825707309  0.363983624
# Tilt_SBP_supine                      -0.051151715  0.540952210
# Tilt_SBP_standing                     0.488300783  0.025417754
# Tilt_DBP_supine                      -0.596005861 -0.154892813
# Tilt_DBP_standing                    -0.496891045  0.003591010
# Tilt_HR_supine                        1.187440422 -0.872529640
# Tilt_HR_standing                      0.325389257  1.006240463
# Tilt_DeltaSBP                         0.060214123  0.409958869
# Tilt_DeltaDBP                        -0.280089865 -0.557269232
# Tilt_DeltaHR                         -0.219443494 -0.468547118
# Stand_SBP_supine                     -3.475707970 -1.787441729
# Stand_SBP_standing                    2.922083369  1.016833343
# Stand_DBP_supine                      1.392689295  3.253328071
# Stand_DBP_standing                   -1.287633681 -3.665029896
# Stand_HR_supine                      -1.074424286 -4.930936985
# Stand_HR_standing                    -0.096086030  4.783603183
# Stand_DeltaSBP                       -2.096316107  1.403765446
# Stand_DeltaDBP                       -2.814602260 -3.042591939
# Stand_DeltaHR                         0.255537169 -4.386690715
# Deep_breath_ratio_30_15_value         0.012618170 -0.003501318
# Deep_breath_ratio_30_15_score        -0.017371870 -0.855960679
# Deep_breath_HR_Max                   -0.940955308  0.185145218
# Deep_breath_HR_Min                    1.258447120  1.417700384
# Deep_breath_Respi_prof               -1.094487982 -0.527390257
# Valsalva_DeltaSBP_Iib                -0.249420259 -0.007336890
# Valsalva_DeltaSBP_Ivb                -0.003210383  0.171947847
# Valsalva_DeltaHR_max                 -0.889211491 -0.936499847
# Valsalva_DeltaHR_min                  0.730574330 -0.044416548
# Hand_grip_SBP_base                    0.065548464 -0.059530429
# Hand_grip_SBP_end                    -0.129901404 -0.033405674
# Hand_grip_DBP_base                    0.416805427  0.066988345
# Hand_grip_DBP_end                     0.042691140  0.030462109
# Ewing_ratio_Valsalva_value            0.733953885  0.454117634
# Ewing_ratio_Valsalva_score            0.437739396  0.401979996
# Ewing_Respi_Ample_value               1.408936364  1.386360868
# Ewing_Respi_Ample_score               0.785090220  0.077741538
# Ewing_ratio_30_15_value               0.012618170 -0.003501318
# Ewing_ratio_30_15_score              -0.017371870 -0.855960679
# Hand_grip_contraction_iso_DeltaSBP   -0.286654086  0.029990195
# Hand_grip_contraction_iso_DeltaDBP   -0.512746885 -0.040471919
# Ortho_contraction_iso_tilt_DeltaSBP   0.623846613 -0.506948933
# Ortho_contraction_iso_tilt_DeltaDBP  -0.001924144  0.176684092
# Ortho_contraction_iso_stand_DeltaSBP -1.115584730 -2.433802586
# Ortho_contraction_iso_stand_DeltaDBP  3.662589135  5.946993116
# Ortho_Tilt_Test_score                 0.881266481  0.591952040
# Ortho_Total_out_of_5                 -1.387342405 -0.071427863
# SUDOCAN_ESC_Mean_Feet                -0.276203481 -0.277322940
# SUDOCAN_Asym_Mean_Feet                0.238789467  0.330082821
# SUDOCAN_ESC_Mean_Hands                0.086049368  0.645252736
# SUDOCAN_Asym_Mean_Hands              -0.249718624  1.271282173

lda_model_scaling <- data.frame(lda_model$scaling)

lda_model_scaling$var <- rownames(lda_model_scaling)

lda_model_scaling %>%
  arrange(LD2) %>%
  mutate(var=factor(var, levels=var)) %>%
  mutate(group=ifelse(LD2>0,"Yes","No")) %>%
  ggplot(aes(var, LD2, colour="white", fill=group)) +
  geom_col(alpha=0.5) +
  coord_flip() +
  scale_colour_manual(values=c("white",  "white")) +
  scale_fill_manual(values=c("#335D87",  "#A32121")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5)) 

lda_scores <- as.data.frame(predict(lda_model)$x) %>%
  mutate(groupes = groups)

centroids <- lda_scores %>%
  group_by(groupes) %>%
  summarise(across(everything(), mean))  


# groupes            LD1    LD2
# 1 PreOP           -2.17   0.512
# 2 STIM_OFF_Med_ON  0.550 -1.81 
# 3 STIM_ON_Med_ON   1.62   1.30 

(((1.62+0.55)/2)-2.17)/2 # -0.5425
(((1.30 +0.512)/2)-1.81 )/2 # -0.452


unique(lda_scores$groupes)

# Predicted class memberships
lda_pred <- predict(lda_model)$class

# Confusion matrix
conf_mat <- table(Predicted = lda_pred, Actual = groups)
conf_mat

# Classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy # 1.00





quadrants <- data.frame(
  xmin = c(-Inf, -0.5425, -Inf, -0.5425),
  xmax = c(-0.5425, Inf, -0.5425, Inf),
  ymin = c(-Inf, -Inf, -0.452, -0.452),
  ymax = c(-0.452, -0.452, Inf, Inf),
  fill = factor(c("Q1", "Q2", "Q3", "Q4"))
)

quad_colors <- c("Q1" = "white",  "Q2" = "#DBC44D", "Q3"="#A32121", "Q4" = "#335D87")
group_colors <- c("PreOP" = "#A32121",
                  "STIM_OFF_Med_ON" = "#DBC44D",
                  "STIM_ON_Med_ON" = "#335D87")

# Plot
ggplot() +
  geom_rect(data = quadrants, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_point(data = lda_scores, aes(LD1, LD2, color = groupes, shape = groupes), size = 3, stroke = 2) +
  stat_ellipse(data = lda_scores, aes(LD1, LD2, color = groupes), level = 0.68, size = 2, alpha = 0.6) +
  geom_text(data = centroids, aes(LD1, LD2, label = groupes), size = 4, fontface = "bold", vjust = -1) +
  geom_vline(xintercept = -0.5425) +
  geom_hline(yintercept = -0.452) +
  scale_fill_manual(values = quad_colors) +
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






vars <- rownames(lda_model$scaling)

domain_map <- data.frame(
  vars = vars,
  domain = dplyr::case_when(
    grepl("^UPDRS_IV", vars) ~ "UPDRS_IV",
    grepl("^UPDRS_III", vars) ~ "UPDRS_III",
    grepl("^HY_", vars) ~ "H&Y",
    grepl("^Tilt_", vars) ~ "Tilt Test",
    grepl("^Stand_", vars) ~ "Stand Test",
    grepl("^Hand_grip", vars) ~ "Handgrip",
    grepl("^QSART", vars) ~ "QSART",
    grepl("^Ewing", vars) ~ "Ewing",
    grepl("^Valsalva", vars) ~ "Valsalva",
    grepl("^Ortho_", vars) ~ "Orthostatic",
    grepl("^Deep_breath", vars) ~ "Deep Breathing",
    grepl("^SUDOCAN", vars) ~ "SUDOCAN",
    TRUE ~ "Other"
  )
)

lda_scaling <- data.frame(lda_model$scaling)
lda_scaling$vars <- rownames(lda_scaling)
lda_scaling <- left_join(lda_scaling, domain_map, by = "vars")


lda_scaling %>%
  group_by(domain) %>%
  summarise(
    mean_abs_LD1 = mean(abs(LD1)),
    mean_abs_LD2 = mean(abs(LD2)),
    total_abs_LD1 = mean(abs(LD1)),
    total_abs_LD2 = mean(abs(LD2)),
    n_vars = n()
  ) %>%
  arrange(desc(total_abs_LD1))



library(tidyr)
library(ggplot2)

domain_summary %>%
  pivot_longer(cols = starts_with("mean_abs_"),
               names_to = "LD", values_to = "importance") %>%
  mutate(LD = recode(LD,
                     "mean_abs_LD1" = "LD1",
                     "mean_abs_LD2" = "LD2")) %>%
  ggplot(aes(x = reorder(domain, importance), y = importance, fill = LD)) +
  geom_col(position = "dodge", alpha = 0.6) +
  coord_flip() +
  labs(
    title = "Domain-Level Contribution to LDA Axes",
    subtitle = "Mean absolute loading per variable (normalized within domain)",
    x = "Domain \n",
    y = "\n Mean |Loading|"
  ) +
  scale_fill_manual(values = c("#335D87", "#A32121")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
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
        plot.margin = margin(5, 5, 5, 5, "pt")) 





# ------------

