library(tidyverse)
clustering_summary_GT <- read.csv("data/simulations/GLOBETROTTER/clustering_summary_gt_smallernNEW.csv")
clustering_summary_DATES <- read.csv("data/simulations/DATES/clustering_summary_gt_smallernNEW.csv")

cv_analysis_GT <- read.csv("data/simulations/GLOBETROTTER/cv_analysis_results_gt.csv")
cv_analysis_DATES <- read.csv("data/simulations/DATES/cv_analysis_results_datesNEW.csv")

clustering_summary_GT$filename<-gsub("output.","", clustering_summary_GT$filename )
clustering_summary_DATES$filename<-gsub("output.","", clustering_summary_DATES$filename )

clustering_summary_GT$cv<-cv_analysis_GT[match(clustering_summary_GT$filename, cv_analysis_GT$filename), "upperquartile_date_to_sd_ratio"]
clustering_summary_DATES$cv<-cv_analysis_DATES[match(clustering_summary_DATES$filename, cv_analysis_DATES$filename), "upperquartile_date_to_sd_ratio"]

clustering_summary_GT$method<-"GT"
clustering_summary_DATES$method<-"DATES"
clustering_summary<-rbind(clustering_summary_DATES, clustering_summary_GT)


clustering_summary$k1_to_k2<-(clustering_summary$k2_likelihood_per_ind - clustering_summary$k1_likelihood_per_ind)/clustering_summary$cv
clustering_summary$k2_to_k3<-(clustering_summary$k3_likelihood_per_ind - clustering_summary$k2_likelihood_per_ind)/clustering_summary$cv
clustering_summary$k3_to_k4<-(clustering_summary$k4_likelihood_per_ind - clustering_summary$k3_likelihood_per_ind)/clustering_summary$cv


clustering_summary$n_true_clusters<-as.factor(clustering_summary$n_true_clusters)

library(ggpmisc)

ggplot(clustering_summary, aes(x = total_inds, y = k1_to_k2, color = n_true_clusters)) +
  geom_point(aes(shape = n_true_clusters)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
               formula = y ~ x, parse = TRUE) +
  facet_wrap(~method) +
  theme_minimal()

##seems like threshold doesnt really change with number of individuals???
#tried standardising gt by # of inds and it didnt help

###DECIDING THRESHOLDS

test1<-filter(clustering_summary, method=="GT", n_true_clusters==1)
summary(test1$k1_to_k2)
quantile(test1$k1_to_k2, 0.98) ## 0.69
quantile(test1$clustering_strength_k2, 0.98) # 0.78
test1_wrong<-filter(test1, clustering_strength_k2 > 0.78 | k1_to_k2 > 0.69 ) #21 / 500 classified wrong


test2<-filter(clustering_summary,  method=="GT",n_true_clusters==2)
summary(test2$k1_to_k2)
summary(test2$clustering_strength_k3)
quantile(test2$k2_to_k3, 0.95) ## 0.43
test2_wrong <- filter(test2, 
                      (k1_to_k2 < 0.69 & clustering_strength_k2 < 0.78) | 
                        k2_to_k3 > 0.43 & k1_to_k2> 0.90) ##126 / 500 classified wrong

test3<-filter(clustering_summary, method=="GT", n_true_clusters==3)
summary(test3$k3_to_k4)
quantile(test3$k3_to_k4, 0.95) #0.31
quantile(test3$k1_to_k2, 0.05) #0.90


test3_wrong <- filter(test3, 
                      (k1_to_k2 < 0.69 & clustering_strength_k2 < 0.78) |  # suggests k=1
                        (k1_to_k2 < 0.90 | k2_to_k3 < 0.43) |              # NEW: both checks for k=2
                        k3_to_k4 > 0.31)                                     # suggests k≥4
## 346 / 500 inds



###now DATES

test1<-filter(clustering_summary, method=="DATES", n_true_clusters==1)
summary(test1$k1_to_k2)
quantile(test1$k1_to_k2, 0.98) ## 1.20
quantile(test1$clustering_strength_k2, 0.98) # 0.983
test1_wrong<-filter(test1, clustering_strength_k2 > 0.983 | k1_to_k2 > 1.20 ) #18 / 500 classified wrong


test2<-filter(clustering_summary,  method=="DATES",n_true_clusters==2)
summary(test2$k1_to_k2)
summary(test2$clustering_strength_k3)
quantile(test2$k2_to_k3, 0.95) ## 0.71
test2_wrong <- filter(test2, 
                      (k1_to_k2 < 1.20 & clustering_strength_k2 < 0.983) | 
                        k2_to_k3 > 0.71 & k1_to_k2 > 2.59) ##115 / 500 classified wrong

test3<-filter(clustering_summary, method=="DATES", n_true_clusters==3)
summary(test3$k3_to_k4)
quantile(test3$k3_to_k4, 0.95) #0.49
quantile(test3$k1_to_k2, 0.05) #2.59
test3_wrong <- filter(test3, 
                      (k1_to_k2 < 1.20 & clustering_strength_k2 < 0.983) |  # suggests k=1
                        (k2_to_k3 < 0.71 | k1_to_k2 <2.59) |                                     # suggests k=2
                        k3_to_k4 > 0.49)      

#315/500 inds

###ADDING PREDICTIONS


predict_k <- function(df, method) {
  predicted <- rep(NA, nrow(df))
  
  if(method == "GT") {
    clust_thresh <- 0.78
    k1_to_k2_thresh <- 0.69
    k1_to_k2_thresh_for_k3 <- 0.90  # NEW: stricter threshold for k>=3
    k2_to_k3_thresh <- 0.43
    k3_to_k4_thresh <- 0.31
  } else {  # DATES
    clust_thresh <- 0.983
    k1_to_k2_thresh <- 1.20
    k1_to_k2_thresh_for_k3 <- 2.59  
    k2_to_k3_thresh <- 0.71
    k3_to_k4_thresh <- 0.49
  }
  
  for(i in 1:nrow(df)) {
    # Start with k=1
    predicted[i] <- 1
    
    # Check if k>=2 (with safe NA handling)
    clust_check <- isTRUE(df$clustering_strength_k2[i] > clust_thresh)
    ll_check <- isTRUE(df$k1_to_k2[i] > k1_to_k2_thresh)
    
    if(clust_check || ll_check) {
      predicted[i] <- 2
      
      # Check if k>=3 (with safe NA handling)
      # BOTH conditions must pass: strong k1->k2 AND strong k2->k3
      k1_to_k2_strong <- isTRUE(df$k1_to_k2[i] > k1_to_k2_thresh_for_k3)
      k2_to_k3_strong <- isTRUE(df$k2_to_k3[i] > k2_to_k3_thresh)
      
      if(k1_to_k2_strong && k2_to_k3_strong) {
        predicted[i] <- 3
        
        # Check if k>=4 (with safe NA handling)
        if(isTRUE(df$k3_to_k4[i] > k3_to_k4_thresh)) {
          predicted[i] <- 4
        }
      }
    }
  }
  
  return(predicted)
}



# THEN add prediction AFTER those columns exist:
clustering_summary$predicted_k <- NA
clustering_summary$predicted_k[clustering_summary$method == "GT"] <- 
  predict_k(clustering_summary[clustering_summary$method == "GT", ], "GT")
clustering_summary$predicted_k[clustering_summary$method == "DATES"] <- 
  predict_k(clustering_summary[clustering_summary$method == "DATES", ], "DATES")

# Classification
clustering_summary$classification <- ifelse(
  clustering_summary$predicted_k == clustering_summary$n_true_clusters,
  "correct",
  paste0("predicted_k", clustering_summary$predicted_k)
)


# Extract generation info for k=1 (single generation)
clustering_summary$k1_generation <- case_when(
  clustering_summary$n_true_clusters == 1 & clustering_summary$n_from_20gen > 0 ~ "20gen",
  clustering_summary$n_true_clusters == 1 & clustering_summary$n_from_50gen > 0 ~ "50gen", 
  clustering_summary$n_true_clusters == 1 & clustering_summary$n_from_75gen > 0 ~ "75gen",
  TRUE ~ NA_character_
)

# Extract generation combinations for k=2 and k=3
clustering_summary$generation_combo <- case_when(
  clustering_summary$n_from_20gen > 0 & clustering_summary$n_from_50gen > 0 & clustering_summary$n_from_75gen > 0 ~ "20+50+75gen",
  clustering_summary$n_from_20gen > 0 & clustering_summary$n_from_50gen > 0 ~ "20+50gen",
  clustering_summary$n_from_20gen > 0 & clustering_summary$n_from_75gen > 0 ~ "20+75gen", 
  clustering_summary$n_from_50gen > 0 & clustering_summary$n_from_75gen > 0 ~ "50+75gen",
  clustering_summary$n_from_20gen > 0 ~ "20gen_only",
  clustering_summary$n_from_50gen > 0 ~ "50gen_only",
  clustering_summary$n_from_75gen > 0 ~ "75gen_only",
  TRUE ~ "unknown"
)

# Calculate cluster balance ratios (you mentioned these before)
calculate_cluster_properties <- function(df) {
  # Get non-zero cluster sizes
  cluster_sizes <- cbind(df$n_from_20gen, df$n_from_50gen, df$n_from_75gen)
  cluster_sizes[cluster_sizes == 0] <- NA
  
  df$max_cluster_size <- apply(cluster_sizes, 1, max, na.rm = TRUE)
  df$min_cluster_size <- apply(cluster_sizes, 1, min, na.rm = TRUE)
  df$cluster_balance_ratio <- df$max_cluster_size / df$min_cluster_size
  
  # Replace infinite values with max_cluster_size (for single cluster cases)
  df$cluster_balance_ratio[is.infinite(df$cluster_balance_ratio)] <- df$max_cluster_size[is.infinite(df$cluster_balance_ratio)]
  
  return(df)
}

clustering_summary <- calculate_cluster_properties(clustering_summary)

# === CREATE PLOT DATA FOR LIKELIHOOD IMPROVEMENTS ===

plot_data_ll <- clustering_summary %>%
  select(n_true_clusters, k1_to_k2, k2_to_k3, k3_to_k4, total_inds, 
         k1_generation, generation_combo, cluster_balance_ratio, 
         classification, predicted_k, method) %>%
  pivot_longer(cols = c(k1_to_k2, k2_to_k3, k3_to_k4), 
               names_to = "transition", 
               values_to = "likelihood_improvement") %>%
  mutate(
    plot_generation = case_when(
      n_true_clusters == 1 ~ k1_generation,
      n_true_clusters %in% c(2, 3) ~ generation_combo,
      TRUE ~ "unknown"
    )
  )
plot_data_ll$transition<-gsub("_", " ", plot_data_ll$transition)
# === CREATE PLOT DATA FOR CLUSTERING STRENGTH (k2 only) ===

plot_data_clust <- clustering_summary %>%
  filter(!is.na(clustering_strength_k2)) %>%
  select(n_true_clusters, clustering_strength_k2, total_inds,
         k1_generation, generation_combo, cluster_balance_ratio,
         classification, predicted_k, method, k1_to_k2) %>%
  mutate(
    plot_generation = case_when(
      n_true_clusters == 1 ~ k1_generation,
      n_true_clusters %in% c(2, 3) ~ generation_combo,
      TRUE ~ "unknown"
    ),
    transition = "k1 to k2"  # For merging with likelihood data
  )

# === PLOT 1: LIKELIHOOD IMPROVEMENTS BY METHOD ===
# Define thresholds for each method
thresholds_GT <- data.frame(
  method = "GT",
  transition = c("k1 to k2", "k2 to k3", "k3 to k4"),
  threshold_value = c(0.69, 0.43, 0.31),
  threshold_type = c("k>=2", "k>=3", "k>=4"),
  stringsAsFactors = FALSE
)

thresholds_DATES <- data.frame(
  method = "DATES",
  transition = c("k1 to k2", "k2 to k3", "k3 to k4"),
  threshold_value = c(1.20, 0.71, 0.49),
  threshold_type = c("k>=2", "k>=3", "k>=4"),
  stringsAsFactors = FALSE
)

# NEW: Add the stricter k1_to_k2 threshold for k>=3
thresholds_k3_GT <- data.frame(
  method = "GT",
  transition = "k1 to k2",
  threshold_value = 0.90,
  threshold_type = "k>=3 (stricter)",
  stringsAsFactors = FALSE
)

thresholds_k3_DATES <- data.frame(
  method = "DATES",
  transition = "k1 to k2",
  threshold_value = 2.59,
  threshold_type = "k>=3 (stricter)",
  stringsAsFactors = FALSE
)

thresholds_combined <- rbind(thresholds_GT, thresholds_DATES)
thresholds_k3_combined <- rbind(thresholds_k3_GT, thresholds_k3_DATES)

# Custom shape scale for predictions
shape_values <- c(
  "correct" = 16,
  "predicted_k1" = 49,
  "predicted_k2" = 50,
  "predicted_k3" = 51,
  "predicted_k4" = 52
)


p_likelihood <- ggplot(plot_data_ll, 
                       aes(x = factor(n_true_clusters), 
                           y = likelihood_improvement)) +
  geom_boxplot(aes(fill = plot_generation), 
               position = position_dodge(width = 0.8), 
               alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = plot_generation, shape = classification), 
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.15),
              alpha = 0.85, size = 3.2) +
  # Original thresholds (red dashed)
  geom_hline(data = thresholds_combined, 
             aes(yintercept = threshold_value), 
             color = "red", linetype = "dashed", linewidth = 0.8) +
  # NEW: k>=3 threshold for k1_to_k2 (blue dotted)
  geom_hline(data = thresholds_k3_combined, 
             aes(yintercept = threshold_value), 
             color = "blue", linetype = "dotted", linewidth = 1) +
  facet_wrap(method ~ transition, scales = "free_y") +
  scale_shape_manual(
    values = shape_values,
    labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4"),
    name = "Classification"
  ) +
  labs(
    x = "True Number of Clusters", 
    y = "Scaled Likelihood Improvement",
    fill = "Generation",
    color = "Generation"
  ) +
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.position = "right",
    legend.text = element_text(size = 23),      # Legend text
    legend.title = element_text(size = 23, face = "bold"),  # Legend titles
    strip.text = element_text(size = 23, face = "bold"),    # Facet labels (increased from 10)
    axis.title = element_text(size = 23, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title
    plot.subtitle = element_text(size = 13),                 # Subtitle
    panel.spacing = unit(1, "lines")
  )



pdf("plots/likelihood_threshold_calcs.pdf", width=18, height=15)
print(p_likelihood)
dev.off()

# === PLOT 2: COMBINED LIKELIHOOD + CLUSTERING STRENGTH (k1→k2 only) ===

# For this, we'll create a scatter plot with clustering strength on one axis
# and likelihood improvement on the other

plot_data_k1tok2 <- clustering_summary %>%
  filter(!is.na(clustering_strength_k2) & !is.na(k1_to_k2), n_true_clusters!=3) %>%
  mutate(
    plot_generation = case_when(
      n_true_clusters == 1 ~ k1_generation,
      n_true_clusters %in% c(2, 3) ~ generation_combo,
      TRUE ~ "unknown"
    )
  )

# Define threshold lines for each method
clust_thresh_GT <- 0.78
ll_thresh_GT <- 0.69
clust_thresh_DATES <- 0.983
ll_thresh_DATES <- 1.20

p_combined <- ggplot(plot_data_k1tok2, 
                     aes(x = clustering_strength_k2, 
                         y = k1_to_k2)) +
  geom_point(aes(color = plot_generation, shape = classification),
             alpha = 0.85, size = 2.5) +
  # Add threshold lines for each method's facet
  geom_vline(data = data.frame(method = "GT", x = clust_thresh_GT),
             aes(xintercept = x), color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(data = data.frame(method = "GT", y = ll_thresh_GT),
             aes(yintercept = y), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(data = data.frame(method = "DATES", x = clust_thresh_DATES),
             aes(xintercept = x), color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(data = data.frame(method = "DATES", y = ll_thresh_DATES),
             aes(yintercept = y), color = "red", linetype = "dashed", linewidth = 1) +
  facet_grid(n_true_clusters ~ method, scales = "free") +
  scale_shape_manual(
    values = shape_values,
    labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4"),
    name = "Classification"
  ) +
  labs(
    x = "Clustering Strength (k=2)",
    y = "Scaled Likelihood Improvement (k1 to k2)",
    color = "Generation"
  ) +
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.position = "right",
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels (increased from 10)
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title
    plot.subtitle = element_text(size = 13),                 # Subtitle
    panel.spacing = unit(1, "lines")
  )

print(p_combined)

pdf("plots/sims_k1tok2_combined_NEWCV.pdf", width=12, height=10)
print(p_combined)
dev.off()

# === SUMMARY STATISTICS ===

# Calculate error rates by method and true k
error_summary <- clustering_summary %>%
  group_by(method, n_true_clusters) %>%
  summarise(
    n_total = n(),
    n_correct = sum(classification == "correct"),
    n_wrong = sum(classification != "correct"),
    error_rate = n_wrong / n_total,
    .groups = "drop"
  )

print(error_summary)

# Confusion matrix for each method
confusion_GT <- table(
  True = clustering_summary[clustering_summary$method=="GT", ]$n_true_clusters,
  Predicted = clustering_summary[clustering_summary$method=="GT", ]$predicted_k
)

confusion_DATES <- table(
  True = clustering_summary[clustering_summary$method=="DATES", ]$n_true_clusters,
  Predicted = clustering_summary[clustering_summary$method=="DATES", ]$predicted_k
)

print("GLOBETROTTER Confusion Matrix:")
print(confusion_GT)
print("\nDATES Confusion Matrix:")
print(confusion_DATES)


# === ANALYsE PROPERTIES OF MISCLASSIFIED CASES ===

cat("\\n=== ANALYSIS OF MISCLASSIFIED CASES ===\\n")

# K=1 misclassified (wrongly clustered)
k1_wrong <- clustering_summary %>% 
  filter(n_true_clusters == 1, classification != "correct")

# K=2 misclassified  
k2_wrong <- clustering_summary %>% 
  filter(n_true_clusters == 2, classification != "correct")

# K=3 misclassified
k3_wrong <- clustering_summary %>% 
  filter(n_true_clusters == 3, classification != "correct")

# === COMPARISON: CORRECT vs WRONG PROPERTIES ===

cat("\\n=== COMPARISON: CORRECT vs MISCLASSIFIED PROPERTIES ===\\n")

comparison_data <- clustering_summary %>%
  group_by(n_true_clusters, classification, method) %>%
  summarise(
    n = n(),
    mean_sample_size = mean(total_inds, na.rm = TRUE),
    mean_balance_ratio = mean(cluster_balance_ratio, na.rm = TRUE),
    median_balance_ratio = median(cluster_balance_ratio, na.rm = TRUE),
    .groups = "drop"
  )

print(comparison_data)
# Define all possible classification levels
all_classifications <- c("correct","predicted_k1" ,"predicted_k2" ,"predicted_k3", "predicted_k4")  # Adjust to your actual levels

clustering_summary <- clustering_summary %>%
  mutate(classification = factor(classification, levels = all_classifications))

# Plot number inds comparison
p_total_inds <- ggplot(clustering_summary, aes(x = factor(n_true_clusters), y = total_inds, 
                                               fill = classification)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  facet_wrap(~method) +
  geom_jitter(aes(color = classification), 
              alpha = 0.8, 
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +
  scale_y_log10() + 
  labs(x = "True Number of Clusters", 
       y = "Number of individuals", 
       fill = "Classification",
       color = "Classification") +  # Added color legend label
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )



pdf("plots/sims_classification_by_total_inds.pdf", width=15, height=10)
p_total_inds
dev.off()

library(dplyr)

# Create binary outcome
clustering_summary <- clustering_summary %>%
  mutate(is_correct = ifelse(classification == "correct", 1, 0))

# Logistic regression for each method and true k
cat("\n=== LOGISTIC REGRESSION RESULTS ===\n\n")

for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s\n", m))
  
  for (k in sort(unique(clustering_summary$n_true_clusters))) {
    subset_data <- clustering_summary %>% 
      filter(method == m, n_true_clusters == k)
    
    # Fit model
    model <- glm(is_correct ~ total_inds, 
                 data = subset_data, 
                 family = binomial)
    
    # Extract results
    coef_summary <- summary(model)$coefficients
    estimate <- coef_summary["total_inds", "Estimate"]
    p_value <- coef_summary["total_inds", "Pr(>|z|)"]
    
    # Odds ratio
    OR <- exp(estimate)
    
    # Sample info
    n_cases <- nrow(subset_data)
    accuracy <- mean(subset_data$is_correct)
    
    cat(sprintf("  k=%s: n=%d, accuracy=%.2f, OR=%.4f, p=%.4f %s\n",
                k, n_cases, accuracy, OR, p_value,
                ifelse(p_value < 0.05, "*", "")))
  }
  cat("\n")
}

cat("\n=== MISCLASSIFICATION PATTERNS ===\n\n")
for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s (True k=1)\n", m))
  
  subset_data <- clustering_summary %>%
    filter(method == m, n_true_clusters == 1)
  
  if (nrow(subset_data) > 0) {
    subset_data <- subset_data %>%
      mutate(misclass_k2 = ifelse(predicted_k == 2, 1, 0))
    
    if (sum(subset_data$misclass_k2) > 5) {
      model_k2 <- glm(misclass_k2 ~ total_inds, 
                      data = subset_data, 
                      family = binomial)
      coef_k2 <- summary(model_k2)$coefficients
      OR_k2 <- exp(coef_k2["total_inds", "Estimate"])
      p_k2 <- coef_k2["total_inds", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=2: OR=%.4f, p=%.4f %s\n",
                  OR_k2, p_k2, ifelse(p_k2 < 0.05, "*", "")))
    }
  }
  cat("\n")
}

for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s\n", m))
  
  for (k_true in sort(unique(clustering_summary$n_true_clusters))) {
    cat(sprintf("\n  True k=%s:\n", k_true))
    
    # Get misclassified cases
    misclass <- clustering_summary %>%
      filter(method == m, n_true_clusters == k_true, classification != "correct")
    
    if (nrow(misclass) > 0) {
      # Count each type of misclassification
      misclass_summary <- misclass %>%
        group_by(predicted_k) %>%
        summarise(
          n_cases = n(),
          mean_inds = mean(total_inds),
          median_inds = median(total_inds),
          .groups = "drop"
        ) %>%
        arrange(desc(n_cases))
      
      for (i in 1:nrow(misclass_summary)) {
        cat(sprintf("    Predicted k=%s: n=%d cases (median individuals=%.0f)\n",
                    misclass_summary$predicted_k[i],
                    misclass_summary$n_cases[i],
                    misclass_summary$median_inds[i]))
      }
    } else {
      cat("    No misclassifications!\n")
    }
  }
  cat("\n")
}

# ============================================================================
# 2. Logistic regression by misclassification type
# ============================================================================

cat("\n=== SAMPLE SIZE EFFECT ON SPECIFIC MISCLASSIFICATIONS ===\n\n")

# For k=2
for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s (True k=2)\n", m))
  
  subset_data <- clustering_summary %>%
    filter(method == m, n_true_clusters == 2)
  
  if (nrow(subset_data) > 0) {
    subset_data <- subset_data %>%
      mutate(
        misclass_k1 = ifelse(predicted_k == 1, 1, 0),
        misclass_k3 = ifelse(predicted_k == 3, 1, 0)
      )
    
    # Model for predicting k=1 when true k=2
    if (sum(subset_data$misclass_k1) > 5) {
      model_k1 <- glm(misclass_k1 ~ total_inds, 
                      data = subset_data, 
                      family = binomial)
      coef_k1 <- summary(model_k1)$coefficients
      OR_k1 <- exp(coef_k1["total_inds", "Estimate"])
      p_k1 <- coef_k1["total_inds", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=1: OR=%.4f, p=%.4f %s\n",
                  OR_k1, p_k1, ifelse(p_k1 < 0.05, "*", "")))
    }
    
    # Model for predicting k=3 when true k=2
    if (sum(subset_data$misclass_k3) > 5) {
      model_k3 <- glm(misclass_k3 ~ total_inds, 
                      data = subset_data, 
                      family = binomial)
      coef_k3 <- summary(model_k3)$coefficients
      OR_k3 <- exp(coef_k3["total_inds", "Estimate"])
      p_k3 <- coef_k3["total_inds", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=3: OR=%.4f, p=%.4f %s\n",
                  OR_k3, p_k3, ifelse(p_k3 < 0.05, "*", "")))
    }
  }
  cat("\n")
}

# For k=3
for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s (True k=3)\n", m))
  
  subset_data <- clustering_summary %>%
    filter(method == m, n_true_clusters == 3)
  
  if (nrow(subset_data) > 0) {
    subset_data <- subset_data %>%
      mutate(
        misclass_k2 = ifelse(predicted_k == 2, 1, 0),
        misclass_k4 = ifelse(predicted_k == 4, 1, 0)
      )
    
    # Model for predicting k=2 when true k=3
    if (sum(subset_data$misclass_k2) > 5) {
      model_k2 <- glm(misclass_k2 ~ total_inds, 
                      data = subset_data, 
                      family = binomial)
      coef_k2 <- summary(model_k2)$coefficients
      OR_k2 <- exp(coef_k2["total_inds", "Estimate"])
      p_k2 <- coef_k2["total_inds", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=2: OR=%.4f, p=%.4f %s\n",
                  OR_k2, p_k2, ifelse(p_k2 < 0.05, "*", "")))
    }
    
    # Model for predicting k=4 when true k=3
    if (sum(subset_data$misclass_k4) > 5) {
      model_k4 <- glm(misclass_k4 ~ total_inds, 
                      data = subset_data, 
                      family = binomial)
      coef_k4 <- summary(model_k4)$coefficients
      OR_k4 <- exp(coef_k4["total_inds", "Estimate"])
      p_k4 <- coef_k4["total_inds", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=4: OR=%.4f, p=%.4f %s\n",
                  OR_k4, p_k4, ifelse(p_k4 < 0.05, "*", "")))
    }
  }
  cat("\n")
}




##### new more simple pplot:
library(dplyr)
library(ggplot2)
library(forcats)

# If you already have a numeric predicted_k column, use that directly.
# Otherwise derive it from your classification factor like:

clustering_summary2 <- clustering_summary %>%
  mutate(
    # make sure true k is numeric, not factor
    n_true_clusters_num = as.integer(as.character(n_true_clusters)),
    
    predicted_k = case_when(
      classification == "correct"        ~ n_true_clusters_num,
      classification == "predicted_k1"   ~ 1L,
      classification == "predicted_k2"   ~ 2L,
      classification == "predicted_k3"   ~ 3L,
      classification == "predicted_k4"   ~ 4L,
      TRUE                               ~ NA_integer_
    ),
    
    over_under = case_when(
      is.na(predicted_k)                      ~ NA_character_,
      predicted_k >  n_true_clusters_num      ~ "over_clustered",
      predicted_k <  n_true_clusters_num      ~ "under_clustered",
      predicted_k == n_true_clusters_num      ~ "correct"
    )
  )

# Define bins of total_inds (tune breaks/labels as you like)
clustering_binned <- clustering_summary2 %>%
  mutate(
    n_bin = cut(
      total_inds,
      breaks = c(0, 25,50, 100, 150, 200),  # adjust as appropriate
      labels = c("<=25", "26–50", "51–100", "101–150", "141-200"),
      right = TRUE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(over_under))  # drop rows where we couldn't define category

# 1. Counts per outcome
over_under_counts <- clustering_binned %>%
  group_by(method, n_bin, n_true_clusters, over_under) %>%
  summarise(n = n(), .groups = "drop")

# 2. Totals per bin (method, n_bin, n_true_clusters)
bin_totals <- over_under_counts %>%
  group_by(method, n_bin, n_true_clusters) %>%
  summarise(total_in_bin = sum(n), .groups = "drop")

# 3. Join and compute percentages
over_under_summary <- over_under_counts %>%
  left_join(bin_totals,
            by = c("method", "n_bin", "n_true_clusters")) %>%
  mutate(
    pct = 100 * n / total_in_bin
  )


gg_over_under <- ggplot(over_under_summary,
                        aes(x = n_bin, y = pct, fill = over_under)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~ method+n_true_clusters) +
  labs(
    x = "Number of individuals",
    y = "Percentage of runs",
    fill = "Outcome"
  ) +
  scale_fill_manual(
    values = c(
      "over_clustered" = "#D55E00",
      "under_clustered" = "#0072B2",
      "correct" = "#009E73"
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold")
  )

pdf("plots/sims_over_under_by_total_inds_bins.pdf", width = 12, height = 9)
print(gg_over_under)
dev.off()



# Define bins of total_inds (tune breaks/labels as you like)
clustering_binned <- clustering_summary2 %>%
  mutate(
    n_bin = cut(
      cluster_balance_ratio,
      breaks = c(0, 2,4, 6, 10, 15),  # adjust as appropriate
      labels = c("<=2", "3-4", "5-6", "7-10", "11-15"),
      right = TRUE,
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(over_under))  # drop rows where we couldn't define category

# 1. Counts per outcome
over_under_counts <- clustering_binned %>%
  group_by(method, n_bin, n_true_clusters, over_under) %>%
  summarise(n = n(), .groups = "drop")

# 2. Totals per bin (method, n_bin, n_true_clusters)
bin_totals <- over_under_counts %>%
  group_by(method, n_bin, n_true_clusters) %>%
  summarise(total_in_bin = sum(n), .groups = "drop")

# 3. Join and compute percentages
over_under_summary <- over_under_counts %>%
  left_join(bin_totals,
            by = c("method", "n_bin", "n_true_clusters")) %>%
  mutate(
    pct = 100 * n / total_in_bin
  )


gg_over_under <- ggplot(filter(over_under_summary, n_true_clusters!=1),
                        aes(x = n_bin, y = pct, fill = over_under)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~ method+n_true_clusters) +
  labs(
    x = "Ratio between minimun and maximum number of individuals in a cluster",
    y = "Percentage of runs",
    fill = "Outcome"
  ) +
  scale_fill_manual(
    values = c(
      "over_clustered" = "#D55E00",
      "under_clustered" = "#0072B2",
      "correct" = "#009E73"
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold")
  )

pdf("plots/sims_over_under_by_tclusterimbalance.pdf", width = 12, height = 9)
print(gg_over_under)
dev.off()





# Plot number inds comparison
p_balance <- ggplot(clustering_summary[clustering_summary$n_true_clusters!=1, ], aes(x = factor(n_true_clusters), y = cluster_balance_ratio, 
                                               fill = classification)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  facet_wrap(~method) +
  geom_jitter(aes(color = classification), 
              alpha = 0.8, 
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +
  scale_y_log10() + 
  labs(x = "True Number of Clusters", 
       y = "Ratio between minimun and maximum number of individuals in a cluster", 
       fill = "Classification",
       color = "Classification") +  # Added color legend label
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )


print(p_balance)
pdf("plots/sims_classification_by_cluster_balance.pdf", width=15, height=10)
p_balance
dev.off()



# For k=2
for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s (True k=2)\n", m))
  
  subset_data <- clustering_summary %>%
    filter(method == m, n_true_clusters == 2)
  
  if (nrow(subset_data) > 0) {
    subset_data <- subset_data %>%
      mutate(
        misclass_k1 = ifelse(predicted_k == 1, 1, 0),
        misclass_k3 = ifelse(predicted_k == 3, 1, 0)
      )
    
    # Model for predicting k=1 when true k=2
    if (sum(subset_data$misclass_k1) > 5) {
      model_k1 <- glm(misclass_k1 ~ cluster_balance_ratio, 
                      data = subset_data, 
                      family = binomial)
      coef_k1 <- summary(model_k1)$coefficients
      OR_k1 <- exp(coef_k1["cluster_balance_ratio", "Estimate"])
      p_k1 <- coef_k1["cluster_balance_ratio", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=1: OR=%.4f, p=%.4f %s\n",
                  OR_k1, p_k1, ifelse(p_k1 < 0.05, "*", "")))
    }
    
    # Model for predicting k=3 when true k=2
    if (sum(subset_data$misclass_k3) > 5) {
      model_k3 <- glm(misclass_k3 ~ cluster_balance_ratio, 
                      data = subset_data, 
                      family = binomial)
      coef_k3 <- summary(model_k3)$coefficients
      OR_k3 <- exp(coef_k3["cluster_balance_ratio", "Estimate"])
      p_k3 <- coef_k3["cluster_balance_ratio", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=3: OR=%.4f, p=%.4f %s\n",
                  OR_k3, p_k3, ifelse(p_k3 < 0.05, "*", "")))
    }
  }
  cat("\n")
}

# For k=3
for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s (True k=3)\n", m))
  
  subset_data <- clustering_summary %>%
    filter(method == m, n_true_clusters == 3)
  
  if (nrow(subset_data) > 0) {
    subset_data <- subset_data %>%
      mutate(
        misclass_k2 = ifelse(predicted_k == 2, 1, 0),
        misclass_k4 = ifelse(predicted_k == 4, 1, 0)
      )
    
    # Model for predicting k=2 when true k=3
    if (sum(subset_data$misclass_k2) > 5) {
      model_k2 <- glm(misclass_k2 ~ cluster_balance_ratio, 
                      data = subset_data, 
                      family = binomial)
      coef_k2 <- summary(model_k2)$coefficients
      OR_k2 <- exp(coef_k2["cluster_balance_ratio", "Estimate"])
      p_k2 <- coef_k2["cluster_balance_ratio", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=2: OR=%.4f, p=%.4f %s\n",
                  OR_k2, p_k2, ifelse(p_k2 < 0.05, "*", "")))
    }
    
    # Model for predicting k=4 when true k=3
    if (sum(subset_data$misclass_k4) > 5) {
      model_k4 <- glm(misclass_k4 ~ cluster_balance_ratio, 
                      data = subset_data, 
                      family = binomial)
      coef_k4 <- summary(model_k4)$coefficients
      OR_k4 <- exp(coef_k4["cluster_balance_ratio", "Estimate"])
      p_k4 <- coef_k4["cluster_balance_ratio", "Pr(>|z|)"]
      
      cat(sprintf("  Misclassify as k=4: OR=%.4f, p=%.4f %s\n",
                  OR_k4, p_k4, ifelse(p_k4 < 0.05, "*", "")))
    }
  }
  cat("\n")
}


# 1. Compute max_accuracy
clustering_summary <- clustering_summary %>%
  mutate(
    max_accuracy = pmax(
      accuracy_k2,
      accuracy_k3,
      accuracy_k4,
      na.rm = TRUE
    )
  )

# 2. Filter out k=1 and focus on k=2
k2_data <- clustering_summary %>%
  filter(n_true_clusters !=1)


pdf("plots/sims_clusteringsaccuracy_newmeasure.pdf", width=15, height=10)
# 3. Plot boxplot + jitter, colored by generation_combo and shaped by classification
ggplot(filter(clustering_summary_pred, n_true_clusters!=1), aes(
  x = factor(generation_combo),  # will be "2" for all
  y = pair_together_pred)) +
  # geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_boxplot(aes(
    color = generation_combo)) +  # Increased from 2 to 3.5
  scale_color_viridis_d(
    name = "Gen Combination",
    option = "plasma"
  ) +
  scale_shape_manual(
    values = shape_values,
    labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4"),
    name = "Classification"
  ) +
  labs(
    x = "Generation combination",
    y = "Percentage of pairs correctly together"
  ) +
  facet_wrap(~method) +
  theme_minimal(base_size = 16) +  # Increased base size
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )



dev.off()



pair_binned_balance <- clustering_summary_pred %>%
  mutate(
    n_bin_balance = cut(
      cluster_balance_ratio,
      breaks = c(0, 2, 4, 6, 10, 15),
      labels = c("<=2", "3–4", "5–6", "7–10", "11–15"),
      right = TRUE,
      include.lowest = TRUE
    )
  )


pdf("plots/sims_clusteringsaccuracy_newmeasure.pdf", width=15, height=10)
# 3. Plot boxplot + jitter, colored by generation_combo and shaped by classification
ggplot(filter(m, n_true_clusters!=1), aes(
  x = factor(n_bin_balance),  # will be "2" for all
  y = pair_wrong_together_pred)) +
  # geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_boxplot(aes(
    )) +  # Increased from 2 to 3.5
  scale_color_viridis_d(
    name = "Gen Combination",
    option = "plasma"
  ) +
  scale_shape_manual(
    values = shape_values,
    labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4"),
    name = "Classification"
  ) +
  labs(
    x = "Generation combination",
    y = "Percentage of pairs correctly together"
  ) +
  facet_wrap(~method) +
  theme_minimal(base_size = 16) +  # Increased base size
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )



dev.off()






pdf("plots/sims_clusteringsaccuracy_vs_balance_ratio.pdf", width=15, height=10)
ggplot(clustering_summary[clustering_summary$n_true_clusters!=1,]) +
  geom_point(aes(x=max_accuracy, y=cluster_balance_ratio, color = generation_combo,
                 shape = classification),
             size = 3.5) +  scale_color_viridis_d(
                   name = "Gen Combination",
                   option = "plasma"
                 ) +
  scale_shape_manual(
    values = shape_values,
    labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4"),
    name = "Classification"
  ) + facet_wrap(~method)+
  labs(x="Clustering accuracy for true k", y="Ratio between minimun and maximum number of individuals in a
cluster")+theme_minimal(base_size = 16) +  # Increased base size
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )
dev.off()




pdf("plots/sims_clusteringsaccuracy_vs_total_inds.pdf", width=15, height=10)
ggplot(clustering_summary_pred[clustering_summary_pred$n_true_clusters!=1,]) +
  geom_point(aes(y=pair_together_pred, x=total_inds, color = generation_combo),
             size = 3.5) +  scale_color_viridis_d(
               name = "Gen Combination",
               option = "plasma"
             ) + facet_wrap(~method)+
  labs(y="Pairs together correct", x="Total individuals")+theme_minimal(base_size = 16) +  # Increased base size
  theme(
    legend.text = element_text(size = 20),      # Legend text
    legend.title = element_text(size = 20, face = "bold"),  # Legend titles
    strip.text = element_text(size = 20, face = "bold"),    # Facet labels
    axis.title = element_text(size = 20, face = "bold"),    # Axis titles
    axis.text = element_text(size = 17),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )
dev.off()


library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)  # install.packages("patchwork") if needed

## 1. Add pairwise measures at predicted k ----

clustering_summary_pred <- clustering_summary2 %>%
  mutate(
    pair_together_pred = case_when(
      predicted_k == 2L ~ pair_together_correct_k2,
      predicted_k == 3L ~ pair_together_correct_k3,
      predicted_k == 4L ~ pair_together_correct_k4,
      TRUE              ~ NA_real_
    ),
    pair_apart_pred = case_when(
      predicted_k == 2L ~ pair_apart_correct_k2,
      predicted_k == 3L ~ pair_apart_correct_k3,
      predicted_k == 4L ~ pair_apart_correct_k4,
      TRUE              ~ NA_real_
    ),
    pair_wrong_together_pred = case_when(
      predicted_k == 2L ~ pair_wrong_together_pred_k2,
      predicted_k == 3L ~ pair_wrong_together_pred_k3,
      predicted_k == 4L ~ pair_wrong_together_pred_k4,
      TRUE              ~ NA_real_
    )
  )

## 2. Plot 1: total_inds bins vs pair_together_pred (points, coloured by gen combo) ----

pair_binned_total_inds <- clustering_summary_pred %>%
  mutate(
    n_bin_inds = cut(
      total_inds,
      breaks = c(0, 25, 50, 100, 150, 200),
      labels = c("<=25", "26–50", "51–100", "101–150", "151–200"),
      right = TRUE,
      include.lowest = TRUE
    )
  )


# 1. Counts per outcome
over_under_counts <- clustering_binned %>%
  group_by(method, n_bin, n_true_clusters, over_under) %>%
  summarise(n = n(), .groups = "drop")

# 2. Totals per bin (method, n_bin, n_true_clusters)
bin_totals <- over_under_counts %>%
  group_by(method, n_bin, n_true_clusters) %>%
  summarise(total_in_bin = sum(n), .groups = "drop")

# 3. Join and compute percentages
over_under_summary <- over_under_counts %>%
  left_join(bin_totals,
            by = c("method", "n_bin", "n_true_clusters")) %>%
  mutate(
    pct = 100 * n / total_in_bin
  )


gg_over_under <- ggplot(filter(over_under_summary, n_true_clusters!=1),
                        aes(x = n_bin, y = pct, fill = over_under)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~ method+n_true_clusters) +
  labs(
    x = "Ratio between minimun and maximum number of individuals in a cluster",
    y = "Percentage of runs",
    fill = "Outcome"
  ) +
  scale_fill_manual(
    values = c(
      "over_clustered" = "#D55E00",
      "under_clustered" = "#0072B2",
      "correct" = "#009E73"
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold")
  )






ggplot(
  filter(pair_binned_total_inds, !n_true_clusters),
  aes(x = n_bin_inds, y = pair_together_pred)
) +
  geom_col(
    position ="stack",
    alpha = 0.8,
    size = 2.8
  ) +
  facet_wrap(~ method+ generation_combo) +

  labs(
    x = "Number of individuals",
    y = "Pairs together correct"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title   = element_text(size = 20, face = "bold"),
    axis.text    = element_text(size = 14),
    strip.text   = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 14)
  )

## 3. Plot 2: cluster_balance_ratio bins vs pair_together_pred (points, coloured by gen combo) ----

pair_binned_balance <- clustering_summary_pred %>%
  mutate(
    n_bin_balance = cut(
      cluster_balance_ratio,
      breaks = c(0, 2, 4, 6, 10, 15),
      labels = c("<=2", "3–4", "5–6", "7–10", "11–15"),
      right = TRUE,
      include.lowest = TRUE
    )
  )

p_balance <- ggplot(
  pair_binned_balance,
  aes(x = n_bin_balance, y = pair_together_pred, colour = generation_combo)
) +
  geom_point(
    position = position_jitter(width = 0.1, height = 0),
    alpha = 0.8,
    size = 2.8
  ) +
  facet_wrap(~ method) +
  scale_color_viridis_d(
    name = "Gen combination",
    option = "plasma"
  ) +
  labs(
    x = "Cluster balance ratio (binned)",
    y = "Pairwise 'together' accuracy\n(at predicted k)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title   = element_text(size = 20, face = "bold"),
    axis.text    = element_text(size = 14),
    strip.text   = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 14)
  )

## 4. Stack the two plots and save as a single PDF ----

combined_plot <- p_inds / p_balance + plot_layout(ncol = 1)

pdf("plots/sims_pairwise_together_pred_by_inds_and_balance_bins.pdf",
    width = 12, height = 12)
print(combined_plot)
dev.off()
pdf("plots/sims_pairwise_together_pred_by_total_inds_bins.pdf", width = 12, height = 8)
ggplot(pair_binned_total,
       aes(x = n_bin, y = mean_pair_together_pred)) +
  geom_col(fill = "#0072B2", alpha = 0.8) +
  facet_wrap(~ method ) +
  labs(
    x = "Number of individuals (binned)",
    y = "Mean pairwise clustering accuracy"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 20, face = "bold"),
    axis.text  = element_text(size = 14),
    strip.text = element_text(size = 18, face = "bold")
  )
dev.off()



###stats test for this
# Spearman correlation by generation combo
cat("\n=== SPEARMAN CORRELATION BY GENERATION COMBO ===\n\n")

for (m in unique(clustering_summary$method)) {
  cat(sprintf("## %s\n\n", m))
  
  for (gen in unique(clustering_summary$generation_combo)) {
    cat(sprintf("  ### %s\n", gen))
    
    subset_data <- clustering_summary %>%
      filter(method == m, n_true_clusters != 1, generation_combo == gen)
    
    if (nrow(subset_data) >= 20) {
      # Total individuals vs accuracy
      cor_inds <- cor.test(subset_data$total_inds, subset_data$max_accuracy, 
                           method = "spearman")
      cat(sprintf("    Total inds vs accuracy: rho=%.3f, p=%.4f %s\n",
                  cor_inds$estimate, cor_inds$p.value,
                  ifelse(cor_inds$p.value < 0.05, "*", "")))
      
      # Balance ratio vs accuracy
      cor_balance <- cor.test(subset_data$cluster_balance_ratio, subset_data$max_accuracy,
                              method = "spearman")
      cat(sprintf("    Balance ratio vs accuracy: rho=%.3f, p=%.4f %s\n",
                  cor_balance$estimate, cor_balance$p.value,
                  ifelse(cor_balance$p.value < 0.05, "*", "")))
    } else {
      cat(sprintf("    (Insufficient data: n=%d)\n", nrow(subset_data)))
    }
    
    cat("\n")
  }
}


###not to show how often they overlap with the true date

library(dplyr)
library(tidyr)
library(ggplot2)


# Function to check if CI overlaps with true date
# Rounds lower CI down and upper CI up to nearest generation
ci_overlaps <- function(ci_lower, ci_upper, true_date) {
  if(is.na(ci_lower) || is.na(ci_upper) || is.na(true_date)) return(FALSE)
  
  # Round lower bound down, upper bound up to nearest generation
  ci_lower_rounded <- floor(ci_lower)
  ci_upper_rounded <- ceiling(ci_upper)
  
  true_date >= ci_lower_rounded & true_date <= ci_upper_rounded
}

# Function to get true dates for a row
get_true_dates <- function(row) {
  dates <- c()
  if(row$n_from_20gen > 0) dates <- c(dates, 20)
  if(row$n_from_50gen > 0) dates <- c(dates, 50)
  if(row$n_from_75gen > 0) dates <- c(dates, 75)
  return(dates)
}

# For each simulation, check how many TRUE DATES are covered by predicted clusters
score_simulation <- function(row) {
  method <- row$method
  filename <- row$filename
  true_k <- row$n_true_clusters
  predicted_k <- row$predicted_k
  classification <- row$classification
  generation_combo <- row$generation_combo
  true_dates <- get_true_dates(row)
  
  n_true_covered <- 0
  
  # For each TRUE DATE, check if ANY predicted cluster covers it
  for(tdate in true_dates) {
    covered <- FALSE
    
    # Check all predicted clusters
    for(j in 1:predicted_k) {
      col_lower <- sprintf("k%d_cluster%d_lower_ci", predicted_k, j)
      col_upper <- sprintf("k%d_cluster%d_upper_ci", predicted_k, j)
      
      if(col_lower %in% names(row) && col_upper %in% names(row)) {
        lower_ci <- row[[col_lower]]
        upper_ci <- row[[col_upper]]
        
        if(ci_overlaps(lower_ci, upper_ci, tdate)) {
          covered <- TRUE
          break  # This true date is covered, move to next true date
        }
      }
    }
    
    if(covered) n_true_covered <- n_true_covered + 1
  }
  
  tibble(
    filename = filename,
    method = method,
    true_k = true_k,
    predicted_k = predicted_k,
    classification = classification,
    generation_combo = generation_combo,
    n_true_dates = length(true_dates),
    n_true_covered = n_true_covered,
    prop_covered = n_true_covered / length(true_dates)
  )
}

# Apply to all rows
results_per_sim <- clustering_summary %>%
  rowwise() %>%
  do(score_simulation(.)) %>%
  ungroup()



# Function to check if CI overlaps with true date
# Rounds lower CI down and upper CI up to nearest generation
ci_overlaps <- function(ci_lower, ci_upper, true_date) {
  if(is.na(ci_lower) || is.na(ci_upper) || is.na(true_date)) return(FALSE)
  
  # Round lower bound down, upper bound up to nearest generation
  ci_lower_rounded <- floor(ci_lower)
  ci_upper_rounded <- ceiling(ci_upper)
  
  true_date >= ci_lower_rounded & true_date <= ci_upper_rounded
}


# Function to get true dates for a row
get_true_dates <- function(row) {
  dates <- c()
  if(row$n_from_20gen > 0) dates <- c(dates, 20)
  if(row$n_from_50gen > 0) dates <- c(dates, 50)
  if(row$n_from_75gen > 0) dates <- c(dates, 75)
  return(dates)
}

# Score by PREDICTED CLUSTERS: how many predicted clusters overlap with a true date
score_by_predicted <- function(row) {
  method <- row$method
  filename <- row$filename
  true_k <- row$n_true_clusters
  predicted_k <- row$predicted_k
  classification <- row$classification
  generation_combo <- row$generation_combo
  true_dates <- get_true_dates(row)
  
  n_predicted_correct <- 0
  
  # For each PREDICTED CLUSTER, check if it overlaps with ANY true date
  for(j in 1:predicted_k) {
    col_lower <- sprintf("k%d_cluster%d_lower_ci", predicted_k, j)
    col_upper <- sprintf("k%d_cluster%d_upper_ci", predicted_k, j)
    
    if(col_lower %in% names(row) && col_upper %in% names(row)) {
      lower_ci <- row[[col_lower]]
      upper_ci <- row[[col_upper]]
      
      # Check if this predicted cluster overlaps with ANY true date
      for(tdate in true_dates) {
        if(ci_overlaps(lower_ci, upper_ci, tdate)) {
          n_predicted_correct <- n_predicted_correct + 1
          break  # Count this cluster once even if it overlaps multiple true dates
        }
      }
    }
  }
  
  tibble(
    filename = filename,
    method = method,
    true_k = true_k,
    predicted_k = predicted_k,
    classification = classification,
    generation_combo = generation_combo,
    n_predicted_clusters = predicted_k,
    n_predicted_correct = n_predicted_correct,
    prop_predicted_correct = n_predicted_correct / predicted_k
  )
}

# Apply to all rows
results_predicted <- clustering_summary %>%
  rowwise() %>%
  do(score_by_predicted(.)) %>%
  ungroup()

# Summarize across simulations
overlap_summary_predicted <- results_predicted %>%
  group_by(method, true_k, classification) %>%
  summarize(
    n_sims = n(),
    mean_prop_correct = mean(prop_predicted_correct),
    sd_prop_correct = sd(prop_predicted_correct),
    .groups = "drop"
  )

print(overlap_summary_predicted)


library(dplyr)
library(ggplot2)

# Combine both datasets
overlap_summary <- overlap_summary %>%
  mutate(metric = "% true dates covered")

overlap_summary_predicted <- overlap_summary_predicted %>%
  rename(mean_prop_covered = mean_prop_correct,
         sd_prop_covered = sd_prop_correct) %>%
  mutate(metric = "% predicted clusters correct")

combined_data <- bind_rows(overlap_summary, overlap_summary_predicted)

# Plot
pdf("plots/sims_date_overlap_precision_sensitivity.pdf", width=15, height=11)

ggplot(combined_data, aes(x = classification, y = mean_prop_covered, fill = classification)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, mean_prop_covered - sd_prop_covered),
                    ymax = pmin(1, mean_prop_covered + sd_prop_covered)),
                width = 0.3) +
  geom_text(aes(label = n_sims, y=1), 
            vjust = -0.5, 
            nudge_y = 0.05,
            size = 5) +
  facet_grid(metric ~ method + true_k) +
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
  #scale_fill_manual(
 #   values = c("correct" = "#2ecc71", 
#               "predicted_k1" = "#e74c3c",
#               "predicted_k2" = "#f39c12",
#               "predicted_k3" = "#9b59b6",
#               "predicted_k4" = "#34495e"),
   # labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4")
  #) +
  labs(x = "Classification",
       y = "Proportion",
       fill = "Classification")+
  theme_minimal(base_size = 14) +
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size=17),
    plot.title = element_text(size = 16, face = "bold")
  )

dev.off()



combined_data2<-filter(combined_data, metric=="% true dates covered")


library(dplyr)

weighted_summary <- combined_data2 %>%
  group_by(method, true_k) %>%
  summarise(
    total_sims = sum(n_sims),
    weighted_mean_prop_covered =
      sum(mean_prop_covered * n_sims) / total_sims,
    .groups = "drop"
  )


library(ggplot2)

# assuming weighted_summary from before:
# columns: method, true_k, total_sims, weighted_mean_prop_covered
pdf("plots/sims_date_overlap_simple.pdf", width=6, height=5)
ggplot(weighted_summary,
       aes(x = true_k,
           y = weighted_mean_prop_covered,
           fill = true_k)) +
  geom_col(alpha = 0.8) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  facet_wrap(~ method) +
  labs(
    x = "True number of clusters (k)",
    y = "Proportion of true dates covered"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
dev.off()

ggplot(combined_data2, aes(x = classification, y = mean_prop_covered, fill = classification)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, mean_prop_covered - sd_prop_covered),
                    ymax = pmin(1, mean_prop_covered + sd_prop_covered)),
                width = 0.3) +
  geom_text(aes(label = n_sims, y=1), 
            vjust = -0.5, 
            nudge_y = 0.05,
            size = 5) +
  facet_grid(~method) +
  scale_y_continuous(limits = c(0, 1.15), breaks = seq(0, 1, 0.2)) +
  #scale_fill_manual(
  #   values = c("correct" = "#2ecc71", 
  #               "predicted_k1" = "#e74c3c",
  #               "predicted_k2" = "#f39c12",
  #               "predicted_k3" = "#9b59b6",
  #               "predicted_k4" = "#34495e"),
  # labels = c("Correct", "Pred: k=1", "Pred: k=2", "Pred: k=3", "Pred: k=4")
  #) +
  labs(x = "Classification",
       y = "Proportion",
       fill = "Classification")+
  theme_minimal(base_size = 14) +
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size=17),
    plot.title = element_text(size = 16, face = "bold")
  )

dev.off()


