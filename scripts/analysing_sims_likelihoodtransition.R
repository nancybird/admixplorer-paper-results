library(tidyverse)
clustering_summary_GT <- read.csv("data/simulations/GLOBETROTTER/clustering_summary_gt_smallern.csv")
clustering_summary_DATES <- read.csv("data/simulations/DATES/clustering_summary_DATES_smallern.csv")

cv_analysis_GT <- read.csv("data/simulations/GLOBETROTTER/cv_analysis_results_gt.csv")
cv_analysis_DATES <- read.csv("data/simulations/DATES/cv_analysis_results_dates.csv")

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


###DECIDING THRESHOLDS

test1<-filter(clustering_summary, method=="GT", n_true_clusters==1)
summary(test1$k1_to_k2)
quantile(test1$k1_to_k2, 0.98) ## 0.56
quantile(test1$clustering_strength_k2, 0.98) # 0.78
test1_wrong<-filter(test1, clustering_strength_k2 > 0.78 | k1_to_k2 > 0.56 ) #17 / 500 classified wrong


test2<-filter(clustering_summary,  method=="GT",n_true_clusters==2)
summary(test2$k1_to_k2)
summary(test2$clustering_strength_k3)
quantile(test2$k2_to_k3, 0.95) ## 0.34
test2_wrong <- filter(test2, 
                      (k1_to_k2 < 0.56 & clustering_strength_k2 < 0.78) | 
                        k2_to_k3 > 0.34 & k1_to_k2> 0.66) ##66 / 500 classified wrong

test3<-filter(clustering_summary, method=="GT", n_true_clusters==3)
summary(test3$k3_to_k4)
quantile(test3$k3_to_k4, 0.95) #0.18
min(test3$k1_to_k2) #0.66
test3_wrong <- filter(test3, 
                      (k1_to_k2 < 0.56 & clustering_strength_k2 < 0.78) |  # suggests k=1
                        (k1_to_k2 < 0.66 | k2_to_k3 < 0.34) |              # NEW: both checks for k=2
                        k3_to_k4 > 0.18)                                     # suggests k≥4
## 166 / 500 inds



###now DATES

test1<-filter(clustering_summary, method=="DATES", n_true_clusters==1)
summary(test1$k1_to_k2)
quantile(test1$k1_to_k2, 0.98) ## 0.99
quantile(test1$clustering_strength_k2, 0.98) # 0.97
test1_wrong<-filter(test1, clustering_strength_k2 > 0.99 | k1_to_k2 > 0.97 ) #15 / 500 classified wrong


test2<-filter(clustering_summary,  method=="DATES",n_true_clusters==2)
summary(test2$k1_to_k2)
summary(test2$clustering_strength_k3)
quantile(test2$k2_to_k3, 0.95) ## 0.58
test2_wrong <- filter(test2, 
                      (k1_to_k2 < 0.99 & clustering_strength_k2 < 0.97) | 
                        k2_to_k3 > 0.58 & k1_to_k2 > 2.21) ##60 / 500 classified wrong

test3<-filter(clustering_summary, method=="DATES", n_true_clusters==3)
summary(test3$k3_to_k4)
quantile(test3$k3_to_k4, 0.95) #0.42
min(test3$k1_to_k2) #2.21
test3_wrong <- filter(test3, 
                      (k1_to_k2 < 0.99 & clustering_strength_k2 < 0.97) |  # suggests k=1
                        (k2_to_k3 < 0.58 | k1_to_k2 <2.21) |                                     # suggests k=2
                        k3_to_k4 > 0.42)      

#169/500 inds

###ADDING PREDICTIONS


predict_k <- function(df, method) {
  predicted <- rep(NA, nrow(df))
  
  if(method == "GT") {
    clust_thresh <- 0.78
    k1_to_k2_thresh <- 0.56
    k1_to_k2_thresh_for_k3 <- 0.66  # NEW: stricter threshold for k>=3
    k2_to_k3_thresh <- 0.34
    k3_to_k4_thresh <- 0.18
  } else {  # DATES
    clust_thresh <- 0.99
    k1_to_k2_thresh <- 0.97
    k1_to_k2_thresh_for_k3 <- 2.21  # Use your DATES equivalent
    k2_to_k3_thresh <- 0.58
    k3_to_k4_thresh <- 0.42
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
    transition = "k1_to_k2"  # For merging with likelihood data
  )

# === PLOT 1: LIKELIHOOD IMPROVEMENTS BY METHOD ===
# Define thresholds for each method
thresholds_GT <- data.frame(
  method = "GT",
  transition = c("k1_to_k2", "k2_to_k3", "k3_to_k4"),
  threshold_value = c(0.56, 0.34, 0.18),
  threshold_type = c("k>=2", "k>=3", "k>=4"),
  stringsAsFactors = FALSE
)

thresholds_DATES <- data.frame(
  method = "DATES",
  transition = c("k1_to_k2", "k2_to_k3", "k3_to_k4"),
  threshold_value = c(0.97, 0.58, 0.42),
  threshold_type = c("k>=2", "k>=3", "k>=4"),
  stringsAsFactors = FALSE
)

# NEW: Add the stricter k1_to_k2 threshold for k>=3
thresholds_k3_GT <- data.frame(
  method = "GT",
  transition = "k1_to_k2",
  threshold_value = 0.66,
  threshold_type = "k>=3 (stricter)",
  stringsAsFactors = FALSE
)

thresholds_k3_DATES <- data.frame(
  method = "DATES",
  transition = "k1_to_k2",
  threshold_value = 2.21,
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
              alpha = 0.6, size = 2) +
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
    color = "Generation",
    title = "Likelihood Improvements by Method",
    subtitle = "Red dashed = base thresholds; Blue dotted = stricter k1→k2 threshold for k≥3"
  ) +
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.position = "right",
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels (increased from 10)
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title
    plot.subtitle = element_text(size = 13),                 # Subtitle
    panel.spacing = unit(1, "lines")
  )


pdf("plots/likelihood_threshold_calcs.pdf", width=12, height=10)
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
ll_thresh_GT <- 0.56
clust_thresh_DATES <- 0.99
ll_thresh_DATES <- 0.97

p_combined <- ggplot(plot_data_k1tok2, 
                     aes(x = clustering_strength_k2, 
                         y = k1_to_k2)) +
  geom_point(aes(color = plot_generation, shape = classification),
             alpha = 0.6, size = 2.5) +
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
    y = "Scaled Likelihood Improvement (k1→k2)",
    color = "Generation",
    title = "k=1 vs k=2 Decision: Clustering Strength vs Likelihood",
    subtitle = "Red lines = method thresholds"
  ) +
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.position = "right",
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels (increased from 10)
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
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

# Plot number inds comparison
p_total_inds <- ggplot(clustering_summary, aes(x = factor(n_true_clusters), y = total_inds, 
                                            fill = classification)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  facet_wrap(~method) +
  geom_jitter(aes(color = classification), 
              alpha = 0.5, 
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +
  scale_y_log10() + 
  labs(x = "True Number of Clusters", 
       y = "Number of individuals", 
       fill = "Classification",
       color = "Classification") +  # Added color legend label
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )



print(p_total_inds)
pdf("plots/sims_classification_by_total_inds.pdf", width=10, height=8)
p_total_inds
dev.off()



# Plot number inds comparison
p_balance <- ggplot(clustering_summary[clustering_summary$n_true_clusters!=1, ], aes(x = factor(n_true_clusters), y = cluster_balance_ratio, 
                                               fill = classification)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  facet_wrap(~method) +
  geom_jitter(aes(color = classification), 
              alpha = 0.5, 
              position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2)) +
  scale_y_log10() + 
  labs(x = "True Number of Clusters", 
       y = "Ratio between minimun and maximum number of individuals in a cluster", 
       fill = "Classification",
       color = "Classification") +  # Added color legend label
  theme_minimal(base_size = 16) +  # Increased base size from default (11) to 16
  theme(
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold"),    # Main title (if you add one)
    plot.subtitle = element_text(size = 13)                  # Subtitle (if you add one)
  )



print(p_balance)
pdf("plots/sims_classification_by_cluster_balance.pdf", width=10, height=8)
p_balance
dev.off()



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


pdf("plots/sims_clusteringsaccuracy.pdf", width=10, height=8)
# 3. Plot boxplot + jitter, colored by generation_combo and shaped by classification
ggplot(k2_data, aes(
  x = factor(n_true_clusters),  # will be "2" for all
  y = max_accuracy)) +
  # geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_jitter(aes(
    color = generation_combo,
    shape = classification),
    width = 0.2,
    alpha = 0.7,
    size = 3.5) +  # Increased from 2 to 3.5
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
    x = "True Number of Clusters",
    y = "Clustering Accuracy for true k",
    title = "Clustering Accuracy by Generation Combination"
  ) +
  facet_wrap(~method) +
  theme_minimal(base_size = 16) +  # Increased base size
  theme(
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold")     # Main title
  )


dev.off()


pdf("plots/sims_clusteringsaccuracy_vs_balance_ratio.pdf", width=12, height=10)
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
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold")     # Main title
  )
dev.off()




pdf("plots/sims_clusteringsaccuracy_vs_total_inds.pdf", width=12, height=10)
ggplot(clustering_summary[clustering_summary$n_true_clusters!=1,]) +
  geom_point(aes(x=max_accuracy, y=total_inds, color = generation_combo,
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
  labs(x="Clustering accuracy for true k", y="Total individuals")+theme_minimal(base_size = 16) +  # Increased base size
  theme(
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),      # Legend text
    legend.title = element_text(size = 15, face = "bold"),  # Legend titles
    strip.text = element_text(size = 14, face = "bold"),    # Facet labels
    axis.title = element_text(size = 15, face = "bold"),    # Axis titles
    axis.text = element_text(size = 13),                     # Axis tick labels
    plot.title = element_text(size = 17, face = "bold")     # Main title
  )
dev.off()

