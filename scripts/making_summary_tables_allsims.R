library(dplyr)
library(stringr)
library(tibble)

# ----------------- 1. Scenario and true date parser -----------------

parse_scenario_info <- function(output_path, method_label = "DATES") {
  fname <- basename(output_path)
  
  # strip suffix and method prefixes
  base <- gsub("\\.output.*$", "", fname)
  base <- gsub("^GT_|^DATES_|^dates_|^GLOBETROTTER_", "", base)
  
  # strip GT-specific suffix if present
  base <- gsub("_admixplorer$", "", base)
  
  # ----- special cases -----
  
  # Italiannorway age-estimation file
  if (grepl("^italiannorway_30_estallages$", base)) {
    return(list(
      scenario = "italiannorway",
      true_generations = 30,
      method = method_label
    ))
  }
  
  # Italiannorway two-pulse files
  if (grepl("^italiannorway_30_50gen$", base)) {
    return(list(
      scenario = "italiannorway",
      true_generations = c(30, 50),
      method = method_label
    ))
  }
  
  if (grepl("^italiannorway_30_75gen$", base)) {
    return(list(
      scenario = "italiannorway",
      true_generations = c(30, 75),
      method = method_label
    ))
  }
  
  if (grepl("^italiannorway_50_75gen$", base)) {
    return(list(
      scenario = "italiannorway",
      true_generations = c(50, 75),
      method = method_label
    ))
  }
  
  # ----- general parser -----
  parts <- strsplit(base, "_")[[1]]
  
  gen_first_idx <- which(grepl("gen", parts))[1]
  
  if (is.na(gen_first_idx)) {
    stop("No 'gen' token found in filename: ", fname)
  }
  
  scenario <- if (gen_first_idx > 1) {
    paste(parts[1:(gen_first_idx - 1)], collapse = "_")
  } else {
    ""
  }
  
  gen_chunk <- paste(parts[gen_first_idx:length(parts)], collapse = "_")
  gen_chunks <- stringr::str_extract_all(gen_chunk, "[0-9]+gen")[[1]]
  
  true_generations <- if (length(gen_chunks) > 0) {
    as.numeric(sub("gen$", "", gen_chunks))
  } else {
    numeric(0)
  }
  
  list(
    scenario = scenario,
    true_generations = true_generations,
    method = method_label
  )
}
pair_input_file <- function(output_path) {
  fin <- sub("\\.output\\.txt$", ".txt", output_path)
  fin <- sub("_admixplorer\\.txt$", ".txt", fin)
  fin
}
ci_overlaps <- function(ci_lower, ci_upper, true_date) {
  if (is.na(ci_lower) || is.na(ci_upper) || is.na(true_date)) return(FALSE)
  ci_lower_rounded <- floor(ci_lower)
  ci_upper_rounded <- ceiling(ci_upper)
  true_date >= ci_lower_rounded & true_date <= ci_upper_rounded
}

summarise_one_scenario <- function(output_path, input_path,
                                   method_label = c("DATES","GT")) {
  method_label <- match.arg(method_label)
  
  output <- read.delim(output_path, comment.char = "#",
                       stringsAsFactors = FALSE)
  input_data <- read.table(input_path, quote = "\"", comment.char = "",
                           stringsAsFactors = FALSE)
  
  # add info from input file
  output$ind_date_se <- input_data[match(output$pop, input_data$V1), 5]
  output$sample_age_range_lower <- input_data[match(output$pop, input_data$V1), 2]
  output$sample_age_range_upper <- input_data[match(output$pop, input_data$V1), 3]
  output$midpoint <- (output$sample_age_range_lower + output$sample_age_range_upper) / 2
  output$ind_date_est_upper <- output$ind_date_est + 1.96 * output$ind_date_se
  output$ind_date_est_lower <- output$ind_date_est - 1.96 * output$ind_date_se
  
  output <- output[order(output$midpoint), ]
  output$pop <- factor(output$pop, levels = unique(output$pop))
  output <- dplyr::filter(output, model_k == output$recommended_k[1])
  
  meta <- parse_scenario_info(output_path, method_label = method_label)
  scenario <- meta$scenario
  method   <- meta$method
  true_dates_vec <- meta$true_generations
  true_k   <- length(true_dates_vec)
  
  predicted_k <- unique(output$recommended_k)
  if (length(predicted_k) != 1) {
    warning("Multiple predicted_k values; using first for ", output_path)
    predicted_k <- predicted_k[1]
  }
  classification_k <- if (predicted_k == true_k) "correct" else paste0("predicted_k", predicted_k)
  
  out_ind <- output %>%
    mutate(
      true_date_ind = sapply(as.character(pop), get_true_date_for_ind),
      cluster_label = cluster
    )
  
  cluster_truth <- out_ind %>%
    group_by(cluster_label) %>%
    summarise(
      majority_true_date = as.numeric(names(which.max(table(true_date_ind)))),
      .groups = "drop"
    )
  
  out_ind <- out_ind %>%
    left_join(cluster_truth, by = "cluster_label") %>%
    rowwise() %>%
    mutate(
      is_correct_cluster = (true_date_ind == majority_true_date),
      
      # joint date coverage, shifted by sample midpoint
      shifted_true_time = midpoint + true_date_ind,
      joint_lower_time  = midpoint + joint_date_lowerci,
      joint_upper_time  = midpoint + joint_date_upperci,
      joint_ci_covers_true = {
        jl <- as.numeric(joint_lower_time)
        ju <- as.numeric(joint_upper_time)
        td <- as.numeric(shifted_true_time)
        if (is.na(jl) || is.na(ju) || is.na(td)) {
          FALSE
        } else {
          ci_overlaps(jl, ju, td)
        }
      },
      
      # sampling-age range reduction: use output sample_age_* columns directly
      original_sampling_lower = sample_age_range_lower,
      original_sampling_upper = sample_age_range_upper,
      original_sampling_range = original_sampling_upper - original_sampling_lower,
      
      est_sampling_lower = sample_age_lowerci_mean,
      est_sampling_upper = sample_age_upperci_mean,
      est_sampling_range = est_sampling_upper - est_sampling_lower,
      
      sampling_range_reduction = if (!is.na(original_sampling_range) &&
                                     original_sampling_range > 0 &&
                                     !is.na(est_sampling_range)) {
        est_sampling_range / original_sampling_range
      } else {
        NA_real_
      },
      
      sampling_midpoint = midpoint,
      est_sampling_overlaps_midpoint = if (!is.na(original_sampling_range) &&
                                           original_sampling_range > 0 &&
                                           !is.na(est_sampling_lower) &&
                                           !is.na(est_sampling_upper)) {
        ci_overlaps(est_sampling_lower, est_sampling_upper, sampling_midpoint)
      } else {
        NA
      }
    ) %>%
    ungroup()
  
  n_ind <- nrow(out_ind)
  
  prop_individuals_correct_cluster <- mean(out_ind$is_correct_cluster, na.rm = TRUE)
  mean_true_dates_covered <- mean(out_ind$joint_ci_covers_true, na.rm = TRUE)
  
  covered_20 <- if (20 %in% out_ind$true_date_ind)
    mean(out_ind$joint_ci_covers_true[out_ind$true_date_ind == 20], na.rm = TRUE) else NA_real_
  covered_50 <- if (50 %in% out_ind$true_date_ind)
    mean(out_ind$joint_ci_covers_true[out_ind$true_date_ind == 50], na.rm = TRUE) else NA_real_
  covered_75 <- if (75 %in% out_ind$true_date_ind)
    mean(out_ind$joint_ci_covers_true[out_ind$true_date_ind == 75], na.rm = TRUE) else NA_real_
  
  mean_sampling_range_reduction <- mean(out_ind$sampling_range_reduction, na.rm = TRUE)
  prop_sampling_midpoint_covered <- mean(out_ind$est_sampling_overlaps_midpoint, na.rm = TRUE)
  
  true_dates_str <- if (length(true_dates_vec) > 0) paste(true_dates_vec, collapse = ";") else NA_character_
  
  tibble(
    scenario = scenario,
    method   = method,
    true_k   = true_k,
    true_dates = true_dates_str,
    predicted_k = predicted_k,
    classification_k = classification_k,
    n_individuals = n_ind,
    prop_individuals_correct_cluster = prop_individuals_correct_cluster,
    mean_true_dates_covered = mean_true_dates_covered,
    covered_20 = covered_20,
    covered_50 = covered_50,
    covered_75 = covered_75,
    mean_sampling_range_reduction = mean_sampling_range_reduction,
    prop_sampling_midpoint_covered = prop_sampling_midpoint_covered
  )
}
library(purrr)

files_dates <- list.files(
  "data/simulations/DATES",
  pattern = "dates_.*\\.output\\.txt$",
  full.names = TRUE
)
files_dates<-files_dates[1:35]


summary_rows <- map_dfr(files_dates, function(fout) {
  fin <- sub("\\.output\\.txt$", ".txt", fout)
  
  if (!file.exists(fin)) {
    warning("Skipping: input file not found for ", fout, " (expected ", fin, ")")
    return(NULL)
  }
  
  tryCatch(
    summarise_one_scenario(
      output_path = fout,
      input_path  = fin,
      method_label = "DATES"
    ),
    error = function(e) {
      warning("Skipping file due to error: ", fout, " (", e$message, ")")
      NULL
    }
  )
})

write.csv(summary_rows, "data/simulation_summary_per_scenario_DATES.csv", row.names = FALSE)


##gt
files_gt <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/gt_filelist.txt", quote="\"", comment.char="")
files_gt<-files_gt$V1
files_gt<-paste0("data/simulations/GLOBETROTTER/", files_gt)
summary_rows_gt <- purrr::map_dfr(files_gt, function(fout) {
  fin <- pair_input_file(fout)
  
  if (!file.exists(fin)) {
    warning("Skipping: input file not found for ", fout, " (expected ", fin, ")")
    return(NULL)
  }
  
  tryCatch(
    summarise_one_scenario(
      output_path = fout,
      input_path  = fin,
      method_label = "GT"
    ),
    error = function(e) {
      warning("Skipping file due to error: ", fout, " (", e$message, ")")
      NULL
    }
  )
})
write.csv(summary_rows_gt, "data/simulation_summary_per_scenario_GT.csv", row.names = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)


summary_rows <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulation_summary_per_scenario_both.csv")
summary_rows$scenario<-gsub("africaeurope", "Africa-Europe", summary_rows$scenario)
summary_rows$scenario<-gsub("italiannorway", "Italy-Norway", summary_rows$scenario)#
summary_rows$scenario<-gsub("_0595", "-05:95", summary_rows$scenario)
summary_rows$scenario<-gsub("_5050", "-50:50", summary_rows$scenario)
#------------------------------------------
# Clean base data
#------------------------------------------
summary_clean <- summary_rows %>%
  mutate(
    scenario    = factor(scenario),
    method      = factor(method, levels = c("DATES", "GT")),
    scenario_lab = paste0(scenario, " (", true_dates, ", original n", original_n, ")")
  )

scenario_order <- summary_clean %>%
  distinct(scenario_lab, true_k, original_n) %>%
  arrange(true_k, scenario_lab) %>%
  pull(scenario_lab)

summary_clean <- summary_clean %>%
  mutate(scenario_lab = factor(scenario_lab, levels = scenario_order))

#------------------------------------------
# (A) heatmap: true vs predicted k (simplified)
#------------------------------------------

k_tile <- summary_clean %>%
  group_by(method, scenario_lab, true_k) %>%
  summarise(
    predicted_k_mode = as.numeric(names(which.max(table(predicted_k)))),
    .groups = "drop"
  )

p_k_simple <- ggplot(k_tile,
                     aes(x = factor(true_k),
                         y = scenario_lab,
                         fill = factor(predicted_k_mode))) +
  geom_tile(color = "white") +
  geom_text(aes(label = predicted_k_mode), size = 4) +
  facet_wrap(~ method, ncol = 2) +
  scale_fill_brewer(palette = "Set1", name = "Predicted k") +
  labs(
    x = "True k",
    y = NULL
  )

#------------------------------------------
# (B) clustering accuracy heatmap (simple)
#------------------------------------------

clust_heat2 <- summary_clean %>%
  group_by(method, scenario_lab, true_k) %>%
  summarise(
    mean_clust = mean(prop_individuals_correct_cluster, na.rm = TRUE),
    mean_n     = mean(n_individuals),
    .groups    = "drop"
  )

p_clust_simple <- ggplot(clust_heat2,
                         aes(x = factor(true_k),
                             y = scenario_lab,
                             fill = mean_clust)) +
  geom_tile(color = "white") +
  facet_wrap(~ method, ncol = 2) +
  scale_fill_viridis_c(name = "Prop.\ncorrectly\nclustered",
                       limits = c(0, 1),
                       na.value = "grey90") +
  labs(
    x = "True k",
    y = NULL
  )

#------------------------------------------
# (C) mean true-date coverage heatmap
#------------------------------------------

cov_heat2 <- summary_clean %>%
  filter(is.na(mean_sampling_range_reduction)) %>%
  group_by(method, scenario_lab, true_k) %>%
  summarise(mean_cov = mean(mean_true_dates_covered, na.rm = TRUE),
            .groups = "drop")

p_cov_simple <- ggplot(cov_heat2,
                       aes(x = factor(true_k),
                           y = scenario_lab,
                           fill = mean_cov)) +
  geom_tile(color = "white") +
  facet_wrap(~ method, ncol = 2) +
  scale_fill_viridis_c(name = "Mean\ntrue-date\ncoverage",
                       limits = c(0, 1),
                       na.value = "grey90") +
  labs(
    x = "True k",
    y = NULL
  )

#------------------------------------------
# (D) sampling-age performance (barplot)
#------------------------------------------

samp_data <- summary_clean %>%
  filter(!is.na(mean_sampling_range_reduction))

p_samp_range <- ggplot(samp_data,
                       aes(x = scenario_lab,
                           y = mean_sampling_range_reduction,
                           fill = method)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean sampling-age range\n(estimated / original)"
  ) +
  scale_fill_brewer(palette = "Set1", name = "Method") +
  theme_bw(base_size = 16) +  # big base text
  theme(
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16)
  )

#------------------------------------------
# Apply bigger text themes to A–C
#------------------------------------------

big_theme_heat <- theme_bw(base_size = 16) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    strip.text  = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14)
  )

p_k_simple     <- p_k_simple     + big_theme_heat
p_clust_simple <- p_clust_simple + big_theme_heat
p_cov_simple   <- p_cov_simple   + big_theme_heat

# make panel labels (A–D) big
combined_plot <- wrap_plots(
  p_k_simple,        # A
  p_clust_simple,    # B
  p_cov_simple,      # C
  p_samp_range,      # D
  ncol = 2
) + plot_annotation(tag_levels = "A",
                    theme = theme(
                      plot.tag = element_text(size = 24, face = "bold")
                    ))

combined_plot

ggsave("plots/allothersims_comparison_summaryNEW.pdf",
       combined_plot,
       width = 18, height = 10)
