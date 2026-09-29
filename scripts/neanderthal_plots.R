###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_noOaseZlatykun_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_ALL", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]
output$pop<-gsub("Shotgun", "", output$pop)
input_data$V1<-gsub("Shotgun", "", input_data$V1)
output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

#output<-filter(output, model_k== output$recommended_k[1])
output<-filter(output, model_k== 4)

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen"))
# First, prepare the annotation text
# First, prepare the annotation text
annotation_text <- output %>%
  # Remove duplicates if any (since joint estimates should be the same across individuals)
  distinct(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  # Create formatted text for each estimate
  mutate(
    text_line = paste0("Joint: ", round(joint_date_est_best, 0), 
                       " (", floor(joint_date_lowerci), "-", 
                       ceiling(joint_date_upperci), ")")
  ) %>%
  # Combine all lines with line breaks
  pull(text_line) %>%
  paste(collapse = "\n")

joint_dates <- output %>%
  select(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  distinct()

# Sort by joint_date_est (smaller values = older dates)
joint_dates_sorted <- joint_dates[order(joint_dates$joint_date_est), ]

joint_date_text <- sapply(1:nrow(joint_dates_sorted), function(i) {
  # Convert to years BP (1960 as present)
  est <- round(1960 - 28 * (joint_dates_sorted$joint_date_est[i] + 1))
  lower <- round(1960 - 28 * (joint_dates_sorted$joint_date_lowerci[i] + 1))
  upper <- round(1960 - 28 * (joint_dates_sorted$joint_date_upperci[i] + 1))
  
  # Convert to BP (positive values = years before 1960)
  est_bp <- 1960 - est
  lower_bp <- 1960 - lower
  upper_bp <- 1960 - upper
  
  # For the range, put the older date (larger BP value) first
  range_start <- max(lower_bp, upper_bp)
  range_end <- min(lower_bp, upper_bp)
  
  sprintf("Date %d: %d BP (%d-%d BP)", 
          i,
          est_bp,
          range_end,
          range_start)
})

joint_date_text <- paste(joint_date_text, collapse = "\n")


library(ggrepel)
# Add the annotation to your plot
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0)), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
    geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1", size = 1) +
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 3, 
             colour = "firebrick1", stroke=1.2) +

  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_text_repel(
    aes(x = pop, y = ind_date_est, label = pop),
    size = 5,
    direction = "y",
    nudge_y = 50,  # Push labels up by 50 units
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    min.segment.length = 0,
    segment.color = "grey50",
    segment.size = 0.3,
    box.padding = 0.5,
    point.padding = 15,  # Large padding to keep labels away from points
    force = 10,  # Strong repulsion force
    force_pull = 0,
    max.overlaps = Inf,
    seed = 123
  ) +annotate("rect", xmin = 0, xmax = 21, ymin = 1539, ymax = 1780,
           fill = "cornflowerblue", alpha = 0.1, size = 0.5) +
  # Add the text annotation

  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 17),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") + 
  ylab("Inferred admixture date (generations)") +
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 6, colour = "forestgreen")



p



pdf("plots/neanderthal_nooasezlatykun.pdf", width=12, height=8)
p
dev.off()






###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_ALL_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_ALL", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]
output$pop<-gsub("Shotgun", "", output$pop)
input_data$V1<-gsub("Shotgun", "", input_data$V1)
output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
#output<-filter(output, model_k== 4)

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen"))
# First, prepare the annotation text
# First, prepare the annotation text
annotation_text <- output %>%
  # Remove duplicates if any (since joint estimates should be the same across individuals)
  distinct(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  # Create formatted text for each estimate
  mutate(
    text_line = paste0("Joint: ", round(joint_date_est_best, 0), 
                       " (", floor(joint_date_lowerci), "-", 
                       ceiling(joint_date_upperci), ")")
  ) %>%
  # Combine all lines with line breaks
  pull(text_line) %>%
  paste(collapse = "\n")

joint_dates <- output %>%
  select(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  distinct()

# Sort by joint_date_est (smaller values = older dates)
joint_dates_sorted <- joint_dates[order(joint_dates$joint_date_est), ]

joint_date_text <- sapply(1:nrow(joint_dates_sorted), function(i) {
  # Convert to years BP (1960 as present)
  est <- round(1960 - 28 * (joint_dates_sorted$joint_date_est[i] + 1))
  lower <- round(1960 - 28 * (joint_dates_sorted$joint_date_lowerci[i] + 1))
  upper <- round(1960 - 28 * (joint_dates_sorted$joint_date_upperci[i] + 1))
  
  # Convert to BP (positive values = years before 1960)
  est_bp <- 1960 - est
  lower_bp <- 1960 - lower
  upper_bp <- 1960 - upper
  
  # For the range, put the older date (larger BP value) first
  range_start <- max(lower_bp, upper_bp)
  range_end <- min(lower_bp, upper_bp)
  
  sprintf("Date %d: %d BP (%d-%d BP)", 
          i,
          est_bp,
          range_end,
          range_start)
})

joint_date_text <- paste(joint_date_text, collapse = "\n")


library(ggrepel)
# Add the annotation to your plot
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0)), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1", size = 1) +
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 3, 
             colour = "firebrick1", stroke=1.2) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_text_repel(
    aes(x = pop, y = ind_date_est, label = pop),
    size = 5,
    direction = "y",
    nudge_y = 50,  # Push labels up by 50 units
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    min.segment.length = 0,
    segment.color = "grey50",
    segment.size = 0.3,
    box.padding = 0.5,
    point.padding = 15,  # Large padding to keep labels away from points
    force = 10,  # Strong repulsion force
    force_pull = 0,
    max.overlaps = Inf,
    seed = 123
  ) +annotate("rect", xmin = 0, xmax = 21, ymin = 1539, ymax = 1780,
              fill = "cornflowerblue", alpha = 0.1, size = 0.5) +
  # Add the text annotation
  
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 17),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") + 
  ylab("Inferred admixture date (generations)") +
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 6, colour = "forestgreen")



p



pdf("plots/neanderthal_ALL_forignroing.pdf", width=12, height=8)
p
dev.off()















###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_noearlyOAAnoOaseZlatykun_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neanderthal/admixfrog_moorjani_ALL", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]
output$pop<-gsub("Shotgun", "", output$pop)
input_data$V1<-gsub("Shotgun", "", input_data$V1)
output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
#output<-filter(output, model_k== 4)

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen"))
# First, prepare the annotation text
# First, prepare the annotation text
annotation_text <- output %>%
  # Remove duplicates if any (since joint estimates should be the same across individuals)
  distinct(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  # Create formatted text for each estimate
  mutate(
    text_line = paste0("Joint: ", round(joint_date_est_best, 0), 
                       " (", floor(joint_date_lowerci), "-", 
                       ceiling(joint_date_upperci), ")")
  ) %>%
  # Combine all lines with line breaks
  pull(text_line) %>%
  paste(collapse = "\n")


joint_dates <- output %>%
  select(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  distinct()

# Sort by joint_date_est (smaller values = older dates)
joint_dates_sorted <- joint_dates[order(joint_dates$joint_date_est), ]

joint_date_text <- sapply(1:nrow(joint_dates_sorted), function(i) {
  # Convert to years BP (1960 as present)
  est <- round(1960 - 28 * (joint_dates_sorted$joint_date_est[i] + 1))
  lower <- round(1960 - 28 * (joint_dates_sorted$joint_date_lowerci[i] + 1))
  upper <- round(1960 - 28 * (joint_dates_sorted$joint_date_upperci[i] + 1))
  
  # Convert to BP (positive values = years before 1960)
  est_bp <- 1960 - est
  lower_bp <- 1960 - lower
  upper_bp <- 1960 - upper
  
  # For the range, put the older date (larger BP value) first
  range_start <- max(lower_bp, upper_bp)
  range_end <- min(lower_bp, upper_bp)
  
  sprintf("Date %d: %d BP (%d-%d BP)", 
          i,
          est_bp,
          range_end,
          range_start)
})

joint_date_text <- paste(joint_date_text, collapse = "\n")

library(ggrepel)
# Add the annotation to your plot
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0)), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1", size = 1) +
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 3, 
             colour = "firebrick1", stroke=1.2) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_text_repel(
    aes(x = pop, y = ind_date_est, label = pop),
    size = 5,
    direction = "y",
    nudge_y = 50,  # Push labels up by 50 units
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    min.segment.length = 0,
    segment.color = "grey50",
    segment.size = 0.3,
    box.padding = 0.5,
    point.padding = 15,  # Large padding to keep labels away from points
    force = 10,  # Strong repulsion force
    force_pull = 0,
    max.overlaps = Inf,
    seed = 123
  ) +annotate("rect", xmin = 0, xmax = 16, ymin = 1539, ymax = 1780,
              fill = "cornflowerblue", alpha = 0.1, size = 0.5) +
  # Add the text annotation
  
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 17),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") + 
  ylab("Inferred admixture date (generations)") +
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 6, colour = "forestgreen")



p



pdf("plots/neanderthal_nooasezlatykun_noOAA.pdf", width=12, height=8)
p
dev.off()
