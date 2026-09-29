###plotting two waves results
library(tidyverse)
library(stringr)

output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen10gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen10gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3010 <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ceiling(ind_date_est + 1.96 * ind_date_se)),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(40, 10), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3010


output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3030 <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60, 30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030

output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3060 <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(90, 60), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060

####now gt

output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen10gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen10gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3010_gt <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(40, 10), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3010_gt


output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3030_gt <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60, 30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030_gt

output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen60gen.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen60gen.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
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

# Add the annotation to your plot
p3060_gt <- ggplot(output) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(90, 60), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060_gt




library(patchwork)
library(grid)

# Add titles
p3010 <- p3010 + ggtitle("30+10 generations two waves") 
p3030 <- p3030 + ggtitle("30+30 generations two waves")
p3060 <- p3060 + ggtitle("30+60 generations two waves")

p3010_gt <- p3010_gt+ ggtitle("30+10 generations two waves")
p3030_gt<- p3030_gt + ggtitle("30+30 generations two waves")
p3060_gt <- p3060_gt + ggtitle("30+60 generations two waves")

# Combine plots
combined_plot <- (p3010 | p3030 | p3060) / (p3010_gt | p3030_gt | p3060_gt) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/twowaves_justonetyoe_combined.pdf", width = 30, height = 15)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("DATES", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("GT", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()


##########OK NOW THE MULTIPLE DATE PLOTS


output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen10gen_40genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen10gen_40genonewave.txt", quote="\"", comment.char="")

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"40genonly"
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

# Add the annotation to your plot
p3010 <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(40,10), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3010



output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen10gen_40genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen10gen_40genonewave.txt", quote="\"", comment.char="")

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"40genonly"
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

# Add the annotation to your plot
p3010_gt <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(40,10), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3010_gt


# Add titles
p3010 <- p3010 + ggtitle("30+10 generations + 40gen only two waves") 

p3010_gt <- p3010_gt+ ggtitle("30+10 generations + 40gen only two waves") 

# Combine plots
combined_plot <- (p3010 ) / (p3010_gt) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/twowaves_3010_40_combined.pdf", width = 10, height = 10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("DATES", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("GT", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()


##now 3030
output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen_60genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen_60genonewave.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"60genonly"


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

# Add the annotation to your plot
p3030 <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030



output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen_60genonewave2.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen30gen_60genonewave2.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"60genonly"
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

# Add the annotation to your plot
p3030_2 <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0),shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030_2


output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_60genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_60genonewave.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"60genonly"
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

# Add the annotation to your plot
p3030_gt <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030_gt



output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_60genonewave2.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_60genonewave2.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"60genonly"
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

# Add the annotation to your plot
p3030_2_gt <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,30), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3030_2_gt





# Add titles
p3030 <- p3030 + ggtitle("30+30 generations + 60gen only two waves") 
p3030_2 <- p3030_2 + ggtitle("30+30 generations + 60gen only two waves") 
p3030_gt <- p3030_gt+ ggtitle("30+30 generations + 60gen only two waves") 
p3030_2_gt <- p3030_2_gt+ ggtitle("30+30 generations + 60gen only two waves") 
# Combine plots
combined_plot <- (p3030 | p3030_2 ) / (p3030_gt | p3030_2_gt) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/twowaves_3030_60_combined.pdf", width = 20, height = 10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("DATES", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("GT", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()





#3060
output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen_90genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen_90genonewave.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"90genonly"


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

# Add the annotation to your plot
p3060 <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(90,60), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060



output <- read.delim("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen_90genonewave2.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/dates_africaeurope_twowave_30gen60gen_90genonewave2.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"90genonly"
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

# Add the annotation to your plot
p3060_2 <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0),shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,90), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060_2


output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_90genonewave.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_90genonewave.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"90genonly"
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

# Add the annotation to your plot
p3060_gt <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,90), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060_gt



output <- read.delim("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_90genonewave2.output.txt", comment.char="#")
input_data <- read.table("data/simulations/two waves/GT_africaeurope_twowave_30gen30gen_90genonewave2.txt", quote="\"", comment.char="")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])

output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen\\d+gen"))
# First, prepare the annotation text

output[is.na(output$generation), "generation"]<-"90genonly"
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

# Add the annotation to your plot
p3060_2_gt <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x=pop, y=sample_age_est_best), pch=0, size=4, colour="grey25", stroke = 1) +
  geom_errorbar(aes(x=pop, ymin=sample_age_upperci_mean, ymax= sample_age_lowerci_mean),colour="grey25", stroke = 2) +
  geom_point(aes(x = pop, y = round(ind_date_est,0), shape=generation), size = 4) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) + 
  geom_point(aes(x = pop, y = round(joint_date_est_best,0)), pch = 8, size = 4, 
             colour = "firebrick1", stroke = 1.5) +
  geom_errorbar(aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)), colour = "firebrick1") +
  labs(x = "Simulated individual", y = "Date (generations)") + 
  geom_hline(yintercept = c(60,90), colour="darkblue", size=2, alpha=0.6) +
  # Add the text annotation
  annotate("text", 
           x = Inf, y = Inf,  # Position at top right
           label = annotation_text,
           hjust = 1.1, vjust = 1.1,  # Adjust positioning slightly inward
           size = 6,  # Adjust text size as needed
           fontface = "bold",
           color = "firebrick1") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3060_2_gt





# Add titles
p3060 <- p3060 + ggtitle("30+60 generations + 90gen only two waves") 
p3060_2 <- p3060_2 + ggtitle("30+60 generations + 90gen only two waves") 
p3060_gt <- p3060_gt+ ggtitle("30+60 generations + 90gen only two waves") 
p3060_2_gt <- p3060_2_gt+ ggtitle("30+60 generations + 90gen only two waves") 
# Combine plots
combined_plot <- (p3060 | p3060_2 ) / (p3060_gt | p3060_2_gt) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/twowaves_3060_90_combined.pdf", width = 20, height = 10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("DATES", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("GT", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 18, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()
