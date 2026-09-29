library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/Both_dates_admixplorer.output.txt", header=T)
input_data1<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/oliviera_dates.txt", sep="", header=F)
input_data2<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/nagele_dates.txt", sep="", header=F)





input_data1$dataset<-"Oliviera"
input_data2$dataset<-"Nagele"
input_data<-rbind(input_data1,input_data2)


papuan_pop_info <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/papuan_pop_info.csv")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se

output$dataset<-input_data[match(output$pop, input_data$V1), 6]
output$region<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "region"]
output$region2<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "region.2"]
output$n_inds<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "n_inds"]

output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
#output<-filter(output, model_k== 2)
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen"))
# First, prepare the annotation text
# First, prepare the annotation text
annotation_text <- output %>%
  # Remove duplicates if any (since joint estimates should be the same across individuals)
  distinct(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  # Create formatted text for each estimate
  mutate(
    text_line = paste0("Joint: ", round(joint_date_est_best, 1), 
                       " (", round(joint_date_lowerci, 1), "-", 
                       round(joint_date_upperci, 1), ")")
  ) %>%
  # Combine all lines with line breaks
  pull(text_line) %>%
  paste(collapse = "\n")




# Get joint date estimate and CI
joint_dates <- output %>%
  select(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  distinct()

# Create annotation text for joint dates without decimal places
format_year <- function(year) {
  if (year < 0) {
    sprintf("%d BCE", abs(year))
  } else {
    sprintf("%d CE", year)
  }
}

joint_date_text <- sapply(1:nrow(joint_dates), function(i) {
  est <- round(2020 - 28 * (joint_dates$joint_date_est[i] + 1))
  lower <- round(2020 - 28 * (joint_dates$joint_date_lowerci[i] + 1))
  upper <- round(2020 - 28 * (joint_dates$joint_date_upperci[i] + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")




library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}


output<-output[order(output$region2, output$region, output$cluster),]
output$pop<-factor(output$pop, levels=output$pop)
output$region<-factor(output$region, levels=unique(output$region))
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 0, size = 4, stroke = 1) +
  scale_shape_manual(values = rep(c(15, 16, 17, 8, 3, 13), 4)) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) +
geom_errorbar(aes(x = pop,
                    ymax = ind_date_est + 1.96 * ind_date_se,
                    ymin = ind_date_est - 1.96 * ind_date_se),
                width = 0.3, linewidth = 0.8) +  
  geom_point(aes(x = pop, y = ind_date_est, colour=region, shape=region2, size=n_inds)) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci), colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  scale_size_continuous(range = c(3, 15) ,breaks = c(1, 5, 25,  50)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 1000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  #coord_cartesian(ylim = c(100, 400)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 17),
    axis.title.y.right = element_text(size = 20, face = "bold"),
    axis.text.y.right = element_text(size = 17),
    legend.text = element_text(size=20),
    legend.title = element_text(size=20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") + 
  ylab("Inferred admixture date (generations)") +
  labs(colour="Population", shape="Region", size="n individuals")+
  annotate("text", x = Inf, y = Inf, 
           label = joint_date_text,
           hjust = 1.5, vjust = 1, size = 7.6, colour = "forestgreen")+
  guides(
    colour = guide_legend(override.aes = list(size = 6)),
    shape  = guide_legend(override.aes = list(size = 6))
  )


p

pdf("plots/Papuan_dates_bothdata.pdf", width=15, height=8)
p
dev.off()



#just oliveria

output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/Oliviera_dates_admixplorer.output.txt", header=T)
input_data1<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/oliviera_dates.txt", sep="", header=F)
input_data2<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/nagele_dates.txt", sep="", header=F)





input_data1$dataset<-"Oliviera"
input_data2$dataset<-"Nagele"
input_data<-rbind(input_data1,input_data2)


papuan_pop_info <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/papuan/papuan_pop_info.csv")


output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se

output$dataset<-input_data[match(output$pop, input_data$V1), 6]
output$region<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "region"]
output$region2<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "region.2"]
output$n_inds<-papuan_pop_info[match(output$pop, papuan_pop_info$ind), "n_inds"]

output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
output<-filter(output, model_k== 3)
output <- output %>%
  mutate(generation = str_extract(pop, "\\d+gen"))
# First, prepare the annotation text
# First, prepare the annotation text
annotation_text <- output %>%
  # Remove duplicates if any (since joint estimates should be the same across individuals)
  distinct(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  # Create formatted text for each estimate
  mutate(
    text_line = paste0("Joint: ", round(joint_date_est_best, 1), 
                       " (", round(joint_date_lowerci, 1), "-", 
                       round(joint_date_upperci, 1), ")")
  ) %>%
  # Combine all lines with line breaks
  pull(text_line) %>%
  paste(collapse = "\n")




# Get joint date estimate and CI
joint_dates <- output %>%
  select(joint_date_est_best, joint_date_lowerci, joint_date_upperci) %>%
  distinct()

# Create annotation text for joint dates without decimal places
format_year <- function(year) {
  if (year < 0) {
    sprintf("%d BCE", abs(year))
  } else {
    sprintf("%d CE", year)
  }
}

joint_date_text <- sapply(1:nrow(joint_dates), function(i) {
  est <- round(2020 - 28 * (joint_dates$joint_date_est[i] + 1))
  lower <- round(2020 - 28 * (joint_dates$joint_date_lowerci[i] + 1))
  upper <- round(2020 - 28 * (joint_dates$joint_date_upperci[i] + 1))
  
  sprintf("Joint date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(lower),
          format_year(upper))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")




library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}


output<-output[order(output$region2, output$region, output$cluster),]
output$pop<-factor(output$pop, levels=output$pop)
output$region<-factor(output$region, levels=unique(output$region))
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 0.6) +
  
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 0, size = 4, stroke = 1) +
  scale_shape_manual(values = rep(c(15, 16, 17, 8, 3, 13), 4)) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) +
  geom_point(aes(x = pop, y = ind_date_est, colour=region, shape=region2, size=n_inds)) +
  geom_errorbar(aes(x = pop,
                    ymax = ind_date_est + 1.96 * ind_date_se,
                    ymin = ind_date_est - 1.96 * ind_date_se),
                width = 0.3, linewidth = 0.8) + 
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci), colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  scale_size_continuous(range = c(3, 15) ,breaks = c(1, 5, 25,  50)) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 1000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  #coord_cartesian(ylim = c(100, 400)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 14),
    axis.title.y.right = element_text(size = 18, face = "bold"),
    axis.text.y.right = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") + 
  ylab("Inferred admixture date (generations)") +
  labs(colour="Population", shape="Region")+
  annotate("text", x = Inf, y = Inf, 
           label = joint_date_text,
           hjust = 1, vjust = 1, size = 8, colour = "forestgreen")


p

pdf("plots/Papuan_dates_Oliviera_k3.pdf", width=14, height=8)
p
dev.off()
