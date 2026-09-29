##neolithic dates
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/British_Isles_all_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/British_Isles_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
  
  sprintf("Joint date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
  neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
  neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)
output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]

output$pop_label2<-factor( output$pop_label2, levels = c( "Ireland_EN","Ireland_EN_MN", "Ireland_N",   "Ireland_LN" ,"Scotland_Megalithic"
                                                         ,"Scotland_N",  "England_N"         ))


# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))



library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}



# Define 17 solid shapes (repeating 5 shape types)
shapes_17 <- rep(c(15, 16, 17, 18, 19), length.out = 17)

# Define 17 colours (you can swap these for your own palette)
library(RColorBrewer)
cols_17 <- rep(brewer.pal(8, "Set2"),3)

# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p
pdf("plots/Neolithic_dates_britihsisles.pdf", width=14, height=8)
p
dev.off()


##SCOTLAND ONLY

library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Scotland_N_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/British_Isles_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
  
  sprintf("Joint date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)
output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]

output$pop_label2<-factor( output$pop_label2, levels = c( "Ireland_EN","Ireland_EN_MN", "Ireland_N",   "Ireland_LN" ,"Scotland_Megalithic"
                                                          ,"Scotland_N",  "England_N"         ))


# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}



# Define 17 solid shapes (repeating 5 shape types)
shapes_17 <- rep(c(15, 16, 17, 18, 19), length.out = 17)

# Define 17 colours (you can swap these for your own palette)
library(RColorBrewer)
cols_17 <- rep(brewer.pal(8, "Set2"),3)

# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")


pdf("plots/Neolithic_dates_scotland.pdf", width=14, height=8)
p
dev.off()






#hugary
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Hungary_all_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Hungary_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
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
          format_year(upper),
          format_year(lower))
})



joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]



# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}

# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_hungary_all.pdf", width=14, height=8)
p
dev.off()






#france
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/France_MN_more_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/France_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]



library(dplyr)
library(stringr)

output <- output %>% 
  mutate(
    site_code = str_sub(pop, 1, 3),
    pop_label = case_when(
      site_code == "GRG" ~ "Gurgy_le_noisats",
      site_code == "OBN" ~ "Obernai",
      site_code == "FLR" ~ "Fleury_sur_orne",
      site_code == "PRI" ~ "Prisse_la_charriere",
      TRUE ~ NA_character_
    )
  )

output[is.na(output$pop_label), "pop_label"]<-output[is.na(output$pop_label), "pop_label2"]
output$pop_label2<-output$pop_label

# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_France_MN_more.pdf", width=14, height=8)
p
dev.off()



#spain
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_MLN_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_Portugal_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]



# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_Spain_MLN_all.pdf", width=14, height=8)
p
dev.off()




#spain
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_C_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_Portugal_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]



# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))

library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = Inf, 
           label = joint_date_text,
           hjust = 1.4, vjust = 1.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_Spain_C_all.pdf", width=14, height=8)
p
dev.off()




#spain
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_Portugal_C_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Spain_Portugal_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]


# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +  annotate("rect", xmin = 0, xmax = nrow(output), ymin = 59+164, ymax = 79+164,
           fill = "cornflowerblue", alpha = 0.5, size = 0.5) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +

 # annotate("rect", xmin = 0, xmax = nrow(output), ymin = 66+172, ymax = 102+172,
  #         fill = "cornflowerblue", alpha = 0.5, size = 0.5) +
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = Inf, 
           label = joint_date_text,
           hjust = 1.4, vjust = 1.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_Spain_Portugal_C_all.pdf", width=14, height=8)
p
dev.off()

#germany lbk
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Germany_Austria_all_1remove_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Germany_Austria_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


output<-output[order(output$midpoint),]
output$pop<-factor(output$pop, levels=unique(output$pop))

output<-filter(output, model_k== output$recommended_k[1])
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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]


# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_Germany_LBK_all.pdf", width=14, height=8)
p
dev.off()


#germany austria lbk
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Germany_Austria_all_dates_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/neolithic/Germany_Austria_all_dates.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  output$ind_date_est + 1.96*  output$ind_date_se
output$ind_date_est_lower<-  output$ind_date_est - 1.96*  output$ind_date_se


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
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



neolithic_ancients <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/Twigstats/neolithic_ancients.csv")
anno <- read.delim("C:/Users/nancy/Downloads/v54.1.p1_1240K_public.anno")
neolithic_ancients<-select(neolithic_ancients, ID, POP_filt)
anno<-select(anno, Genetic.ID, Group.ID)
colnames(anno)<-colnames(neolithic_ancients)
neolithic_ancients<-rbind(neolithic_ancients, anno)
neolithic_ancients$ID<-gsub("_noUDG", "", neolithic_ancients$ID)
neolithic_ancients$POP_filt<-gsub("_noUDG", "", neolithic_ancients$POP_filt)
neolithic_ancients$POP_filt<-gsub(".SG", "", neolithic_ancients$POP_filt)

output$pop_label2<-neolithic_ancients[match(output$pop, neolithic_ancients$ID), "POP_filt"]


# order by pop, then within pop by cluster
output <- output[order(output$pop_label2, output$cluster), ]

# if you want pop to stay in this new order as a factor
output$pop <- factor(output$pop, levels = unique(output$pop))


library(ggrepel)

# Function to convert generations to years (CE/BCE)
gen_to_year <- function(gen) {
  1950 - (28 * gen)
}

# Inverse function to convert years back to generations
year_to_gen <- function(year) {
  (1950 - year) / 28
}
# Create the plot with secondary axis
p <- ggplot(output) + 
  geom_tile(aes(x = pop, y = (sample_age_range_lower + sample_age_range_upper)/2, 
                height = sample_age_range_upper + 0.5 - sample_age_range_lower), 
            fill = "pink", alpha = 1) +
  #geom_point(aes(x=pop, y=sample_age_est_best,),pch=0,  size=4,  stroke = 1) +
  scale_shape_manual(
    values     = shapes_17,
    name       = "Population",      # legend title
    guide = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values     = cols_17,
    name       = "Population",      # same legend title if you like
    guide = guide_legend(ncol = 1)
  ) +
  geom_errorbar(aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
                colour = "grey25", stroke = 2) + 
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  geom_errorbar(aes(x = pop,
                    ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
                    ymin = floor(ind_date_est - 1.96 * ind_date_se)),
                width = 0.3, linewidth = 0.8) +   
  geom_point(aes(x = pop, y = round(ind_date_est,1), colour = pop_label2, shape=pop_label2), size = 5) +
  geom_errorbar(aes(x = pop, ymin = joint_date_lowerci, ymax = joint_date_upperci),   colour = "firebrick1") +
  geom_point(aes(x = pop, y = joint_date_est_best), pch = 8, size = 3, 
             colour = "firebrick1", stroke = 1) +
  
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ 1950 - (28 * .), 
                        name = "Inferred admixture date (year)",
                        breaks = seq(-10000, 2000, by = 2000),
                        labels = function(x) {
                          ifelse(x < 0, paste0(abs(x), " BCE"), 
                                 ifelse(x > 0, paste0(x, " CE"), "0"))
                        })
  ) +
  
  coord_cartesian(ylim = c(100, 400)) +
  
  labs(x = "Simulated individual", y = "Date (generations)") + 
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
  labs(colour="Population", shape="Population")+
  annotate("text", x = Inf, y = -Inf, 
           label = joint_date_text,
           hjust = 1, vjust = -0.5, size = 8, colour = "forestgreen")

p


pdf("plots/Neolithic_dates_Germany_Austria_LBK_all.pdf", width=14, height=8)
p
dev.off()

