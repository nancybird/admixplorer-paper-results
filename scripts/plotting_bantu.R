###Bantu plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_Malawi_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_Malawi.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


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
  est <- round(1960 - 28 * (joint_dates$joint_date_est[i] + 1))
  lower <- round(1960 - 28 * (joint_dates$joint_date_lowerci[i] + 1))
  upper <- round(1960 - 28 * (joint_dates$joint_date_upperci[i] + 1))
  
  sprintf("Joint date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")








metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-
  factor(output$NewPop, levels=c("ElandCave", "Mfongosi", "Newcastle","Pedi","Sotho","Ndebele","Northern_Shoto","Swazi","Tsanga","Tswana","Zulu"  ))




output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)




library(ggrepel)
# Add the annotation to your plot
p_malawi <-ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(0, 90)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1, vjust = 1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )


p_malawi


###congo plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_Congo_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_Congo.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


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
                       " (", floor(joint_date_lowerci), "-", 
                       ceiling(joint_date_upperci), ")")
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
  est <- round(1960 - 28 * (round(joint_dates$joint_date_est[i],1) + 1))
  lower <- round(1960 - 28 * (floor(joint_dates$joint_date_lowerci[i]) + 1))
  upper <- round(1960 - 28 * (ceiling(joint_dates$joint_date_upperci[i]) + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")








metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-gsub("Congo_", "", output$NewPop)
output[is.na(output$NewPop), "NewPop"]<-"Yombe"
output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)



# Define 17 solid shapes (repeating 5 shape types)
shapes_17 <- rep(c(15, 16, 17, 18, 19), length.out = 17)

# Define 17 colours (you can swap these for your own palette)
library(RColorBrewer)
cols_17 <- rep(brewer.pal(8, "Set2"),3)
library(ggrepel)
# Add the annotation to your plot
p_congo <- ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(-100, 300)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1.8, vjust =1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )
p_congo
pdf("plots/bantu_Congo_GT_results.pdf", width = 15,height = 8)
p_congo
dev.off()










###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_SouthAfrica_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_SouthAfrica.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


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
  est <- round(1960 - 28 * (round(joint_dates$joint_date_est[i],1) + 1))
  lower <- round(1960 - 28 * (floor(joint_dates$joint_date_lowerci[i]) + 1))
  upper <- round(1960 - 28 * (ceiling(joint_dates$joint_date_upperci[i]) + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-
  factor(output$NewPop, levels=c("ElandCave", "Mfongosi", "Newcastle","Pedi","Sotho","Ndebele","Northern_Shoto","Swazi","Tsanga","Tswana","Zulu"  ))




output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)


output[output$ancient=="no", "sample_age_range_lower"]<-NA
output[output$ancient=="no", "sample_age_range_upper"]<-NA
output[output$ancient=="no", "sample_age_est_best"]<-NA
library(ggrepel)



# Add the annotation to your plot
p_southafrica_gt <- ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  annotate("rect", xmin = 0, xmax = nrow(output), ymin = 20, ymax = 29,
            fill = "seagreen2", alpha = 0.5, size = 0.5) +
  
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(0, 90)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1, vjust = 1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )


p_southafrica_gt

pdf("plots/bantu_southafrica_GT_resultsNEW.pdf", width = 16, height = 8)
p_southafrica_gt
dev.off()









####DATES south africa
###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/DATES_results_SouthAfrica_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/DATES_results_SouthAfrica.txt", sep="", header=F)

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
  est <- round(1960 - 28 * (round(joint_dates$joint_date_est[i],1) + 1))
  lower <- round(1960 - 28 * (floor(joint_dates$joint_date_lowerci[i]) + 1))
  upper <- round(1960 - 28 * (ceiling(joint_dates$joint_date_upperci[i]) + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-
  factor(output$NewPop, levels=c("ElandCave", "Mfongosi", "Newcastle","Pedi","Sotho","Ndebele","Northern_Shoto","Swazi","Tsanga","Tswana","Zulu"  ))




output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)


output[output$ancient=="no", "sample_age_range_lower"]<-NA
output[output$ancient=="no", "sample_age_range_upper"]<-NA
output[output$ancient=="no", "sample_age_est_best"]<-NA
library(ggrepel)

34.32	7.7224


# Add the annotation to your plot
p_southafrica_dates <- ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  annotate("rect", xmin = 0, xmax = nrow(output), ymin = 26, ymax = 43,
           fill = "seagreen2", alpha = 0.5, size = 0.5) +
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(0, 90)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1, vjust = 1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )
p_southafrica_dates

pdf("plots/bantu_southafrica_DATES_results.pdf", width = 16, height = 8)
p_southafrica_dates
dev.off()



library(patchwork)
library(grid)


# Combine plots
combined_plot <- (p_southafrica_gt) / (p_southafrica_dates) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/bantu_sputhafrica_gtdates_resultsNEW.pdf", width = 16, height = 16)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("fastGLOBETROTTER", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 30, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("DATES", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 30, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()









### now just zulu 
###Neanderthal plots
library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_SouthAfrica_Zulu_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/fixed/GT_results_SouthAfrica.txt", sep="", header=F)

output$ind_date_se<-input_data[match(output$pop, input_data$V1), 5]

output$sample_age_range_lower<-input_data[match(output$pop, input_data$V1), 2]
output$sample_age_range_upper<-input_data[match(output$pop, input_data$V1), 3]
output$midpoint <-(output$sample_age_range_lower + output$sample_age_range_upper)/2
output$ind_date_est_upper <-  ceiling(output$ind_date_est + 1.96*  output$ind_date_se)
output$ind_date_est_lower<-  floor(output$ind_date_est - 1.96*  output$ind_date_se)


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
  est <- round(1960 - 28 * (round(joint_dates$joint_date_est[i],1) + 1))
  lower <- round(1960 - 28 * (floor(joint_dates$joint_date_lowerci[i]) + 1))
  upper <- round(1960 - 28 * (ceiling(joint_dates$joint_date_upperci[i]) + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-
  factor(output$NewPop, levels=c("ElandCave", "Mfongosi", "Newcastle","Pedi","Sotho","Ndebele","Northern_Shoto","Swazi","Tsanga","Tswana","Zulu"  ))




output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)


output[output$ancient=="no", "sample_age_range_lower"]<-NA
output[output$ancient=="no", "sample_age_range_upper"]<-NA
output[output$ancient=="no", "sample_age_est_best"]<-NA
library(ggrepel)
# Add the annotation to your plot
p_southafrica_gt <- ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(0, 90)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1, vjust = 1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )
p_southafrica_gt

pdf("plots/bantu_southafrica_Zulu_GT_results.pdf", width = 16, height = 8)
p_southafrica_gt
dev.off()









####DATES south africa

library(tidyverse)
library(stringr)
output <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/DATES_results_SouthAfrica_Zulu_admixplorer.output.txt", header=T)
input_data<-read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/real/bantu/DATES_results_SouthAfrica.txt", sep="", header=F)

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
  est <- round(1960 - 28 * (round(joint_dates$joint_date_est[i],1) + 1))
  lower <- round(1960 - 28 * (floor(joint_dates$joint_date_lowerci[i]) + 1))
  upper <- round(1960 - 28 * (ceiling(joint_dates$joint_date_upperci[i]) + 1))
  
  sprintf("Date %d: %s (%s-%s)", 
          i,
          format_year(est),
          format_year(upper),
          format_year(lower))
})


joint_date_text <- paste(joint_date_text, collapse = "\n")



metdata <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/PhD/Cameroon.spreadsheets/Chromopainter/WAfricaandextras/metdataHumanOriginsandancients_NB_2020v2.csv")
output$NewPop<-metdata[match(output$pop, metdata$ID), "NewPop"]
output$NewPop<-gsub("Bantu_N-Sotho","Bantu_Sotho",  output$NewPop)
output$NewPop<-gsub("Bantu_","SouthAfrica_",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_SA","Botswana_Tswana",  output$NewPop)
output$NewPop<-gsub("Zulu","SouthAfrica_Zulu",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_N-Pedi","SouthAfrica_Pedi",  output$NewPop)
output$NewPop<-gsub("Botswana_Tswana","SouthAfrica_Tswana",  output$NewPop)
output$NewPop<-gsub("SouthAfrica_","",  output$NewPop)
output$ancient<-"no"
output[output$pop %in% c("Mfongosi", "Newcastle", "ElandCave"), "ancient"]<-"yes"

output$NewPop<-
  factor(output$NewPop, levels=c("ElandCave", "Mfongosi", "Newcastle","Pedi","Sotho","Ndebele","Northern_Shoto","Swazi","Tsanga","Tswana","Zulu"  ))




output<-output[order(output$cluster),]
output$pop<-factor(output$pop, levels = output$pop)


output[output$ancient=="no", "sample_age_range_lower"]<-NA
output[output$ancient=="no", "sample_age_range_upper"]<-NA
output[output$ancient=="no", "sample_age_est_best"]<-NA
library(ggrepel)
p_southafrica_dates <- ggplot(output) + 
  geom_tile(
    aes(
      x = pop,
      y = (sample_age_range_lower + sample_age_range_upper)/2,
      height = sample_age_range_upper + 0.5 - sample_age_range_lower
    ), 
    fill = "pink", alpha = 1
  ) +
  geom_point(aes(x = pop, y = sample_age_est_best), pch = 15, size = 2, stroke = 1) +
  scale_shape_manual(
    values = shapes_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  scale_color_manual(
    values = cols_17,
    name   = "Population",
    guide  = guide_legend(ncol = 1)
  ) +
  geom_errorbar(
    aes(x = pop, ymin = sample_age_upperci_mean, ymax = sample_age_lowerci_mean),
    colour = "grey25", stroke = 2
  ) +
  geom_errorbar(
    aes(
      x    = pop,
      ymax = ceiling(ind_date_est + 1.96 * ind_date_se),
      ymin = floor(ind_date_est - 1.96 * ind_date_se)
    ),
    width = 0.3, linewidth = 0.8
  ) +
  geom_point(
    aes(x = pop, y = round(ind_date_est, 0), shape = NewPop, colour = NewPop),
    size = 5, stroke = 2
  ) +
  geom_errorbar(
    aes(x = pop, ymin = floor(joint_date_lowerci), ymax = ceiling(joint_date_upperci)),
    colour = "firebrick1"
  ) +
  geom_point(
    aes(x = pop, y = round(joint_date_est_best, 0)),
    pch = 8, size = 3, colour = "firebrick1", stroke = 1
  ) +
  scale_y_continuous(
    name = "Inferred admixture date (generations)",
    sec.axis = sec_axis(
      trans = ~ 1960 - (.+1)*28,
      name  = "Calendar year"
    )
  ) +
  coord_cartesian(ylim = c(0, 90)) +   # back to zooming, not dropping
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x        = element_blank(),
    axis.title         = element_text(size = 25, face = "bold"),
    axis.text.y        = element_text(size = 17),
    legend.text        = element_text(size = 20),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  xlab("Individual") +
  annotate(
    "text", x = Inf, y = Inf,
    label = joint_date_text,
    hjust = 1, vjust = 1, size = 8,
    colour = "forestgreen", fontface = "bold"
  )

p_southafrica_dates


pdf("plots/bantu_southafrica_Zulu_DATES_results.pdf", width = 16, height = 8)
p_southafrica_dates
dev.off()



library(patchwork)
library(grid)


# Combine plots
combined_plot <- (p_southafrica_gt) / (p_southafrica_dates) +
  plot_layout(heights = c(1, 1))

# Save with custom row labels
pdf("plots/bantu_sputhafrica_zulu_gtdates_results.pdf", width = 16, height = 16)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.05, 0.95))))
grid.text("fastGLOBETROTTER", x = 0.5, y = 0.75, rot = 90, 
          gp = gpar(fontsize = 30, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("DATES", x = 0.5, y = 0.25, rot = 90, 
          gp = gpar(fontsize = 30, fontface = "bold"),
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(combined_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()






