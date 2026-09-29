#####ok here we go, plotting the different types of sims. cv first
###dates
dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_africaeurope_5050_30gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_africaeurope_5050_50gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_60gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_africaeurope_5050_60gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_75gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_africaeurope_5050_75gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_100gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_africaeurope_5050_100gen.txt", quote="\"", comment.char="")

dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-30

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_60gen$cv<-dates_africaeurope_5050_60gen$V4/dates_africaeurope_5050_60gen$V5
dates_africaeurope_5050_60gen$true<-60

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75

dates_africaeurope_5050_100gen$cv<-dates_africaeurope_5050_100gen$V4/dates_africaeurope_5050_100gen$V5
dates_africaeurope_5050_100gen$true<-100

dates_africaeurope<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen, dates_africaeurope_5050_60gen, dates_africaeurope_5050_75gen, dates_africaeurope_5050_100gen)

dates_africaeurope$method<-"DATES"
dates_africaeurope$type<-"Malawi_French"
##globetrotter

dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_africaeurope_5050_30gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_africaeurope_5050_50gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_60gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_africaeurope_5050_60gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_75gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_africaeurope_5050_75gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_100gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_africaeurope_5050_100gen.txt", quote="\"", comment.char="")

dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-30

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_60gen$cv<-dates_africaeurope_5050_60gen$V4/dates_africaeurope_5050_60gen$V5
dates_africaeurope_5050_60gen$true<-60

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75

dates_africaeurope_5050_100gen$cv<-dates_africaeurope_5050_100gen$V4/dates_africaeurope_5050_100gen$V5
dates_africaeurope_5050_100gen$true<-100

GT_africaeurope<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen, dates_africaeurope_5050_60gen, dates_africaeurope_5050_75gen, dates_africaeurope_5050_100gen)
GT_africaeurope$method<-"GT"
GT_africaeurope$type<-"Malawi_French"





##spansihjapan
###dates

dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/spanish_japan_20gen_dates_first30.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/spanish_japan_50gen_dates_first30.txt", quote="\"", comment.char="")
dates_africaeurope_5050_60gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/spanish_japan_75gen_dates_first30.txt", quote="\"", comment.char="")


dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-20

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75


dates_spanishjapan<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen, dates_africaeurope_5050_75gen)

dates_spanishjapan$method<-"DATES"
dates_spanishjapan$type<-"Spanish_Japan"
##globetrotter

dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_20gen_first30.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_50gen_first30.txt", quote="\"", comment.char="")
dates_africaeurope_5050_75gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_75gen_first30.txt", quote="\"", comment.char="")

dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-20

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75



GT_spanishjapan<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen,  dates_africaeurope_5050_75gen)
GT_spanishjapan$method<-"GT"
GT_spanishjapan$type<-"Spanish_Japan"

##italiannorway
###dates

dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_italiannorway_30gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_italiannorway_50gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_60gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/DATES/dates_italiannorway_75gen.txt", quote="\"", comment.char="")


dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-30

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75


dates_italiannorway<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen, dates_africaeurope_5050_75gen)

dates_italiannorway$method<-"DATES"
dates_italiannorway$type<-"Italian_Norway"
##globetrotter

dates_africaeurope_5050_30gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_italiannorway_30gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_50gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_italiannorway_50gen.txt", quote="\"", comment.char="")
dates_africaeurope_5050_75gen <- read.table("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_italiannorway_75gen.txt", quote="\"", comment.char="")

dates_africaeurope_5050_30gen$cv<-dates_africaeurope_5050_30gen$V4/dates_africaeurope_5050_30gen$V5
dates_africaeurope_5050_30gen$true<-30

dates_africaeurope_5050_50gen$cv<-dates_africaeurope_5050_50gen$V4/dates_africaeurope_5050_50gen$V5
dates_africaeurope_5050_50gen$true<-50

dates_africaeurope_5050_75gen$cv<-dates_africaeurope_5050_75gen$V4/dates_africaeurope_5050_75gen$V5
dates_africaeurope_5050_75gen$true<-75



GT_italiannorway<-rbind(dates_africaeurope_5050_30gen,dates_africaeurope_5050_50gen,  dates_africaeurope_5050_75gen)
GT_italiannorway$method<-"GT"
GT_italiannorway$type<-"Italian_Norway"





all_data<-rbind(GT_africaeurope, GT_spanishjapan, dates_africaeurope, dates_spanishjapan,GT_italiannorway,dates_italiannorway)


pdf("plots/differentsims_zscorecomp.pdf", height=8,width=10)
ggplot(all_data) + 
  geom_hline(data = data.frame(method = c("GT", "DATES"), 
                               hline = c(1, 2.5)),
             aes(yintercept = hline), 
             linetype = "dashed", colour = "grey50", linewidth = 1) +
  geom_jitter(aes(x = true, y = cv, colour = type), 
              width = 3, height = 0, size = 3, alpha = 0.7) +
  facet_wrap(~method) + 
  theme_minimal(base_size = 16) + 
  theme(
    strip.text = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 17),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "True number of generations ago", 
       y = "Dating precision metric, c",
       colour = "Type") +
  scale_colour_brewer(palette = "Set1")
dev.off()



###now we want a figure of likelihood jumps for different simsssssssssssssss
#ok i think i need to make an excel sheet one sec
likelihood_changes <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/likelihood_changes..csv")


library(ggplot2)
library(dplyr)
library(tidyr)
  
# First, create a longer format with both raw and standardized jumps
plot_data <- likelihood_changes %>%
  mutate(
    raw_jump = k1_to_k2,
    standardised_jump = k1_to_k2 / z.score,
    # Add indicator for whether z-score passes threshold
    passes_threshold = case_when(
      METHOD == "GT" & z.score >= 1 ~ "Pass",
      METHOD == "DATES" & z.score >= 2.5 ~ "Pass",
      TRUE ~ "Fail"
    )
  ) %>%
  pivot_longer(cols = c(raw_jump, standardised_jump),
               names_to = "jump_type",
               values_to = "jump_value") %>%
  mutate(
    jump_type = factor(jump_type, 
                       levels = c("raw_jump", "standardised_jump"),
                       labels = c("Raw Likelihood Jump", "Scaled Jump")),
    SCENARIO = factor(SCENARIO, 
                      levels = c("italiannorway", "spanishjapan", "africa_europe"),
                      labels = c("Italian-Norway", 
                                 "Spanish-Japan", 
                                 "Malawi-French")),
    true_k = factor(true_k, labels = c("True K=1", "True K=2"))
  )



pdf("plots/likelihood_standardisation_demo.pdf", width=12, height=8)
# Side-by-side comparison plot with free scales for both rows and columns


ggplot(plot_data, aes(x = SCENARIO, y = log(jump_value), 
                      colour = true_k, group = true_k)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  geom_point(aes(shape = passes_threshold), 
             position = position_jitter(width = 0.15, seed = 42),
             size = 3) +
  facet_grid(METHOD ~ jump_type, scales = "free") +
  scale_shape_manual(values = c("Pass" = 19, "Fail" = 4),
                     name = "Passes Threshold") +
  scale_colour_manual(values = c("True K=1" = "#E69F00", "True K=2" = "#56B4E9"),
                      name = "True Clusters") +
  labs(
    x = "Admixture Scenario",
    y = "Likelihood Improvement (log)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x  = element_text(size = 17, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 17),
    strip.text   = element_text(size = 20, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.grid.minor = element_blank()
  )

dev.off()


