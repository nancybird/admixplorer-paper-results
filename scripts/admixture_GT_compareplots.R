library(tidyverse)

###gt results first
GT_results_group <- read.csv("data/simulations/GLOBETROTTER/allgtresults_grouped.txt", sep="")
admixplorer_20gen <- read.delim("data/simulations/GLOBETROTTER/GT_spanishjapanese_20gen_first30.output.txt", comment.char="#")
admixplorer_50gen <- read.delim("data/simulations/GLOBETROTTER/GT_spanishjapanese_50gen_first30.output.txt", comment.char="#")
admixplorer_75gen <- read.delim("data/simulations/GLOBETROTTER/GT_spanishjapanese_75gen_first30.output.txt", comment.char="#")


GT_results_group<-GT_results_group %>% select(Recipient, date, upper, lower) %>%  distinct()
GT_results_group$method<-"GT_grouped"
GT_results_group$ind_date<-NA

admixplorer_20gen$pop<-"sims_spanishjapanese_5050_lambda_20"
admixplorer_50gen$pop<-"sims_spanishjapanese_5050_lambda_50"
admixplorer_75gen$pop<-"sims_spanishjapanese_5050_lambda_75"
admixplorer_all<-rbind(admixplorer_20gen, admixplorer_50gen, admixplorer_75gen) 
admixplorer_all$method<-"admixplorer"
admixplorer_all<- admixplorer_all%>% filter(model_k==1) %>% 
  select(pop, joint_date_est_best,  joint_date_lowerci, joint_date_upperci, method, ind_date_est)

colnames(admixplorer_all)<-colnames(GT_results_group)

all_results<-rbind(admixplorer_all, GT_results_group)

all_results$Recipient<-gsub("5050_lambda_", "", all_results$Recipient)
all_results$Recipient<-gsub("sims_spanishjapanese_", "", all_results$Recipient)
all_results$Recipient<-paste0(all_results$Recipient, "gen")
all_results <- all_results %>%
  mutate(
    lower_new = pmin(lower, upper),
    upper_new = pmax(lower, upper),
    lower = lower_new,
    upper = upper_new
  ) %>%
  select(-lower_new, -upper_new)


pdf("plots/admixplorer_vs_gt_grouped.pdf", width=8, height=8)
ggplot(all_results) +
  geom_hline(yintercept = c(20, 50, 75)) +

   geom_violin(data = subset(all_results, method == "admixplorer"),
              aes(x = Recipient, y = round(ind_date,0), group = Recipient),
              position = position_nudge(x = -0.1875),
              alpha = 0.3,
              fill = "#E69F00") +
  geom_errorbar(aes(x = Recipient, ymin = floor(lower), ymax = ceiling(upper), 
                    color = method, group = method),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  geom_point(aes(x = Recipient, y = round(date,0), color = method, 
                 shape = method, group = method),
             position = position_dodge(width = 0.75),
             size = 5) +
  scale_color_manual(values = c("admixplorer" = "#E69F00", "GT_grouped" = "#56B4E9")) +
  scale_shape_manual(values = c("admixplorer" = 16, "GT_grouped" = 17)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1),  # slant x-axis labels
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +   # legend labels
  labs(color = "Method", shape = "Method", 
       x = "Simulation", y = "Inferred admixture date (generations)") +
  ylim(0, 125)


dev.off()


##now dates

GT_results_group <- read.table(data/simulations/DATES/dates_results_grouped.txt", sep="")
GT_results_group$V2<-GT_results_group$V2-1
admixplorer_20gen <- read.delim("data/simulations/DATES/spanish_japan_20gen_dates_last30.output.txt", comment.char="#")
admixplorer_50gen <- read.delim("data/simulations/DATES/spanish_japan_50gen_dates_first30.output.txt", comment.char="#")
admixplorer_75gen <- read.delim("data/simulations/DATES/spanish_japan_75gen_dates_first30.output.txt", comment.char="#")

GT_results_group$V4<-GT_results_group$V2 - 1.96 * GT_results_group$V3
GT_results_group$V5<-GT_results_group$V2 + 1.96 * GT_results_group$V3
colnames(GT_results_group)<-c("Recipient", "date", "se","lower", "upper")
GT_results_group<-GT_results_group[-grep("2dates", GT_results_group$Recipient),]

GT_results_group<-GT_results_group %>% select(Recipient, date, upper, lower) %>%  distinct()
GT_results_group$method<-"DATES_grouped"
GT_results_group$ind_date<-NA

admixplorer_20gen$pop<-"sims_spanishjapanese_20"
admixplorer_50gen$pop<-"sims_spanishjapanese_50"
admixplorer_75gen$pop<-"sims_spanishjapanese_75"
admixplorer_all<-rbind(admixplorer_20gen, admixplorer_50gen, admixplorer_75gen) 
admixplorer_all$method<-"admixplorer"
admixplorer_all<- admixplorer_all%>% filter(model_k==1) %>% 
  select(pop, joint_date_est_best,  joint_date_lowerci, joint_date_upperci, method, ind_date_est)

colnames(admixplorer_all)<-colnames(GT_results_group)

all_results<-rbind(admixplorer_all, GT_results_group)

all_results$Recipient<-gsub("5050_", "", all_results$Recipient)
all_results$Recipient<-gsub("sims_spanishjapanese_", "", all_results$Recipient)
all_results$Recipient<-gsub("spanishjapanese_", "", all_results$Recipient)
all_results$Recipient<-paste0(all_results$Recipient, "gen")
all_results <- all_results %>%
  mutate(
    lower_new = pmin(lower, upper),
    upper_new = pmax(lower, upper),
    lower = lower_new,
    upper = upper_new
  ) %>%
  select(-lower_new, -upper_new)


pdf("plots/admixplorer_vs_dates_grouped.pdf", width=8, height=8)
ggplot(all_results) +
  geom_hline(yintercept = c(20, 50, 75)) +
  geom_violin(data = subset(all_results, method == "admixplorer"),
              aes(x = Recipient, y = round(ind_date,0), group = Recipient),
              position = position_nudge(x = -0.1875),
              alpha = 0.3,
              fill = "#E69F00") +
  geom_errorbar(aes(x = Recipient, ymin = floor(lower), ymax = ceiling(upper), 
                    color = method, group = method),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  geom_point(aes(x = Recipient, y = round(date,0), color = method, 
                 shape = method, group = method),
             position = position_dodge(width = 0.75),
             size = 5) +
  scale_color_manual(values = c("admixplorer" = "#E69F00", "DATES_grouped" = "#56B4E9")) +
  scale_shape_manual(values = c("admixplorer" = 16, "DATES_grouped" = 17)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1),  # slant x-axis labels
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +    # legend labels
  labs(color = "Method", shape = "Method", 
       x = "Simulation", y = "Inferred admixture date (generations)") +
  ylim(0, 125)
dev.off()





###NOW two dates GT
GT_results_group <- read.csv("data/simulations/GLOBETROTTER/allgtresults_groupings.txt", sep="")
admixplorer_20gen <- read.delim("data/simulations/GLOBETROTTER/GT_spanishjapanese_20gen_50gen_first30.output.txt", comment.char="#")
admixplorer_50gen <- read.delim("data/simulations/GLOBETROTTER/GT_spanishjapanese_50gen_75gen_first30.output.txt", comment.char="#")

GT_results_group<-GT_results_group %>% select(Recipient, date, upper, lower) %>%  distinct()
GT_results_group$method<-"GT_grouped"
GT_results_group$ind_date<-NA

admixplorer_20gen$pop<-"sims_spanishjapanese_5050_lambda_20"
admixplorer_50gen$pop<-"sims_spanishjapanese_5050_lambda_50"

admixplorer_all<-rbind(admixplorer_20gen, admixplorer_50gen) 
admixplorer_all$method<-"admixplorer"
admixplorer_all<- admixplorer_all%>% filter(model_k==2) %>% 
  select(pop, joint_date_est_best,  joint_date_lowerci, joint_date_upperci, method, ind_date_est)

colnames(admixplorer_all)<-colnames(GT_results_group)

all_results<-rbind(admixplorer_all, GT_results_group)

all_results$Recipient<-gsub("5050_lambda_", "", all_results$Recipient)
all_results$Recipient<-gsub("sims_", "", all_results$Recipient)
all_results$Recipient<-paste0(all_results$Recipient, "gen")
  all_results$Recipient<-gsub("_50gen", "_50gen_75gen", all_results$Recipient)
  all_results$Recipient<-gsub("_20gen", "_20gen_50gen", all_results$Recipient)
  all_results$Recipient<-gsub("spanishjapanese_", "", all_results$Recipient)

  all_results <- all_results %>%
    mutate(
      lower_new = pmin(lower, upper),
      upper_new = pmax(lower, upper),
      lower = lower_new,
      upper = upper_new
    ) %>%
    select(-lower_new, -upper_new)
  
  
pdf("plots/admixplorer_vs_gt_grouped_2dates.pdf", width=8, height=8)
ggplot(all_results) +
  geom_hline(yintercept = c(20, 50, 75)) +
  geom_violin(data = subset(all_results, method == "admixplorer"),
              aes(x = Recipient, y = round(ind_date,0), group = Recipient),
              position = position_nudge(x = -0.1875),
              alpha = 0.3,
              fill = "#E69F00") +
  geom_errorbar(aes(x = Recipient, ymin = floor(lower), ymax = ceiling(upper),
                    color = method, group = method),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  geom_point(aes(x = Recipient, y = round(date,0), color = method, 
                 shape = method, group = method),
             position = position_dodge(width = 0.75),
             size = 5) +
  scale_color_manual(values = c("admixplorer" = "#E69F00", "GT_grouped" = "#56B4E9")) +
  scale_shape_manual(values = c("admixplorer" = 16, "GT_grouped" = 17)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1),  # slant x-axis labels
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +   # legend labels
  labs(color = "Method", shape = "Method", 
       x = "Simulation", y = "Inferred admixture date (generations)") +
  ylim(0, 125)
dev.off()


GT_results_group <- read.table("data/simulations/DATES/dates_results_grouped.txt", sep="")
GT_results_group$V2<-GT_results_group$V2-1
admixplorer_20gen <- read.delim("data/simulations/DATES/spanish_japan_20gen_50gen_dates.output.txt", comment.char="#")
admixplorer_50gen <- read.delim("data/simulations/DATES/spanish_japan_50gen_75gen_dates.output.txt", comment.char="#")


GT_results_group$V4<-GT_results_group$V2 - 1.96 * GT_results_group$V3
GT_results_group$V5<-GT_results_group$V2 + 1.96 * GT_results_group$V3
colnames(GT_results_group)<-c("Recipient", "date", "se","lower", "upper")
GT_results_group<-GT_results_group[grep("2dates", GT_results_group$Recipient),]

GT_results_group<-GT_results_group %>% select(Recipient, date, upper, lower) %>%  distinct()
GT_results_group$method<-"DATES_grouped"
GT_results_group$ind_date<-NA

admixplorer_20gen$pop<-"sims_spanishjapanese_20"
admixplorer_50gen$pop<-"sims_spanishjapanese_50"

admixplorer_all<-rbind(admixplorer_20gen, admixplorer_50gen) 
admixplorer_all$method<-"admixplorer"
admixplorer_all<- admixplorer_all%>% filter(model_k==2) %>% 
  select(pop, joint_date_est_best,  joint_date_lowerci, joint_date_upperci, method, ind_date_est)

colnames(admixplorer_all)<-colnames(GT_results_group)

all_results<-rbind(admixplorer_all, GT_results_group)

all_results$Recipient<-gsub("5050_", "", all_results$Recipient)
all_results$Recipient<-gsub("sims_", "", all_results$Recipient)
all_results$Recipient<-gsub("_2dates", "", all_results$Recipient)
all_results$Recipient<-paste0(all_results$Recipient, "gen")
all_results$Recipient<-gsub("_50gen", "_50gen_75gen", all_results$Recipient)
all_results$Recipient<-gsub("_20gen", "_20gen_50gen", all_results$Recipient)
all_results$Recipient<-gsub("spanishjapanese_", "", all_results$Recipient)

all_results <- all_results %>%
  mutate(
    lower_new = pmin(lower, upper),
    upper_new = pmax(lower, upper),
    lower = lower_new,
    upper = upper_new
  ) %>%
  select(-lower_new, -upper_new)

pdf("plots/admixplorer_vs_dates_grouped_2dates.pdf", width=8, height=8)
ggplot(all_results) +
  geom_hline(yintercept = c(20, 50, 75)) +
  geom_violin(data = subset(all_results, method == "admixplorer"),
              aes(x = Recipient, y = round(ind_date,0), group = Recipient),
              position = position_nudge(x = -0.1875),
              alpha = 0.3,
              fill = "#E69F00") +
  geom_errorbar(aes(x = Recipient, ymin = floor(lower), ymax = ceiling(upper), 
                    color = method, group = method),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  geom_point(aes(x = Recipient, y = round(date,0), color = method, 
                 shape = method, group = method),
             position = position_dodge(width = 0.75),
             size = 5) +
  scale_color_manual(values = c("admixplorer" = "#E69F00", "DATES_grouped" = "#56B4E9")) +
  scale_shape_manual(values = c("admixplorer" = 16, "DATES_grouped" = 17)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1),  # slant x-axis labels
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20)) +    # legend labels
  labs(color = "Method", shape = "Method", 
       x = "Simulation", y = "Inferred admixture date (generations)") +
  ylim(0, 125)
dev.off()






spanish_japan_20gen_dates_first30.output <- read.delim("data/simulations/DATES/spanish_japan_20gen_dates_last30.output.txt", comment.char="#")

spanish_japan_20gen_dates_first30 <- read.delim("data/simulations/DATES/spanish_japan_20gen_dates_last30.txt", comment.char="#", header=F, sep=" ")


spanish_japan_20gen_dates_first30.output$ind_date_se<-spanish_japan_20gen_dates_first30[match(spanish_japan_20gen_dates_first30.output$pop, spanish_japan_20gen_dates_first30$V1), 5]

spanish_japan_20gen_dates_first30.output<-filter(spanish_japan_20gen_dates_first30.output, model_k==spanish_japan_20gen_dates_first30.output$recommended_k[1])

spanish_japan_20gen_dates_first30.output$ind_date_est_upper <-  ceiling(spanish_japan_20gen_dates_first30.output$ind_date_est + 1.96*  spanish_japan_20gen_dates_first30.output$ind_date_se)
spanish_japan_20gen_dates_first30.output$ind_date_est_lower<-  floor(spanish_japan_20gen_dates_first30.output$ind_date_est - 1.96*  spanish_japan_20gen_dates_first30.output$ind_date_se)

spanish_japan_20gen_dates_first30.output<-filter(spanish_japan_20gen_dates_first30.output, model_k==1)

pdf("plots/DATES_admixplrer_grouped_spanishjapan_20gen.pdf", width=10, height=8)
ggplot(spanish_japan_20gen_dates_first30.output) + 
    geom_hline(aes(yintercept = 20), colour="darkblue", size=3, alpha=0.6) +
  geom_errorbar(aes(x=pop, ymin=ind_date_est_lower, ymax=ind_date_est_upper), size=2, alpha=0.7) + 
  geom_point(aes(x=pop, y=round(ind_date_est,0)), size=5, alpha=0.6) + 
  geom_point(aes(x=pop, y=round(joint_date_est_best,0)), colour="red", pch=8, size=3, stroke=1.5) +
  geom_errorbar(aes(x=pop, ymin=floor(joint_date_lowerci), ymax=ceiling(joint_date_upperci)), colour="red", size=1.5) +
# geom_errorbar(aes(x=pop, ymin = sample_age_lowerci_mean, ymax=sample_age_upperci_mean), colour="blue")+
  labs(x="Individual", y="Inferred admixture date")+
  theme_minimal()+
  theme(
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  ) 

dev.off()
