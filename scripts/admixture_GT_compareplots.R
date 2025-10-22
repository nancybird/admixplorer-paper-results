library(tidyverse)

GT_results_group <- read.csv("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/allgtresults_grouped.txt", sep="")
admixplorer_20gen <- read.delim("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_20gen_first30.output.txt", comment.char="#")
admixplorer_50gen <- read.delim("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_50gen_first30.output.txt", comment.char="#")
admixplorer_75gen <- read.delim("C:/Users/nancy/OneDrive - University College London/Documents/POSTDOC/NewGTMethod/admixplorer-paper-results/data/simulations/GLOBETROTTER/GT_spanishjapanese_75gen_first30.output.txt", comment.char="#")


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
all_results$Recipient<-gsub("sims_", "", all_results$Recipient)
all_results$Recipient<-paste0(all_results$Recipient, "gen")


pdf("plots/admixplorer_vs_gt_grouped.pdf", width=8, height=8)
ggplot(all_results) +
  geom_hline(yintercept = c(20, 50, 75)) +
  geom_violin(data = subset(all_results, method == "admixplorer"),
              aes(x = Recipient, y = ind_date, group = Recipient),
              position = position_nudge(x = -0.1875),
              alpha = 0.3,
              fill = "#E69F00") +
  geom_errorbar(aes(x = Recipient, ymin = lower, ymax = upper, 
                    color = method, group = method),
                position = position_dodge(width = 0.75),
                width = 0.2) +
  geom_point(aes(x = Recipient, y = date, color = method, 
                 shape = method, group = method),
             position = position_dodge(width = 0.75),
             size = 3) +
  scale_color_manual(values = c("admixplorer" = "#E69F00", "GT_grouped" = "#56B4E9")) +
  scale_shape_manual(values = c("admixplorer" = 16, "GT_grouped" = 17)) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),  # slant x-axis labels
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +   # legend labels
  labs(color = "Method", shape = "Method", 
       x = "Simulation", y = "Inferred admixture date (generations)") +
  ylim(0, 125)
dev.off()

