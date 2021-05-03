library(tidyverse)
library(scales)
library(ggpubr)
setwd("~/Desktop/Brandvain Lab/BG and kinship/")

fitnessColors = c("#333333",
                  "#4B5899", 
                  "#5181A6", 
                  "#60AEB3", 
                  "#5EBF9C", 
                  "#64CC7B",
                  "#999999")


litterColors = c("#528FCC",
                 "#4747B3", 
                 "#562D80", 
                 "#730A3F", 
                 "#400D0E")

litterColorsNo2 = c("#528FCC",
#                 "#4747B3", 
                 "#562D80", 
                 "#730A3F", 
                 "#400D0E")


litterColors2 = c("#E6D273",
                  "#AEBF56", 
                  "#4A943B", 
                  "#24663A", 
                  "#0F332D")

fitnessLabels = c("-1",
                  "-0.014",
                  "-0.012",
                  "-0.010",
                  "-0.008",
                  "-0.006",
                  " 0")

custom_theme <- function (base_size = 12, base_family = "") {
  theme_bw(base_size, base_family, base_line_size = base_size / 30) %+replace%
    theme(text = element_text(colour = "black",
                              family = "Geneva"),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.text = element_text(family = "Courier",
                                   size = base_size / 1.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA,
                                      color = "black"),
          panel.grid = element_line(color = "#999999"),
          strip.background = element_rect(color = "black",
                                          fill = "#EEEEEE"),
          plot.background = element_rect(fill = NA,
                                         color = NA)
          )
  }

#         Data

#large increments, mutation introducd in early()
earlyStats <- read_csv("Data/largeIncrements.csv")

earlyRow1 <- ggplot(earlyStats %>% 
                      filter(benefit < 0.2) %>% 
                      mutate(fitness = factor(fitness)),
                    aes(x = fitness,
                        y = percentFixed,
                        group = factor(benefit),
                        fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  #scale_y_continuous(labels = scientific) +
  scale_y_continuous(trans = "log1p") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #facet_wrap(~benefit, scales = "free_y") +
  facet_wrap(~benefit) +
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  ylab("Fixation rate of altruistic allele") + 
  theme(axis.title.x=element_blank()) +
  ggtitle("Fixation by Benefit")

earlyRow2 <- ggplot(earlyStats %>% 
                      filter(benefit >= 0.2) %>% 
                      filter(benefit != 0.22) %>%
                      mutate(fitness = factor(fitness)),
                    aes(x = fitness,
                        y = percentFixed,
                        group = factor(benefit),
                        fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  #scale_y_continuous(labels = scientific) +
  scale_y_continuous(trans = "log1p") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  #facet_wrap(~benefit, scales = "free_y") +
  facet_wrap(~benefit) +
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab(" ")

ggarrange(earlyRow1, earlyRow2, 
          ncol = 1, nrow = 2, 
          common.legend = TRUE,
          legend = "right"
          )

#normal early plot
ggplot(earlyStats %>% 
         #filter(benefit != 0.19) %>% 
         mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit, scales = "free_y") +
  #facet_wrap(~benefit) +
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  custom_theme()



# late
lateStats <- "Data/lateLargeIncrements.csv"

ggplot(earlyStats %>% 
         filter(benefit != 0.22) %>% 
         mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific_format(width = 7)) +
  facet_wrap(~benefit, scales = "free_y") +
  #facet_wrap(~benefit) +
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit: Early Mutation") +
  custom_theme()


ggplot(lateStats %>% mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific_format(width = 7)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit, scales = "free_y") +
  geom_hline(yintercept = 1/2000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit: Late Mutation") + 
  custom_theme()

# combine late and early
lateStats$type = "late"
earlyStats$type = "early"

#0

early0 <- ggplot(earlyStats %>% 
                   filter(benefit == 0) %>% 
                   mutate(fitness = factor(fitness)),
                 aes(x = fitness,
                     y = percentFixed,
                     group = factor(benefit),
                     fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific,
                     limits = c(0, 1.1/6000)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation: Early") + 
  custom_theme()

late0 <- ggplot(lateStats %>% 
                  filter(benefit == 0) %>% 
                  mutate(fitness = factor(fitness)),
                aes(x = fitness,
                    y = percentFixed,
                    group = factor(benefit),
                    fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific,
                     limits = c(0, 1.1/2000)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank()) +
  geom_hline(yintercept = 1/2000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ggtitle("Fixation: Late") + 
  custom_theme()

ggarrange(early0, late0,
          ncol = 2,
          common.legend = TRUE,
          legend = "right")

early2 <- ggplot(earlyStats %>% 
                   filter(benefit == 0.2) %>% 
                   mutate(fitness = factor(fitness)),
                 aes(x = fitness,
                     y = percentFixed,
                     group = factor(benefit),
                     fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific,
                     limits = c(0, 8.7/6000)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Early Mutation") + 
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
late2 <- ggplot(lateStats %>% 
                  filter(benefit == 0.2) %>% 
                  mutate(fitness = factor(fitness)),
                aes(x = fitness,
                    y = percentFixed,
                    group = factor(benefit),
                    fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific,
                     limits = c(0, 8.7/2000)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank()) +
  geom_hline(yintercept = 1/2000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ggtitle("Late Mutation") +
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank())

ggarrange(early2, late2,
          ncol = 2,
          common.legend = TRUE,
          legend = "right") + 


#both <- full_join(lateStats, BGKstats)

#ggplot(both %>% mutate(fitness = factor(fitness)), 
#       aes(x = fitness, 
#           y = percentFixed, 
#           group = factor(type))) +
#  geom_line() + 
#  facet_wrap(~type+benefit, scales = "free_y") + 
#  geom_hline(yintercept = 1/2000, color = "red") + 
#  geom_hline(yintercept = 1/6000, color = "blue")


# compare early and late for benefit = 0.2
both2 <- full_join(lateStats %>% filter(benefit == 0.2), 
                   BGKstats %>% filter(benefit == 0.2))

ggplot(both2 %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(type),
            fill = fitness)) +
  scale_fill_manual(values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~type, scales = "free_y") + 
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Early vs Late at Benefit = 0.20")

#compare early and late for benefit = 0.21
both21 <- full_join(lateStats %>% filter(benefit == 0.21),
                    BGKstats %>% filter(benefit == 0.21))

ggplot(both21 %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(type),
            fill = fitness)) +
  scale_fill_manual(values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~type, scales = "free_y") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Early vs Late at Benefit = 0.21")

# smaller increments
fineStats <- read_csv("Data/fineIncrements.csv")

ggplot(fineStats %>% 
         filter(benefit >= 0.193) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = number_format(trim = FALSE)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit, scales = "free_y") + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit from Being Helped") + 
  custom_theme()

ggplot(fineStats %>% 
         filter(benefit >= 0.195 & benefit < 0.201) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit,
             scales = "free_y",
             ncol = 2) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of dominant altruistic allele") + 
  custom_theme()  %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(fineStats %>% 
         filter(benefit >= 0.196) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit,
             scales = "free_y",
             ncol = 2) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of dominant altruistic allele") + 
  custom_theme()  %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#ggplot(het6 %>% 
ggplot(hetStats %>% filter(numInter == 1) %>%
         filter(benefit >= 0.198) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit,
             scales = "free_y",
             ncol = 2) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of non-dominant altruistic allele") + 
  custom_theme()  %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(fineStats %>% 
         filter(benefit >= 0.1956) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed * 6000,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(#labels = scientific,
                     trans = "log10") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit) + 
  geom_hline(yintercept = 1,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit: Small Increments")



fineRow1 <- ggplot(fineStats %>% 
         filter(benefit < 0.196) %>%
           filter(benefit > 0.192) %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific_format(width = 7)) +
  facet_wrap(~benefit) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  ylab(" ") + 
  ggtitle("Fixation by Benefit: Small Increments") +
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank())

fineRow2 <- ggplot(fineStats %>% 
                     filter(benefit < 0.199) %>%
                     filter(benefit >= 0.196) %>% 
                     mutate(fitness = factor(fitness)),
                   aes (x = fitness, 
                        y  = percentFixed,
                        group = factor(benefit),
                        fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific_format(width = 7)) +
  facet_wrap(~benefit) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  ylab("Fixation rate of altruistic allele") +
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank())


fineRow3 <- ggplot(fineStats %>% 
                     filter(benefit >= 0.199) %>% 
                     mutate(fitness = factor(fitness)),
                   aes (x = fitness, 
                        y = percentFixed,
                        group = factor(benefit),
                        fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific_format(width = 7)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab(" ") +
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left = element_text(family = "Courier"))
  
ggarrange(fineRow1, fineRow2, fineRow3,
          nrow = 3,
          common.legend = TRUE,
          legend = "right")

#litter
litterStats <- read_csv("Data/litter.csv")

ggplot(litterStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                    values = litterColors,
                    guide = guide_legend(reverse = TRUE)) +
  geom_line(data = litterStats %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = litterStats %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = litterStats %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/4000,
             color = litterColors[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit and Litter Size") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

litterNo2 = litterStats %>% filter(litterSize != 2)

ggplot(litterNo2 %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                     values = litterColorsNo2,
                     guide = guide_legend(reverse = TRUE)) +
  geom_line(data = litterNo2 %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = litterNo2 %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = litterNo2 %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/4000,
             color = litterColorsNo2[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/6000,
             color = litterColorsNo2[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColorsNo2[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColorsNo2[4],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColorsNo2[5],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

ggplot(litterStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            #color = factor(litterSize),
            fill = factor(litterSize))) +
  scale_fill_manual(name = "Litter Size",
                    values = litterColors2) +
  #geom_line() +
  geom_col(position = position_dodge()) +
  facet_wrap(~benefit, scales = "free_y") + 
  geom_hline(yintercept = 1/4000,
             color = litterColors[1],
             alpha = 0.9) +
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.9) +
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha = 0.9) +
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.9) +
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.9) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit and Litter Size")


ggplot(litterStats %>% 
         filter(litterSize == 2) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            fill = factor(fitness))) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_y_continuous(labels = scientific) +
  geom_col() +
  facet_wrap(~benefit, scales = "fixed") + 
  geom_hline(yintercept = 1/4000,
             #color = "dodgerblue3",
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit when Litter Size = 2") + 
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#recombination
recomStats <- read_csv("Data/recombination.csv")

ggplot(recomStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(recom_rate),
            fill = factor(fitness))) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  geom_col() +
  facet_wrap(~recom_rate) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Recombination Rate") + 
  custom_theme() %+replace%
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#one-way interactions
oneStats <- read_csv("Data/oneWayInteractions.csv")

oneL4 <- oneStats %>% filter(litterSize == 4)
oneL2 <- oneStats %>% filter(litterSize == 2)

filteredOne <- oneStats %>% filter(benefit > 0.195) %>% filter(litterSize != 2)

ggplot(oneStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                     values = litterColors) +
  geom_line(data = oneStats %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = oneStats %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = oneStats %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/4000,
             color = litterColors[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit and Litter Size") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

ggplot(filteredOne %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                     values = litterColors) +
  geom_line(data = filteredOne %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = filteredOne %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = filteredOne %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/6000,
             color = litterColors[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColors[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColors[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[4],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit and Litter Size") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))


ggplot(oneL4 %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = number_format(trim = FALSE)) +
  facet_wrap(~benefit,
             scales = "free_y") + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of altruistic allele") + 
  ggtitle("Fixation Chance by Benefit from Being Helped") + 
  custom_theme()  %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggplot(oneL2 %>% filter(benefit != 0.195) %>%
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = number_format(trim = FALSE)) +
  facet_wrap(~benefit,
             scales = "fixed") + 
  geom_hline(yintercept = 1/4000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of altruistic allele") + 
  ggtitle("Fixation Chance by Benefit from Being Helped") + 
  custom_theme()  %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#single interaction
singleStats <- read_csv("Data/singleInteractions.csv")

singleFine <- singleStats %>% filter(benefit > 0.19 & benefit < 0.21)
singleBig <- singleStats %>% filter(benefit <= 0.19 | benefit >= 0.21 | benefit == 0.2)

ggplot(singleFine %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                     values = litterColors) +
  geom_line(data = singleFine %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = singleFine %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = singleFine %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/4000,
             color = litterColors[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

singleNo2 = singleFine %>% filter(litterSize != 2)

ggplot(singleNo2 %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            color = factor(litterSize))) +
  geom_point() +
  scale_color_manual(name = "Litter Size",
                     values = litterColorsNo2) +
  geom_line(data = singleNo2 %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = singleNo2 %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = singleNo2 %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/6000,
             color = litterColorsNo2[1],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/8000,
             color = litterColorsNo2[2],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/10000,
             color = litterColorsNo2[3],
             alpha = 0.9) + 
  geom_hline(yintercept = 1/12000,
             color = litterColorsNo2[4],
             alpha = 0.9) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

ggplot(singleBig %>% filter(litterSize == 4) %>%
         mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  scale_x_discrete(labels = fitnessLabels) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit, scales = "free_y") +
  #facet_wrap(~benefit) +
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

#heterozygosity
hetStats <- read_csv("Data/heterozygous50.csv")

het6 <- hetStats %>% filter(numInter == 6)

domSingle <- singleFine %>% filter(litterSize == 4)
domSingle$numInter = 1

domSix <- fineStats %>% filter(benefit > 0.195)
domSix$numInter = 6

domStats <- full_join(domSingle, domSix)

interColors = c("#88C5EB",
                "#5AB0E6",
                "#284E66")

ggplot(hetStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(numInter),
            color = factor(numInter))) +
  scale_color_manual(name = "Number of\nInteractions",
                     values = interColors) +
  geom_point() +
  geom_line(data = hetStats %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = hetStats %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = hetStats %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/6000,
             color = "blue",
             alpha = 1) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))

ggplot(domStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(numInter),
            color = factor(numInter))) +
  geom_point() +
  geom_line(data = domStats %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = domStats %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = domStats %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/6000,
             alpha = 1) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Completely Dominant") +
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))


recStats <- recStats1

ggplot(recStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(numInter),
            color = factor(numInter))) +
  scale_color_manual(name = "Number of\nInteractions",
                     values = interColors) +
  geom_point() +
  geom_line(data = recStats %>% 
              mutate(fitness = factor(fitness)) %>% 
              filter(fitness != 0) %>%
              filter(fitness != -1),
            size = 1) + 
  geom_line(data = recStats %>% 
              filter(fitness <= -0.014) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_line(data = recStats %>% 
              filter(fitness >= -0.006) %>%
              mutate(fitness = factor(fitness)),
            linetype = "dashed") +
  geom_hline(yintercept = 1/6000,
             color = "blue",
             alpha = 1) + 
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  custom_theme() %+replace%
  theme(axis.text.x = element_text(angle = 90, hjust = 0),
        axis.ticks.y = element_line(),
        panel.grid.major.x = element_line(color = "#BBBBBB"))
