library(tidyverse)
library(scales)
library(ggpubr)
setwd("~/Desktop/Brandvain Lab/BG and kinship/")

processRaw <- function(rawData) {
  filtered <- rawData %>% 
                  #group_by(replicate) %>%
                  #mutate(maxGen = max(generation)) %>%
                  #ungroup() %>%
                  #filter(generation == maxGen) %>%
                  #select(-maxGen) %>%
                  filter(status != "ongoing")
  return(filtered)
}

getStats <- function(data) {
  stats <- data %>%
                  group_by(fitness, benefit, cost) %>%
                  summarize(numLost = sum(status == "lost"), 
                            numFixed = sum(status == "fixed"),
                            numTotal = numLost + numFixed,
                            percentFixed = numFixed / numTotal) %>%
                  ungroup()
  return(stats)
}

getStats2 <- function(data) {
  stats <- data %>%
    group_by(fitness, benefit, cost, litterSize) %>%
    summarize(numLost = sum(status == "lost"), 
              numFixed = sum(status == "fixed"),
              numTotal = numLost + numFixed,
              percentFixed = numFixed / numTotal) %>%
    ungroup()
  return(stats)
}

getStatsR <- function(data) {
  stats <- data %>%
    group_by(fitness, benefit, cost, recom_rate) %>%
    summarize(numLost = sum(status == "lost"), 
              numFixed = sum(status == "fixed"),
              numTotal = numLost + numFixed,
              percentFixed = numFixed / numTotal) %>%
    ungroup()
  return(stats)
}

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

#seeds
seedAnalysis <- read_csv("CSVs/seedAnalysis.csv",
                         col_names = c("fitness",
                                       "cost",
                                       "benefit",
                                       "popSize",
                                       "numNeutral",
                                       "numNeg",
                                       "pi",
                                       "seedPath"))

seedAnalysis$Ne <- seedAnalysis$pi / (4 * 5e-08)

hist(seedAnalysis$Ne)

seedSummary <- seedAnalysis %>% 
  group_by(fitness) %>%
  summarize(popSize = mean(popSize),
            numNeutral = mean(numNeutral),
            numNeg = mean(numNeg),
            meanNeg = mean(fitness) * numNeg * -1,
            pi = mean(pi)) %>%
  ungroup()

seedsBGS <- seedSummary %>%
  filter(fitness != 0) %>%
  filter(fitness != -1)

seedsCtrl <- seedSummary %>%
  filter(fitness %% 1 == 0)

mean(seedsBGS$pi)
mean(seedsCtrl$pi)

# early
#BGKRaw <- read_csv("CSVs/boundTotal.csv")
#BGKdata <- processRaw(BGKRaw)
#BGKstats <- getStats(BGKdata)
#write_csv(BGKstats, 
#          path = "CSVs/earlyStats0.csv", 
#          col_names = TRUE)

earlyStats0 <- read_csv("CSVs/earlyStats0.csv")

#early1 <- read_csv("CSVs/earlyTotal1.csv")
#earlyStats1 <- getStats(early1)
#write_csv(earlyStats1,
#          path = "CSVs/earlyStats1.csv",
#          col_names = TRUE)
earlyStats1 <- read_csv("CSVs/earlyStats1.csv")


#early2 <- read_csv("CSVs/earlyTotal2.csv")
#early2n <- early2 %>% filter(cost > 0) #cuts out one row with NA values
#earlyStats2 <- getStats(early2n)
#write_csv(earlyStats2,
#          path = "CSVs/earlyStats2.csv",
#          col_names = TRUE)
earlyStats2 <- read_csv("CSVs/earlyStats2.csv")

#early3 <- read_csv("CSVs/earlyTotal3.csv")
#earlyStats3 <- getStats(early3)
#write_csv(earlyStats3,
#          path = "CSVs/earlyStats3.csv",
#          col_names = TRUE)
earlyStats3 <- read_csv("CSVs/earlyStats3.csv")

allEarly <- full_join(earlyStats0, earlyStats1)
allEarly <- full_join(allEarly, earlyStats2)
allEarly <- full_join(allEarly, earlyStats3)
allEarly <- allEarly %>% filter(benefit != 0.22)
earlyStats <- allEarly %>%
  group_by(fitness, benefit, cost) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

#write_csv(earlyStats,
#          path = "Data/largeIncrements.csv",
#          col_names = TRUE)

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
  ggtitle("Fixation by Benefit") +
  custom_theme()

# late
#lateRaw <- read_csv("CSVs/lateTotal.csv")
#late <- processRaw(lateRaw)
#lateStats <- getStats(late)
#write_csv(lateStats,
#          "CSVs/lateStats.csv",
#          col_names = TRUE)
lateStats1 <- read_csv("CSVs/lateStats.csv")

#late2 <- read_csv("CSVs/lateTotal2.csv")
#lateStats2 <- getStats(late2)
#write_csv(lateStats2,
#          "CSVs/lateStats2.csv",
#          col_names = TRUE)
lateStats2 <- read_csv("CSVs/lateStats2.csv")

allLate <- full_join(lateStats1, lateStats2)
lateStats <- allLate %>%
  group_by(fitness, benefit, cost) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

#write_csv(lateStats,
#          path = "Data/lateLargeIncrements.csv",
#          col_names = TRUE)

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

# smaller increments for benefit

#fineRaw2 <- read_csv("CSVs/fineTotal2.csv")
#fine2 <- processRaw(fineRaw2)
#write_csv(fine2, 
#          path = "CSVs/fineTotal2-processed.csv", 
#          col_names = TRUE)
#rm(fineRaw2)
#fine2Stats <- getStats(fine2)
#write_csv(fine2Stats,
#          path = "CSVs/fine2Stats.csv",
#          col_names = TRUE)
#rm(fine2)
fine2Stats <- read_csv("CSVs/fine2Stats.csv")

#fineRaw3 <- read_csv("CSVs/fineTotal3.csv")
#fine3 <- processRaw(fineRaw3)
#write_csv(fine3,
#          path = "CSVs/fineTotal3-processed.csv",
#          col_names = TRUE)
#rm(fineRaw3)
#fine3Stats <- getStats(fine3)
#write_csv(fine3Stats,
#          path = "CSVs/fine3Stats.csv",
#          col_names = TRUE)
#rm(fine3)
fine3Stats <- read_csv("CSVs/fine3Stats.csv")

#fineRaw4 <- read_csv("CSVs/fineTotal4.csv")
#fine4 <- processRaw(fineRaw4)
#write_csv(fine4,
#          path = "CSVs/fineTotal4-processed.csv",
#          col_names = TRUE)
#rm(fineRaw4)
#fine4Stats <- getStats(fine4)
#write_csv(fine4Stats,
#          path = "CSVs/fine4Stats.csv",
#          col_names = TRUE)
#rm(fine4)
fine4Stats <- read_csv("CSVs/fine4Stats.csv")


allStats <- full_join(fine2Stats,fine3Stats)
allStats <- full_join(allStats, fine4Stats)
fineStats <- allStats %>%
            group_by(fitness, benefit, cost) %>%
            summarize(numLost = sum(numLost),
                      numFixed = sum(numFixed),
                      numTotal = sum(numTotal),
                      percentFixed = numFixed/numTotal)

#write_csv(fineStats,
#          path = "Data/fineIncrements.csv",
#          col_names = TRUE)

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
  scale_y_continuous(labels = number_format(trim = FALSE)) +
  facet_wrap(~benefit,
             scales = "free_y",
             ncol = 2) + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness effect of detrimental allele") + 
  ylab("Fixation chance of altruistic allele") + 
  ggtitle("Fixation Chance by Benefit from Being Helped") + 
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
#litterRaw <- read_csv("CSVs/litterTotal.csv")
#litter <- processRaw(litterRaw)
#write_csv(litter,
#          path = "CSVs/litter-processed.csv",
#          col_names = TRUE)
#rm(litterRaw)
#litterStats <- getStats2(litter)
#write_csv(litterStats,
#          "CSVs/litterStats.csv",
#          col_names = TRUE)
litterStats1 <- read_csv("CSVs/litterStats.csv")

#litterRaw2 <- read_csv("CSVs/litterTotal2.csv")
#litter2 <- processRaw(litterRaw2)
#write_csv(litter2,
#          path = "CSVs/litter2-processed.csv",
#          col_names = TRUE)
#rm(litterRaw2)
#litterStats2 <- getStats2(litter2)
#write_csv(litterStats2,
#          "CSVs/litterStats2.csv",
#          col_names = TRUE)
litterStats2 <- read_csv("CSVs/litterStats2.csv")

#litterRaw3 <- read_csv("CSVs/litterTotal3.csv")
#litter3 <- processRaw(litterRaw3)
#write_csv(litter3,
#          path = "CSVs/litter3-processed.csv",
#          col_names = TRUE)
#rm(litterRaw3)
#litterStats3 <- getStats2(litter3)
#write_csv(litterStats3,
#          "CSVs/litterStats3.csv",
#          col_names = TRUE)
litterStats3 <- read_csv("CSVs/litterStats3.csv")

#litter4 <- read_csv("CSVs/litterTotal4.csv")
#litterStats4 <- getStats2(litter4)
#write_csv(litterStats4,
#          "CSVs/litterStats4.csv",
#          col_names = TRUE)
litterStats4 <- read_csv("CSVs/litterStats4.csv")

#litter5 <- read_csv("CSVs/litterTotal5.csv")
#litterStats5 <- getStats2(litter5)
#write_csv(litterStats5,
#          "CSVs/litterStats5.csv",
#          col_names = TRUE)
litterStats5 <- read_csv("CSVs/litterStats5.csv")



allLitter <- full_join(litterStats1,litterStats2)
allLitter <- full_join(allLitter,litterStats3)
allLitter <- full_join(allLitter,litterStats4)
allLitter <- full_join(allLitter,litterStats5)


litterStats <- allLitter %>%
  group_by(fitness, benefit, cost, litterSize) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

#write_csv(litterStats,
#          path = "Data/litter.csv",
#          col_names = TRUE)

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


ggplot(blah %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            fill = factor(fitness))) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors,
                    labels = fitnessLabels) +
  geom_col() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept = 0.000166666666667,
             #color = "dodgerblue3",
             color = "red",
             alpha = 0.75) +
  geom_hline(yintercept = 0.000281220217284,
             #color = "dodgerblue3",
             color = "green",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele")


#recombination
#recom1 <- read_csv("CSVs/recomTotal1.csv")
#recomStats1 <- getStatsR(recom1)
#write_csv(recomStats1,
#          "CSVs/recomStats1.csv",
#          col_names = TRUE)
recomStats1 <- read_csv("CSVs/recomStats1.csv")

recomStats0 <- fineStats %>% filter(benefit == 0.2)
recomStats0$recom_rate <- 1e-8

recomStats <- full_join(recomStats0, recomStats1)

write_csv(recomStats,
          path = "Data/recombination.csv",
          col_names = TRUE)

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
#one1 <- read_csv("CSVs/oneTotal1.csv")
#oneStats1 <- getStats2(one1)
#write_csv(oneStats1,
#          "CSVs/oneStats1.csv",
#          col_names = TRUE)
oneStats1 <- read_csv("CSVs/oneStats1.csv")

#one2 <- read_csv("CSVs/oneTotal2.csv")
#oneStats2 <- getStats2(one2)
#write_csv(oneStats2,
#          "CSVs/oneStats2.csv",
#          col_names = TRUE)
oneStats2 <- read_csv("CSVs/oneStats2.csv")

#one3 <- read_csv("CSVs/oneTotal3.csv")
#oneStats3 <- getStats2(one3)
#write_csv(oneStats3,
#          "CSVs/oneStats3.csv",
#          col_names = TRUE)
oneStats3 <- read_csv("CSVs/oneStats3.csv")

#one4 <- read_csv("CSVs/oneTotal4.csv")
#oneStats4 <- getStats2(one4)
#write_csv(oneStats4,
#          "CSVs/oneStats4.csv",
#          col_names = TRUE)
oneStats4 <- read_csv("CSVs/oneStats4.csv")

allOne <- full_join(oneStats1, oneStats2)
allOne <- full_join(allOne, oneStats3)
allOne <- full_join(allOne, oneStats4)

oneStats <- allOne %>%
  group_by(fitness, benefit, cost, litterSize) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

write_csv(oneStats,
          path = "Data/oneWayInteractions.csv",
          col_names = TRUE)

oneL4 <- oneStats %>% filter(litterSize == 4)
oneL2 <- oneStats %>% filter(litterSize == 2)

filteredOne <- oneStats %>% filter(benefit > 0.195)

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

