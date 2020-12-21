library(tidyverse)
library(scales)
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
                  "0")

#         Data

# early
#BGKRaw <- read_csv("CSVs/boundTotal.csv")
#BGKdata <- processRaw(BGKRaw)
#BGKstats <- getStats(BGKdata)

#write_csv(BGKstats, 
#          path = "CSVs/BGKStats.csv", 
#          col_names = TRUE)

BGKstats <- read_csv("CSVs/BGKStats.csv")

ggplot(BGKstats %>% mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           fill = fitness)) +
  scale_fill_manual(values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit, scales = "free_y") +
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit")

ggplot(BGKstats %>% mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit))) +
  geom_line() +
  scale_y_continuous(labels = scientific) +
  facet_wrap(~benefit, scales = "free_y") +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit")


# late
lateRaw <- read_csv("CSVs/lateTotal.csv")
late <- processRaw(lateRaw)
lateStats <- getStats(late)

write_csv(lateStats,
          "CSVs/lateStats.csv",
          col_names = TRUE)
lateStats <- read_csv("CSVs/lateStats.csv")

# combine late and early
lateStats$type = "late"
BGKstats$type = "early"

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

#need to schedule some more late simulations

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

ggplot(fineStats %>% mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            fill = fitness)) +
  scale_fill_manual(name = "Fitness",
                    values = fitnessColors) +
  geom_col() +
  scale_y_continuous(labels = scientific) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit, scales = "free_y")  + 
  geom_hline(yintercept = 1/6000,
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit: Small Increments")


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

allLitter <- full_join(litterStats1,litterStats2)
allLitter <- full_join(allLitter,litterStats3)

litterStats <- allLitter %>%
  group_by(fitness, benefit, cost, litterSize) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

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
             alpha = 0.75) +
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.75) +
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha= 0.75) +
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.75) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.75) +
  facet_wrap(~benefit, scales = "free_y") +
  scale_y_continuous(labels = scientific,
                     trans = "log1p") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit and Litter Size")

ggplot(litterStats %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(litterSize),
            #color = factor(litterSize),
            fill = factor(litterSize))) +
  scale_fill_manual(name = "Litter Size",
                    values = litterColors) +
  #geom_line() +
  geom_col(position = position_dodge()) +
  facet_wrap(~benefit, scales = "free_y") + 
  geom_hline(yintercept = 1/4000,
             color = litterColors[1],
             alpha = 0.75) +
  geom_hline(yintercept = 1/6000,
             color = litterColors[2],
             alpha = 0.75) +
  geom_hline(yintercept = 1/8000,
             color = litterColors[3],
             alpha= 0.75) +
  geom_hline(yintercept = 1/10000,
             color = litterColors[4],
             alpha = 0.75) + 
  geom_hline(yintercept = 1/12000,
             color = litterColors[5],
             alpha = 0.75) +
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
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~benefit, scales = "fixed") + 
  geom_hline(yintercept = 1/4000,
             #color = "dodgerblue3",
             color = "red",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit when Litter Size = 2")


#                   ISLANDS

#islands
#for m = 0.1
islandRaw1 <- read_csv("CSVs/islaM1.csv")
island1 <- processRaw(islandRaw1)
islandStats1 <- getStats(island1)
islandStats1$migration = 0.1

#for m = 0.5
islandRaw5 <- read_csv("CSVs/islaM5.csv")
island5 <- processRaw(islandRaw5)
islandStats5 <- getStats(island5)
islandStats5$migration = 0.5

#for m = 0.01
islandRaw01 <- read_csv("CSVs/islaM01.csv")
island01 <- processRaw(islandRaw01)
islandStats01 <- getStats(island01)
islandStats01$migration = 0.01

islandStats <- full_join(islandStats1, islandStats5)
islandStats <- full_join(islandStats01, islandStats)

ggplot(islandStats01# %>% filter(migration == 0.01)
       %>% mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit))) + 
  geom_line() +
  facet_wrap(~benefit, scales = "free_y")  + 
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit—Island Model—Migration Rate: 0.01")

ggplot(islandStats %>% filter(migration == 0.1)
       %>% mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit))) + 
  geom_line() +
  facet_wrap(~benefit, scales = "free_y")  + 
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit—Island Model—Migration Rate: 0.1")


ggplot(islandStats %>% mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           color = factor(benefit))) + 
  geom_line() + 
  geom_hline(yintercept = 1/6000,
             color = "dodgerblue3",
             alpha = 0.75) +
  ggtitle("Fixation by Benefit in an island model")
