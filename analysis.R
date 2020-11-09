library(tidyverse)
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

#         Data

# early
BGKRaw <- read_csv("CSVs/boundTotal.csv")
BGKdata <- processRaw(BGKRaw)
BGKstats <- getStats(BGKdata)

ggplot(BGKstats %>%
         mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit))) +
  geom_line() +
  facet_wrap(~benefit, scales = "free_y") +
  geom_hline(yintercept = 1/6000, color = "red") +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit")

ggplot(BGKstats %>%
         mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit))) +
  geom_line() +
  facet_wrap(~benefit, scales = "free_y") +
  xlab("Fitness of detrimental allele") +
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit (no reference line)")


# late
lateRaw <- read_csv("CSVs/lateTotal.csv")
late <- processRaw(lateRaw)
lateStats <- getStats(late)

# combine late and early
lateStats$type = "late"
BGKstats$type = "early"

both <- full_join(lateStats, BGKstats)

ggplot(both %>% mutate(fitness = factor(fitness)), 
       aes(x = fitness, 
           y = percentFixed, 
           group = factor(type))) +
  geom_line() + 
  facet_wrap(~type+benefit, scales = "free_y") + 
  geom_hline(yintercept = 1/2000, color = "red") + 
  geom_hline(yintercept = 1/6000, color = "blue")


# compare early and late for benefit = 0.2
both2 <- full_join(lateStats %>% filter(benefit == 0.2), 
                   BGKstats %>% filter(benefit == 0.2))

ggplot(both2 %>% 
         mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(type))) + 
  geom_line() +
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
            group = factor(type))) + 
  geom_line() +
  facet_wrap(~type, scales = "free_y") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Early vs Late at Benefit = 0.21")

#need to schedule some more late simulations

# smaller increments for benefit
fineRaw <- read_csv("CSVs/fineTotal.csv")
fine <- processRaw(fineRaw)
fineStats <- getStats(fine)

#more cases but there's an issue with the file
fineRaw2 <- read_csv("CSVs/fineTotal2.csv")
fine2 <- processRaw(fineRaw2)
fineStats2 <- getStats(fine2)
fineStats2 <- fineStats2 %>% filter(numTotal != 0)

ggplot(fineStats %>% mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit),
            color = factor(benefit))) + 
  geom_line() +
  facet_wrap(~benefit, scales = "free_y")  + 
  geom_hline(yintercept = 1/6000, color = "red") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit—smaller increments")

#islands
islandRaw <- read_csv("CSVs/islandTotal.csv")
island <- processRaw(islandRaw)
islandStats <- getStats(island)

ggplot(islandStats %>% mutate(fitness = factor(fitness)),
       aes (x = fitness, 
            y  = percentFixed,
            group = factor(benefit))) + 
  geom_line() +
  facet_wrap(~benefit, scales = "free_y")  + 
  #geom_hline(yintercept = 1/6000, color = "red") +
  xlab("Fitness of detrimental allele") + 
  ylab("Fixation rate of altruistic allele") + 
  ggtitle("Fixation by Benefit—Island Model")

ggplot(islandStats %>% mutate(fitness = factor(fitness)),
       aes(x = fitness,
           y = percentFixed,
           group = factor(benefit),
           color = factor(benefit))) + 
  geom_line() + 
  geom_hline(yintercept = 1/6000, color = "red") + 
  ggtitle("Fixation by Benefit in an island model")




