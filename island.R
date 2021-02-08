
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
