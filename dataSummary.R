#loading and formatting data

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

getStats3 <- function(data) {
  stats <- data %>%
    group_by(fitness, benefit, cost, litterSize, numInter) %>%
    summarize(numLost = sum(status == "lost"), 
              numFixed = sum(status == "fixed"),
              numTotal = numLost + numFixed,
              percentFixed = numFixed / numTotal) %>%
    ungroup()
  return(stats)
}


getStatsR <- function(data) {
  stats <- data %>%
    group_by(fitness, benefit, cost, litterSize, recom_rate) %>%
    summarize(numLost = sum(status == "lost"), 
              numFixed = sum(status == "fixed"),
              numTotal = numLost + numFixed,
              percentFixed = numFixed / numTotal) %>%
    ungroup()
  return(stats)
}



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

write_csv(earlyStats,
          path = "Data/largeIncrements.csv",
          col_names = TRUE)


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

write_csv(lateStats,
          path = "Data/lateLargeIncrements.csv",
          col_names = TRUE)



#fine

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

write_csv(fineStats,
          path = "Data/fineIncrements.csv",
          col_names = TRUE)



#litter sizes
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

write_csv(litterStats,
          path = "Data/litter.csv",
          col_names = TRUE)

#recombination
recom1 <- read_csv("CSVs/recomTotal1.csv")
recomStats1 <- getStatsR(recom1)
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



#single pairing examined

#single1 <- read_csv("CSVs/singleTotal1.csv")
#singleStats1 <- getStats2(single1)
#write_csv(singleStats1,
#          "CSVs/singleStats1.csv",
#         col_names = TRUE)
singleStats1 <- read_csv("CSVs/singleStats1.csv")

#single2 <- read_csv("CSVs/singleTotal2.csv")
#singleStats2 <- getStats2(single2)
#write_csv(singleStats2,
#          "CSVs/singleStats2.csv",
#          col_names = TRUE)
singleStats2 <- read_csv("CSVs/singleStats2.csv")

#single3 <- read_csv("CSVs/singleTotal3.csv")
#singleStats3 <- getStats2(single3)
#write_csv(singleStats3,
#          "CSVs/singleStats3.csv",
#          col_names = TRUE)
singleStats3 <- read_csv("CSVs/singleStats3.csv")

#single4 <- read_csv("CSVs/singleTotal4.csv")
#singleStats4 <- getStats2(single4)
#write_csv(singleStats4,
#          "CSVs/singleStats4.csv",
#          col_names = TRUE)
singleStats4 <- read_csv("CSVs/singleStats4.csv")

#single5 <- read_csv("CSVs/singleTotal5.csv")
#singleStats5 <- getStats2(single5)
#write_csv(singleStats5,
#          "CSVs/singleStats5.csv",
#          col_names = TRUE)
singleStats5 <- read_csv("CSVs/singleStats5.csv")

#single6 <- read_csv("CSVs/singleTotal6.csv")
#singleStats6 <- getStats2(single6)
#write_csv(singleStats6,
#          "CSVs/singleStats6.csv",
#          col_names = TRUE)
singleStats6 <- read_csv("CSVs/singleStats6.csv")

#single7 <- read_csv("CSVs/singleTotal7.csv")
#singleStats7 <- getStats2(single7)
#write_csv(singleStats7,
#          "CSVs/singleStats7.csv",
#          col_names = TRUE)
singleStats7 <- read_csv("CSVs/singleStats7.csv")

#single8 <- read_csv("CSVs/singleTotal8.csv")
#singleStats8 <- getStats2(single8)
#write_csv(singleStats8,
#          "CSVs/singleStats8.csv",
#          col_names = TRUE)
singleStats8 <- read_csv("CSVs/singleStats8.csv")

allSingle = full_join(singleStats1, singleStats2)
allSingle = full_join(allSingle, singleStats3)
allSingle = full_join(allSingle, singleStats4)
allSingle = full_join(allSingle, singleStats5)
allSingle = full_join(allSingle, singleStats6)
allSingle = full_join(allSingle, singleStats7)
allSingle = full_join(allSingle, singleStats8)

singleStats <- allSingle %>% 
  group_by(fitness, benefit, cost, litterSize) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

write_csv(singleStats,
          path = "Data/singleInteractions.csv",
          col_names = TRUE)



#het
#het1 <- read_csv("CSVs/hetTotal1.csv")
#hetStats1 <- getStats3(het1)
#write_csv(hetStats1,
#          "CSVs/hetStats1.csv",
#          col_names = TRUE)
hetStats1 <- read_csv("CSVs/hetStats1.csv")

#het2 <- read_csv("CSVs/hetTotal2.csv")
#hetStats2 <- getStats3(het2)
#write_csv(hetStats2,
#          "CSVs/hetStats2.csv",
#          col_names = TRUE)
hetStats2 <- read_csv("CSVs/hetStats2.csv")

#het3 <- read_csv("CSVs/hetTotal3.csv")
#hetStats3 <- getStats3(het3)
#write_csv(hetStats3,
#          "CSVs/hetStats3.csv",
#          col_names = TRUE)
hetStats3 <- read_csv("CSVs/hetStats3.csv")

#het4 <- read_csv("CSVs/hetTotal4.csv")
#hetStats4 <- getStats3(het4)
#write_csv(hetStats4,
#          "CSVs/hetStats4.csv",
#          col_names = TRUE)
hetStats4 <- read_csv("CSVs/hetStats4.csv")

#het5 <- read_csv("CSVs/hetTotal5.csv")
#hetStats5 <- getStats3(het5)
#write_csv(hetStats5,
#          "CSVs/hetStats5.csv",
#          col_names = TRUE)
hetStats5 <- read_csv("CSVs/hetStats5.csv")

allHet = full_join(hetStats1, hetStats2)
allHet = full_join(allHet, hetStats3)
allHet = full_join(allHet, hetStats4)
allHet = full_join(allHet, hetStats5)

hetStats <- allHet %>% 
  group_by(fitness, benefit, cost, litterSize, numInter) %>%
  summarize(numLost = sum(numLost),
            numFixed = sum(numFixed),
            numTotal = sum(numTotal),
            percentFixed = numFixed/numTotal)

write_csv(hetStats,
          path = "Data/heterozygous50.csv",
          col_names = T)


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

#write_csv(oneStats,
#          path = "Data/oneWayInteractions.csv",
#          col_names = TRUE)



#rec

#rec1 <- read_csv("CSVs/recTotal1.csv")
#recStats1 <- getStats3(rec1)
#write_csv(recStats1,
#          "CSVs/recStats1.csv",
#          col_names = TRUE)
recStats1 <- read_csv("CSVs/recStats1.csv")
