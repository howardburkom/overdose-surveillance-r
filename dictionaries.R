#####
library(tidyverse)
library(tidytext)
#####

######################################################################################################

##### Read and split data
enriched <- read.csv("enriched.csv", stringsAsFactors = FALSE)
enriched$Chief.Narrative <- gsub("heroine", "heroin", enriched$Chief.Narrative)
enriched$Chief.Complaint <- gsub("heroine", "heroin", enriched$Chief.Complaint)
overdose <- filter(enriched, man.od == 1)
not_overdose <- filter(enriched, man.od == 0)
#####

######################################################################################################

#setwd("D:/Revised")

##### Create PI Dictionary from enriched data
od_pi_terms <- overdose %>% 
  group_by(Primary.Impression) %>% 
  count() %>% 
  arrange(-n) %>% 
  #mutate(od_prop = n/nrow(overdose)) %>% 
  as.data.frame()

total_overdose <- sum(od_pi_terms$n) ## find total number of overall words

not_od_pi_terms <- not_overdose %>% 
  group_by(Primary.Impression) %>% 
  count() %>% 
  arrange(-n) %>%
  #mutate(not_prop = n/nrow(not_overdose)) %>% 
  as.data.frame()

total_not_overdose <- sum(not_od_pi_terms$n) ## find total number of overall words

proptest <- full_join(not_od_pi_terms, od_pi_terms, by = "Primary.Impression")
proptest[is.na(proptest)] <- 0 ## NA's to zero

proptest <- proptest %>% ## formatting df for proptesting
  rename(not_n = n.x, od_n = n.y) %>%
  select(Primary.Impression, not_n, od_n) %>%
  mutate(not_prop = not_n/total_not_overdose) %>%
  mutate(od_prop = od_n/total_overdose) %>%
  mutate(sigdiff = 0) %>%
  mutate(wordtype = 0) %>%
  mutate(pval = 1) %>%
  as.data.frame()

bonferroni = 0.05/nrow(proptest) ## bonferonni correction for multiple comparisons

for (i in 1:nrow(proptest)) {
  
  a <- c(proptest[i, 2], proptest[i, 3]) ## proportions for same word 
  b <- c(total_not_overdose, total_overdose) ## total words NOT number of entries
  x <- prop.test(a, b)
  
  if (x$p.value <= bonferroni & x$conf.int[1] < 0) { ## selecting significant p-vals (+)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- 1
    proptest[i, 8] <- x$p.value
    
  } else if (x$p.value <= bonferroni & x$conf.int[1] > 0) { ## selecting significant p-vals (-)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- -1
    proptest[i, 8] <- x$p.value
    
  }
} 

od_pi_terms <- filter(proptest, sigdiff == 1)

x <- quantile(od_pi_terms$pval, c(0, .2, .4, .6, .8, 1.0))
x <- unname(x)

od_pi_terms <- mutate(od_pi_terms, score = ifelse(pval >= x[5] & wordtype == 1, 1, 
                                                  ifelse(pval >= x[4] & pval < x[5] & wordtype == 1, 2,
                                                         ifelse(pval >= x[3] & pval < x[4] & wordtype == 1, 3,
                                                                ifelse(pval >= x[2] & pval < x[3] & wordtype == 1, 4,
                                                                       ifelse(pval >= x[1] & pval < x[2] & wordtype == 1, 5,
                                                                              ifelse(wordtype == -1, -5, 0)))))))
rm(x)

write.csv(od_pi_terms, "od_pi_terms_2.csv")
#####
######################################################################################################

##### Create CC Word Dictionary from enriched data
od_cc_counts <- overdose %>%
  select(Chief.Complaint) %>%
  unnest_tokens(word, Chief.Complaint) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

total_overdose <- sum(od_cc_counts$n) ## find total number of overall words

not_od_cc_counts <- not_overdose %>%
  select(Chief.Complaint) %>%
  unnest_tokens(word, Chief.Complaint) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n)) 

total_not_overdose <- sum(not_od_cc_counts$n) ## find total number of overall words

proptest <- full_join(not_od_cc_counts, od_cc_counts, by = "word") ## join not OD and OD

proptest <- proptest %>% ## formatting df for proptesting
  rename(not_n = n.x, od_n = n.y) %>%
  select(word, not_n, od_n) %>%
  mutate(not_prop = not_n/total_not_overdose) %>%
  mutate(od_prop = od_n/total_overdose) %>%
  mutate(sigdiff = 0) %>%
  mutate(wordtype = 0) %>%
  mutate(pval = 1) %>%
  as.data.frame()

proptest[is.na(proptest)] <- 0 ## NA's to zero

bonferroni = 0.05/nrow(proptest) ## bonferonni correction for multiple comparisons

for (i in 1:nrow(proptest)) {
  
  a <- c(proptest[i, 2], proptest[i, 3]) ## proportions for same word 
  b <- c(total_not_overdose, total_overdose) ## total words NOT number of entries
  x <- prop.test(a, b)
  
  if (x$p.value <= bonferroni & x$conf.int[1] < 0) { ## selecting significant p-vals (+)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- 1
    proptest[i, 8] <- x$p.value
    
  } else if (x$p.value <= bonferroni & x$conf.int[1] > 0) { ## selecting significant p-vals (-)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- -1
    proptest[i, 8] <- x$p.value
    
  }
} 

od_cc_words <- filter(proptest, sigdiff == 1)

x <- quantile(od_cc_words$pval, c(0, .2, .4, .6, .8, 1.0))
x <- unname(x)

od_cc_words <- mutate(od_cc_words, score = ifelse(pval >= x[5] & wordtype == 1, 1, 
                                                  ifelse(pval >= x[4] & pval < x[5] & wordtype == 1, 2,
                                                         ifelse(pval >= x[3] & pval < x[4] & wordtype == 1, 4,
                                                                ifelse(pval >= x[2] & pval < x[3] & wordtype == 1, 7,
                                                                       ifelse(pval >= x[1] & pval < x[2] & wordtype == 1, 11,
                                                                              ifelse(wordtype == -1, -3, 0)))))))

od_cc_words[(od_cc_words$word == "heroin"), 9] <- 11

rm(x)

#setwd("D:/Revised")
write.csv(od_cc_words, "od_cc_words_2.csv")
#####

######################################################################################################
######################################################################################################

##### Create CN Word Dictionary from enriched data
od_cn_counts <- overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

num_only <- grep("^[[:digit:]]*$", od_cn_counts$word) ## identify numeric only
od_cn_counts <- od_cn_counts[-num_only, ] ## remove numeric only
total_overdose <- sum(od_cn_counts$n) ## find total number of overall words

not_od_cn_counts <- not_overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative) %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n)) 

num_only <- grep("^[[:digit:]]*$", not_od_cn_counts$word) ## identify numeric only
not_od_cn_counts <- not_od_cn_counts[-num_only, ] ## remove numeric only
total_not_overdose <- sum(not_od_cn_counts$n) ## find total number of overall words

proptest <- full_join(not_od_cn_counts, od_cn_counts, by = "word") ## join not OD and OD

proptest <- proptest %>% ## formatting df for proptesting
  rename(not_n = n.x, od_n = n.y) %>%
  select(word, not_n, od_n) %>%
  mutate(not_prop = not_n/total_not_overdose) %>%
  mutate(od_prop = od_n/total_overdose) %>%
  mutate(sigdiff = 0) %>%
  mutate(wordtype = 0) %>%
  mutate(pval = 1) %>%
  as.data.frame()

proptest[is.na(proptest)] <- 0 ## NA's to zero

bonferroni = 0.05/nrow(proptest) ## bonferonni correction for multiple comparisons

for (i in 1:nrow(proptest)) {
  
  a <- c(proptest[i, 2], proptest[i, 3]) ## proportions for same word 
  b <- c(total_not_overdose, total_overdose) ## total words NOT number of entries
  x <- prop.test(a, b)
  
  if (x$p.value <= bonferroni & x$conf.int[1] < 0) { ## selecting significant p-vals (+)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- 1
    proptest[i, 8] <- x$p.value
    
  } else if (x$p.value <= bonferroni & x$conf.int[1] > 0) { ## selecting significant p-vals (-)
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- -1
    proptest[i, 8] <- x$p.value
    
  }
} 


od_cn_words <- filter(proptest, sigdiff == 1)

x <- quantile(od_cn_words$pval, c(0, .2, .4, .6, .8, 1.0))
x <- unname(x)

od_cn_words <- mutate(od_cn_words, score = ifelse(pval >= x[5] & wordtype == 1, 1, 
                                                  ifelse(pval >= x[4] & pval < x[5] & wordtype == 1, 2,
                                                         ifelse(pval >= x[3] & pval < x[4] & wordtype == 1, 3,
                                                                ifelse(pval >= x[2] & pval < x[3] & wordtype == 1, 4,
                                                                       ifelse(pval >= x[1] & pval < x[2] & wordtype == 1, 5,
                                                                              ifelse(wordtype == -1, -5, 0)))))))
rm(x)

write.csv(od_cn_words, "od_cn_words_2.csv")
#####

######################################################################################################

##### Create CN Bigram Dictionary from enriched data
od_cn_counts <- overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ") %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

total_overdose <- sum(od_cn_counts$n) ## find total number of overall words

not_od_cn_counts <- not_overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative, token = "ngrams", n = 2) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ") %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

total_not_overdose <- sum(not_od_cn_counts$n) ## find total number of overall words

proptest <- full_join(not_od_cn_counts, od_cn_counts, by = "word") ## join not OD and OD

proptest <- proptest %>% ## formatting df for proptesting
  rename(not_n = n.x, od_n = n.y) %>%
  select(word, not_n, od_n) %>%
  mutate(not_prop = not_n/total_not_overdose) %>%
  mutate(od_prop = od_n/total_overdose) %>%
  mutate(sigdiff = 0) %>%
  mutate(wordtype = 0) %>%
  mutate(pval = 1) %>%
  as.data.frame()

proptest[is.na(proptest)] <- 0 ## NA's to zero

bonferroni = 0.05/nrow(proptest) ## bonferonni correction for multiple comparisons

for (i in 1:nrow(proptest)) {
  
  a <- c(proptest[i, 2], proptest[i, 3])
  b <- c(total_not_overdose, total_overdose) ## total words NOT number of entries
  x <- prop.test(a, b)
  
  if (x$p.value <= bonferroni & x$conf.int[1] < 0) {
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- 1
    proptest[i, 8] <- x$p.value
    
  } else if (x$p.value <= bonferroni & x$conf.int[1] > 0) {
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- -1
    proptest[i, 8] <- x$p.value
    
  }
} 

od_cn_bigrams <- filter(proptest, sigdiff == 1)

x <- quantile(od_cn_bigrams$pval, c(0, .2, .4, .6, .8, 1.0))
x <- unname(x)

od_cn_bigrams <- mutate(od_cn_bigrams, score = ifelse(pval >= x[5] & wordtype == 1, 1, 
                                                  ifelse(pval >= x[4] & pval < x[5] & wordtype == 1, 2,
                                                         ifelse(pval >= x[3] & pval < x[4] & wordtype == 1, 3,
                                                                ifelse(pval >= x[2] & pval < x[3] & wordtype == 1, 4,
                                                                       ifelse(pval >= x[1] & pval < x[2] & wordtype == 1, 5,
                                                                              ifelse(wordtype == -1, -3, 0)))))))
rm(x)

write.csv(od_cn_bigrams, "od_cn_bigrams_2.csv")
#####

######################################################################################################

##### Create CN Trigram Dictionary from enriched data
od_cn_counts <- overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative, token = "ngrams", n = 3) %>%
  separate(word, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(word, word1, word2, word3, sep = " ") %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

total_overdose <- sum(od_cn_counts$n) ## find total number of overall words

not_od_cn_counts <- not_overdose %>%
  select(Chief.Narrative) %>%
  unnest_tokens(word, Chief.Narrative, token = "ngrams", n = 3) %>%
  separate(word, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>%
  unite(word, word1, word2, word3, sep = " ") %>%
  group_by(word) %>%
  summarise (n = n()) %>%
  filter(n > 1) %>%
  mutate(freq = n / sum(n))

total_not_overdose <- sum(not_od_cn_counts$n) ## find total number of overall words

proptest <- full_join(not_od_cn_counts, od_cn_counts, by = "word") ## join not OD and OD

proptest <- proptest %>% ## formatting df for proptesting
  rename(not_n = n.x, od_n = n.y) %>%
  select(word, not_n, od_n) %>%
  mutate(not_prop = not_n/total_not_overdose) %>%
  mutate(od_prop = od_n/total_overdose) %>%
  mutate(sigdiff = 0) %>%
  mutate(wordtype = 0) %>%
  mutate(pval = 1) %>%
  as.data.frame()

proptest[is.na(proptest)] <- 0 ## NA's to zero

bonferroni = 0.05/nrow(proptest) ## bonferonni correction for multiple comparisons

for (i in 1:nrow(proptest)) {
  
  a <- c(proptest[i, 2], proptest[i, 3])
  b <- c(total_not_overdose, total_overdose) ## total words NOT number of entries
  x <- prop.test(a, b)
  
  if (x$p.value <= bonferroni & x$conf.int[1] < 0) {
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- 1
    proptest[i, 8] <- x$p.value
    
  } else if (x$p.value <= bonferroni & x$conf.int[1] > 0) {
    
    proptest[i, 6] <- 1 
    proptest[i, 7] <- -1
    proptest[i, 8] <- x$p.value
    
  }
} 

od_cn_trigrams <- filter(proptest, sigdiff == 1)


x <- quantile(od_cn_trigrams$pval, c(0, .2, .4, .6, .8, 1.0))
x <- unname(x)

od_cn_trigrams <- mutate(od_cn_trigrams, score = ifelse(pval >= x[5] & wordtype == 1, 1, 
                                                      ifelse(pval >= x[4] & pval < x[5] & wordtype == 1, 2,
                                                             ifelse(pval >= x[3] & pval < x[4] & wordtype == 1, 3,
                                                                    ifelse(pval >= x[2] & pval < x[3] & wordtype == 1, 4,
                                                                           ifelse(pval >= x[1] & pval < x[2] & wordtype == 1, 5,
                                                                                  ifelse(wordtype == -1, -3, 0)))))))
rm(x)

write.csv(od_cn_trigrams, "od_cn_trigrams_2.csv")
######