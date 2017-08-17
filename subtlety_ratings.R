library(dplyr)
library(tidyr)

dat <- readr::read_csv("mturkcodingraw.csv") %>% 
  gather(key=measure, value=response, p1_1:p40_3, factor_key=TRUE, na.rm=TRUE) %>%
  group_by(measure) %>% 
  summarise(value=mean(response)) %>% 
  separate(measure, c("paradigm", "question"), "[_]") %>% 
  mutate(question=factor(ifelse(question==1, "figure",
                                ifelse(question==2, "fake", "obvious")))) %>% 
  spread(question, value)

dat[40,1] <- "p4"
dat[40,2] <- colSums(d1[which(d1$paradigm %in% c("p1","p2")),2:4])[[1]]/2
dat[40,3] <- colSums(d1[which(d1$paradigm %in% c("p1","p2")),2:4])[[2]]/2
dat[40,4] <- colSums(d1[which(d1$paradigm %in% c("p1","p2")),2:4])[[3]]/2
