setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
df <-read_csv("results.csv")
unique(df$Item)
df2 <- read_csv('freq_for_data.csv')
participants <- read_csv('participants.csv')
participants <- participants[participants$errors_adj < 20 & participants$age <=65, ]
participants <- c(participants$id)
df <- merge(df, df2, by.df = "NP")
df %>% 
  filter(RT < 20000,
         RT > 0,
         Correct >= 0,
         freq > 0) %>% 
  mutate(log_RT = log(RT),
         log_freq = log(freq)) ->
  filtered


filtered <- filtered[ filtered$id %in% participants, ]

# При анализе разбиваем буквальное, метафору и десемантизированное значение на высокочастотные и низкочастотные группы. 
# Для каждой группы убираем все что выходит за +- 2 средних отклонения

literal_metaphor <- filtered[filtered$Type == 'literal_m' | filtered$Type == 'metaphor', ]
literal_bleaching <- filtered[filtered$Type == 'literal_b' | filtered$Type == 'bleaching', ]

hist(literal_metaphor$RT, breaks=100)

literal_metaphor %>%
  filter(freq >500) -> high_literal_metaphor

literal_m_high <- high_literal_metaphor[high_literal_metaphor$Type == 'literal_m',]
literal_m_high <- 
  literal_m_high[literal_m_high$RT > (mean(literal_m_high$RT) - 2*sd(literal_m_high$RT)) & 
              literal_m_high$RT < (mean(literal_m_high$RT) + 2*sd(literal_m_high$RT)), ]


metaphor_high <- high_literal_metaphor[high_literal_metaphor$Type == 'metaphor',]
metaphor_high <- metaphor_high[metaphor_high$RT > (mean(metaphor_high$RT) - 2*sd(metaphor_high$RT)) & 
                                 metaphor_high$RT < (mean(metaphor_high$RT) + 2*sd(metaphor_high$RT)), ]

literal_metaphor %>%
  filter(freq < 500) -> low_literal_metaphor

literal_m_low <- low_literal_metaphor[low_literal_metaphor$Type == 'literal_m',]
literal_m_low <- 
  literal_m_low[literal_m_low$RT > (mean(literal_m_low$RT) - 2*sd(literal_m_low$RT)) & 
                  literal_m_low$RT < (mean(literal_m_low$RT) + 2*sd(literal_m_low$RT)), ]


metaphor_low <- low_literal_metaphor[low_literal_metaphor$Type == 'metaphor',]
metaphor_low <- metaphor_low[metaphor_low$RT > (mean(metaphor_low$RT) - 2*sd(metaphor_low$RT)) & 
                              metaphor_low$RT < (mean(metaphor_low$RT) + 2*sd(metaphor_low$RT)), ]


low_m <- rbind(literal_m_low, metaphor_low)
low_m <- low_m[low_m$RT > (mean(low_m$RT) - 2*sd(low_m$RT)) & 
                 low_m$RT < (mean(low_m$RT) + 2*sd(low_m$RT)), ]
                            
high_m <- rbind(literal_m_high, metaphor_high)
high_m <- high_m[high_m$RT > (mean(high_m$RT) - 2*sd(high_m$RT)) & 
                   high_m$RT < (mean(high_m$RT) + 2*sd(high_m$RT)), ]

literal_metaphor <- rbind(low_m, high_m)
unique(literal_metaphor$NP)
hist(literal_metaphor$RT, breaks= 100)

unique(literal_bleaching$NP)
#####
literal_bleaching %>%
  filter(freq >500) -> high_literal_bleaching

literal_b_high <- high_literal_bleaching[high_literal_bleaching$Type == 'literal_b',]
literal_b_high <- 
 literal_b_high[literal_b_high$RT > (mean(literal_b_high$RT) - 2*sd(literal_b_high$RT)) & 
                   literal_b_high$RT < (mean(literal_b_high$RT) + 2*sd(literal_b_high$RT)), ]


bleaching_high <- high_literal_bleaching[high_literal_bleaching$Type == 'bleaching',]
bleaching_high <- bleaching_high[bleaching_high$RT > (mean(bleaching_high$RT) - 2*sd(bleaching_high$RT)) & 
                                   bleaching_high$RT < (mean(bleaching_high$RT) + 2*sd(bleaching_high$RT)), ]


literal_bleaching %>%
  filter(freq < 500) -> low_literal_bleaching

literal_b_low <- low_literal_bleaching[low_literal_bleaching$Type == 'literal_b',]
literal_b_low <- 
  literal_b_low[literal_b_low$RT > (mean(literal_b_low$RT) - 2*sd(literal_b_low$RT)) & 
                  literal_b_low$RT < (mean(literal_b_low$RT) + 2*sd(literal_b_low$RT)), ]


bleaching_low <- low_literal_bleaching[low_literal_bleaching$Type == 'bleaching',]
bleaching_low <- bleaching_low[bleaching_low$RT > (mean(bleaching_low$RT) - 2*sd(bleaching_low$RT)) & 
                                bleaching_low$RT < (mean(bleaching_low$RT) + 2*sd(bleaching_low$RT)), ]

low_b <- rbind(literal_b_low, bleaching_low)
low_b <- low_b[low_b$RT > (mean(low_b$RT) - 2*sd(low_b$RT)) & 
                 low_b$RT < (mean(low_b$RT) + 2*sd(low_b$RT)), ]

high_b <- rbind(literal_b_high, bleaching_high)
high_b <- high_b[high_b$RT > (mean(high_b$RT) - 2*sd(high_b$RT)) & 
                   high_b$RT < (mean(high_b$RT) + 2*sd(high_b$RT)), ]


literal_bleaching <- rbind(low_b, high_b)

### Подсчет средних ###
# Буквальное значение - Десемантизированное
lb_high <- literal_bleaching[literal_bleaching$Type=="literal_b" & literal_bleaching$freq> 500, ]
lb_high.RT.mean <- mean(lb_high$RT)
lb.high.RT.sd <- sd(lb_high.RT.mean)

lb_high.correct.percent <- mean(lb_high$Correct)
lb_high.correct.abs <- lb_high[lb_high$Correct == 1, ]

b_high <- literal_bleaching[literal_bleaching$Type=="bleaching" & literal_bleaching$freq> 500, ]
b_high.RT.mean <- mean(lb_high$RT)
b.high.RT.sd <- sd(lb_high.RT.mean)

b_high.correct.percent <- mean(b_high$Correct)
b_high.correct.abs <- b_high[b_high$Correct == 1, ]

lb_low <- literal_bleaching[literal_bleaching$Type=="literal_b" & literal_bleaching$freq< 500, ]
lb_low.RT.mean <- mean(lb_low$RT)
lb.low.RT.sd <- sd(lb_low.RT.mean)

lb_low.correct.percent <- mean(lb_low$Correct)
lb_low.correct.abs <- lb_low[lb_low$Correct == 1, ]

b_low <- literal_bleaching[literal_bleaching$Type=="bleaching" & literal_bleaching$freq< 500, ]
b_low.RT.mean <- mean(b_low$RT)
b.low.RT.sd <- sd(b_low.RT.mean)

b_low.correct.percent <- mean(b_low$Correct)
b_low.correct.abs <- b_low[b_low$Correct == 1, ]

# Буквальное значение - Метафора
lm_high <- literal_metaphor[literal_metaphor$Type=="literal_m" & literal_metaphor$freq> 500, ]
lm_high.RT.mean <- mean(lm_high$RT)
lm.high.RT.sd <- sd(lm_high.RT.mean)

lm_high.correct.percent <- mean(lm_high$Correct)
lm_high.correct.abs <- lm_high[lm_high$Correct == 1, ]

m_high <- literal_metaphor[literal_metaphor$Type=="bleaching" & literal_metaphor$freq> 500, ]
m_high.RT.mean <- mean(m_high$RT)
m.high.RT.sd <- sd(m_high.RT.mean)

m_high.correct.percent <- mean(m_high$Correct)
m_high.correct.abs <- m_high[m_high$Correct == 1, ]

lm_low <- literal_metaphor[literal_metaphor$Type=="literal_b" & literal_metaphor$freq< 500, ]
lm_low.RT.mean <- mean(lm_low$RT)
lm.low.RT.sd <- sd(lm_low.RT.mean)

lm_low.correct.percent <- mean(lm_low$Correct)
lm_low.correct.abs <- lm_low[lm_low$Correct == 1, ]

m_low <- literal_metaphor[literal_metaphor$Type=="bleaching" & literal_metaphor$freq< 500, ]
m_low.RT.mean <- mean(m_low$RT)
m.low.RT.sd <- sd(m_low.RT.mean)

m_low.correct.percent <- mean(m_low$Correct)
m_low.correct.abs <- m_low[m_low$Correct == 1, ]

### Анализ ###
literal_metaphor$Type <- as.factor(literal_metaphor$Type)
literal_metaphor$Type <- relevel(literal_metaphor$Type, ref = "literal_m")

literal_metaphor$Freq_Type <- ifelse(literal_metaphor$freq>500, 'high', 'low')
literal_metaphor$Freq_Type <- as.factor(literal_metaphor$Freq_Type)
literal_metaphor$Freq_Type <- relevel(literal_metaphor$Freq_Type, ref = "high")
contrasts(literal_metaphor$Freq_Type) <- contr.sum(2)
levels(literal_metaphor$Freq_Type)
levels(literal_metaphor$Type)

literal_metaphor$id <- as.factor(literal_metaphor$id)
literal_metaphor$Item <- as.factor(literal_metaphor$Item)

library(lme4)
library(lmerTest)

lime1 <- lmer(log_RT ~ Freq_Type/Type + 
       (1|id) + (1|Item), 
     data = literal_metaphor, REML = FALSE,
     control = lmerControl(optimizer = "bobyqa"))
qqnorm(residuals(lime1))
summary(lime1)

lime1_correct <- glmer(Correct ~ Freq_Type/Type + 
                (1|id) + (1|Item), 
              data = literal_metaphor, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
summary(lime1_correct)


literal_bleaching$Type <- as.factor(literal_bleaching$Type)
literal_bleaching$Type <- relevel(literal_bleaching$Type, ref = "literal_b")

literal_bleaching$Freq_Type <- ifelse(literal_bleaching$freq>500, 'high', 'low')
literal_bleaching$Freq_Type <- as.factor(literal_bleaching$Freq_Type)
contrasts(literal_bleaching$Freq_Type) <- contr.sum(2)
levels(literal_bleaching$Freq_Type)
levels(literal_bleaching$Type)

literal_bleaching$id <- as.factor(literal_bleaching$id)
literal_bleaching$Item <- as.factor(literal_bleaching$Item)

lime2 <- lmer(log_RT ~ Freq_Type/Type + 
                (1|id) + (1|Item), 
              data = literal_bleaching, REML = FALSE,
              control = lmerControl(optimizer = "bobyqa"))


qqnorm(residuals(lime2))
summary(lime2)

lime2_correct <- glmer(Correct ~ Freq_Type/Type + 
                         (1|id) + (1|Item), 
                       data = literal_bleaching, family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
summary(lime2_correct)
