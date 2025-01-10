load(tidyverse)

exp1_order <- read.csv('data/exp1_order_clean.csv')
exp1_order <- exp1_order %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 1,
         test = 'order memory',
         response = gsub('.png', '', response))

exp2_order <- read.csv('data/exp2_order_clean.csv')
exp2_order <- exp2_order %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 2,
         test = 'order memory',
         response = gsub('.png', '', response))

exp3_order <- read.csv('data/exp3_order_clean.csv')
exp3_order <- exp3_order %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 3,
         test = 'order memory',
         response = gsub('.png', '', response))

exp1_spatial <- read.csv('data/exp1_spatial_clean.csv')
exp1_spatial <- exp1_spatial %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 1,
         test = 'spatial memory',
         response = gsub('.png', '', response))

exp2_spatial <- read.csv('data/exp2_spatial_clean.csv')
exp2_spatial <- exp2_spatial %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 2,
         test = 'spatial memory',
         response = gsub('.png', '', response))

exp3_recog <- read.csv('data/exp3_recog_clean.csv')
exp3_recog <- exp3_recog %>%
  select(id, list, event, seq_pos, condition, response, rt, accuracy) %>%
  mutate(experiment = 3,
         test = 'item memory',
         response = gsub('.png', '', response))

memory_data <- rbind(exp1_order, exp2_order, exp3_order, exp1_spatial, exp2_spatial, exp3_recog)

memory_data <- memory_data %>%
  mutate(condition = ifelse(condition == 'pred', 'predictable', 'variable'),
         rt = round(rt, digits = 2),
         chance = ifelse(test == 'order memory', 1/6,
                         ifelse(test == 'spatial memory', 1/4, 1/2))) %>%
  rename(block = list, subject = id, position = seq_pos)

write.csv(memory_data, file = 'data/all_memory_data.csv', row.names = F)
