### Week 6 ###


tuesdata <- tidytuesdayR::tt_load(2025, week = 6)

cdc_datasets <- tuesdata$cdc_datasets
fpi_codes <- tuesdata$fpi_codes
omb_codes <- tuesdata$omb_codes

library(tidyverse)

cdc_datasets_long <- cdc_datasets %>%
  separate_rows(tags, sep = ",") %>% 
  mutate(tags = str_trim(tags, side = "left"))

# Count the occurrences of each tag
tag_counts <- cdc_datasets_long %>%
  count(tags, sort = TRUE)

print(tag_counts)



cdc_datasets_clean <- cdc_datasets %>%
  mutate(issued = parse_date_time(issued, orders = c("ymd", "dmy", "B d, Y"))) %>%
  mutate(issued = format(issued, "%m/%d/%Y"))


ggplot(cdc_datasets_clean %>% 
         drop_na(issued), aes(x=issued))+geom_bar()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  