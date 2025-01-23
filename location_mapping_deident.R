# Location mapping


library(tidyverse)
love_science_data <- read_csv("data/2024/burningman_2024/love-science-pre-event-survey-burning-man-2024-10.csv") %>% 
  filter(!(lastName == "Glaser")) %>% 
  mutate(id = str_c(firstName, lastName, sep = " "))
cluster_data <- read_csv("data/2024/burningman_2024/KM7_cluster_4_regret_6-9_eval_64-59_skew_g_64_sz_253.csv") %>% 
  filter(!(id == "Jim Glaser")) %>% 
  mutate(color = case_match(label,
                            0 ~ "red",
                            1 ~ "orange",
                            2 ~ "blue",
                            3 ~ "pink",
                            4 ~ "yellow",
                            5 ~ "white",
                            6 ~ "green"
  ))


location_var <- colnames(ls_data_final_set)[707:721]

ls_data_deident_location <- inner_join(love_science_data, cluster_data, by = "id") %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(deidentified_id = row_number()) %>% 
  select(deidentified_id, wantsDates, KidsTimeline, monogamousVsPolyamorousRelationships, location_var)
  

ls_data_deident_location %>% 
    ggplot(aes(x = `Which region? (4rkl46f)`)) + geom_bar() + theme(axis.text.x = element_text(angle = 30))
# filter(!(country == "NA")) %>% ggplot(aes(x = country)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#  View()
nrow()

# write_csv(ls_data_deident_location, "data/2024/burningman_2024/deidentified_location_bm24.csv")

summary_country <- ls_data_deident_location %>%
 # group_by(`Which region? (4rkl46f)`) %>% 
  group_by(country) %>%
  summarize(n = n()) %>% 
  arrange(desc(n)) 

ls_data_deident_location %>% 
  ggplot(aes(x = KidsTimeline)) + geom_bar()
