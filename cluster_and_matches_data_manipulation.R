
# Import love science data and make CSVs to import to mailchimp to send out emails

# Import data
library(tidyverse)
cluster_data <- read_csv("data/2024/burningman_2024/KM7_cluster_4_regret_6-9_eval_64-59_skew_g_64_sz_253.csv") %>% 
  mutate(color = case_match(label,
                            0 ~ "red",
                            1 ~ "orange",
                            2 ~ "blue",
                            3 ~ "pink",
                            4 ~ "yellow",
                            5 ~ "white",
                            6 ~ "green"
  ))
love_science_data <- read_csv("data/2024/burningman_2024/love-science-pre-event-survey-burning-man-2024-10.csv") %>% 
  filter(!(lastName == "Glaser")) %>% 
  mutate(id = str_c(firstName, lastName, sep = " "))
# romantic_matches_data <- read_csv("data/2024/burningman_2024/bm_romantic_matches_long.csv") %>% 
#   mutate(
#     unique_id_1 = str_replace_all(unique_id_1, "Jim Glaser", "."),
#     unique_id_2 = str_replace_all(unique_id_2, "Jim Glaser", ".")
#   )
# platonic_matches_data <- read_csv("data/2024/burningman_2024/bm_platonic_matches_long.csv") %>% 
#   mutate(
#     unique_id_1 = str_replace_all(unique_id_1, "Jim Glaser", "."),
#     unique_id_2 = str_replace_all(unique_id_2, "Jim Glaser", ".")
#   )


rom_old <- read_csv("data/2024/burningman_2024/bm_romantic_matches_long.csv")
rom_new <- read_csv("data/2024/burningman_2024/bm_romantic_matches_long_new.csv")
rom_3 <- read_csv("data/2024/burningman_2024/bm_romantic_matches_long_3.csv")

plat_old <- read_csv("data/2024/burningman_2024/bm_platonic_matches_long.csv")
plat_new <- read_csv("data/2024/burningman_2024/bm_platonic_matches_long_new.csv")
plat_3 <- read_csv("data/2024/burningman_2024/bm_platonic_matches_long_3.csv")

romantic_matches_data <- rom_3
platonic_matches_data <- plat_3

all(rom_old$unique_id_1 %in% rom_new$unique_id_1)
all(plat_old$unique_id_1 %in% plat_new$unique_id_1)

all(rom_old$unique_id_1 %in% rom_3$unique_id_1)
all(plat_old$unique_id_1 %in% plat_3$unique_id_1)


# Combine
combined_data <- left_join(cluster_data, love_science_data, by = "id") %>% 
  distinct(id, .keep_all = TRUE)

# CLUSTER EMAIL

# Just the columns needed for cluster email and change to the color
cluster_selected <- combined_data %>% 
  select(id, label, theirEmail, firstName, lastName, `Time Finished (UTC)`, goingToBurningMan2024) %>% 
  mutate(color = case_match(label,
                            0 ~ "red",
                            1 ~ "orange",
                            2 ~ "blue",
                            3 ~ "pink",
                            4 ~ "yellow",
                            5 ~ "white",
                            6 ~ "green"
                            ))

write_csv(cluster_selected, "data/2024/burningman_2024/color_name_email.csv")

# MATCHES EMAIL

ls_data_select_matches <- combined_data %>% 
  mutate(id = str_c(firstName, lastName, sep = " ")) %>% 
  select(firstName, lastName, id, theirEmail, color, 
         wantsEmailIntroToMatches, wantsEmailIntroForDating, wantsEmailIntroForFriendship
         )

table(ls_data_select_matches$wantsEmailIntroToMatches)
table(ls_data_select_matches$wantsEmailIntroForDating)
table(ls_data_select_matches$wantsEmailIntroForFriendship)
table(ls_data_select_matches$color)
nrow(ls_data_select_matches)

# romantic_matches_data_wide <- romantic_matches_data %>% 
#   select(-c(`...1`)) %>% 
#   mutate(rank = paste0("romantic_", rank)) %>%
#   pivot_wider(names_from = "rank",
#               values_from = "unique_id_2")

# emails
df_people <- combined_data %>% 
  select(id, theirEmail, color) %>% 
  rename(name = id,
         email = theirEmail)

# romantic df to email
df_romantic_wide <- romantic_matches_data %>% 
  select(-c(`...1`)) %>% 
  left_join(df_people, by = c("unique_id_2" = "name")) %>% 
  mutate(
    romantic = paste0("romantic_", rank),
    rom_email = paste0("rom_email_", rank),
    rom_color = paste0("rom_color_", rank)
  ) %>%
  pivot_wider(
    names_from = c(romantic, rom_email, rom_color),
    values_from = c(unique_id_2, email, color),
    id_cols = "unique_id_1"
    # values_fn = list(
    #   unique_id_2 = ~paste(.x, collapse = ", "),
    #   email = ~paste(.x, collapse = ", ")
    # )
  ) %>% 
  rename(id = unique_id_1,
         romantic_name_1 = unique_id_2_romantic_1_rom_email_1_rom_color_1,
         romantic_name_2 = unique_id_2_romantic_2_rom_email_2_rom_color_2,
         romantic_name_3 = unique_id_2_romantic_3_rom_email_3_rom_color_3,
         romantic_name_4 = unique_id_2_romantic_4_rom_email_4_rom_color_4,
         romantic_name_5 = unique_id_2_romantic_5_rom_email_5_rom_color_5,
         romantic_email_1 = email_romantic_1_rom_email_1_rom_color_1,
         romantic_email_2 = email_romantic_2_rom_email_2_rom_color_2,
         romantic_email_3 = email_romantic_3_rom_email_3_rom_color_3,
         romantic_email_4 = email_romantic_4_rom_email_4_rom_color_4,
         romantic_email_5 = email_romantic_5_rom_email_5_rom_color_5,
         romantic_color_1 = color_romantic_1_rom_email_1_rom_color_1,
         romantic_color_2 = color_romantic_2_rom_email_2_rom_color_2,
         romantic_color_3 = color_romantic_3_rom_email_3_rom_color_3,
         romantic_color_4 = color_romantic_4_rom_email_4_rom_color_4,
         romantic_color_5 = color_romantic_5_rom_email_5_rom_color_5
         ) %>% 
  mutate(romantic_last_initial_1 = str_extract(romantic_name_1, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(romantic_last_initial_2 = str_extract(romantic_name_2, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(romantic_last_initial_3 = str_extract(romantic_name_3, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(romantic_last_initial_4 = str_extract(romantic_name_4, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(romantic_last_initial_5 = str_extract(romantic_name_5, "\\b\\w*$") %>% str_sub(1, 1))


# platonic df to email
df_platonic_wide <- platonic_matches_data %>% 
  select(-c(`...1`)) %>% 
  left_join(df_people, by = c("unique_id_2" = "name")) %>% 
  mutate(
    platonic = paste0("platonic_", rank),
    plat_email = paste0("plat_email_", rank),
    plat_color = paste0("plat_color_", rank)
  ) %>%
  pivot_wider(
    names_from = c(platonic, plat_email, plat_color),
    values_from = c(unique_id_2, email, color),
    id_cols = "unique_id_1"
    # values_fn = list(
    #   unique_id_2 = ~paste(.x, collapse = ", "),
    #   email = ~paste(.x, collapse = ", ")
    # )
  ) %>% 
  rename(id = unique_id_1,
         platonic_name_1 = unique_id_2_platonic_1_plat_email_1_plat_color_1,
         platonic_name_2 = unique_id_2_platonic_2_plat_email_2_plat_color_2,
         platonic_name_3 = unique_id_2_platonic_3_plat_email_3_plat_color_3,
         platonic_name_4 = unique_id_2_platonic_4_plat_email_4_plat_color_4,
         platonic_name_5 = unique_id_2_platonic_5_plat_email_5_plat_color_5,
         platonic_email_1 = email_platonic_1_plat_email_1_plat_color_1,
         platonic_email_2 = email_platonic_2_plat_email_2_plat_color_2,
         platonic_email_3 = email_platonic_3_plat_email_3_plat_color_3,
         platonic_email_4 = email_platonic_4_plat_email_4_plat_color_4,
         platonic_email_5 = email_platonic_5_plat_email_5_plat_color_5,
         platonic_color_1 = color_platonic_1_plat_email_1_plat_color_1,
         platonic_color_2 = color_platonic_2_plat_email_2_plat_color_2,
         platonic_color_3 = color_platonic_3_plat_email_3_plat_color_3,
         platonic_color_4 = color_platonic_4_plat_email_4_plat_color_4,
         platonic_color_5 = color_platonic_5_plat_email_5_plat_color_5
  ) %>% 
  mutate(platonic_last_initial_1 = str_extract(platonic_name_1, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(platonic_last_initial_2 = str_extract(platonic_name_2, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(platonic_last_initial_3 = str_extract(platonic_name_3, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(platonic_last_initial_4 = str_extract(platonic_name_4, "\\b\\w*$") %>% str_sub(1, 1)) %>% 
  mutate(platonic_last_initial_5 = str_extract(platonic_name_5, "\\b\\w*$") %>% str_sub(1, 1))

# # romantic df to email ---- original code no color
# df_romantic_wide <- romantic_matches_data %>% 
#   select(-c(`...1`)) %>% 
#   left_join(df_people, by = c("unique_id_2" = "name")) %>% 
#   mutate(
#     romantic = paste0("romantic_", rank),
#     rom_email = paste0("rom_email_", rank)
#   ) %>%
#   pivot_wider(
#     names_from = c(romantic, rom_email),
#     values_from = c(unique_id_2, email),
#     id_cols = "unique_id_1"
#     # values_fn = list(
#     #   unique_id_2 = ~paste(.x, collapse = ", "),
#     #   email = ~paste(.x, collapse = ", ")
#     # )
#   ) %>% 
#   rename(id = unique_id_1,
#          romantic_name_1 = unique_id_2_romantic_1_rom_email_1,
#          romantic_name_2 = unique_id_2_romantic_2_rom_email_2,
#          romantic_name_3 = unique_id_2_romantic_3_rom_email_3,
#          romantic_name_4 = unique_id_2_romantic_4_rom_email_4,
#          romantic_name_5 = unique_id_2_romantic_5_rom_email_5,
#          romantic_email_1 = email_romantic_1_rom_email_1,
#          romantic_email_2 = email_romantic_2_rom_email_2,
#          romantic_email_3 = email_romantic_3_rom_email_3,
#          romantic_email_4 = email_romantic_4_rom_email_4,
#          romantic_email_5 = email_romantic_5_rom_email_5) 
# 
# # platonic df to email
# df_platonic_wide <- platonic_matches_data %>% 
#   select(-c(`...1`)) %>% 
#   left_join(df_people, by = c("unique_id_2" = "name")) %>% 
#   mutate(
#     platonic = paste0("platonic_", rank),
#     rom_email = paste0("rom_email_", rank)
#   ) %>%
#   pivot_wider(
#     names_from = c(platonic, rom_email),
#     values_from = c(unique_id_2, email),
#     id_cols = "unique_id_1"
#     # values_fn = list(
#     #   unique_id_2 = ~paste(.x, collapse = ", "),
#     #   email = ~paste(.x, collapse = ", ")
#     # )
#   ) %>% 
#   rename(id = unique_id_1,
#          platonic_name_1 = unique_id_2_platonic_1_rom_email_1,
#          platonic_name_2 = unique_id_2_platonic_2_rom_email_2,
#          platonic_name_3 = unique_id_2_platonic_3_rom_email_3,
#          platonic_name_4 = unique_id_2_platonic_4_rom_email_4,
#          platonic_name_5 = unique_id_2_platonic_5_rom_email_5,
#          platonic_email_1 = email_platonic_1_rom_email_1,
#          platonic_email_2 = email_platonic_2_rom_email_2,
#          platonic_email_3 = email_platonic_3_rom_email_3,
#          platonic_email_4 = email_platonic_4_rom_email_4,
#          platonic_email_5 = email_platonic_5_rom_email_5) 

# ls_data_select_matches
# df_romantic_wide
# df_platonic_wide

# romantic_sample <- ls_data_select_matches  %>% select(c(id, color)) %>% 
#   left_join(df_romantic_wide, by = "id") %>% 
#   select(c(color, romantic_color_1, romantic_color_2, romantic_color_3, romantic_color_4, romantic_color_5))
# 
# platonic_sample <- ls_data_select_matches  %>% select(c(id, color)) %>% 
#   left_join(df_platonic_wide, by = "id") %>% 
#   select(c(color, platonic_color_1, platonic_color_2, platonic_color_3, platonic_color_4, platonic_color_5))
# 

df_matches_to_export <- ls_data_select_matches %>% 
  left_join(df_romantic_wide, by = "id") %>% 
  left_join(df_platonic_wide, by = "id")

romantic_people <- romantic_matches_data %>% pull(unique_id_1) %>% unique()
platonic_people <- platonic_matches_data %>% pull(unique_id_1) %>% unique()

contact_people <- union(romantic_people, platonic_people)
length(contact_people)

contact_romantic_only <- setdiff(romantic_people, platonic_people)
contact_platonic_only <- setdiff(platonic_people, romantic_people)
contact_romantic_and_platonic <- intersect(romantic_people, platonic_people)

df_matches_contact_romantic_only <- df_matches_to_export %>% 
  filter(id %in% contact_romantic_only)

df_matches_contact_platonic_only <- df_matches_to_export %>% 
  filter(id %in% contact_platonic_only)

df_matches_contact_both <- df_matches_to_export %>% 
  filter(id %in% contact_romantic_and_platonic)

write_csv(df_matches_contact_romantic_only, "data/2024/burningman_2024/df_matches_contact_romantic_only.csv")
write_csv(df_matches_contact_platonic_only, "data/2024/burningman_2024/df_matches_contact_platonic_only.csv")
write_csv(df_matches_contact_both, "data/2024/burningman_2024/df_matches_contact_both.csv")

cluster_export <- cluster_selected %>% 
  select(firstName, lastName, theirEmail, color)

write_csv(cluster_export, "data/2024/burningman_2024/bm_2024_cluster_colors.csv")
