

if (is.null(data_clean)) stop("Run 'prepare-stem-profile.R' first to get the clean stem data")


tmp <- list()

## !!! Update clean stems by removing buttress Diam measurement !!!
data_clean$stem <- data_clean$stem |>
  mutate(
    log_diam_ob = case_when(
      updated_tree_code == "227Lp036" & log_no == 1 ~ 21.8,
      updated_tree_code == "361Cs054" & log_no == 1 ~ 12.3,
      updated_tree_code == "460An054" & log_no == 1 ~ 15.8,
      TRUE ~ log_diam_ob
    )
  )

tmp$log_base <- data_clean$stem |>
  select(
    updated_tree_code, log_no, 
    log_base_pom = log_base_pom, 
    log_base_diam_ob = log_diam_ob, 
    log_base_diam_ub = log_diam_ub
    )

tmp$tree_info <- data_clean$stem |>
  select(-starts_with("log_"), -measurement_type, -hr, -dr) |>
  distinct()

data_clean$stem_v <- data_clean$stem |>
  select(
    updated_tree_code, log_no, 
    log_top_pom = log_base_pom, 
    log_top_diam_ob = log_diam_ob, 
    log_top_diam_ub = log_diam_ub
    ) |>
  group_by(updated_tree_code) |>
  mutate(
    log_no_v = log_no - 1,
    log_no = if_else(log_no == 1, 1, log_no - 1)
    ) |>
  ungroup() |>
  left_join(tmp$log_base, by = join_by(updated_tree_code, log_no)) |>
  mutate(
    log_base_pom = if_else(log_no_v == 0, 0, log_base_pom),
    log_length = log_top_pom - log_base_pom,
    log_v = round(pi / 80000 * log_length * (log_base_diam_ob^2 + log_top_diam_ob^2), 4)
  ) |>
  select(updated_tree_code, log_no_v, log_length, log_v, starts_with("log_base"), starts_with("log_top"))

# summary(data_clean$stem_v)
# tmp$check <- data_clean$stem_v |> filter(is.na(log_base_pom))
# tt <- tmp$check2 <- data_clean$stem_v |> filter(updated_tree_code == "227Lp036")
# tt <- tmp$check2 <- tmp$log_base |> filter(updated_tree_code == "227Lp036")

tmp$tree_stem_v <- data_clean$stem_v |>
  group_by(updated_tree_code) |>
  summarise(
    n_log_total = n(),
    log_top_diam = min(log_base_diam_ob, na.rm = T),
    tree_stem_v = sum(log_v, na.rm = T)
    )

tmp$tree_stem_v10 <- data_clean$stem_v |>
  filter(log_top_diam_ob >= 10) |>
  group_by(updated_tree_code) |>
  summarise(
    n_log10 = n(),
    log_top_diam10 = min(log_top_diam_ob, na.rm = T),
    tree_stem_v10 = sum(log_v, na.rm = T)
  )

tmp$tree_stem_v20 <- data_clean$stem_v |>
  filter(log_top_diam_ob >= 20) |>
  group_by(updated_tree_code) |>
  summarise(
    n_log20 = n(),
    log_top_diam20 = min(log_top_diam_ob, na.rm = T),
    tree_stem_v20 = sum(log_v, na.rm = T)
  )

data_clean$tree_stem_v <- tmp$tree_info |>
  left_join(tmp$tree_stem_v, by = join_by(updated_tree_code)) |>
  left_join(tmp$tree_stem_v10, by = join_by(updated_tree_code)) |>
  left_join(tmp$tree_stem_v20, by = join_by(updated_tree_code)) |>
  mutate(tree_d2h = (tree_dbh/100)^2 * tree_total_length)

## Checks 
summary(data_clean$tree_stem_v)
# summary(data_clean$tree_stem_v$log_top_diam20)  
# summary(data_clean$tree_stem_v$log_top_diam10)  
# 
# tmp$check <- data_clean$tree_stem_v |> filter(log_top_diam20 > 25) |>
#   pull(updated_tree_code) |>
#   unique()

data_clean$tree_stem_v |>
  filter(tree_dbh < 70) |>
  ggplot(aes(x = tree_dbh, color = species_name)) +
  geom_point(aes(y = tree_stem_v), shape = 1) +
  geom_point(aes(y = tree_stem_v20), shape = 4) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~species_name)

data_clean$tree_stem_v |>
  ggplot(aes(x = tree_d2h, color = species_name)) +
  geom_point(aes(y = tree_stem_v), shape = 1) +
  geom_point(aes(y = tree_stem_v20), shape = 4) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~species_name)


tmp$outlier <- data_clean$tree_stem_v |>
  filter(tree_d2h > 40, tree_stem_v < 10)
tmp$outlier2 <- data_clean$tree_stem_v |>
  filter(tree_dbh > 110)

data_clean_gg$check_v <- data_clean$tree_stem_v |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v, color = species_name)) +
  geom_point() +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outlier2, shape = 21, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier, aes(label = updated_tree_code)) +
  theme(legend.position = "bottom") +
  labs(color = "")

print(data_clean_gg$check_v)
# ggsave(
#   plot = data_clean_gg$check_v, paste0("res/cleaning-examples/check_v-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )

## !!! Remove trees with large early branches
data_clean$tree_stem_v <- data_clean$tree_stem_v |>
  filter(!updated_tree_code %in% "551Pr100")


tmp$outlier3 <- data_clean$tree_stem_v |>
  filter(updated_tree_code == "186Sr045") 

data_clean$tree_stem_v |>
  filter(tree_dbh < 40) |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v, color = species_name)) +
  geom_point() +
  geom_point(data = tmp$outlier3, shape = 21, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier3, aes(label = updated_tree_code)) +
  theme(legend.position = "bottom") +
  labs(color = "")


#rm(tmp)