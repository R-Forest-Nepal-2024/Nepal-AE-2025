
if (!"usr" %in% ls())      source("R/user/user-inputs.R")
if (!"nlme_out" %in% ls()) source("R/setup/init.R")
if (length(data_init) == 0) source("R/setup/get-data.R")

ls_stem_log_scripts <- list.files("R/user", pattern = "stem-log", full.names = T)
if (!("stem_log" %in% names(data_clean))) walk(ls_stem_log_scripts, source)

tmp <- list()


tmp$tree_info <- data_clean$stem_log |>
  select(-starts_with("log_"), -hr, -dr) |>
  distinct()

tmp$tree_stem_v <- data_clean$stem_log |>
  group_by(updated_tree_code) |>
  summarise(
    n_log_total = n(),
    log_top_diam = min(log_base_diam_ob, na.rm = T),
    tree_stem_v = sum(log_v, na.rm = T)
  )

tmp$tree_stem_v10 <- data_clean$stem_log |>
  filter(log_top_diam_ob >= 10) |>
  group_by(updated_tree_code) |>
  summarise(
    n_log10 = n(),
    log_top_diam10 = min(log_top_diam_ob, na.rm = T),
    tree_stem_v10 = sum(log_v, na.rm = T)
  )

tmp$tree_stem_v20 <- data_clean$stem_log |>
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

gg_stemV_cleaning <- data_clean$tree_stem_v |>
  filter(tree_dbh < 40) |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v, color = species_name)) +
  geom_point() +
  geom_point(data = tmp$outlier3, shape = 21, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier3, aes(label = updated_tree_code)) +
  theme(legend.position = "bottom") +
  labs(color = "")

print(gg_stemV_cleaning)


## Check
tmp$outliers <- data_clean$tree_stem_v |>
  filter(
    (species_name == "Largestroemia parviflora" & tree_dbh > 60) |
      (species_name == "Schima Wallichi" & tree_dbh > 60) |
      (species_name == "Shorea robusta" & tree_dbh > 110) |
      is.na(species_name)
  ) 

data_clean$tree_stem_v |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point(aes(color = species_name), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 21, col = "red", size = 6) +
  theme(legend.position = "none") +
  facet_wrap(~species_group)


data_clean$tree_stem_v |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point(aes(color = species_name), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 21, col = "red", size = 6) +
  theme(legend.position = "none")

