

if (is.null(data_clean)) stop("Run 'prepare-stem-profile.R' first to get the clean stem data")
if (!"tree_stem_v" %in% names(data_clean)) stop("Run 'prepare-stem-volume.R' first to get tree volume data")

tmp <- list()


## Group field measurements of Quarters
table(data_init$lab_b$disc_quarter, useNA = "ifany")

tmp$quarters <- data_init$lab_b |>
  filter(disc_quarter %in% c("Q1", "Q3")) |>
  distinct(updated_tree_code, disc_no, disc_quarter, .keep_all = T) |>
  group_by(updated_tree_code, disc_no) |>
  summarise(
    fresh_wt_ob = sum(fresh_wt_QOB),
    fresh_wt_ub = sum(fresh_wt_QUB),
    vol_ob = sum(volume_cc.OB.),
    vol_ub = sum(volume_cc.UB.),
    fresh_wt_bark = sum(net_wt_bark.gm.),
    lab_airdry = mean(lab_airdry),
    lab_ovendry = mean(lab_ovendry),
    .groups = "drop"
  )

## Add quarters
data_clean$lab_disc <- data_init$lab_b |>
  filter(!disc_quarter %in% c("Q1", "Q3")) |>
  distinct(updated_tree_code, disc_no, .keep_all = T) |>
  select(
    updated_tree_code, disc_no, fresh_wt_OB, fresh_wt_UB, 
    vol_ob = volume_cc.OB., vol_ub = volume_cc.UB., 
    lab_airdry, lab_ovendry
    ) |>
  rename_with(str_to_lower) |>
  bind_rows(tmp$quarters) |>
  mutate(
    fd_oven = round(lab_ovendry / fresh_wt_ub, 4),
    wd = round(lab_ovendry / vol_ob, 4)
  ) |>
  distinct(updated_tree_code, disc_no, .keep_all = T) |>
  mutate(flag_wd = if_else(wd < 0.2 | wd > 0.8, 1, 0)) |>
  filter(flag_wd == 0)


## Get average WD and FD for each tree
data_clean$lab_tree <- data_clean$lab_disc |>
  mutate(count = 1, count_noerr = if_else(is.na(wd), NA_integer_, 1)) |>
  group_by(updated_tree_code) |>
  summarise(
    count = sum(count),
    count_noerr = sum(count_noerr, na.rm = T),
    fd_oven_mean = mean(fd_oven, na.rm = T),
    wd_mean = mean(wd, na.rm = T)
  )

data_clean$lab_tree

## Check lab aggregation
nrow(data_clean$lab_tree)
nrow(filter(data_clean$lab_tree, !is.na(wd_mean)))
sum(data_clean$lab_tree$count)
sum(data_clean$lab_tree$count_noerr)


## Add disc no to stem data
tmp$stem_b <- data_clean$stem_v |>
  mutate(
    disc_num = ceiling((log_base_pom - 3.3)/2 + 3),
    disc_no = case_when(
      log_base_pom < 1.3 ~ "S01",
      log_base_pom < 3.3 ~ "S02",
      disc_num <  10 ~ paste0("S0", disc_num),
      disc_num >= 10 ~ paste0("S", disc_num),
      TRUE ~ NA_character_
    )
  )

table(tmp$stem_b$disc_no, useNA = "ifany")
summary(tmp$stem_b$log_base_pom)


## Get tree mean WD ready for join
tmp$lab_tree <- data_clean$lab_tree |>
  filter(!is.na(wd_mean)) |>
  select(updated_tree_code, fd_oven_mean, wd_mean)

tmp$check <- tmp$lab_tree |> distinct(updated_tree_code, .keep_all = T)


## Get lab data ready for join
tmp$lab_disc <- data_clean$lab_disc |>
  filter(!is.na(wd)) |>
  select(updated_tree_code, disc_no, fd_oven, wd)

tmp$check <- tmp$lab_disc |> distinct(updated_tree_code, disc_no,  .keep_all = T)

## Join lab data at disc level and mean data from tree to fill gaps
data_clean$stem_b <- tmp$stem_b |>
  left_join(tmp$lab_disc, by = join_by(updated_tree_code, disc_no)) |>
  left_join(tmp$lab_tree, by = join_by(updated_tree_code)) |>
  mutate(
    wd_corr = if_else(is.na(wd), wd_mean, wd),
    log_b = wd_corr * log_v 
  ) 

#summary(data_clean$stem_b)

tmp$tree_stem_b <- data_clean$stem_b |>
  summarise(tree_stem_b = sum(log_b, na.rm = T), .by = updated_tree_code)

data_clean$tree_stem_b <- data_clean$tree_stem_v |>
  left_join(tmp$tree_stem_b, by = join_by(updated_tree_code)) |>
  left_join(tmp$lab_tree, by = join_by(updated_tree_code))



## Check
data_clean_gg$check_b <- data_clean$tree_stem_b |>
  filter(tree_stem_b != 0, tree_dbh <= 40) |>
  ggplot(aes(x = tree_dbh, y = tree_stem_b)) +
  geom_point(aes(color = species_name), shape = 21) +
  facet_wrap(~species_group) +
  theme(legend.position = "none")

print(data_clean_gg$check_b)

data_clean_gg$check_b <- data_clean$tree_stem_b |>
  filter(tree_stem_b != 0, tree_dbh <= 110) |>
  filter(
    !(species_name == "Largestroemia parviflora" & tree_dbh > 60),
    !(species_name == "Schima Wallichi" & tree_dbh > 60),
    !(species_name == "Shorea robusta" & tree_dbh > 110)
  ) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_b)) +
  geom_point(aes(color = species_name)) +
  #facet_wrap(~species_group, scales = "free") +
  facet_wrap(~species_group) +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")

print(data_clean_gg$check_b)


