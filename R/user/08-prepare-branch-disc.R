

tmp <- list()


## Steps
## - group Quarters
## - make average per subbranch

tmp$bb_disc <- data_init$bb_disk |>
  distinct(
    updated_tree_code, disc_no1, main_branch_id, subbranch_id, disc_no, disc_id, 
    bb_disc_wood_od = branch_od_wt,
    bb_disc_bark_od = bark_OD,
    bb_disc_vob = volume_cc_OB,
    bb_disc_vub = volume_cc_UB,
    is_quarter  = rk2
    ) 

## Check duplicates
# nrow(data_init$bb_disk) == nrow(tmp$bb_disc)
# data_init$bb_disk |> 
#   summarise(count = n(), .by = c(updated_tree_code, disc_no1, main_branch_id, subbbranch_id, disc_no, disc_id, rk2)) |>
#   filter(count > 1)

## Check values
tmp$bb_disc |>
  ggplot(aes(x = bb_disc_vub, y = bb_disc_wood_od)) +
  geom_point()


tmp$quarters <- tmp$bb_disc |>
  filter(is_quarter %in% c("Q1", "Q3")) |>
  group_by(updated_tree_code, disc_no1, main_branch_id, subbranch_id, disc_no, disc_id) |>
  summarise(
    bb_disc_wood_od = sum(bb_disc_wood_od),
    bb_disc_bark_od = sum(bb_disc_bark_od),
    bb_disc_vob = sum(bb_disc_vob),
    bb_disc_vub = sum(bb_disc_vub),
    .groups = "drop"
  )

tmp$single <- tmp$bb_disc |>
  filter(!is_quarter %in% c("Q1", "Q3"))

bb_disc <- tmp$single |>
  bind_rows(tmp$quarters) |>
  mutate(
    bb_disc_wd     = bb_disc_wood_od / bb_disc_vub,
    bb_disc_wdbark = bb_disc_bark_od / (bb_disc_vob - bb_disc_vub)
  ) |>
  mutate(
    species_code = str_sub(updated_tree_code, 4, 5)
  )

bb_disc |>
  #filter(bb_disc_vub < 500) |> 
  ggplot(aes(x = bb_disc_vub, y = bb_disc_wd)) +
  geom_point()
 

## Make averages ####
tmp$bb_wdtree <- bb_disc |>
  group_by(updated_tree_code) |>
  summarise(
    bb_log_mean_wdtree = mean(bb_disc_wd, na.rm = T),
    bb_log_mean_wdtree_bark = mean(bb_disc_wdbark, na.rm = T),
    .groups = "drop"
  )

tmp$bb_wdsp <- bb_disc |>
  group_by(species_code) |>
  summarise(
    bb_log_mean_wdsp = mean(bb_disc_wd, na.rm = T),
    bb_log_mean_wdsp_bark = mean(bb_disc_wdbark, na.rm = T),
    .groups = "drop"
  )

bb_wd <- bb_disc |>
  group_by(updated_tree_code, subbranch_id, species_code) |>
  summarise(
    bb_log_mean_wd = mean(bb_disc_wd, na.rm = T),
    bb_log_mean_wdbark = mean(bb_disc_wdbark, na.rm = T),
    .groups = "drop"
  ) |>
  left_join(tmp$bb_wdtree, by = join_by(updated_tree_code)) |>
  left_join(tmp$bb_wdsp, by = join_by(species_code)) |>
  mutate(
    bb_log_wd = case_when(
      !is.na(bb_log_mean_wd) ~ bb_log_mean_wd,
      !is.na(bb_log_mean_wdtree) ~ bb_log_mean_wdtree,
      TRUE ~ bb_log_mean_wdsp
      ),
    bb_log_wdbark = case_when(
      !is.na(bb_log_mean_wdbark) ~ bb_log_mean_wdbark,
      !is.na(bb_log_mean_wdtree_bark) ~ bb_log_mean_wdtree_bark,
      TRUE ~ bb_log_mean_wdsp_bark
    ),
    bb_log_wd_status = case_when(
      !is.na(bb_log_mean_wd) ~ "same branch",
      !is.na(bb_log_mean_wdtree) ~ "all branches tree",
      TRUE ~ "all branches species"
    ),
    bb_log_wdbark_status = case_when(
      !is.na(bb_log_mean_wdbark) ~"same branch",
      !is.na(bb_log_mean_wdtree_bark) ~ "all branches tree",
      TRUE ~ "all branches species"
    )
  )

nrow(tmp$single) + nrow(tmp$quarters) * 2 == nrow(tmp$bb_disc)

summary(bb_wd$bb_log_mean_wd)
summary(bb_wd$bb_log_wd)


## assign to clean data ####

data_clean$bb_wd <- bb_wd |> 
  select(
    updated_tree_code, 
    bb_section_sub_id = subbranch_id,
    bb_log_wd,
    bb_log_wd_status,
    bb_log_wdbark,
    bb_log_wdbark_status
    )

data_clean$bb_wdsp <- tmp$bb_wdsp
