
tmp <- list()

bb_logb <- data_clean$bb_log |>
  mutate(
    species_code = str_sub(updated_tree_code, 4, 5)
  ) |>
  left_join(data_clean$bb_wd, by = join_by(updated_tree_code, bb_section_sub_id)) |>
  left_join(data_clean$bb_wdsp, by = join_by(species_code)) |>
  mutate(
    bb_log_wd_corr = if_else(is.na(bb_log_wd), bb_log_mean_wdsp, bb_log_wd),
    bb_log_wdbark_corr = if_else(is.na(bb_log_wdbark), bb_log_mean_wdsp_bark, bb_log_wdbark),
    bb_logb_wood = bb_log_vub * bb_log_wd_corr * 1000,
    bb_logb_bark = bb_log_vbark * bb_log_wdbark_corr * 1000,
    bb_logb = if_else(is.na(bb_log_vub), bb_log_vob * bb_log_wd_corr, bb_logb_wood + bb_logb_bark)
  )
  
summary(bb_logb$bb_logb)
summary(bb_logb$bb_log_vub)
summary(bb_logb$bb_log_vob)
summary(bb_logb$bb_log_wd_corr)

# tmp$missing <- bb_logb |> filter(is.na(bb_logb))
# tmp$missing

data_clean$bb_log <- bb_logb

