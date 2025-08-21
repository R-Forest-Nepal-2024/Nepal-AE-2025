
tmp <- list()

tmp$bb_section <- data_init$bbranch |>
  select(
    updated_tree_code,
    bb_section_main_id = main_branch_id,
    bb_section_sub_id  = subbranch_id,
    bb_section_position = length.interval_update,
    bb_section_position_no = branch_section_position,
    bb_section_diam_ob = dia_OB,
    bb_section_diam_ub = dia_UB
    ) |>
  mutate(
    bb_section_position = as.numeric(if_else(bb_section_position == "base", "0", bb_section_position)),
    bb_section_position = if_else(is.na(bb_section_position), bb_section_position_no, bb_section_position),
    bb_section_id = if_else(
      bb_section_position_no < 10, 
      paste0(bb_section_sub_id, "-0", bb_section_position_no),
      paste0(bb_section_sub_id, "0" ,bb_section_position_no),
    ),
    bb_section_diam_ob = as.numeric(bb_section_diam_ob),
    bb_section_diam_ub = as.numeric(bb_section_diam_ub)
  ) |> 
  filter(!is.na(bb_section_diam_ob))

summary(tmp$bb_section$bb_section_position)
summary(tmp$bb_section$bb_section_diam_ob)
summary(tmp$bb_section$bb_section_diam_ub)

## Check unique IDs for branch/sub-branch sections
# nrow(data_init$bbranch)
# tmp$bb_section |> distinct(updated_tree_code, bb_section_sub_id, bb_section_position_no) |> nrow()
# 
# tt <- tmp$bb_section |>
#   group_by(updated_tree_code, bb_section_sub_id, bb_section_position_no) |>
#   summarise(count = n(), .groups = "drop") |>
#   filter(count > 1)
# tt


tmp$log_top <- tmp$bb_section |>
  select(
    updated_tree_code, bb_section_sub_id, bb_section_position_no,
    bb_log_top_position = bb_section_position,
    bb_log_top_diam_ob = bb_section_diam_ob,
    bb_log_top_diam_ub = bb_section_diam_ub
  ) |>
  mutate(bb_section_position_no = bb_section_position_no - 1)

bb_log <- tmp$bb_section |>
  rename(
    bb_log_base_position = bb_section_position,
    bb_log_base_diam_ob = bb_section_diam_ob,
    bb_log_base_diam_ub = bb_section_diam_ub
  ) |>
  left_join(tmp$log_top, by = join_by(updated_tree_code, bb_section_sub_id, bb_section_position_no)) |>
  filter(!is.na(bb_log_top_position)) |>
  mutate(
    bb_log_length = bb_log_top_position - bb_log_base_position,
    bb_log_vob = round(pi / 80000 * bb_log_length * (bb_log_base_diam_ob^2 + bb_log_top_diam_ob^2), 4),
    bb_log_vub = round(pi / 80000 * bb_log_length * (bb_log_base_diam_ub^2 + bb_log_top_diam_ub^2), 4),
    bb_log_vbark = bb_log_vob - bb_log_vub
    )

summary(bb_log$bb_log_length)
summary(bb_log$bb_log_vob)
summary(bb_log$bb_log_vub)

## Check negative or too big log length
bb_log |>
  ggplot(aes(x = bb_log_base_diam_ob, y = bb_log_length)) +
  geom_point()
  
tt <- tmp$error <- bb_log |> filter(bb_log_length <= 0)
tt <- tmp$error <- bb_log |> filter(bb_log_length > 2.5)

bb_log |>
  ggplot(aes(x = bb_log_base_diam_ob, y = bb_log_vob/bb_log_length)) +
  geom_point()



## + assign to clean data
data_clean$bb_log <- bb_log







