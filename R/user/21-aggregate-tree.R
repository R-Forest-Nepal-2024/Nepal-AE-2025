

tmp <- list()
save_csv <- F

##
## Make tree ####
##

## + Tmp objects ####
tmp$plot <- data_init$plot |>
  select(
    updated_tree_code, plot_physio = physiographic_region, plot_prov = province,
    plot_dist = district, plot_alt = altitude, plot_ftype = forest_type
  )

tmp$tree_wd <- data_clean$stem_disc |>
  distinct(updated_tree_code, tree_wd = wd_wood_meantree, tree_wdsp = wd_wood_meansp)

tmp$tree_length <- data_clean$stem_log |>
  distinct(updated_tree_code, tree_total_length)

tmp$agg_stem <- data_clean$stem_log |>
  group_by(updated_tree_code) |>
  summarise(
    log_ntotal = n(),
    log_top_diam = min(log_base_diam_ob, na.rm = T),
    tree_stem_v = sum(log_vob, na.rm = T),
    tree_stem_b = sum(log_b, na.tm = T)
  )

tmp$agg_stem10 <- data_clean$stem_log |>
  filter(log_top_diam_ob >= 10) |>
  group_by(updated_tree_code) |>
  summarise(
    log_n10 = n(),
    log_top_diam10 = min(log_top_diam_ob, na.rm = T),
    tree_stem_v10 = sum(log_vob, na.rm = T)
  )

tmp$agg_stem20 <- data_clean$stem_log |>
  filter(log_top_diam_ob >= 20) |>
  group_by(updated_tree_code) |>
  summarise(
    log_n20 = n(),
    log_top_diam20 = min(log_top_diam_ob, na.rm = T),
    tree_stem_v20 = sum(log_vob, na.rm = T)
  )

## + Combine into tree ####
tree <- data_init$tree |>
  select(-tree_code, -new_tree_code, -starts_with("photo")) |>
  left_join(tmp$plot, by = join_by(updated_tree_code)) |>
  mutate(
    tree_total_length = NA,
    n_log_total       = NA,
    log_top_diam      = NA,
    tree_stem_v       = NA,
    tree_stem_b       = NA,
    tree_wd           = NA,
    tree_wdsp         = NA, 
    n_log10           = NA,
    log_top_diam10    = NA,
    tree_stem_v10     = NA,
    n_log20           = NA,
    log_top_diam20    = NA,
    tree_stem_v20     = NA
  ) |>
  left_join(tmp$tree_wd    , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$tree_length, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem   , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem10 , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem20 , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_species_code = str_sub(updated_tree_code, 4, 5),
    tree_d2h = (tree_dbh/100)^2 * tree_total_length,
    tree_d2hwd = tree_d2h*tree_wd
    ) |>
  select(
    updated_tree_code, tree_dbh, tree_total_length, tree_d2h, tree_wd, tree_wdsp, 
    tree_stem_v, tree_stem_v10, tree_stem_v20, tree_stem_b, everything()
    )

## Checks 
summary(tree)
# summary(data_clean$tree_stem_v$log_top_diam20)  
# summary(data_clean$tree_stem_v$log_top_diam10)  
# 
# tmp$check <- data_clean$tree_stem_v |> filter(log_top_diam20 > 25) |>
#   pull(updated_tree_code) |>
#   unique()

tree |>
  filter(tree_dbh < 70) |>
  ggplot(aes(x = tree_dbh, color = tree_species_code)) +
  geom_point(aes(y = tree_stem_v), shape = 1) +
  geom_point(aes(y = tree_stem_v20), shape = 4) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~tree_species_code)

tree |>
  ggplot(aes(x = tree_d2h, color = tree_species_code)) +
  geom_point(aes(y = tree_stem_v), shape = 1) +
  geom_point(aes(y = tree_stem_v20), shape = 4) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~tree_species_code)


tmp$outlier <- tree |> filter(tree_d2h > 40, tree_stem_v < 10)
tmp$outlier2 <- tree |> filter(tree_dbh > 110)
tmp$outlier3 <- tree |> filter(updated_tree_code == "186Sr045") 

data_clean_gg$check_v <- tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_v, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier, aes(label = updated_tree_code)) +
  theme(legend.position = "bottom") +
  labs(color = "")

print(data_clean_gg$check_v)
# ggsave(
#   plot = data_clean_gg$check_v, paste0("res/cleaning-examples/check_v-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )

## !!! Remove trees with large early branches
tree <- tree |> filter(!updated_tree_code %in% "551Pr100")

## NOT NEEDED
# gg_stemV_cleaning <- tree |>
#   filter(tree_dbh < 40) |>
#   ggplot(aes(x = tree_dbh, y = tree_stem_v, color = tree_species_code)) +
#   geom_point() +
#   geom_point(data = tmp$outlier3, shape = 23, size = 6, col = "red") +
#   geom_text_repel(data = tmp$outlier3, aes(label = updated_tree_code)) +
#   theme(legend.position = "bottom") +
#   labs(color = "")
# 
# print(gg_stemV_cleaning)


## Check
tmp$outliers <- tree |>
  filter(
    (tree_species_name == "Largestroemia parviflora" & tree_dbh > 60) |
      (tree_species_name == "Schima Wallichi" & tree_dbh > 60) |
      (tree_species_name == "Shorea robusta" & tree_dbh > 110) |
      is.na(tree_species_name)
  ) 

tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point(aes(color = tree_species_name), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  ggplot(aes(x = tree_dbh, y = tree_stem_v)) +
  geom_point(aes(color = tree_species_name), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none")

## 
## Check Biomass ####
##

tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_dbh, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2hwd, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2hwd, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")


data_clean_gg$check_b <- tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_stem_b, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  theme(legend.position = "bottom") +
  labs(color = "")

print(data_clean_gg$check_b)


##
## Get final data ####
##

data_clean$tree <- tree

 if (save_csv) write_csv(tree, paste0("res/tree-", Sys.Date(), ".csv"))

rm(tree, tmp)
