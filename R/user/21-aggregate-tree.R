

tmp <- list()
save_csv <- F


## + ACRONYMS ####
## stem        : st
## big branch  : bb
## small branch: sb
## leaves      : le
## fruits      : fr

## total: tot

## length     : L in m
## volume     : V in m3
## dry biomass: B in ton  

##
## Make final tree table ####
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
  distinct(updated_tree_code, tree_Ltot = tree_total_length)

tmp$agg_stem <- data_clean$stem_log |>
  group_by(updated_tree_code) |>
  summarise(
    tree_Nlog_st = n(),
    tree_top_diam = min(log_base_diam_ob, na.rm = T),
    tree_Vst = sum(log_vob, na.rm = T),
    tree_Bst = sum(log_b, na.tm = T)/1000 ## Convert kg to ton
  )

tmp$agg_stem10 <- data_clean$stem_log |>
  filter(log_top_diam_ob >= 10) |>
  group_by(updated_tree_code) |>
  summarise(
    tree_Nlog_st10 = n(),
    tree_top10_diam = min(log_top_diam_ob, na.rm = T),
    tree_Vst10 = sum(log_vob, na.rm = T)
  )

tmp$agg_stem20 <- data_clean$stem_log |>
  filter(log_top_diam_ob >= 20) |>
  group_by(updated_tree_code) |>
  summarise(
    tree_Nlog_st20 = n(),
    tree_top20_diam = min(log_top_diam_ob, na.rm = T),
    tree_Vst20 = sum(log_vob, na.rm = T)
  )

tmp$agg_bb <- data_clean$bb_log |>
  group_by(updated_tree_code) |>
  summarise(
    tree_Nlog_bb = n(),
    tree_Bbb     = sum(bb_logb, na.rm = T),
    tree_Vbb     = sum(bb_log_vob, na.tm = T)
  )
  

## + Combine into tree ####
tree <- data_init$tree |>
  select(-tree_code, -new_tree_code, -starts_with("photo")) |>
  left_join(tmp$plot, by = join_by(updated_tree_code)) |>
  mutate(
    tree_Ltot       = NA,
    tree_Nlog_st    = NA,
    tree_top_diam   = NA,
    tree_Vst        = NA,
    tree_Bst        = NA,
    tree_wd         = NA,
    tree_wdsp       = NA, 
    tree_Nlog_st10  = NA,
    tree_top10_diam = NA,
    tree_Vst10      = NA,
    tree_Nlog_st20  = NA,
    tree_top20_diam = NA,
    tree_Vst20      = NA,
    tree_Bbb        = NA,
    tree_Nlog_bb    = NA,
    tree_Vbb        = NA,
    tree_Bbb        = NA
  ) |>
  left_join(tmp$tree_wd    , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$tree_length, by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem   , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem10 , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_stem20 , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  left_join(tmp$agg_bb     , by = join_by(updated_tree_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## + add tree level variables ####
tree <- tree |>
  mutate(
    tree_species_code = str_sub(updated_tree_code, 4, 5),
    tree_d2h = (tree_dbh/100)^2 * tree_Ltot,
    tree_d2hwd = if_else(!is.na(tree_wd), tree_d2h*tree_wd, tree_d2h*tree_wdsp),
    tree_Bbb = if_else(is.na(tree_Bbb), 0, tree_Bbb),
    tree_Btot = tree_Bst + tree_Bbb
  ) |>
  select(
    updated_tree_code, tree_dbh, tree_Ltot, tree_d2h, tree_wd, tree_wdsp, tree_d2hwd, 
    tree_Vst, tree_Vst10, tree_Vst20, tree_Bst, tree_Bbb, tree_Vbb, everything()
  )


##
## Checks ####
##

summary(tree)


# summary(data_clean$tree_stem_v$log_top_diam20)  
# summary(data_clean$tree_stem_v$log_top_diam10)  
# 
# tmp$check <- data_clean$tree_stem_v |> filter(log_top_diam20 > 25) |>
#   pull(updated_tree_code) |>
#   unique()

## +  Check stem volume ####
tree |>
  ggplot(aes(x = tree_dbh, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(tree_dbh < 110) |>
  ggplot(aes(x = tree_dbh, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  #filter(tree_dbh < 70) |>
  ggplot(aes(x = tree_d2h, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(tree_dbh < 110) |>
  ggplot(aes(x = tree_d2h, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(tree_d2h < 40) |>
  ggplot(aes(x = tree_d2h, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "")

tree |>
  filter(tree_dbh < 110) |>
  ggplot(aes(x = tree_d2h, color = tree_species_code)) +
  geom_point(aes(y = tree_Vst), shape = 1) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~tree_species_code)


tmp$outlier <- tree |> filter(tree_d2h > 40)
tmp$outlier2 <- tree |> filter(tree_dbh > 110)
tmp$outlier3 <- tree |> filter(updated_tree_code == "186Sr045") 

data_clean_gg$check_Vst <- tree |>
  #filter(tree_dbh <= 110) |>
  ggplot(aes(x = tree_d2h, y = tree_Vst, color = tree_species_code)) +
  geom_point() +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outlier2, shape = 22, size = 6, col = "red") +
  geom_text_repel(data = tmp$outlier, aes(label = updated_tree_code)) +
  theme(legend.position = "bottom") +
  labs(color = "")
print(data_clean_gg$check_Vst)

# ggsave(
#   plot = data_clean_gg$check_v, paste0("res/cleaning-examples/check_v-", Sys.time(), ".png"),
#   width = 15, height = 12, units = "cm", dpi = 300
#   )


## + Check species level Volume ####

tmp$outliers <- tree |>
  filter(tree_d2h <= 40) |>
  filter(
    (tree_species_code == "Lp" & tree_dbh > 60) |
      (tree_species_code == "Sw" & tree_dbh > 60) |
      (tree_species_code == "Sr" & tree_dbh > 110) |
      is.na(tree_species_name)
  ) 

data_clean_gg$check_Vst_sp <- tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_dbh, y = tree_Vst)) +
  geom_point(aes(color = tree_species_code), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)
print(data_clean_gg$check_Vst_sp)

tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_dbh, y = tree_Vst)) +
  geom_point(aes(color = tree_species_name), shape = 21) + 
  geom_point(data = tmp$outliers, shape = 22, col = "red", size = 6) +
  theme(legend.position = "none")


## + Check stem biomass ####

tree |>
  ggplot(aes(x = tree_dbh, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2h, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2hwd, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")


tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_d2hwd, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  theme(legend.position = "none")

tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_dbh, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_d2h, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)

tree |>
  filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_d2h, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2hwd, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")


data_clean_gg$check_Bst <- tree |>
  #filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_d2hwd, y = tree_Bst, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)
print(data_clean_gg$check_Bst)

## + Check branch biomass #### 

tree |>
  ggplot(aes(x = tree_dbh, y = tree_Bbb, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

data_clean_gg$check_Bbb <- tree |>
  #filter(tree_d2h <= 40) |>
  ggplot(aes(x = tree_d2hwd, y = tree_Bbb, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)
print(data_clean_gg$check_Bbb)


## + Check total biomass ####

tree |>
  ggplot(aes(x = tree_dbh, y = tree_Btot, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2h, y = tree_Btot, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2hwd, y = tree_Btot, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none")

tree |>
  ggplot(aes(x = tree_d2hwd, y = tree_Btot, color = tree_species_code)) +
  geom_point(shape = 21) +
  geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)


data_clean_gg$check_Btot <- tree |>
  filter(tree_d2hwd <= 30) |>
  ggplot(aes(x = tree_d2hwd, y = tree_Btot, color = tree_species_code)) +
  geom_point(shape = 21) +
  #geom_point(data = tmp$outlier, shape = 21, size = 6, col = "red") +
  geom_point(data = tmp$outliers, shape = 22, size = 6, col = "red") +
  theme(legend.position = "none") +
  facet_wrap(~tree_species_code)
print(data_clean_gg$check_Btot)



##
## Get final data ####
##

data_clean$tree <- tree

 if (save_csv) write_csv(tree, paste0("res/tree-", Sys.Date(), ".csv"))

#rm(tree, tmp)
