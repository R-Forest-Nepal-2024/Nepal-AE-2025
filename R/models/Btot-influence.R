

source("R/setup/init.R")

tree_init <- read_csv("data/final_tree_data_all_23_08_2025.csv", show_col_types = F)

names(tree_init)
summary(tree_init)

## Rename cols
tree <- tree_init |>
  rename(
    D = dbh,
    H = ht,
    WD = wd_final,
    V = vol_stem_ob,
    V10 = vol_stem_10_ub,
    V20 = vol_stem_20_ub,
    Bst = biom_stem,
    Bbb = biom_bb,
    Bsb = dry_sb_biom_kg,
    Bdb = biom_dead_b,
    Ble = biom_foliage,
    Btot_orig = biom_tree_total
  ) |>
  mutate(
    Ble = if_else(is.na(Ble), 0, Ble),
    Btot = Bst + Bbb + Bsb + Bdb + Ble,
    D2H = (D/100)^2*H,
    D2HWD = D2H * WD / 1000 #conv kg/m3 to t/m3
  ) |>
  mutate(across(starts_with("B"), \(x) x / 1000))

md <- lm(log(Btot) ~  log(D2HWD), data = tree)

#plot(md)
influencePlot(
  md, 
  id.method = "identify",
  main      = "Influence Plot",
  sub       = "Circle size ~ Cook's distance"
  )

tree <- tree |>
  mutate(
    pred     = predict(md),
    res      = residuals(md, type = "response"),
    res_std  = residuals(md, type = "pearson"),
    cooks    = cooks.distance(md),
    leverage = hatvalues(md)
  )

outliers <- tree |> filter(D2H > 40)
#outliers2 <- tree |> filter(cooks > 4 / nrow(tree) )
outliers3 <- tree |> arrange(desc(cooks)) |> slice_head(n = 4)


tree |>
  ggplot(aes(x = exp(pred), y = res_std)) +
  geom_point(aes(size = cooks, fill = cooks), shape = 21) +
  geom_point(data = outliers, shape = 21, size = 6, col = "red") +
  #geom_point(data = outliers2, shape = 22, size = 6, col = "red") +
  geom_label_repel(data = outliers3, aes(label = tree_id), col = "red" , min.segment.length = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(
    caption = "red circle: D2H > 40 m3"
  )

tree |>
  ggplot(aes(x = leverage, y = res_std)) +
  geom_point(aes(size = cooks, fill = cooks), shape = 21) +
  geom_point(data = outliers, shape = 21, size = 6, col = "red") +
  #geom_point(data = outliers2, shape = 22, size = 6, col = "red") +
  geom_label_repel(data = outliers3, aes(label = tree_id), col = "red" , min.segment.length = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(
    caption = "red circle: D2H > 40 m3"
  )


tree |>
  ggplot(aes(x = D2H, y = Btot)) +
  geom_point(aes(size = cooks, fill = cooks), shape = 21) +
  geom_point(data = outliers, shape = 21, size = 6, col = "red") +
  #geom_point(data = outliers2, shape = 22, size = 6, col = "red") +
  geom_label_repel(data = outliers3, aes(label = tree_id), col = "red" , min.segment.length = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(
    caption = "red circle: D2H > 40 m3"
  )

tree |>
  filter(D2H < 1) |>
  ggplot(aes(x = D2H, y = Btot)) +
  geom_point(aes(size = cooks, fill = cooks), shape = 21) +
  #geom_point(data = outliers, shape = 23, size = 6, col = "red") +
  #geom_point(data = outliers2, shape = 22, size = 6, col = "red") +
  geom_label_repel(data = outliers3, aes(label = tree_id), col = "red" , min.segment.length = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(
    caption = "red circle: D2H > 40 m3"
  )


tree |>
  ggplot(aes(x = D2H, y = Btot)) +
  geom_point(aes(size = cooks, fill = cooks), shape = 21) +
  geom_point(data = outliers, shape = 23, size = 6, col = "red") +
  #geom_point(data = outliers2, shape = 22, size = 6, col = "red") +
  geom_label_repel(data = outliers3, aes(label = tree_id), col = "red" , min.segment.length = 0) +
  scale_fill_viridis_c(direction = -1) +
  labs(
    caption = "red circle: D2H > 40 m3"
  ) +
  facet_wrap(~species)


