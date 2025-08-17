
tmp <- list()

data_md <- data_clean$tree_stem_b |>
  filter(tree_stem_b != 0, tree_dbh <= 40) |>
  mutate(
    tree_d2hwd = tree_dbh^2 * tree_total_length * wd_mean,
    no_group = "a"
  )

# data_md <- data_clean$tree_stem_b |>
#   filter(
#     tree_stem_b != 0,
#     !(species_name == "Largestroemia parviflora" & tree_dbh > 60),
#     !(species_name == "Schima Wallichi" & tree_dbh > 60),
#     !(species_name == "Shorea robusta" & tree_dbh > 110)
#   ) 


##
## 2 params - Bstem = a.DBH^b ####
##

## + No group ####

start <- coef(lm(log(tree_stem_b) ~ log(tree_d2hwd), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_b ~ a * tree_d2hwd^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~species_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_nogp <- nlme_out(.data = data_md, .out_var = tree_stem_b, .in_var = tree_d2hwd, .start = start, .md = md, .name_dev = "gs")
md_2p_nogp
md_2p_nogp$md_info$m_ranef
