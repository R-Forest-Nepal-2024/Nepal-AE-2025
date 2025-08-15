

data_md <- data_clean$tree_stem_v |> 
  filter(tree_dbh <= 110) |>
  mutate(no_group = "a")

##
## 2 params - V = a.DBH^b ####
##

## + No group ####

start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh), data = data_md))
start[1] <- exp(start[1])
start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_nogp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_2p_nogp

## + Species group ####

## + Species name ####

## + conif ####

##
## 2 params - V = a.D2H^b ####
##

## + No group ####

## + Species group ####

## + Species name ####

## + conif ####

##
## 3 params - V = a.DBH^b.H^c ####
##

## + No group ####

## + Species group ####

## + Species name ####

## + conif ####


