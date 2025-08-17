

data_md <- data_clean$tree_stem_v |> 
  #filter(tree_dbh <= 60) |>
  #filter(tree_d2h <= 20) |>
  #filter(species_name == "Pinus roxburghii") |>
  mutate(no_group = "a")

##
## 2 params - V = a.DBH^b ####
##

## + No group ####

start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
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

start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~species_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_spgp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_2p_spgp


## + Species name ####

## + conif ####


start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~conif,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_conif <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_2p_conif

##
## 2 params - V = a.D2H^b ####
##

## + No group ####

start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_d2h^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_d2h_nogp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md, .name_dev = "gs")
md_2p_d2h_nogp

## + Species group ####

start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_d2h^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~species_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_d2h_spgp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md, .name_dev = "gs")
md_2p_d2h_spgp

## + Species name ####

## + conif ####
start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_d2h^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~conif,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_d2h_conif <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md, .name_dev = "gs")
md_2p_d2h_conif

## + Province ####
start <- coef(lm(log(tree_stem_v) ~ log(tree_d2h), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b")

md <- nlme(
  model = tree_stem_v ~ a * tree_d2h^b, 
  data = data_md,
  fixed = a + b ~ 1,
  groups = ~province,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_2p_d2h_prov <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_d2h, .start = start, .md = md, .name_dev = "gs")
md_2p_d2h_prov

##
## 3 params - V = a.DBH^b.H^c ####
##

## + No group ####
start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh) + log(tree_total_length), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b", "c")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b * tree_total_length^c, 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3p_nogp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_3p_nogp

## + Species group ####
start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh) + log(tree_total_length), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b", "c")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b * tree_total_length^c, 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~species_group,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3p_spgp <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_3p_spgp

## + Species name ####

## + conif ####

start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh) + log(tree_total_length), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b", "c")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b * tree_total_length^c, 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~conif,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3p_conif <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_3p_conif


## + Province ####
start <- coef(lm(log(tree_stem_v) ~ log(tree_dbh) + log(tree_total_length), data = data_md))
start[1] <- exp(start[1])
#start <- c(1, 1)
names(start) <- c("a", "b", "c")

md <- nlme(
  model = tree_stem_v ~ a * tree_dbh^b * tree_total_length^c, 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~province,
  start = start, 
  weights = varPower(form = ~tree_dbh),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3p_prov <- nlme_out(.data = data_md, .out_var = tree_stem_v, .in_var = tree_dbh, .start = start, .md = md, .name_dev = "gs")
md_3p_prov
