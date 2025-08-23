
tmp <- list()

data_md <- data_clean$tree |>
  filter(
    !(tree_species_name == "Largestroemia parviflora" & tree_dbh > 60),
    !(tree_species_name == "Schima Wallichi" & tree_dbh > 60),
    !(tree_species_name == "Shorea robusta" & tree_dbh > 110),
    !is.na(tree_species_name)
  ) |>
  mutate(no_group = "a")

## Check
ggplot(data_md, aes(x = tree_dbh, y = tree_stem_b)) +
  geom_point() +
  facet_wrap(~tree_species_code)


##
## First model ####
##

## Step by step
## 1. Define model characteristics: equation, random paramters, variance function
## 2. prepare data
## 3. find starting values
## 4. run model
## 5. get AIC, errors and fitted values
## 6. make graphs 

## 1. Bstem = a*DBH^b, no group, for Pr, variance c for DBH

## 2. prepare data 
data_pr <- data_md |> filter(tree_species_code == "Pr")

ggplot(data_pr, aes(x = tree_dbh, y = tree_stem_b)) +
  geom_point(aes(color = plot_alt)) +
  facet_wrap(~tree_species_code)

## 3. find starting values
start <- coef(lm(log(tree_stem_b) ~ log(tree_dbh), data = data_pr))
start[1] <- exp(start[1])
# start <- c(0.001, 1)
# start <- c(10, 100)

## 4. run model
md <- nlme(
  model = tree_stem_b ~ a * tree_dbh^b, 
  data = data_pr,
  fixed = a + b ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh)#,
  #control = nlmeControl(maxIter = 100)
)

## 5. get AIC, errors and fitted values
summary(md)
AIC(md)
fixef(md)

c_exp <- round(summary(md)$modelStruct$varStruct[1], 4)

data_pr <- data_pr |>
  mutate(
    pred    = predict(md),
    res     = residuals(md),
    res_std = residuals(md, type = "pearson"),
    res_w   = residuals(md) * 1 / tree_dbh^c_exp
  )

## 6. make graphs 
gg1 <- ggplot(data_pr, aes(x = tree_dbh)) +
  geom_point(aes(y = tree_stem_b), size = 0.1) +
  geom_line(aes(y = pred)) +
  theme(legend.position = "none")
print(gg1)

gg2 <- ggplot(data_pr, aes(x = pred)) +
  geom_point(aes(y = tree_stem_b), size = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = "lightgreen")
print(gg2)

gg3 <- ggplot(data_pr, aes(x = pred, y = res)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg3)

gg4 <- ggplot(data_pr, aes(x = pred, y = res_std)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg4)

gg5 <- ggplot(data_pr, aes(x = pred, y = res_w)) +
  geom_point(size = 0.1) +
  geom_smooth(se = FALSE)
print(gg5)

pr_b_d <- nlme_out(
  .data = data_pr,
  .out_var = tree_stem_b,
  .in_var = tree_dbh,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
pr_b_d$graph

## Exercise ####
## Develop model B = a*(DBH^2.H)^b and compare AIC
## Develop similar model for all species

## + B = a*(DBH^2.H)^b ####
start <- coef(lm(log(tree_stem_b) ~ log(tree_d2h), data = data_pr))
start[1] <- exp(start[1])
# start <- c(0.001, 1)

md <- nlme(
  model = tree_stem_b ~ a * tree_d2h^b, 
  data = data_pr,
  fixed = a + b ~ 1,
  groups = ~tree_quality,
  start = start, 
  weights = varPower(form = ~tree_dbh)#,
  #control = nlmeControl(maxIter = 100)
)
AIC(md)
fixef(md)
ranef(md)

pr_b_d2h <- nlme_out(
  .data = data_pr,
  .out_var = tree_stem_b,
  .in_var = tree_d2h,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
pr_b_dh$md_info$AIC
pr_b_dh$graph

## + B = a*DBH^b*H^c ####
start <- coef(lm(log(tree_stem_b) ~ log(tree_dbh) + log(tree_total_length), data = data_pr))
start[1] <- exp(start[1])
# start <- c(0.001, 1)
# start <- c(10, 100)

md <- nlme(
  model = tree_stem_b ~ a * tree_dbh^b * tree_total_length^c, 
  data = data_pr,
  fixed = a + b + c ~ 1,
  groups = ~no_group,
  start = start, 
  weights = varPower(form = ~tree_dbh)#,
  #control = nlmeControl(maxIter = 100)
)

pr_b_dxh <- nlme_out(
  .data = data_pr,
  .out_var = tree_stem_b,
  .in_var = tree_dbh,
  .start = start,
  .md = md,
  .name_dev = "GS"
)
pr_b_dxh$graph



## + Compare models ####
md_out <- bind_rows(pr_b_d$md_info, pr_b_dh$md_info, pr_b_dxh$md_info)
md_out

