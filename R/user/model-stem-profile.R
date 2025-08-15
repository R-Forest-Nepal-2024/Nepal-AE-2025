

tmp <- list()


data_md <- data_clean$stem_taper |> 
  mutate(no_group = "a")

# group <- "no_group"
# group <- "species_group"
# tmp$col <- data_md |> pull(as_name(group))
# data_md <- data_clean$stem_taper |> mutate(group = tmp$col)



##
## MODEL ALL PARAMS ####
##

## + No group ####

start <- c(1.02, 0.62, 0.15, 38, 4.48)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:5]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^e), 
  data = data_md,
  fixed = a + b + c + d + e ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_5params_nogp <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_5params_nogp


##
## M. 4 PARAMS: no e ####
##

## + No group ####

start <- c(0.1, 1, 1, 100)
start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)
summary(md)$numIter

md_4params_nogp <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_4params_nogp

## + species_group ####

start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ species_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)
summary(md)$numIter

md_4params_species_group <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_4params_species_group

## + species_name ####

start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ species_name,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_4params_species_name <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_4params_species_name


## + coniferous split ####

start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ conif,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_4params_conif <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_4params_conif

##
## M. 3 PARAMS: no d, e ####
##

## + No group ####

start <- c(1.02, 0.62, 0.15)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:3]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-24.7 * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3params_nogp <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_3params_nogp

md_3params_nogp$md_info$m_ranef


## + species group ####

start <- c(1.02, 0.62, 0.15)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:3]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-24.7 * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~ species_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3params_species_group <- nlme_out(.data = data_md, .out_var = dr, .in_var = hr, .start = start, .md = md, .name_dev = "gs")
md_3params_species_group

md_3params_species_group$md_info$m_ranef



##
## M. 4 PARAMS: no b ####
##

## NO CONVERGENCE

## + No group ####
# start <- c(1.0, 0.3, 24.7, 1.8)
# start <- c(10, 10, 100, 10)
# names(start) <- c("a", "c", "d", "e")
# 
# md <- nlme(
#   model = dr ~ a * ((1 - hr) * (1 + c * exp(-d * hr)) - hr^e), 
#   data = data_md,
#   fixed = a + c + d + e ~ 1,
#   groups = ~ no_group,
#   start = start, 
#   #weights = varPower(form = ~hr),
#   control = nlmeControl(maxIter = 100)
# )
# 
# AIC(md)
# 
# md_out <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
# md_out
# tt <- md_out$parameters


## 
## More graphs - best model ####
##
md_final    <- md_4params_species_name
group_final <- md_final$md_info$group

md_final$md_info$m_ranef

md_final$data |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred, color = !!sym(group_final))) +
  theme(legend.position = "bottom") +
  labs(color = "")

stem_taper$gg1 <- md_final$data |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred, color = !!sym(group_final))) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(sym(group_final))

stem_taper$gg_all <- md_final$graph
ggsave(
  plot = stem_taper$gg_all, paste0("res/stem-taper/md-4p-species-", Sys.time(), ".png"),
  width = 15, height = 12, units = "cm", dpi = 300
)
ggsave(
  plot = stem_taper$gg1, paste0("res/stem-taper/md-4p-species_facets-", Sys.time(), ".png"),
  width = 15, height = 12, units = "cm", dpi = 300
)

md_final$md_info$m_ranef


##
## COMPARE ALL MODELS ####
##

vec_md <- str_subset(ls(), pattern = "md_")
vec_md <- vec_md[-length(vec_md)]

stem_taper_info <- map(vec_md, ~get(.x)$md_info) |> list_rbind()
# stem_taper_info2 <- stem_taper_info |> select(-gr)

save(stem_taper_info2, file = "res/stem_taper_info2.Rdata")
write_csv(stem_taper_info2, file = "res/stem_taper_info2.csv")
write_csv(md_final$parameters, file = "res/stem_taper_info2_params.csv")
rm(tmp, vec_md)
