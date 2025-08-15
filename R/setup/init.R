

use_package <- function(.pkg_name, .github = FALSE, .gh_repo = NULL, .load = TRUE) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)) {
    if (!.github) {
      install.packages(pkg_name, dep =TRUE)
    } else if (.github) {
      if (!require(remotes, quietly = TRUE)) install.packages("remotes", dep = TRUE)
      remotes::install_github(paste0(.gh_repo))
    }
  }
  if (.load) library(pkg_name, character.only = T, quietly = TRUE)
}

use_package(readxl)
use_package(nlme)
use_package(systemfit)
use_package(ggrepel)
use_package(ggpubr)
use_package(rlang)
use_package(tidyverse)



## Set ggplot theme
theme_set(theme_bw())


## Create dirs
if (!"data" %in% list.files())             dir.create("data")
if (!"data-clean" %in% list.files("data")) dir.create("data/data-clean")
if (!"res" %in% list.files())              dir.create("res")


## Function for getting quick nlme outputs

## !!! FOR TESTING ONLY
# .data = data_md
# .start = start
# .md = md
# .name_dev = "gs"
## !!!

nlme_out <- function(.data, .out_var, .in_var, .start, .md, .name_dev){

  out_var <- enquo(.out_var)
  in_var <- enquo(.in_var)
  
  ## Output 1: model summary
  sum_out <- summary(.md)
  
  ## Output 2: data with pred and res
  data_out <- .data |>
    mutate(
      pred = predict(.md),
      res  = residuals(.md),
      res_std = residuals(.md, type = "pearson")
    )
  
  ## Output 3: graphs
  
  ## Get grouping variable
  md_group <- names(sum_out$modelStruct$reStruct)
  
  gg1 <- ggplot(data_out, aes(x = !!in_var)) +
    geom_point(aes(y = !!out_var), size = 0.1) +
    geom_line(aes(y = pred, color = !!sym(md_group))) +
    theme(legend.position = "none")
  
  gg2 <- ggplot(data_out, aes(x = pred)) +
    geom_point(aes(y = !!out_var), size = 0.1) +
    geom_abline(intercept = 0, slope = 1, col = "lightgreen")
  
  gg3 <- ggplot(data_out, aes(x = pred, y = res)) +
    geom_point(size = 0.1) +
    geom_smooth(se = FALSE)
  
  gg4 <- ggplot(data_out, aes(x = pred, y = res_std)) +
    geom_point(size = 0.1) +
    geom_smooth(se = FALSE)
  
  ## Check if power model applied
  is_c <- !is.null(sum_out$modelStruct$varStruct[1])
  
  if (is_c) {
    
    c_exp <- round(sum_out$modelStruct$varStruct[1], 4) 
    c_var <- as.character(attr(sum_out$modelStruct$varStruct, "formula"))[2]
    
    vec_c_var <- pull(data_out, as_name(c_var))
    
    data_out$res_w <- residuals(.md) * 1 / vec_c_var^c_exp
    
    gg5 <- ggplot(data_out, aes(x = pred, y = res_w)) +
      geom_point(size = 0.1) +
      geom_smooth(se = FALSE)
    
    c_out <- list(c(c_var, c_exp))
    
  } else {
    
    c_out <- "No residual variance model"
    
  }
  
  ## Combine graphs 
  if (is_c) {
    gg_all <- ggpubr::ggarrange(gg1, gg2, gg3, gg5, align = "hv")
  } else {
    gg_all <- ggpubr::ggarrange(
      gg1, 
      ggpubr::ggarrange(gg2, gg4, nrow = 1, align = "hv"),  
      nrow = 2
      )
  }
  
  ## Output 4: Key info  in one table
  ## + param table
  tt_params <- as_tibble(sum_out$tTable) |>
    mutate(
      param = rownames(sum_out$tTable),
      start = .start
    ) |>
    select(param, start, everything()) |>
    rename_with(str_replace, pattern = "-|\\.", replace = "_") |>
    rename_with(str_to_lower)
  
  
  ## + All info
  tt_out <- tibble(
    name_dev = .name_dev, 
    date = Sys.time(),
    formula = as.character(sum_out$call)[2],
    n_obs = sum_out$dims$N,
    group = md_group,
    AIC = AIC(.md), 
    c_exp = c_out,
    m_fixef = list(round(fixef(.md), 4)),
    m_ranef = list(round(ranef(.md), 4)),
    gr = list(gg_all)
  )
  
  out <- list(sum_out, data_out, gg_all, tt_params, tt_out)
  names(out) <- c("md_summary", "data", "graph", "parameters", "md_info")

  out
  
}

