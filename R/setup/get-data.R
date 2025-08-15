
## Check path
if (!exists("usr")) stop("Object 'usr' missing")
  
if (is.null(usr$data_file)) stop("Path to data file missing")
 
if (!usr$data_file %in% list.files(".", recursive = T, full.names = T)) stop("Path to data in 'usr$data_file' not in directory")

## get data
data_init$plot  <- read_xlsx(usr$data_file, sheet = "01-info", na = "NA")
data_init$tree  <- read_xlsx(usr$data_file, sheet = "03-tree", na = "NA") 
data_init$stem  <- read_xlsx(usr$data_file, sheet = "04-stem", na = "NA", guess_max = 10000)
data_init$lab_b <- read_xlsx(usr$data_file, sheet = "07-disk-stem", na = "NA", guess_max = 5000)

## To compare old data:
# data_init$stem  <- read_xlsx(usr$data_file, sheet = "04-stem-old", na = "NA", guess_max = 10000)
# data_init$stem  <-  data_init$stem  |>
#   mutate(log_disk_pom = as.numeric(log_disk_pom)) |>
#   mutate(across(starts_with("log_base_pom"), as.numeric)) |>
#   mutate(across(starts_with("log_diam"), as.numeric))
