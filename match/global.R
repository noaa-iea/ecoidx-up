librarian::shelf(
  dplyr, DT, ggplot2, glue, googleAuthR, here, plotly, purrr, readr, shiny, shinyglide, stringr, tidyr)
options(readr.show_col_types = FALSE)

# options(error = browser()) # stop on error
# options(warn = 2)          # turn warnings into errors
options(warn = 0); .Options$error <- NULL # default

#* variables ----
#dir_ecoidx   <- "/Users/bbest/github/noaa-iea/ecoidx"
dir_ecoidx   <- "/share/github/ecoidx"
datasets_csv <- file.path(dir_ecoidx, "data-raw/_cciea_datasets.csv")
lut_cmp_csv  <- here("data/lut_components.csv")

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.cloud.google.com/apis/credentials/oauthclient/596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com?authuser=2&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-ko4kk0or16f1iju41rok8jc7ld0mmuch.apps.googleusercontent.com")

#* load dataset choices ----
d_cmp <- read_csv(lut_cmp_csv)

d_datasets <- read_csv(datasets_csv) %>% 
  mutate(
    cmp = str_replace(dataset_id, "cciea_([A-Z]+).*", "\\1")) %>% 
  left_join(
    d_cmp, by = "cmp") %>% 
  arrange(component, title)
# d_datasets %>% filter(dataset_id == "cciea_EI_HCI") %>% pull(title) # "Habitat Compression Index"
# d_datasets %>% filter(dataset_id == "cciea_EI_FBN") %>% pull(title) # "Habitat Compression Index"
# EI:  "Forage Biomass, California Current North"

vec_datasets <- with(d_datasets, setNames(dataset_id, title))

d <- d_datasets %>% 
  select(component, dataset_id, title) %>% 
  group_by(component) %>% 
  nest()
chs_datasets <- list()
for (component in d$component){ # component = d$component[1]
  chs_datasets[component] <- list(with(
    d %>% 
      filter(component == !!component) %>% 
      pull(data) %>% .[[1]],
    setNames(dataset_id, title)))
}
