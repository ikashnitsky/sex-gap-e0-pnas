#===============================================================================
# 2021-03-11 -- sex gap e0
# Prepare data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))


# hmd life tables and deaths  ----------------------------------------------

# !!! The code below assumes that one has full HMD downloaded and unpacked locally. To get a copy of it go to
# https://www.mortality.org/cgi-bin/hmd/hmd_download.php
# and choose "All statistics for HMD", currently 160326 Kb
# Ilya: my local copy was downloaded on 2021-02-02

# functions to read local HMD directories
devtools::source_gist("0f93062f2b67eeac69949554027fa84f")

# the local HMD path (Ilya)
hmdpath <- fs::as_fs_path("~/data/hmd/")

# life tables
lt1x1 <- bind_rows(
    b = path(hmdpath, "lt_both", "bltper_1x1") %>% fread_hmd_dir(),
    f = path(hmdpath, "lt_female", "fltper_1x1") %>% fread_hmd_dir(),
    m = path(hmdpath, "lt_male", "mltper_1x1") %>% fread_hmd_dir(),
    .id = "sex"
)

save(lt1x1, file = "dat/lt1x1.rda" %>% lp, compress = "xz")

# deaths
d1x1 <- path(hmdpath, "deaths", "Deaths_1x1") %>% fread_hmd_dir() %>%
    transmute(country, year, age, b = total, f = female, m = male) %>% 
    pivot_longer(b:m, names_to = "sex", values_to = "death")

save(d1x1, file = "dat/d1x1.rda" %>% lp, compress = "xz")

# exposures
e1x1 <- path(hmdpath, "exposures", "Exposures_1x1") %>% fread_hmd_dir() %>%
    transmute(country, year, age, b = total, f = female, m = male) %>% 
    pivot_longer(b:m, names_to = "sex", values_to = "expo")

save(e1x1, file = "dat/e1x1.rda" %>% lp, compress = "xz")

# # cohort death rates
# cdr1x1 <-  path(hmdpath, "c_death_rates", "cMx_1x1") %>% fread_hmd_dir()
# save(cdr1x1, file = "dat/cdr1x1.rda", compress = "xz")


# selection of countries for analysis -------------------------------------

# all countries with data from 1960
# GBR, FRA, and NZL -- NP populations
# DE to be unified
# except ISL and LUX -- keep for now, maybe drop later

# selection of countries
cntr <- e1x1 %>% 
    filter(year == 1960) %>% 
    distinct(country) %>%
    filter(
        !country %in% c(
            "GBRCENW", "GBRTENW", "GBR_NIR", "GBR_SCO", 
            "FRACNP",  "NZL_MA",  "NZL_NM"
        )
    ) %>% 
    pull(country)


# a table with names for later use, with unified Germany
ids <- tibble(country = c(cntr, "DEU")) %>% 
    mutate(
        name = country %>%
            countrycode(origin = "iso3c", destination = "country.name")
    ) %>%
    mutate(
        name = case_when(
            country=="FRATNP" ~ "France",
            country=="GBR_NP" ~ "United Knigdom",
            country=="NZL_NP" ~ "New Zealand",
            TRUE ~ name
        )
    ) %>% 
    drop_na(name) %>% 
    arrange(country)

save(ids, file = "dat/ids.rda" %>% lp, compress = "xz")
