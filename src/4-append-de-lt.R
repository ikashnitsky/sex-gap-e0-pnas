#===============================================================================
# 2021-03-15 -- sex gap e0
# Append DEU life table to the selection of countries
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))

# own functions
source("src/2-own-functions.R" %>% lp)

# load
load("dat/lt1x1.rda" %>% lp)
load("dat/ids.rda" %>% lp)
load("dat/de_lt.rda" %>% lp)

lt33 <- lt1x1 %>%
    filter(!sex == "b") %>% # remove both sex LTs
    bind_rows(de_lt) %>% 
    right_join(ids, "country") %>% 
    # arrange by the time series length
    group_by(country, sex, age) %>% 
    mutate(nyrs = n()) %>% 
    ungroup() %>% 
    arrange(nyrs %>% desc, country, year, sex, age) %>% 
    mutate(
        country = country %>% as_factor %>% fct_inorder(),
        name = name %>% as_factor %>% fct_inorder()
    ) %>% 
    ungroup()

# save the final life tables
save(lt33, file = "dat/lt33.rda", compress = "xz")
