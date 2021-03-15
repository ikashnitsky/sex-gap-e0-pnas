#===============================================================================
# 2021-03-15 -- sex gap e0
# Life tables for the pooled Germany population
# Virgina Zarulli, vzarulli@sdu.dk
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))

# own functions
source("src/2-own-functions.R" %>% lp)


# load hmd deaths and exposures
load("dat/lt1x1.rda" %>% lp)
load("dat/d1x1.rda" %>% lp)
load("dat/e1x1.rda" %>% lp)



# join and calc raw mx
de_mx <- left_join(
    d1x1, e1x1
) %>% 
    filter(
        country %in% c("DEUTE", "DEUTW")
    ) %>% 
    group_by(year, sex, age) %>% 
    summarise(
        death = death %>% sum,
        expo = expo %>% sum
    ) %>% 
    mutate(mx = death / expo) %>% 
    # fix division by zero
    mutate(mx = mx %>% replace_na(1))

# smooth death rates above age 95 
de_mx_old <- de_mx %>% 
    filter(age %>% is_weakly_greater_than(80)) %>% 
    group_by(year, sex) %>% 
    mutate(mx = kannisto_smooth(mx, death, expo))

# join back the smoothed rates
de_mx_smooth <- bind_rows(
    de_mx %>% filter(age %>% is_less_than(80)),
    de_mx_old
) %>% 
    arrange(year, sex, age)

# tibble wrapper for lt
lt_tib <- function(df, sex) {
    lt(df$mx, sex)
}

# calculate life tables
de_lt <- de_mx_smooth %>% 
    filter(!sex=="b") %>% 
    group_by(year, sex) %>% 
    group_modify(~ {lt_tib(.x, sex = .y$sex)}) %>% 
    mutate(country = "DEU")

# save DEU life tables
save(de_lt, file = "dat/de_lt.rda" %>% lp, compress = "xz")



# smooth with plateau 0.7 -------------------------------------------------

# smooth death rates above age 95 
de_mx_old_07 <- de_mx %>% 
    filter(age %>% is_weakly_greater_than(80)) %>% 
    group_by(year, sex) %>% 
    mutate(mx = kannisto_smooth(mx, death, expo, plateau = .7))

# join back the smoothed rates
de_mx_smooth_07 <- bind_rows(
    de_mx %>% filter(age %>% is_less_than(80)),
    de_mx_old_07
) %>% 
    arrange(year, sex, age)

# tibble wrapper for lt
lt_tib <- function(df, sex) {
    lt(df$mx, sex)
}

# calculate life tables
de_lt_07 <- de_mx_smooth_07 %>% 
    filter(!sex=="b") %>% 
    group_by(year, sex) %>% 
    group_modify(~ {lt_tib(.x, sex = .y$sex)}) %>% 
    mutate(country = "DEU")

# save DEU life tables
save(de_lt_07, file = "dat/de_lt_07.rda" %>% lp, compress = "xz")
