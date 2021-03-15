#===============================================================================
# 2021-03-15 -- sex gap e0
# Decomposition
# Virgina Zarulli, vzarulli@sdu.dk
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))

# own functions
source("src/2-own-functions.R" %>% lp)

# load life tables
load("dat/lt33.rda" %>% lp)

# prepare sex specific mx
mx_decomp <- lt33 %>% 
    transmute(country, sex, year, age, mx) %>% 
    pivot_wider(names_from = sex, values_from = mx, names_prefix = "mx_") %>% 
    drop_na()

# decompose the difference in e0 between f and m by age [takes time, ~24 min]
decomp <- mx_decomp %>%
    group_by(country, year) %>%
    summarise(
        age = age,
        ctb = stepwise_replacement(
            func = e0,
            pars1 = mx_m, pars2 = mx_f,
            symmetrical = TRUE, direction = "up"
        )
    ) %>%
    mutate(rel_ctb = ctb %>% prop.table()) %>%
    ungroup()
    
# save the outpot
save(decomp, file = "dat/decomp.rda" %>% lp, compress = "xz")



# decomp Germany plateau 0.7 ----------------------------------------------
load("dat/de_lt_07.rda" %>% lp)

# prepare sex specific mx
decomp_de_07 <- de_lt_07 %>% 
    transmute(country, sex, year, age, mx) %>% 
    pivot_wider(names_from = sex, values_from = mx, names_prefix = "mx_") %>% 
    drop_na() %>% 
    # decompose the difference in e0 between f and m by age [takes time, ~1 min]
    group_by(country, year) %>%
    summarise(
        age = age,
        ctb = stepwise_replacement(
            func = e0,
            pars1 = mx_m, pars2 = mx_f,
            symmetrical = TRUE, direction = "up"
        )
    ) %>%
    mutate(rel_ctb = ctb %>% prop.table()) %>%
    ungroup()

# save the output
save(decomp_de_07, file = "dat/decomp_de_07.rda" %>% lp, compress = "xz")


# decomp changes in e0 gap --------------------------------------------------

# years of max sex gap
years_max_gap <- decomp %>% 
    filter(year %>% is_weakly_greater_than(1950)) %>% 
    ungroup() %>% 
    group_by(country, year) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    summarise(
        first = year %>% min,
        mid = case_when(ctb == max(ctb) ~ year),
        last = year %>% max,
    ) %>% 
    drop_na() %>% 
    pivot_longer(first:last, names_to = "mark", values_to = "year")

save(years_max_gap, file = "dat/years_max_gap.rda" %>% lp, compress = "xz")

# filter the needed mx
mx_gap_decomp <- lt33 %>% 
    transmute(country, sex, year, age, mx) %>% 
    right_join(years_max_gap) %>% 
    select(-year) %>% 
    pivot_wider(names_from = "mark", values_from = mx, names_prefix = "mx_")

# decompose [takes time, ~2 min]
gap_decomp <- mx_gap_decomp %>%
    group_by(country) %>%
    summarise(
        age = age,
        ctb_early = stepwise_replacement(
            func = e0gap,
            pars1 = mx_first, pars2 = mx_mid,
            symmetrical = TRUE, direction = "up"
        ),
        ctb_late = stepwise_replacement(
            func = e0gap,
            pars1 = mx_mid, pars2 = mx_last,
            symmetrical = TRUE, direction = "up"
        )
    ) %>%
    ungroup() %>% 
    # summarise over sex
    group_by(country, age) %>%
    summarise(
        ctb_early = ctb_early %>% sum(na.rm = T),
        ctb_late = ctb_late %>% sum(na.rm = T)
    ) %>%
    ungroup()

# save the outpot
save(gap_decomp, file = "dat/gap_decomp.rda" %>% lp, compress = "xz")
