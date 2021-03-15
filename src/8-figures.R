#===============================================================================
# 2020-02-18 -- sex gap e0
# Final plots -- submission of the manuscripts
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================
# UPD  2020-04-03 ------------------------------
# "Probability of death" where possible, "Risk" on vertical axes
# UPD  2020-09-09 ------------------------------
# - new age categories "0", "1-14", "15-39", "40-59", "60-79", "80+"

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))


# theme -------------------------------------------------------------------
load("dat/palettes.rda")

theme_custom <- theme_minimal(base_family = font_rc) +
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .25),
        panel.ontop = T
    )


load("dat/a6gap33cntrs.rda")

# Fig 1 -- RELATIVE ----------------------------------

# relative
df6 %>% 
    filter(country %>% is_in(c("SWE", "USA", "JPN", "RUS"))) %>%
    mutate(
        name = name %>% 
            fct_recode(USA = "United States") %>% 
            fct_rev()
    ) %>%
    ggplot() +
    geom_col(
        aes(year, ctb_rel %>% multiply_by(100), fill = age_group),
        position = position_stack(reverse = TRUE),
        color = NA,
        width = 1
    ) +
    facet_grid(name ~ ., scales = "free_y", space = "free") +
    coord_cartesian(ylim = c(-10, 120), expand = FALSE)+
    scale_x_continuous(breaks = seq(1800, 2000, 50))+
    scale_y_continuous(breaks = seq(0, 100, 25), position = "right")+
    scale_fill_manual(
        values = pal_six, 
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    ) +
    theme_minimal(base_family = font_rc, base_size = 20) +
    theme(
        # legend.position = "bottom",
        legend.position = c(.6, .5),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.spacing = unit(0, "lines"),
        panel.ontop = T
    )+
    labs(x = NULL,
         y = "Contribution, %",
         fill = "Age group")+
    # label countries
    geom_text(data = . %>% select(name, row, column) %>%  distinct(),
              aes(label = name, color = name), 
              x = 2015, y = 120, 
              hjust = 1, vjust = 1, size = 9, fontface = 2,
              family = font_rc)+
    scale_color_manual(values = pal_four %>% rev, 
                       guide = FALSE)

one_outer <- last_plot()




# plot qx
load("dat/lt1x1.rda")


# plot ratio
load("dat/df4qx.rda" %>% lp)

df4qx %>%
    pivot_wider(names_from = sex, values_from = qx) %>% 
    ggplot(aes(age, y = m/f, color = country))+
    geom_hline(yintercept = 1, color = "gray25",  size = .5)+
    # geom_point(shape = 16, size = .5)+
    geom_smooth(se = F, size = 1, color = "#ffffff", span = .25)+
    geom_smooth(se = F, size = .5, span = .25)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log", 
        breaks = c(.5, 1, 2, 3), 
        labels = c("", 1, 2, 3),
        limits = c(.75, 3.5)
    )+
    scale_color_manual(NULL, values = pal_four)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "none",
        panel.grid.minor = element_blank()
    )+
    labs(
        # y = bquote(q[x]^male / q[x]^female ~", log scale"),
        y = "Sex ratio, log scale",
        x = "Age"
    )+
    annotate(
        "text", x = 50, y = .9, 
        label = "Most recent year",
        size = 8.5, color = "grey50", alpha = .5,
        vjust = 1, family = font_rc, fontface = 2
    )

one_a <- last_plot()

# Death risk Ratio, Sweden, years 1750, 1800, 1850, 1900, 1960, 2019
# plot qx
load("dat/qxdiff.rda" %>% lp)

qxdiff %>% 
    filter(country == "SWE", 
           year %>% is_in(c(1800, 1900, 1960, 2019 ))) %>% 
    ggplot(aes(age, y = ratio, color = year %>% factor))+
    geom_hline(yintercept = 1, color = "gray25",  size = .5)+
    geom_smooth(se = F, size = .75, span = .4)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log", 
        breaks = c(.5, 1, 2, 3), 
        labels = c("", 1, 2, 3),
        limits = c(.75, 3.5)
    )+
    scale_color_viridis_d(end = .97)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = c(.85, .75),
        legend.spacing.x = unit(.1, "line"),
        legend.key.height = unit(1, "line"),
        panel.grid.minor = element_blank()
    )+
    labs(
        color = "Year",
        y = "Sex ratio, log scale",
        x = "Age"
    )+
    annotate(
        "text", x = 50, y = .9, 
        label = "Sweden",
        size = 8.5, color = "#009C9C", 
        vjust = 1, family = font_rc, fontface = 2
    )

one_b <- last_plot()


# plot difference
df4qx %>%
    pivot_wider(names_from = sex, values_from = qx) %>% 
    ggplot(aes(x = age, y = m - f, color = country, group = country)) +
    geom_path(size = .5)+
    scale_color_manual(NULL, values = pal_four)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log",
        breaks = c(.0001, .001, .01, .05),
        labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", "."),
        limits = c(9e-6, .1)
    )+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(legend.position = c(.77, .25),
          legend.spacing.x = unit(.1, "line"),
          legend.key.height = unit(1, "line"),
          legend.text = element_text(size = 16),
          panel.grid.minor = element_blank())+
    labs(
        # y = bquote(q[x]^male - q[x]^female ~", log scale"),
        y = "Sex gap, log scale",
        x = "Age"
    )

one_c <- last_plot()

# UPD  2020-09-09 ------------------------------
# REMOVE 1D

# # plot log log qx vs qx gap for the US only
# lt1x1 %>% 
#     filter(country %>% is_in(c("USA")),
#            year %>% is_in(2012:2016), 
#            age %>% is_less_than(96)) %>% 
#     group_by(age, sex) %>% 
#     summarise(qx = qx %>% mean, na.rm = T) %>% 
#     ungroup() %>% 
#     pivot_wider(names_from = sex, values_from = qx) %>% 
#     # add 6 colored age groups
#     mutate(age_group = age %>% 
#                cut(c(0, 1, 15, 40, 65, 85, 111), right = FALSE) %>% 
#                str_replace(",", "-") %>% 
#                str_remove("\\[") %>% 
#                str_remove("\\)") %>% 
#                str_replace("-111", "+") %>% 
#                as_factor() %>% 
#                fct_relevel("15-40", after = 0),
#            size = ifelse(age_group %>% is_in("15-40"), 1.25, .75),
#            shape = ifelse(age_group %>% is_in("15-40"), 16, 21)) %>% 
#     ggplot(aes(f, (m-f)))+
#     geom_abline(slope = 1, size = 1, color = "#ffffff")+
#     geom_abline(slope = 1, size = .5, color = "grey25")+
#     geom_path(aes(color = age_group), size = .2)+
#     geom_point(aes(color = age_group, size = size, shape = shape), fill = "#ffffff")+
#     scale_x_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .05),
#         labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_y_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .05),
#         labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_color_manual(
#         guide  = guide_legend(ncol = 1),
#         values = pal_six
#     ) +
#     scale_size_identity()+
#     scale_shape_identity()+
#     labs(x = "Female probability\nof death, log scale",
#          y = "Sex gap, log scale",
#          color = "Age group")+
#     theme_minimal(base_family = font_rc, base_size = 16)+
#     theme(
#         legend.position = c(.88, .42),
#         legend.spacing.x = unit(.1, "line"),
#         legend.key.height = unit(1, "line"),
#         legend.text = element_text(size = 14),
#         panel.grid.minor = element_blank()
#     )+
#     annotate(
#         "text", label = "USA",
#         x = 1e-4, y = .05, size = 9, color = pal_four[2],
#         hjust = 0, vjust = 1, family = font_rc, fontface = 2
#     )+
#     annotate(
#         "text", x = 4e-3, y = 4e-5, 
#         label = "2012-16 avg",
#         size = 7, color = "grey50", alpha = .5,
#         hjust = .5, vjust = 1, family = font_rc, fontface = 2
#     )
# 
# 
# one_d <- last_plot()


# arrange and save
blank <- ggplot(tibble(x = 1, y = 1), aes(x, y))+
    geom_rect(xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf,
              fill = "#ffffff",
              # fill = "red",
              color = NA)+
    theme_void()

one <- ggdraw() +
    draw_plot(one_outer) +
    # white space for plots
    draw_plot(blank, x = 0, y = .75, width = 0.7, height = 0.25)+
    draw_plot(blank, x = 0, y = .55, width = 0.33, height = 0.42)+
    draw_plot(blank, x = 0, y = .33, width = 0.33, height = 0.67)+
    # inset plots
    draw_plot(one_a, x = 0, y = .66, width = .33, height = .33)+
    draw_plot(one_c, x = .34, y = .66, width = .33, height = .33)+
    draw_plot(one_b, x = 0, y = 0.35, width = .33, height = .33)+
    # draw_plot(one_d, x = .32, y = 0.33, width = .3, height = .32)+
    # annotate plot letters
    draw_text(
        LETTERS[c(1,3,2,4)],  
        x = c(.01, .35, .01, .01),
        y = c(.99, .99, .66, .3), 
        hjust = 0,  vjust = 1, size = 20, 
        family = font_rc, fontface = 2
    )

ggsave(
    filename = "out/main-one.png" %>% lp, plot = one, 
    width = 10, height = 10, 
    type = "cairo-png"
)



# Fig 2 -- ABSOLUTE -------------------------------------------------------

# absolute
df6 %>%
    filter(country %>% is_in(c("SWE", "USA", "JPN", "RUS"))) %>%
    mutate(
        name = name %>% 
            fct_recode(USA = "United States") %>% 
            fct_rev()
        
    ) %>%
    ggplot() +
    geom_col(
        aes(year, ctb, fill = age_group),
        position = position_stack(reverse = TRUE),
        color = NA,
        width = 1
    ) +
    facet_grid(name ~ ., scales = "free_y", space = "free") +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = seq(1800, 2000, 50)) +
    scale_y_continuous(breaks = seq(0, 14, 2), position = "right")+
    scale_fill_manual(
        values = pal_six, 
        guide  = guide_legend(ncol = 1, reverse = TRUE)
    ) +
    theme_minimal(base_family = font_rc, base_size = 20) +
    theme(
        legend.position = c(.65, .4),
        strip.text.y = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.spacing = unit(2, "lines"),
        panel.ontop = T
    ) +
    # label countries
    geom_text(data = . %>% select(name, row, column) %>%  
                  distinct() %>% 
                  mutate(ypos = case_when(name=="Russia"~ 14, TRUE ~ 8),
                         xpos = case_when(
                             name=="Russia"~ 1985,
                             name=="Japan"~ 1985 ,
                             TRUE ~ 2015
                         )
                  ),
              aes(label = name, color = name, y = ypos, x = xpos), 
              hjust = 1, vjust = 1, size = 9, fontface = 2,
              family = font_rc)+
    scale_color_manual(values = pal_four %>% rev, 
                       guide = FALSE)+
    labs(x = NULL,
         y = "Contribution, years",
         fill = "Age group")

two_outer <- last_plot()


# data frame time series
df4qx_ts <- lt1x1 %>%
    filter(
        country %>% is_in(c("SWE", "USA", "JPN", "RUS")),
        sex %>% is_in("b") %>% not,
        age %>% is_less_than(96) & country != "RUS" |
            age %>% is_less_than(86) & country == "RUS"
    ) %>%
    transmute(sex, country, year, age, qx) %>% 
    mutate(
        country = country %>% as_factor() %>% 
            fct_recode(
                Sweden = "SWE",
                Japan = "JPN",
                Russia = "RUS"
            ) %>% 
            # re-order  to match the outer plot
            fct_relevel("USA", after = 0) %>% 
            fct_relevel("Sweden", after = 0)
    ) %>% 
    arrange(country, year, age)


# q0 SEX DIFF time series
df4qx_ts %>% 
    filter(age == 0) %>%
    filter(year %>% is_greater_than(1849)) %>%
    pivot_wider(names_from = sex, values_from = qx) %>%
    ggplot(aes(year, color = country))+
    geom_path(aes(y = m-f), size = .5)+
    scale_y_continuous(
        limits = c(-.003, .035),
        breaks = seq(0, .03, .01),
        labels = seq(0, .03, .01) %>% paste %>% str_replace("0.", ".")
    )+
    scale_x_continuous(breaks = c(1850, 1900, 1950, 1975, 2000), 
                       labels = c("1850", "1900", "1950", "'75", "2000"))+
    scale_color_manual(NULL, values = pal_four)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = c(.2, .27),
        legend.spacing.x = unit(.1, "line"),
        legend.key.height = unit(1, "line"),
        legend.text = element_text(size = 16),
        panel.grid.minor = element_blank()
    )+
    labs(
        y = "Sex gap",
        x =  "Year"
    )+
    # annotate(
    #     "text", x = 1875, y = .037, 
    #     label = "Age 0",
    #     size = 8.5, color = "grey50", alpha = .5,
    #     hjust = .5, vjust = 1, 
    #     family = font_rc, fontface = 2
    # )+
    annotate(
        "text", x = 1860, y = .035, 
        label = "Sex gap in probability\nof death at age 0",
        size = 7, color = "black", hjust = 0, vjust = 1, 
        family = font_rc, lineheight = .9
    )

two_a <- last_plot()

load("dat/qxdiff.rda" %>% lp)

# IMR Female VS Diff -- log log + normalization
qxdiff %>% 
    filter(country %>% is_in(c("SWE")),
           age %>% equals(0)) %>% 
    mutate(col_year = year %>% 
               cut(seq(1750, 2050, 50), right = FALSE) %>% 
               lvls_revalue(
                   paste0(
                       seq(1750, 2000, 50), "-", c(99, 49, 99, 49, 99, 19)
                   )
               )) %>% 
    ggplot(aes(f, ratio, color = col_year))+
    geom_point()+
    # add average line for 1900--2017
    geom_segment(data = . %>% 
                   filter(year %>% is_weakly_greater_than(1900)) %>% 
                   summarise(avg = ratio %>% mean(na.rm = T),
                             minf = f %>% min,
                             maxf = f %>% max),
                 aes(x = minf, xend = maxf, y = avg, yend = avg),
                 size = 1, color = "#A14500")+
    annotate("text", label = "Mean for\n1900–2017\nis 1.27",
             x = .009, y = 1.17, hjust = 0, vjust = 1, 
             size = 4.5,  color =  "#A14500", 
             family = font_rc, lineheight = .9)+
    # # average mark for each period
    # geom_point(data = . %>% 
    #                group_by(col_year) %>% 
    #                summarise(ratio = ratio %>% mean(na.rm=T),
    #                          f  = f %>% mean(na.rm=T)) %>% 
    #                ungroup(),
    #            size = 15, shape = 95)+
    scale_x_continuous(
        trans = "log", 
        breaks = c(.0001, .001, .01, .1, .25),
        labels = c(.0001, .001, .01,  .1, .25) %>% 
            paste %>% str_replace("0.", ".")
    )+
    scale_y_continuous(
        trans = "log", limits = c(1, 2),
        breaks = c(1, 1.1, 1.25, 1.5)
    )+
    scale_color_viridis_d(
        guide = guide_legend(ncol = 2, 
                             override.aes = list(size=3), 
                             reverse = T), 
        end = .97
    )+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.spacing.x = unit(.1, "line"),
        legend.key.height = unit(1, "line"),
        legend.text = element_text(size = 14),
        panel.grid.minor = element_blank()
    )+
    labs(
        color = NULL,
        x = "Female IMR",
        y = "Male IMR / Female IMR"
    )

two_b <- last_plot()




# probability of duying between 15 and 40

lt1x1 %>%
    filter(country %>% is_in(c("SWE", "USA", "JPN", "RUS")),
           sex %>% is_in("b") %>% not,
           age %>% is_in(c(15, 40))) %>%
    group_by(country, year, sex) %>% 
    summarise(p = lx[2]/lx[1]) %>% 
    ungroup() %>% 
    mutate(country = country %>% as_factor() %>% 
               fct_shift(2))%>% 
    pivot_wider(names_from = sex, values_from = p) %>%
    # plot
    ggplot(aes(year, color = country))+
    geom_path(aes(y = (f - m)), size = .5)+
    scale_color_manual(NULL, values = pal_four)+
    coord_cartesian(ylim = c(-.02, .2), expand = FALSE)+
    scale_x_continuous(
        breaks = c(1750, 1800, 1850, 1900, 1950, 1975, 2000),
        labels = c("1750", "1800", "1850", "1900", "1950", "'75", "2000")
    )+
    # facet_wrap(~country, ncol = 2)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(legend.position = "none",
          strip.text = element_blank(),
          panel.grid.minor = element_blank())+
    labs(
        y = "Sex gap",
        x =  "Year"
    )+
    annotate(
        "text", x = 1800, y = .19, 
        label = "Sex gap in probability of death\nfrom age 15 to age 40",
        size = 7, color = "black", hjust = 0, vjust = 1, 
        family = font_rc, lineheight = .9
    )

two_c <- last_plot()


# lexis surface for Sweden
load("dat/gap33cntrs.rda" %>% lp)

df %>%
    filter(country %>% is_in("SWE")) %>%
    mutate(
        ctb = ctb %>%
            cut(
                c(-Inf, -.05, -0.01, 0.01, .05, .1, .15, .25, .5, Inf), 
                right = FALSE
            ) %>% 
            fct_recode(`< -0.05` = "[-Inf,-0.05)",
                       `>= 0.5` = "[0.5, Inf)")
    ) %>%
    ggplot() +
    geom_tile(aes(year, age, fill = ctb),
              color = NA) +
    geom_abline(slope = 1, intercept = c(-1900, -1930), 
                color = "#551A8B",
                size = .1
    )+
    coord_cartesian(expand = FALSE, ylim = c(0, 111)) +
    scale_x_continuous(breaks = c(1800, 1850, 1900, 1930, 1950, 2000)) +
    scale_y_continuous(breaks = c(0, 15, 40, 60, 80, 110), position = "right") +
    scale_fill_manual(
        guide  = guide_legend(ncol = 3),
        values = pal_heat_207
    ) +
    theme_minimal(base_family = font_rc, base_size = 20) +
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .25),
        panel.ontop = T
    ) +
    labs(x = NULL,
         y = "Age",
         fill = "Contribution of\nsingle years of age",
         title = NULL)

two_e <- last_plot()


# arrange and save
two_ad <- ggdraw() +
    draw_plot(two_outer) +
    # white space for plots
    draw_plot(blank, x = 0, y = .75, width = 0.72, height = 0.2)+
    draw_plot(blank, x = 0, y = .55, width = 0.5, height = 0.4)+
    draw_plot(blank, x = 0, y = .3, width = 0.5, height = 0.65)+
    
    # inset plots
    draw_plot(two_a, x = 0, y = .59, width = .35, height = .4)+
    draw_plot(two_b, x = .36, y = .59, width = .33, height = .4)+
    draw_plot(two_c, x = 0, y = 0.25, width = .5, height = .33)+
    # draw_plot(two_e, x = 0, y = 0, width = 1, height = -.3)+
    # annotate plot letters
    draw_text(
        LETTERS[1:4],  
        x = c(.01, .35, .01, .01),
        y = c(.99, .99, .59, .25), 
        hjust = 0,  vjust = 1, size = 20, 
        family = font_rc, fontface = 2
    )

two <- ggdraw() +
    draw_plot(blank) +
    # inset plots
    draw_plot(two_ad, x = 0, y = .3, width = 1, height = .7)+
    draw_plot(two_e, x = 0, y = 0, width = 1, height = .3)+
    # annotate plot E
    draw_text(
        LETTERS[5],  
        x = .01,
        y = .32, 
        hjust = 0,  vjust = 1, size = 20, 
        family = font_rc, fontface = 2
    )

ggsave(
    filename = "out/main-two.png" %>% lp, two, 
    width = 10, height = 14, 
    type = "cairo-png"
)



# appendix one ------------------------------------------------------------
# plot faceted death risk ratio for 20 countries

load("dat/lt33.rda" %>% lp)

# A -- last year
lt33 %>%
    # filter out last available year
    group_by(sex, country, age) %>% 
    filter(year == year %>% last(),
           age %>% is_less_than(96)) %>% 
    ungroup() %>% 
    select(1:4, name,  6) %>% 
    pivot_wider(names_from = sex, values_from = qx) %>% 
    mutate(country = country %>% as_factor %>% fct_inorder(),
           name = name %>% as_factor %>% fct_inorder()) %>%
    # plot
    ggplot(aes(age, y = m/f))+
    geom_hline(yintercept = 1, color = "gray25",  size = .5)+
    geom_smooth(data = . %>% select(-name), aes(group = country), se = F, 
                span = .25,
                size = .25, color = "grey75")+
    geom_point(shape = 1, size = 1, color = "#df356b")+
    geom_smooth(se = F, size = 1, color = "#ffffff", span = .25)+
    geom_smooth(se = F, size = .5, color = "#df356b", span = .25)+
    geom_text(aes(label = year), x = 47.5, y = -.1, 
              size = 4, color = "grey75",
              vjust = 1, fontface = 2)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log", 
        breaks = c(.5, 1, 2, 3), 
        labels = c("", 1, 2, 3),
        limits = c(.75, 3.5)
    )+
    scale_color_manual(NULL, values = pal_six)+
    facet_wrap(~name, ncol = 5, dir = "v")+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
    )+
    labs(
        y = "Sex ratio, log scale",
        x = "Age",
        title = "A. Ratio of male to female probability of death, last available year" 
    )

one_app_a <- last_plot()

ggsave(
    filename = "out/appendix-1a.png" %>% lp, 
    one_app_a, width = 8, height = 10,
    type = "cairo-png"
)


# B -- 1960
lt33 %>%
    # filter out last available year
    group_by(sex, country, age) %>% 
    filter(year == 1960,
           age %>% is_less_than(96)) %>% 
    ungroup() %>% 
    select(1:4, name,  6) %>% 
    pivot_wider(names_from = sex, values_from = qx) %>% 
    mutate(country = country %>% as_factor %>% fct_inorder(),
           name = name %>% as_factor %>% fct_inorder()) %>% 
    # plot
    ggplot(aes(age, y = m/f))+
    geom_hline(yintercept = 1, color = "gray25",  size = .5)+
    geom_smooth(data = . %>% select(-name), aes(group = country), se = F, 
                size = .25, color = "grey75", span = .25)+
    geom_point(shape = 1, size = 1, color = "#08479A")+
    geom_smooth(se = F, size = 1, color = "#ffffff", span = .25)+
    geom_smooth(se = F, size = .5, color = "#08479A", span = .25)+
    geom_text(aes(label = year), x = 47.5, y = -.1, 
              size = 4, color = "grey75",
              vjust = 1, fontface = 2)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log", 
        breaks = c(.5, 1, 2, 3), 
        labels = c("", 1, 2, 3),
        limits = c(.75, 3.5)
    )+
    scale_color_manual(NULL, values = pal_six)+
    facet_wrap(~name, ncol = 5, dir = "v")+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
    )+
    labs(
        y = "Sex ratio, log scale",
        x = "Age",
        title = "B. Ratio of male to female probability of death, 1960" 
    )

one_app_b <- last_plot()

ggsave(
    filename = "out/appendix-1b.png" %>% lp, 
    one_app_b, width = 8, height = 10,
    type = "cairo-png"
)

# C -- 1900 for the available populations
lt33 %>%
    # filter out first available year
    group_by(sex, country, age) %>% 
    filter(year == 1900,
           age %>% is_less_than(96)) %>% 
    ungroup() %>% 
    select(1:4, name,  6) %>% 
    pivot_wider(names_from = sex, values_from = qx) %>% 
    droplevels() %>% 
    mutate(country = country %>% as_factor %>% fct_inorder(),
           name = name %>% as_factor %>% fct_inorder()) %>% 
    ggplot(aes(age, y = m/f))+
    geom_hline(yintercept = 1, color = "gray25",  size = .5)+
    geom_smooth(data = . %>% select(-name), aes(group = country), se = F, 
                size = .25, color = "grey75", span = .25)+
    geom_point(shape = 1, size = 1, color = "#009C9C")+
    geom_smooth(se = F, size = 1, color = "#ffffff", span = .25)+
    geom_smooth(se = F, size = .5, color = "#009C9C", span = .25)+
    geom_text(aes(label = year), x = 47.5, y = -.1, 
              size = 4, color = "grey75",
              vjust = 1, fontface = 2)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log", 
        breaks = c(.5, 1, 2, 3), 
        labels = c("", 1, 2, 3),
        limits = c(.75, 3.5)
    )+
    scale_color_manual(NULL, values = pal_six)+
    facet_wrap(~name, ncol = 5, dir = "v")+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
    )+
    labs(
        y = "Sex ratio, log scale",
        x = "Age",
        title = "C. Ratio of male to female probability of death, 1900" 
    )

one_app_c <- last_plot()

ggsave(
    filename = "out/appendix-1c.png" %>% lp, 
    one_app_c, width = 8, height = 3.5,
    type = "cairo-png"
)



# appendix two ------------------------------------------------------------

# the following plots go to appendix
# male and female qx
df4qx %>% 
    pivot_wider(names_from = sex, values_from = qx) %>%
    ggplot(aes(age, color = country))+
    geom_path(aes(y = m), size = .5)+
    geom_path(aes(y = f), size = .1)+
    geom_ribbon(aes(ymin = m, ymax = f, fill = country), alpha = .25, 
                color = NA)+
    scale_color_manual(NULL, values = pal_four)+
    scale_fill_manual(NULL, values = pal_four)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        breaks = seq(0, .3, .1),
        labels = seq(0, .3, .1) %>% paste %>% str_replace("0.", ".")
    )+
    facet_wrap(~country, ncol = 2)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(legend.position = "none",
          strip.text = element_blank(),
          panel.grid.minor = element_blank())+
    labs(
        y = "Probability of death",
        x = "Age"
    )+
    # label countries
    geom_text(data = . %>% select(country) %>%  distinct(),
              aes(label = country, color = country), 
              x = 0, y = .27, size = 7, fontface = 2,
              hjust = 0, vjust = 1, 
              family = font_rc)

two_app_a <- last_plot()


# male and female qx -- LOG SCALE
df4qx %>% 
    pivot_wider(names_from = sex, values_from = qx) %>% 
    ggplot(aes(age, color = country))+
    geom_path(aes(y = m), size = .5)+
    geom_path(aes(y = f), size = .1)+
    geom_ribbon(aes(ymin = m, ymax = f, fill = country), alpha = .25, 
                color = NA)+
    scale_x_continuous(breaks = c(0, 15, 40, 60, 80))+
    scale_y_continuous(
        trans = "log",
        breaks = c(.0001, .001, .01, .1, .5),
        labels = c(.0001, .001, .01, .1, .5) %>% paste %>% 
            str_replace("0.", "."),
        limits = c(9e-6, .5)
    )+
    scale_color_manual(NULL, values = pal_four)+
    scale_fill_manual(NULL, values = pal_four)+
    facet_wrap(~country, ncol = 2)+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(legend.position = "none",
          strip.text = element_blank(),
          panel.grid.minor = element_blank())+
    labs(
        y = "Probability of death,\nlog scale",
        x = "Age"
    )+
    # label sexes
    geom_text(
        data = tibble(
            country = "Russia" %>% 
                factor(
                    levels = c("France", "Japan", "USA", "Russia")
                ),
            sex = c("Female", "Male"),
            ypos = c(4e-4, 5e-3),
            xpos = c(45, 20),
            fontface = c(1, 2)
        ),
        aes(label = sex, x = xpos, y = ypos, 
            color = country, fontface = fontface), 
        size = 5, 
        hjust = 0, vjust = 0, angle = 30, 
        family = font_rc
    )

two_app_b <- last_plot()


two_app <- two_app_a + two_app_b + 
    plot_annotation(
        # title = "Age profiles of death risk",
        tag_levels = "A",
        theme = theme(
            text = element_text(family = font_rc, size = 15)
        )
    )

ggsave(
    filename = "out/appendix-2.png" %>% lp, 
    two_app, width = 7, height = 4,
    type = "cairo-png"
)


# # appendix three ----------------------------------------------------------
# # UPD  2020-09-09 ------------------------------
# # REMOVE A3
# 
# load("data/qxdiff.rda")
# 
# # plot log log qx vs qx gap for the four countries and years 1960, 1990, last
# qxdiff %>% 
#     filter(
#         country %>% is_in(c("SWE", "USA", "JPN", "RUS")),
#         year == 2017 & country == "SWE" | 
#             year == 2016 & country == "USA" |
#             year == 2016 & country == "JPN"|
#             year == 2014 & country == "RUS" |
#             year %>% is_in(c(1960, 1990)),
#         age %>% is_less_than(96) & country != "RUS" |
#             age %>% is_less_than(86) & country == "RUS"
#     ) %>%
#     mutate(year = year %>% as_factor() %>% 
#                fct_other(keep = c("1960", "1990"), other_level = "Last")) %>% 
#     # rename and reorder countries
#     mutate(
#         country = country %>% as_factor() %>% 
#             fct_recode(
#                 Sweden = "SWE",
#                 Japan = "JPN",
#                 Russia = "RUS"
#             ) %>% 
#             # re-order  to match the outer plot
#             fct_relevel("USA", after = 0) %>% 
#             fct_relevel("Sweden", after = 0)
#     ) %>% 
#     arrange(country, age) %>% 
#     ggplot(aes(f, gap))+
#     geom_point(aes(color = age_group))+
#     geom_path(aes(color = age_group), size = .2)+
#     geom_abline(slope = 1, size = 1, color = "#ffffff")+
#     geom_abline(slope = 1, size = .5, color = "grey25")+
#     scale_x_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .05),
#         labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_y_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .05),
#         labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     facet_grid(country~year)+
#     coord_fixed()+
#     scale_color_manual(
#         guide  = guide_legend(nrow = 1),
#         values = pal_six
#     ) +
#     labs(x = "Female probability of death, log scale",
#          y = "Sex gap in probability of death, log scale",
#          color = "Age group")+
#     theme_minimal(base_family = font_rc, base_size = 16)+
#     theme(
#         legend.position = "bottom",
#         legend.spacing.x = unit(.1, "line"),
#         legend.key.height = unit(1, "line"),
#         legend.text = element_text(size = 14),
#         panel.grid.minor = element_blank()
#     )
# 
# 
# three_app <- last_plot()
# 
# ggsave(filename = "out/appendix-3.png", 
#        three_app, width = 7, height = 9)
# 

# # appendix three -----------------------------------------------------------
# # UPD  2020-09-15 ------------------------------
# # Remove appendix 3 (before 4)
# 
# # sex gap in IMR vs female IMR
# 
# 
# # linear scales
# qxdiff %>% 
#     filter(country %>% is_in(c("SWE")),
#            age %>% equals(0)) %>% 
#     mutate(col_year = year %>% 
#                cut(seq(1750, 2050, 50), right = FALSE) %>% 
#                lvls_revalue(
#                    paste0(
#                        seq(1750, 2000, 50), "-", c(99, 49, 99, 49, 99, 17)
#                    )
#                )) %>% 
#     ggplot(aes(f, gap, color = col_year))+
#     geom_point()+
#     scale_y_continuous(
#         breaks = seq(0, .03, .01),
#         labels = seq(0, .03, .01) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_x_continuous(
#         breaks = seq(0, .25, .05),
#         labels = seq(0, .25, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_color_viridis_d(end = .97, 
#                           guide  = guide_legend(nrow = 1, reverse = TRUE,
#                                                 override.aes = list(size=5)))+
#     theme_minimal(base_family = font_rc, base_size = 16)+
#     theme(
#         legend.position = "bottom",
#         legend.spacing.x = unit(1, "line"),
#         legend.text = element_text(size = 20),
#         panel.grid.minor = element_blank()
#     )+
#     labs(
#         color = NULL,
#         x = "Female IMR",
#         y = "Sex gap in IMR"
#     )
# 
# imr_lin <- last_plot()
# 
# 
# # log log
# qxdiff %>% 
#     filter(country %>% is_in(c("SWE")),
#            age %>% equals(0)) %>% 
#     mutate(col_year = year %>% 
#                cut(seq(1750, 2050, 50), right = FALSE) %>% 
#                lvls_revalue(
#                    paste0(
#                        seq(1750, 2000, 50), "-", c(99, 49, 99, 49, 99, 17)
#                    )
#                )) %>% 
#     ggplot(aes(f, gap, color = col_year))+
#     geom_point()+
#     scale_x_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .1, .25),
#         labels = c(.0001, .001, .01,  .1, .25) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_y_continuous(
#         trans = "log", 
#         breaks = c(.0001, .001, .01, .05),
#         labels = c(.0001, .001, .01, .05) %>% paste %>% str_replace("0.", ".")
#     )+
#     scale_color_viridis_d(end = .97, 
#                           guide  = guide_legend(nrow = 1, reverse = TRUE,
#                                                 override.aes = list(size=5)))+
#     theme_minimal(base_family = font_rc, base_size = 16)+
#     theme(
#         legend.position = "bottom",
#         legend.spacing.x = unit(1, "line"),
#         legend.text = element_text(size = 20),
#         panel.grid.minor = element_blank()
#     )+
#     labs(
#         color = NULL,
#         x = "Female IMR, log scale",
#         y = "Sex gap in IMR, log scale"
#     )
# 
# imr_log <- last_plot()    
# 
# 
# imr <- (imr_lin + imr_log) / guide_area() + 
#     plot_layout(ncol = 1, guides = "collect", heights = c(5,1))+
#     plot_annotation(title = "Infant mortality rate, Sweden",
#                     theme = theme(plot.title = element_text(family = font_rc, size = 20)), caption = "A")
# 
# ggsave(filename = "out/appendix-3.png", imr, 
#        width = 10, height = 5)



# appendix three -----------------------------------------------------------

load("dat/a6gap33cntrs.rda" %>% lp)
load("dat/palettes.rda" %>% lp)

# relative, 33 countries, 6 age groups 
df6 %>% 
    ggplot()+
    geom_col(aes(year, ctb_rel %>% multiply_by(100), fill = age_group),
             position = position_stack(reverse = TRUE), 
             color = NA, width = 1)+
    facet_grid(row~column, scales = "free_x", space="free")+
    coord_cartesian(ylim = c(-20, 120), expand = FALSE)+
    scale_x_continuous(breaks = seq(1800, 2000, 50))+
    scale_y_continuous(breaks = seq(0, 100, 25))+
    scale_fill_manual(values = pal_six, guide  = guide_legend(nrow = 1))+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.ontop = T
    )+
    geom_text(data = . %>% select(name, row, column) %>%  distinct(),
              aes(label = name), 
              x = 2015, y = 120, 
              hjust = 1, vjust = 1, size = 3,
              family = font_rc, color = "grey20")+
    labs(x = NULL, y = "Relative contribution, %",
         fill = "Age group",
         title = "Relative age-specific contribution to sex gap in life expectancy at birth")

three_app <- last_plot()

ggsave(
    "out/appendix-3.png" %>% lp, 
    three_app, width = 8, height = 10,
    type = "cairo-png"
)


# appendix four ------------------------------------------------------------


# absolute, 33 countries, 6 age groups 
df6 %>% 
    ggplot()+
    geom_col(aes(year, ctb, fill = age_group),
             position = position_stack(reverse = TRUE), 
             color = NA, width = 1)+
    facet_grid(row~column, scales = "free_x", space="free")+
    coord_cartesian(ylim = c(-1, 14), expand = FALSE)+
    scale_x_continuous(breaks = seq(1800, 2000, 50))+
    scale_y_continuous(breaks = seq(0, 12, 2))+
    scale_fill_manual(values = pal_six, guide  = guide_legend(nrow = 1))+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.ontop = T
    )+
    geom_text(data = . %>% select(name, row, column) %>%  distinct(),
              aes(label = name), 
              x = 2015, y = 14, 
              hjust = 1, vjust = 1, size = 3,
              family = font_rc, color = "grey20")+
    labs(x = NULL, y = "Absolute contribution, years",
         fill = "Age group",
         title = "Age-specific contribution to sex gap in life expectancy at birth")

four_app <- last_plot()

ggsave(
    "out/appendix-4.png" %>% lp, 
    four_app, width = 8, height = 10,
    type = "cairo-png"
)



# appendix five ----------------------------------------------------------


# lexis surface
load("dat/gap33cntrs.rda" %>% lp)
load("dat/palettes.rda" %>% lp)

# absolute
df %>% 
    mutate(
        ctb = ctb %>%
            cut(
                c(-Inf, -.05, -0.01, 0.01, .05, .1, .15, .25, .5, Inf), 
                right = FALSE
            ) %>% 
            fct_recode(`< -0.05` = "[-Inf,-0.05)",
                       `>= 0.5` = "[0.5, Inf)")
    ) %>%
    drop_na() %>% 
    ggplot()+
    geom_tile(aes(year, age, fill = ctb),
              color = NA)+
    geom_abline(
        data = . %>% distinct(row, column) %>% 
            crossing(tibble(intercept = c(-1900, -1930))) %>%  
            mutate(slope = 1),
        aes(slope = slope, intercept = intercept),
        color = "#551A8B",
        size = .1
    )+
    facet_grid(row~column, scales = "free_x", space="free")+
    coord_cartesian(expand = FALSE, ylim = c(0,130))+
    scale_x_continuous(
        breaks = c(1800, 1850, 1900, 1930, 1950, 2000),
        labels = c("1800", "1850", "1900", "'30", "'50", "2000")
    )+
    scale_y_continuous(breaks = c(0, 15, 40, 60, 80, 110))+
    scale_fill_manual(
        guide  = guide_legend(ncol = 3),
        values = pal_heat_207
    ) +
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.ontop = T
    )+
    geom_text(
        data = . %>% select(name, row, column) %>%  distinct(),
        aes(label = name), 
        x = 2015, y = 114, 
        hjust = 1, vjust = 0, size = 3,
        family = font_rc, color = "grey20"
    )+
    labs(x = NULL, y = "Age",
         fill = "Contribution of\nsingle years of age",
         title = "Lexis surface: Absolute age-specific contribution to sex gap in life expectancy at birth")

five_app <- last_plot()

ggsave(
    "out/appendix-5.png" %>% lp, 
    five_app, width = 8, height = 10,
    type = "cairo-png"
)




# appendix six -- palteu 0.7 --------------------------------------------

load("dat/df_plateau.rda" %>% lp) # data is just for the EU

# absolute plot
df_plateau %>% 
    ggplot()+
    geom_col(aes(year, value, fill = age_group),
             position = position_stack(reverse = TRUE), 
             color = NA, width = 1)+
    facet_grid(type~plateau, scales = "free_y")+
    coord_cartesian(expand = FALSE)+
    scale_fill_manual(values = pal_six, guide  = guide_legend(nrow = 1))+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "#339999"),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .25),
        panel.spacing = unit(1, "line"),
        panel.ontop = T
    )+
    labs(x = NULL, y = NULL,
         fill = "Age group",
         title = "Age-specific contribution to sex gap in life expectancy at birth",
         subtitle = "Comparison of the two assumptions of plateau level, Germany")

ggsave("out/appendix-6.png", width = 6, height = 4.5)



# appendix seven -- sensitivity check for age boundary 50 vs 40 ------------

# 6 age groups -- 1, 15, 50, 60, 80

df40 <- df %>% 
    mutate(age_group = age %>% 
               cut(c(0, 1, 15, 40, 60, 80, 111), right = FALSE)) %>% 
    group_by(country, name, row, column, year, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T),
              ctb_rel = rel_ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-39", "40-59", "60-79", "80+"))
    )

df50 <- df %>% 
    mutate(age_group = age %>% 
               cut(c(0, 1, 15, 50, 60, 80, 111), right = FALSE)) %>% 
    group_by(country, name, row, column, year, age_group) %>% 
    summarise(ctb = ctb %>% sum(na.rm = T),
              ctb_rel = rel_ctb %>% sum(na.rm = T)) %>% 
    ungroup() %>% 
    # relevel age_group factor
    mutate(
        age_group = age_group %>% 
            str_replace(",", "-") %>% 
            as_factor() %>% 
            lvls_revalue(c("0", "1-14", "15-49", "50-59", "60-79", "80+"))
    )


# compare just 15-40 VS 15-50 lines
df40 %>% ggplot()+
    geom_path(data = df40 %>% 
                  filter(age_group %>% is_in(c("15-39"))) %>% 
                  group_by(country, name, row, column, year) %>% 
                  mutate(avg_ctb_rel = ctb_rel %>% divide_by(25) %>% 
                             multiply_by(100)) %>% 
                  ungroup() %>% 
                  filter(! avg_ctb_rel > 5),
              aes(year, avg_ctb_rel, color = age_group), size = 1)+
    geom_path(data = df50 %>% 
                  filter(age_group %>% is_in(c("15-49"))) %>% 
                  group_by(country, name, row, column, year) %>% 
                  mutate(avg_ctb_rel = ctb_rel %>% divide_by(35) %>% 
                             multiply_by(100)) %>% 
                  ungroup() %>% 
                  filter(! avg_ctb_rel > 5),
              aes(year, avg_ctb_rel, color = age_group), size = 1)+
    geom_hline(yintercept = 0, color = "#666666", size = .5)+
    facet_grid(row~column, scales = "free_x", space="free")+
    coord_cartesian(ylim = c(-1, 4), expand = FALSE)+
    scale_x_continuous(breaks = seq(1800, 2000, 50))+
    scale_color_manual(values = c("#003737FF", "#3FB3F7FF"), 
                       guide  = guide_legend(nrow = 1))+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.ontop = T
    )+
    geom_text(data = . %>% select(name, row, column) %>%  distinct(),
              aes(label = name), 
              x = 2015, y = 4, 
              hjust = 1, vjust = 1,
              family = font_rc, color = "grey20")+
    labs(x = NULL, y = "Average relative contribution, %",
         color = "Age group",
         title = "Average contribution per year of age in the age group")


seven_app <- last_plot()

ggsave(
    "out/appendix-7.png" %>% lp, 
    seven_app, width = 8, height = 10,
    type = "cairo-png"
)




# appendix eight -- change of gap decomposition ---------------------------

load("dat/df_gap_decomp.rda" %>% lp)

df_gap_decomp %>% 
    ggplot(aes(ctb, age_group))+
    geom_vline(
        xintercept = 0, size = .5, color = "#999999"
    )+
    geom_text(
        data = . %>% filter(period == "early"),
        aes(label = year_label %>% str_wrap(5), color = period), 
        family = font_rc, fontface = 2,
        x = 1.5, y = 1.75, size = 4, lineheight = .9, alpha = .1
    )+
    geom_text(
        data = . %>% filter(period == "late"),
        aes(label = year_label %>% str_wrap(5), color = period), 
        family = font_rc, fontface = 2,
        x = -1.5, y = 1.75, size = 4, lineheight = .9, alpha = .1
    )+
    geom_col(
        aes(fill = period, color = period),
        stat="identity", position="dodge", color = NA, width =.5
    )+
    facet_wrap(~name, ncol = 5, dir = "v")+
    scale_fill_manual(values = c("#003737FF", "#3FB3F7FF"))+
    scale_color_manual(values = c("#003737FF", "#3FB3F7FF"))+
    theme_minimal(base_family = font_rc, base_size = 14)+
    theme(
        legend.position = "none",
        panel.grid.minor =  element_blank(),
        panel.grid.major =  element_line(size = .1),
        panel.ontop = T
    )+
    labs(
        y = "Age group, years",
        x = "Absolute contribution, years",
        title = "Age-specific contributions to the change in the sex gap in life expectancy at birth" %>% str_wrap(60)
    )

eight_app <- last_plot()

ggsave(
    "out/appendix-8.png" %>% lp, 
    eight_app, width = 8, height = 10,
    type = "cairo-png"
)





