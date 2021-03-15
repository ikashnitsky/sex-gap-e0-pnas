#===============================================================================
# 2021-03-13-- sex gap e0
# Colors
# Virgina Zarulli, vzarulli@sdu.dk
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source(lp("src/0-prepare-session.R"))


# color blind safe palette
pal <- paletteer_d("rcartocolor::Safe")
pal[1:5] %>% color %>% plot 
pal <- pal[c(3, 4, 1, 5, 2)]

# safe palette trough rotating blues9
pal <- blues9[c(3,5,7,8,9)] %>% color %>% 
    clr_rotate(degrees = seq(30, 150, length.out =  5)) 

# close to original Virginia's colors rotating blues9
pal <- blues9[c(3,5,6,7,9)] %>% color %>% 
    clr_rotate(degrees = c(15, 0, 170, 145, 90)) 

pal <- pal[c(2,1,4,3,5)]

# smoother transitions than blues 9
bluesN <- colorRampPalette(blues9)
blues100 <- bluesN(100)

pal <- blues100[c(30,50,70,85,100)] %>% color %>% 
    clr_rotate(degrees = c(0, 15, 170, 135, 90)) 

pal <- pal[c(2,1,4,3,5)]

# a function to test colors
test_color <- function(color_vec) {
    require(magrittr)
    require(prismatic)
    par(mfrow = c(2,3), mai = c(0, 0, .5, 0))
    color_vec %>% color %>% plot; title("Color palette")
    color_vec %>% color %>% clr_grayscale %>%  plot; title("Grayscale")
    color_vec %>% color %>% clr_negate %>%  plot; title("Negate")
    color_vec %>% color %>% clr_protan %>%  plot; title("Protan")
    color_vec %>% color %>% clr_deutan %>%  plot; title("Deutan")
    color_vec %>% color %>% clr_tritan %>%  plot; title("Tritan")
    par(mfrow = c(1,1))
}

# a selection of 5 colors, print and colorblind friendly
pal_safe_five <- c(
    "#eec21f", # default R 4.0 yellow
    "#009C9C", # light shade of teal: no red, equal green and blue
    "#df356b", # default R 4.0 red
    "#08479A", # blues9[8] "#08519C" made a bit darker
    "#003737" # very dark shade of teal
)

pal_safe_five_ordered <- pal_safe_five[c(5,2,1,3,4)]


# selection of material colors
# https://www.materialui.co/colors
pal_material_5 <- c( # original selection
    "#00695C", # [15,40)
    "#4FC3F7", # [0,15)
    "#26A69A", # [40,65)
    "#E65100", # [65,85)
    "#FFA000" # [85,111)
    
)


pal_material_6 <- c(
    "#4FC3F7", # [0,1)
    "#B2EBF2", # [5,15)
    "#00695C", # [15,40)
    "#26A69A", # [40,65)
    "#FFA000", # [65,85)
    "#E65100"  # [85,111)
)

pal_five <- c(
    "#005050", # [15,40)
    "#268A8A", # [40,65)
    "#eec21f", # [65,85)
    "#A14500", # [85,111)
    "#3FB3F7" # [0,15)
)

pal_six <- c(
    "#084488", # [0, 1)
    "#3FB3F7", # [1,15)
    "#003737", # [15,40)
    "#268A8A", # [40,60)
    "#eec21f", # [60,80)
    "#A14500" # [80,111)
)

pal_six[c(5,2,4,6,3,1)] %>% test_color()

# save the diagnostics plot
Cairo::CairoPNG( 
    filename = "fig/test-chosen-palette.png", 
    width = 1600, height = 900, res = 300
)
pal_six[c(5,2,4,6,3,1)] %>% test_color()
dev.off()


# color palette for heat maps
bluesN <- colorRampPalette(blues9)
blues100 <- bluesN(100)

blues7 <- blues100[seq(25, 100, length.out = 7)] 

pal_heat_207 <- c("#FFA000", "#FFE0B2", "grey97", blues7)

pal_yll_7 <- c(
    "#F7F7F7", # grey light
    "#E1F5FE", # paletteer_d("ggsci", "light-blu_material", 7) [1:4]
    "#B3E5FC", 
    "#81D4FA", 
    "#4FC3F7",
    "#BA68C8",# paletteer_d("ggsci", "purple_material", 7) [c(4,7)]
    "#8E24AA"
)

# select 4 clours for countries
pal_four <- pal_safe_five_ordered[c(2,5,3,4)]

save(
    file = "dat/palettes.rda",
    pal_five, pal_six, pal_material_5, pal_material_6, pal_safe_five, pal_safe_five_ordered, pal_heat_207, pal_yll_7, pal_four
)

