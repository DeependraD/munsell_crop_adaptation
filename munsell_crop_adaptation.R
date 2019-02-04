# Creating a primitive munsell color chart with the help of munsell package while discussing the basics of 
# this system of color specification.

require(tidyverse)
# using munsell library
library(munsell)
library(munsellinterpol)

# To begin with, one can check how the color palette of single hues (A sequential palette) looks like.
# It might also strike as appropriate qualitative description of colorimetric features of plant world.
# This is very plausible and at the same time worthwile to explore too, because sequential colors can
# very well be used in comparative studies.

# HVC/HSV/HCL system of color specfication is probably one of the formats that resonates well with human preception
# In this system three bands represent the color properties of an image: Hue, Chroma (Saturation), Luminance (Value)
# Additionally, another band -- Power -- is also described but this might not be relevant for the Munsell color
# we are exclusively basing ourself in.

# Properties of each of HSV bands can be so kept that pixels can take a range of values (not luminance "Value").
# For example, a has been so implemented that Hue (Henceforth signified by "H" with/without suffix) can be
# anywhere between -360 and +360. An instance of if it is the sequential color palette accessible through R
# function colorspace::choose_palette(). In this particular palette, each band has two representation: H1, H2, 
# C1, C2, ... and so on. But to obtain a sequential palette, P2 is held at 0 and H2 is fixed at 300. 
# The other bands can be adjusted anywhere between the range.

## Munsell color system

# In colorimetry, the Munsell color system is a color space that specifies colors based on three properties of color: hue, value (lightness), and chroma (color purity). 
# It was created by Professor Albert H. Munsell in the first decade of the 20th century and adopted by the USDA as the official color system for soil research in the 1930s.

# Several earlier color order systems had placed colors into a three-dimensional color solid of one form or another, but Munsell was the first to separate hue, value, and chroma into perceptually uniform and independent dimensions, and he was the first to illustrate the colors systematically in three-dimensional space.
# Munsell's system, particularly the later renotations, is based on rigorous measurements of human subjects' visual responses to color, putting it on a firm experimental scientific basis. Because of this basis in human visual perception, Munsell's system has outlasted its contemporary color models, it is still in wide use today [Wikipedia].

# munsell color specification is in format: "H V/C", where H = Hue, V = Value and C = Chroma.
# list of all hues are:
mnsl_hues() # only 40 hues

# V is an integer between 0 and 10 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
# C is an even integer between 0 and 10 (0, 2, 4, 6, 8, 10)

# conversion to hex value
mnsl("5R 5/10")

# simple unlabelled plotting with plot_mnsl_unlab
plot_mnsl_unlab <- function (cols, back.col = "white", ...) 
{
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.", 
         call. = FALSE)
  }
  add.ops <- NULL
  if (length(cols) > 1) {
    add.ops <- list(ggplot2::facet_wrap(~num))
  }
  cols <- check_mnsl(cols)
  cols <- in_gamut(cols, ...)
  df <- data.frame(num = 1:length(cols), names = factor(cols, levels = c(unique(cols))), 
                   hex = mnsl2hex(cols), x = 0, 
                   y = 0, stringsAsFactors = FALSE) %>% 
    mutate(hue = str_extract(names, "\\d+\\.*\\d*\\w"))
    
  df$labels <- factor(df$names, levels = c(unique(cols), "NA"))
  df$labels[is.na(df$labels)] <- "NA"
  ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = hex)) + 
    add.ops + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) + 
    ggplot2::scale_y_continuous(expand = c(0,0)) + 
    ggplot2::coord_fixed() + 
    # theme_munsell(back.col) + 
    ggplot2::scale_fill_identity() + 
    ggplot2::scale_colour_identity() + 
    ggplot2::theme(strip.background = ggplot2::element_blank(), 
                   strip.text = ggplot2::element_blank(), 
                   axis.title = ggplot2::element_blank(), 
                   axis.text = ggplot2::element_blank(), 
                   axis.ticks = ggplot2::element_blank())
}

# plotting (simple) in HCV colorspace
plot.new()
rect(0, 0, 1 ,1, col = mnsl("5R 5/10"))
plot_mnsl("5R 6/10")
plot_mnsl_unlab("5R 6/10") +
  geom_label(aes(label = labels, color = text_colour(as.character(names))), size = 3, nudge_y = -0.4)

# plotting (advanced) in HCV colorspace
p <- expand.grid(c(2.5, 5, 7.5, 10), c("P", "PB", "R"), " ", "5/10") %>% 
  tidyr::unite(paste, sep = "") %>% 
  pull("paste") %>% 
  plot_mnsl_unlab()
p + ggplot2::facet_wrap(~num, ncol = 12)
  
# the above code doesn't work for green because color specification of green 
# hues does not work with given values of V and C

# all possible green hues (combinations of V and C) can be plotted as below
g_hues <- expand.grid(c(2.5, 5, 7.5, 10), c("G"), " ", 0:10, "/", seq(0, 24, by = 2)) %>% 
  tidyr::unite(paste, sep = "", remove = FALSE) %>% 
  rename("pasted_comb" = "paste")

greenness_df <- expand.grid(c("H1", "H2", "H3", "H4"), 
            "Green", 
            c(paste("Dark", 5:1), "Medium bright", paste("Light", 1:5)), 
            c("Fully desaturated", "Highly desaturated", "Mostly desaturated",
              "Desaturated", "Mildly desaturated", "Slightly desaturated", 
              "Medium saturation",
              "Slightly saturated", "Mildly saturated", "Saturated", 
              "Mostly saturated", "Highly saturated", "Fully saturated")) %>% 
  tidyr::unite(paste, sep = " ", remove = FALSE) %>% 
  rename("green_class" = "paste")

g_hues$green_class <- greenness_df$green_class
p <- g_hues %>% 
  mutate(pasted_comb_hex = mnsl(pasted_comb)) %>% 
  na.omit() %>% 
  # arrange(pasted_comb) %>% 
  pull(pasted_comb) %>% 
  # plot_mnsl() %>% 
  plot_mnsl_unlab()

p$data <- p$data %>% 
  left_join(g_hues[, c("pasted_comb", "green_class")], by = c("labels" = "pasted_comb"))

p <- p +
  geom_text(aes(label = str_wrap(green_class, 18), color = text_colour(as.character(names))), size = 1.8, nudge_y = -0.3)
p <- p + 
  ggplot2::facet_wrap(~num, ncol = 12)
# p

ggsave("./g_hues.png", plot = p, width = 14, height = 10, units = "in")

# plotting in most human readable form
my_blue <- "5PB 5/8"
p <- plot_mnsl(c(
  lighter(my_blue, 2),      my_blue,   darker(my_blue, 2),
  desaturate(my_blue, 2),   my_blue,   saturate(my_blue, 2),
  rygbp(my_blue, 2),        my_blue,   pbgyr(my_blue, 2)))
p

# functional plotting of hues at different values (lowest to highest darkness value)
p <- plot_mnsl(sapply(0:6, darker, col = "5PB 7/4"))
p + ggplot2::facet_wrap(~ num, nrow = 1)

# functional plotting of hues at different chromas (highest to lowest chroma saturation)
p <- plot_mnsl(sapply(0:5, desaturate, col = "5PB 7/10"))
p + ggplot2::facet_wrap(~ num, nrow = 1)

# hue slice for given hue
hslice <- function (hue.name = ..., back.col = "white") 
{
  ggplot2::ggplot(ggplot2::aes(x = factor(chroma), y = factor(value)), data = subset(munsell.map, hue %in% hue.name)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col,size = 1) + 
    ggplot2::geom_text(ggplot2::aes(label = name, colour = text_colour(name)), angle = 45, size = 2) + 
    ggplot2::scale_colour_identity() + ggplot2::scale_x_discrete("Chroma") + 
    ggplot2::scale_y_discrete("Value", expand = c(0.125, 0)) + 
    ggplot2::scale_fill_identity() + 
    ggplot2::facet_wrap(~hue)
}


## plotting hue variants of color sample
## all possible green hues (combinations of V and C) can be plotted as below

# color sample plotting: rose red sampling
rgb2mnsl(173, 4, 37)
rgb2mnsl(188, 6, 44)
rgb2mnsl(188, 6, 44) %>% plot_mnsl()

# more accurate representation
RGBtoMunsell(c(188, 6, 44))
RGBtoMunsell(c(173, 4, 37))

hue_slice("7.5R")

r_hues <- expand.grid(c(2.5, 5, 7.5, 10), c("R"), " ", 0:10, "/", seq(0, 24, by = 2)) %>% 
  tidyr::unite(paste, sep = "", remove = FALSE) %>% 
  rename("pasted_comb" = "paste")

redness_df <- expand.grid(c("H1", "H2", "H3", "H4"), 
                            "Red", 
                            c(paste("Dark", 5:1), "Medium bright", paste("Light", 1:5)), 
                            c("Fully desaturated", "Highly desaturated", "Mostly desaturated",
                              "Desaturated", "Mildly desaturated", "Slightly desaturated", 
                              "Medium saturation",
                              "Slightly saturated", "Mildly saturated", "Saturated", 
                              "Mostly saturated", "Highly saturated", "Fully saturated")) %>% 
  tidyr::unite(paste, sep = " ", remove = FALSE) %>% 
  rename("red_class" = "paste")

r_hues$red_class <- redness_df$red_class
p <- r_hues %>% 
  mutate(pasted_comb_hex = mnsl(pasted_comb)) %>% 
  na.omit() %>% 
  # arrange(pasted_comb) %>% 
  pull(pasted_comb) %>% 
  # plot_mnsl() %>% 
  plot_mnsl_unlab()

p$data <- p$data %>% 
  left_join(r_hues[, c("pasted_comb", "red_class")], by = c("labels" = "pasted_comb"))

p <- p +
  geom_text(aes(label = str_wrap(red_class, 18), color = text_colour(as.character(names))), size = 1.6, nudge_y = -0.3)
p <- p + 
  ggplot2::facet_wrap(~num, ncol = 12)
# p

ggsave("./r_hues.png", plot = p, width = 14, height = 10, units = "in")
