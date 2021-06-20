library(ggplot2)

theme_set(theme_bw())

# Paleta de colores

tmas_colors <- c(
  `dark blue`  = "#151F35",
  `blue`       = "#2C3C5B",
  `grey`       = "#5E738E",
  `green`      = "#0AC5A8",
  `light green`= "#51F3DA",
  `light grey` = "#F5F5F5")

#' Function to extract tmas colors as hex codes
#'
#' @param ... Character names of tmas_colors 
#'
tmas_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (tmas_colors)
  
  tmas_colors[cols]
}

tmas_cols("blue")

ggplot(mtcars, aes(hp, mpg)) +
  geom_point(color = tmas_cols("green"),
             size = 4, alpha = .8)

tmas_palettes <- list(
  `main`  = tmas_cols("dark blue", "light green", "grey"),
  
  `cool`  = tmas_cols("blue", "green"),
  
  `hot`   = tmas_cols("light green", "blue", "light grey"),
  
  `mixed` = tmas_cols("dark blue", "grey", "blue", "light grey", "light green"),
  
  `grey`  = tmas_cols("grey", "dark grey")
)

#' Return function to interpolate a tmas color palette
#'
#' @param palette Character name of palette in tmas_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
tmas_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- tmas_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

tmas_pal("cool")
#> function (n) 
#> {
#>     x <- ramp(seq.int(0, 1, length.out = n))
#>     if (ncol(x) == 4L) 
#>         rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
#>     else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
#> }
#> <bytecode: 0x7fd7e497ba38>
#> <environment: 0x7fd7e497af78>

tmas_pal("cool")(10)



#' Color scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in tmas_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_tmas <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- tmas_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("tmas_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for tmas colors
#'
#' @param palette Character name of palette in tmas_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_tmas <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- tmas_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("tmas_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_color_tmas()

# Color by numeric variable with cool palette
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_color_tmas(discrete = FALSE, palette = "mixed")

# Fill by discrete variable with different palette + remove legend (guide)
ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_tmas(palette = "mixed", guide = "none")

theme_tmas <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major.x = element_blank(),    #strip major gridlines
      panel.grid.major.y = element_line(size = 1,linetype = "dotted"),
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      axis.line = element_line(size = 1, colour = "black"),
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        size = 30,                           #set font size
        face = 'bold',                       #bold typeface
        hjust = 0,                           #left align
        vjust = 2,                           #raise slightly
        colour = "#2C3C5B",
        margin=margin(0,0,0,0)),               
      
      plot.subtitle = element_text(          #subtitle
        size = 14,                           #font size
        hjust = 0,                           #left align
        colour = "#151F35",
        margin=margin(0,0,20,0)),               
      
      plot.caption = element_text(           #caption
        size = 14,                            #font size
        hjust = 1,
        color='#2C3C5B', face="bold"),                          #right align
      
      axis.title = element_text(             #axis titles
        size = 12, face = "bold"),                          #font size
      
      axis.text.y = element_text(face="bold", size=12, margin = margin(0,0,0,0.5, unit = "cm")),
      axis.text.x = element_text(face="bold", size=14, margin = margin(0.2,0,0.1,0, unit = "cm")),
      #axis.title = element_text(face="bold", size =12),
      
      # axis.text = element_text(              #axis text
      #  size = 10),                         #font size
      
      # axis.text.x = element_text(            #margin for axis text
      #   margin=margin(5, b = 4)),
      
      #axis.text.x = element_text(            #margin for axis text
      #  margin=margin(l = 5, r = 4)),
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
      
      plot.background = element_rect(fill = "#FcFcFc"),
      panel.background = element_rect(fill = "#FcFcFc", color = NA),
      panel.border = element_rect(linetype = "dashed", fill = NA, color = NA),
      legend.background = element_rect(fill = "#FcFcFc", color = NA),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.8, l = 0.5, unit = "cm")
    )
}