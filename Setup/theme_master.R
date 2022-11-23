source("Setup/Init.R")

# if the theme does not print the correct font, please uncomment the font_import() command
# font_import()
loadfonts(device = "win", quiet = TRUE)

theme_master <- function(){
  
  # choose same font as Master Thesis main Body
  font <- "Palatino Linotype"
  
  theme_minimal() %+replace%
    
    theme(
      
      # new backbround; before it was grey93
      plot.background = element_rect(fill = "grey99", colour = "white"), 
      
      # get a panel border
      # panel.border = element_rect_round(color = "grey28", fill = NA, ),
      
      # change horizontal lines
      panel.grid.minor.x = element_line(linetype = "solid", color = "grey90", size = .2),
      panel.grid.major.x = element_line(linetype = "solid", color = "grey90", size = .2),
      
      
      # change vertical line type
      panel.grid.minor.y = element_line(linetype = "solid", color = "grey90", size = .2),
      panel.grid.major.y = element_line(linetype = "solid", color = "grey90", size = .2),
      
      # get rounded corners! (devtools::install_github("teunbrand/elementalist"))
      panel.background = element_rect_round(radius = unit(.5, "cm"), fill = NA, size = 1.0, color = "grey61"),
      
      # color axis and remove ticks
      axis.line.x = element_line(size = 1, color = "darkorange2"),
      axis.ticks = element_blank(),
      
      
      # text changes
      plot.title = element_text(
        family = font,
        size = 14,
        face = "bold",
        hjust = 0,
        margin = margin(t = 5)
      ), 
      
      plot.subtitle = element_text(
        family = font,
        face = "italic",
        size = 11,
        hjust = 0,
        margin = margin(t = 2, b = 5)
      ),
      
      
      axis.title = element_text(
        family = font,
        size = 12
      ),
      
      
      axis.text = element_text(
        family = font,
        size = 11,
        color = "grey61"
      ),
      
      
      axis.text.x = element_text(
        margin = margin(5, b = 10)
      ),
      
      
      # legend changes
      legend.title = element_text(
        family = font,
        size = 9,
        face = "bold"
      ),
      
      
      legend.text = element_text(
        family = font,
        size = 8, 
        color = "grey61"
      ),
      
      legend.key = element_rect(
        fill = "grey93",
        color = "gray85"
      ),
      
      legend.position = "bottom",
      
      plot.caption = element_text(
        family = font,
        colour = "grey23", 
        hjust = 1
      ),
      
      complete = T
      
    )
  
}
