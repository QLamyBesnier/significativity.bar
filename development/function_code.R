##### arguments : #####

# REQUIRED ARGUMENTS
# (1) the plot -> must be a ggplot
# (2) the groups between which the significance bar is going to be added -> must be a vector with 2 numbers/names, from left to right (for now)
# could be by number (starting from 1 from left to right) but probably better if it's by name ?
# (3) characters/stars to be written on top of the bar

# FALCULTATIVE / COSMETIC ARGUMENTS
# (4) Comestic arguments for the bar (color, size, style) 
# (5) Comestic arguments for the text (idem)

##### structure : #####
# check installation of packages
# check if everything user entered is OK
# check what type of plot it is 
# do the corresponding bar for the plot


significavity_bar <- function(plot, groups, text = "*", text_height = 0.0275, size_bar = 1, color_bar = "black", size_text = 8, color_text = "black", font_face = 1, font_style = "Arial", line_type = "solid"){
  
  if (!require("ggplot2", character.only=T, quietly=T)){
    install.packages("ggplot2")
    library(ggplot2, character.only=T)
  }
  
  if (class(plot)[1] != "gg"){
    stop("Your input plot is not a ggplot")
  }
  if (length(groups) != 2){
    stop("Please select only 2 groups between which you want the error bar")
  }
  if (!is.vector(groups)){
    stop("Please input your 2 selected groups in a vector")
  }
  if (!is.character(text)) {
    stop("Please input the text above the bar as character")
  }
  if (!is.numeric(text_height) | length(text_height) > 1){
    stop("Please input one numeric value for the text height")
  }
  if (!is.numeric(size_bar) | length(size_bar) > 1){
    stop("Please input one numeric value for the bar size")
  }
  if (!is.character(color_bar)){
    stop("Please input an existing R color, as a character, for the color of the bar")
  }
  if (!is.numeric(size_text) | length(size_text) > 1){
    stop("Please input one numeric value for the text size")
  }
  if (!is.numeric(font_face) | length(font_face) > 1){
    stop("Please input one numeric value for the font face")
  }
  if (!is.character(color_text)){
    stop("Please input an existing R color, as a character, for the color of the text")
  }
  if (!is.character(font_style)){
    stop("Please input an existing font family, as a character, for the color of the bar")
  }
  if (!is.character(line_type)){
    stop("Please input an existing line style, as a character, for the color of the bar")
  }
  
  if (text_height >=1){
    warning("text_height should be between 0 and 1, default value for * and around 0.04 for text are advised")
  }
  
  
  if (class(as.list.environment(plot$layers[[1]])$geom)[1] == "GeomPoint"){
    coords = ggplot_build(plot)$data[[1]]
    xcoords = c()
    ycoords = c()
    for (i in groups){
      xcoord_temp = unique(coords$x)[i]
      xcoords = append(xcoords, xcoord_temp)
    }
    for (i in c(1,2)){
      ycoord_temp = max(coords[coords$x == xcoords[i],]$y)
      ycoords = append(ycoords, ycoord_temp)
    }
    
    y_range = ggplot_build(p)$layout$panel_ranges[[1]]$y.range
    y_sum = sum(abs(y_range))
    y_scale = (7.5/100)*y_sum 
    bar_height = y_scale + ((5/100)*y_sum) 
    
    ycoord_top = max(ycoords) 
    coord_bar = data.frame(x = c(xcoords[1], xcoords[1], xcoords[2], xcoords[2]), y = c(ycoord_top + y_scale, ycoord_top + bar_height, ycoord_top + bar_height, ycoord_top + y_scale))
    
    star_x = mean(xcoords)
    star_y = ycoord_top + bar_height + ((2.75/100)*y_sum)
    coord_star = c(star_x, star_y)
    
    plot = plot + geom_path(data = coord_bar, aes(x=x, y=y), size = 1) + annotate("text", x = star_x, y = star_y, label = text, size = 8)
    print(plot)
    
  } else if (class(as.list.environment(plot$layers[[1]])$geom)[1] == "GeomBar") {
    coords = ggplot_build(plot)$data[[1]]
    xcoords = c()  
    ycoords = c()
    for (i in groups){                                      
      xcoord_temp = mean(c(coords[i,]$xmin, coords[i,]$xmax))
      xcoords = append(xcoords, xcoord_temp)
      ycoord_temp = coords[i,6]
      ycoords = append(ycoords, ycoord_temp)
    }
    
    y_range = ggplot_build(plot)$layout$panel_ranges[[1]]$y.range
    y_sum = sum(abs(y_range))
    y_scale = (7.5/100)*y_sum 
    bar_height = y_scale + ((5/100)*y_sum) 
    
    ycoord_top = max(ycoords) 
    coord_bar = data.frame(x = c(xcoords[1], xcoords[1], xcoords[2], xcoords[2]), y = c(ycoord_top + y_scale, ycoord_top + bar_height, ycoord_top + bar_height, ycoord_top + y_scale))
    
    star_x = mean(xcoords)
    star_y = ycoord_top + bar_height + (text_height*y_sum)
    coord_star = c(star_x, star_y)
    
    plot = plot + geom_path(data = coord_bar, aes(x=x, y=y), size = size_bar, color = color_bar, linetype = line_type) + annotate("text", x = star_x, y = star_y, label = text, size = size_text, color = color_text, fontface = font_face, family = font_style)
    print(plot)
  }
}



# might be problem with polices (worst case : goes back to default with a warning so prolly OK)
# annotate
# write help


# https://stackoverflow.com/questions/5595512/what-is-the-difference-between-require-and-library
