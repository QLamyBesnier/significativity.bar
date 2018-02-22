#' Significativity bar between 2 groups
#'
#' This function allows you to automatically draw a significativity bar between 2 groups in a ggplot
#' @param plot The ggplot on which you want to draw the bar
#' @param groups A vector containing the number (from left to right) of the 2 groups you want to draw the bar between.
#' @param text The text (must be a character string) you want on top of the bar. Defaults to "*".
#' @param text_height By how much the text above the bar should be above it. Should be between 0 and 1. Defaults to 0.0275 which is optimal for stars, 0.04 is advised for text. 
#' @param size_bar The size of the bar. Defaults to 1. 
#' @param color_bar The color of the bar. Defaults to "black".
#' @param size_text The size of the text above the bar. Defaults to 8.
#' @param color_text The color of the text. Defaults to "black".
#' @param font_face The face (italic, bold, ...) of the text. Defaults to 1 (=normal).
#' @param font_style The font familty (times, arial, ...) of the text. Defaults to "Arial". 
#' @param line_type The style (solid, otted, ...) of the bar. Defaults to "solid".
#' @keywords ggplot 
#' @export 
#' @examples
#' significavity_bar(plot = my_plot, group = c(1, 3), text = "**")


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

