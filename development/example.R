library(ggplot2)
library(extrafont) # to get all the polices imported well into R
font_import()
extrafont::loadfonts(device="win")

####################### start with barplot #######################

data_tooth <- ToothGrowth
means <- c(mean(data_tooth[data_tooth$dose == 0.5,]$len), mean(data_tooth[data_tooth$dose == 1,]$len), mean(data_tooth[data_tooth$dose == 2,]$len)) 
df <- data.frame(dose=c("D0.5", "D1", "D2"), len=means)
p <- ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", fill = "lightblue", width = 0.3) + expand_limits(y = c(0, 35))
p

############## add the significance bar by hand ##############

# draw the bar
p <- p + geom_path(data = data.frame(x = c(2, 2, 3, 3), y = c(28, 30, 30, 28)), aes(x = x, y = y), size = 1)
p

# add the *
p <- p + annotate("text", x = 2.495, y = 31, label = "*", size = 8)
p


############## add the significance bar "automatically" ##############
# for this example, the bar will be between the first and second bars

# (1) get coordinates for the significance line

# (1-1) get x coordinates
# should be just the mean of the bar

coords = ggplot_build(p)$data[[1]]
xcoord_bar1 = mean(c(coords[1,]$xmin, coords[1,]$xmax))
xcoord_bar2 = mean(c(coords[2,]$xmin, coords[2,]$xmax))
xcoord_bar3 = mean(c(coords[3,]$xmin, coords[3,]$xmax))

# (1-2) get y coordinates
# need 2 values, bottom and top of |
# should be above the highest of the 2 bars for the low value, and the high value should just be a fixed proportion higher than the low

ycoord_bar1 = coords[1,6]
ycoord_bar2 = coords[2,6]
ycoord_bar3 = coords[3,6]

# I guess "above" cannot be a fixed value, but needs to be a proportion depending on the size of the y breaks of the graph

y_range = ggplot_build(p)$layout$panel_ranges[[1]]$y.range
y_sum = sum(abs(y_range))
y_scale = (7.5/100)*y_sum # this will be by how much the bottom of the | will be up the bar
bar_height = y_scale + ((5/100)*y_sum) # this will be the height of the | bar

# finally
ycoord_top = max(ycoord_bar1, ycoord_bar2) # the bar must start above the highest of the two bars
coord_bar = data.frame(x = c(xcoord_bar1, xcoord_bar1, xcoord_bar2, xcoord_bar2), y = c(ycoord_top + y_scale, ycoord_top + bar_height, ycoord_top + bar_height, ycoord_top + y_scale))

# (2) get coordinates for the *
# should be in the middle of the bar for the x coordinate, and just a bit above it for y (using relative "above" again)

star_x = mean(c(xcoord_bar1, xcoord_bar2))
star_y = ycoord_top + bar_height + ((2.75/100)*y_sum)
coord_star = c(star_x, star_y)

# (3) Final

p = p + geom_path(data = coord_bar, aes(x=x, y=y), size = 1) + annotate("text", x = star_x, y = star_y, label = "*", size = 8)
p

####################### DotPlot #######################

data_tooth2 <- ToothGrowth[,-2]
p2 <- ggplot(data = data_tooth2, aes(x = dose, y = len)) + geom_point(color = "lightblue")
p2

############## add the significance bar "automatically" ##############
# for this example, the bar will be between the first and last group

# (1) get coordinates for the significance line

# (1-1) get x coordinates
# should be just the x position of the points

coords = ggplot_build(p2)$data[[1]]
xcoord_bar1 = unique(coords$x)[1]
xcoord_bar2 = unique(coords$x)[2]
xcoord_bar3 = unique(coords$x)[3]

# (1-2) get y coordinates
# need 2 values, bottom and top of |
# should be above the highest of the 2 bars for the low value, and the high value should just be a fixed proportion higher than the low

ycoord_bar1 = max(coords[coords$x == xcoord_bar1,]$y)
ycoord_bar2 = max(coords[coords$x == xcoord_bar2,]$y)
ycoord_bar3 = max(coords[coords$x == xcoord_bar3,]$y)

# I guess "above" cannot be a fixed value, but needs to be a proportion depending on the size of the y breaks of the graph

y_range = ggplot_build(p2)$layout$panel_ranges[[1]]$y.range
y_sum = sum(abs(y_range))
y_scale = (7.5/100)*y_sum # this will be by how much the bottom of the | will be up the bar
bar_height = y_scale + ((5/100)*y_sum) # this will be the height of the | bar

# finally
ycoord_top = max(ycoord_bar1, ycoord_bar3) # the bar must start above the highest of the two groups
coord_bar = data.frame(x = c(xcoord_bar1, xcoord_bar1, xcoord_bar3, xcoord_bar3), y = c(ycoord_top + y_scale, ycoord_top + bar_height, ycoord_top + bar_height, ycoord_top + y_scale))

# (2) get coordinates for the *
# should be in the middle of the bar for the x coordinate, and just a bit above it for y (using relative "above" again)

star_x = mean(c(xcoord_bar1, xcoord_bar3))
star_y = ycoord_top + bar_height + ((2.75/100)*y_sum)
coord_star = c(star_x, star_y)

# (3) Final

p2 = p2 + geom_path(data = coord_bar, aes(x=x, y=y), size = 1) + annotate("text", x = star_x, y = star_y, label = "*", size = 8)
p2
