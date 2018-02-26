# significativity.bar

R package for drawing significativity bars in ggplot. 

Works for dotplots and barplots.

# Installation

This code installs the significativity.bar package from this depository :

```r
install.package("devtools")
library(devtools)

install_github("EvenStar69/significativity.bar/significativity.bar")
library(significativity.bar)
```

# Example

This code provide an example of utilisation how the package to draw a significativity bar in a dotplot.

```r
my_ggplot <- ggplot(data = ToothGrowth[,-2], aes(x = dose, y = len)) + geom_point(color = "lightblue")
my_ggplot

my_ggplot_with_bar <- significativity_bar(plot = my_ggplot, groups = c(1,3), text = "**")
my_ggplot_with_bar
```
