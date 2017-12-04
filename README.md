# ggplot.spaghetti

The goal of ggplot.spaghetti is to aid preliminary data investigation into longitudinal/time-series data through visualization. By being able to create plots using different grouping variable, the investigator can have a better idea which variables may be worthwhile to control or include in a hypothesis test. Also these images can be used to help rely to other non-statistical collaborators the trends from a mixed-effects or other type of longitudinal model. 

## Installation

You can install ggplot.spaghetti from github with:


``` r
# install.packages("devtools")
devtools::install_github("williazo/ggplot.spaghetti")
```

## Example

This is an example using the `Orthodont` data set from the `nlme` package. Children were measured at 8, 10, 12, and 14 years to determine the distance from the pituitary to the pterygomaxillary fissure in millimeters. We can group these time measurements by `Gender`, and I have also created another binary variable, `Race`, to highlight the ability to look at multiple grouping variables at the same time. 

``` r
library(ggplot.spaghetti)
library(nlme)
data("Orthodont")
Orthodont = data.frame(Orthodont, Race = rep(ifelse(rbinom(n = 27, size = 1, prob = 0.5)==0, "White", "Non-White"), each = 4))
attach(Orthodont)

#specifying just group
ortho_plot_group <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                               alpha = 0.3, group = Sex, method = "lm")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Gender", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Gender", values = c("dashed", "solid"))
ortho_plot_group
```
![Graphing trends by age using greyscale](ortho_plot_group.jpg)

``` r
#specifying just wrap
ortho_plot_wrap <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                               alpha = 0.3, wrap = Race, method = "loess")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Race", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Race", values = c("dashed", "solid"))
ortho_plot_wrap
```
![Graphing trends by race using LOESS smoth](ortho_plot_wrap.jpg)

``` r
#specifying both group and wrap
ortho_plot <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                               alpha = 0.3, group = Sex, wrap = Race,
                               method = "glm")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Gender", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Race", values = c("dashed", "solid"))
ortho_plot
```
![Graphing trends by age and race](ortho_plot.jpg)
