# ggplot.spaghetti

The goal of ggplot.spaghetti is to aid preliminary data investigation into longitudinal/time-series data through visualization. By being able to create plots using different grouping variable, the investigator can have a better idea which variables may be worthwhile to control or include in a hypothesis test. Also these images can be used to help rely to other non-statistical collaborators the trends from a mixed-effects or other type of longitudinal model.

## Installation

You can install ggplot.spaghetti from github with:


``` r
# install.packages("devtools")
devtools::install_github("williazo/ggplot.spaghetti")
```

## Technical Notes
- Data needs to be in long format with each row representing one observation per ID
- Variable used to specify `time` in the function must be numeric
- To control the visibility of individual trajectories use `alpha`=[0, 1]
- Plots can be slow for large data with many individuals


## Example

This is an example using the `Orthodont` data set from the `nlme` package. Children were measured at 8, 10, 12, and 14 years to determine the distance from the pituitary to the pterygomaxillary fissure in millimeters. We can group these time measurements by `Gender`, and I have also created another binary variable, `Race`, to highlight the ability to look at multiple grouping variables at the same time.

### Data
Reading in the dataset and creating a random variable to represent a binary racial cateogry as White vs. Non-White.
``` r
library(ggplot.spaghetti)
library(nlme)
data("Orthodont")
Orthodont = data.frame(Orthodont, Race = rep(ifelse(rbinom(n = 27, size = 1, prob = 0.5)==0, "White", "Non-White"), each = 4))
attach(Orthodont)
```
### Graphing group trends in a single plot
In order to look at trends for a categorical variable with overall trends plotted within a single image specify only the `group` option as shown below.
```r
ortho_plot_group <- ggplot_spaghetti(y = distance, id = Subject, time = age,
                               alpha = 0.3, group = Sex, method = "lm")+
  xlab("Age (yrs.)")+
  ylab("Distance")+
  scale_color_grey(name = "Gender", start = 0.0, end = 0.5)+
  scale_linetype_manual(name = "Gender", values = c("dashed", "solid"))
ortho_plot_group
```
![Graphing trends by age using greyscale](https://github.com/williazo/ggplot.spaghetti/blob/master/Images/ortho_plot_group.jpg)

### Graphing group trends across facetted display
In order to look at trends for categorical variables with overall trends plotted in separate plots specify only the `wrap` option as shown below.
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
![Graphing trends by race using LOESS smoth](https://github.com/williazo/ggplot.spaghetti/blob/master/Images/ortho_plot_wrap.jpg)

### Graphing two-way group trends
In order to look at trends for a potential interaction between two categorical variables with overall trends specify both the `group` and `wrap` option as shown below.
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
![Graphing trends by age and race](https://github.com/williazo/ggplot.spaghetti/blob/master/Images/ortho_plot.jpg)
