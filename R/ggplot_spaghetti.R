#'Spaghetti Plots using \code{ggplot2}
#'
#'This function allows the user to create spaghetti plots for individuals with time varying covariates. You can also break this down into subgroups to analyze different trentds.
#'
#'Note that the data must be in long format.
#'
#'@param y This is the y-axis parameter to specify. Generally it is a continuous variable.
#'@param id This is the id parameter that identifies the unique individuals or units.
#'@param time This is the time vector and must be numeric.
#'@param alpha Scalar value between [0,1] that specifies the transparencey of the lineplots.
#'@param method Character value that specifies which type of method to use for fitting. Optional methods come from stat_smooth() funciton.
#'@param jit Scalar value that specifies how much you want to jitter each individual observation. Useful if many of the values share the same y values at a time point.
#'@param group Specifies a grouping variable to be used, and will plot it by color on one single plot.
#'@param wrap Another possible grouping variable, but this will use `facet_wrap()` to create separate plots for each grouping variable.
#'@param scales Specify the scales paremeter for `facet_wrap()` in ggplot2.
#'
#'@return Plots a time series data by each individual/unit with group trends overlayed.
#'
#'@examples
#'library(nlme)
#'data("Orthodont")
#'attach(Orthodont)
#'ortho_plot <- ggplot_spaghetti(distance, Subject, age, alpha = 0.3, group = Sex, method = "lm")+
#'xlab("Age (yrs.)")+
#'ylab("Distance")+
#'scale_color_discrete(name="Gender")
#'ortho_plot
#'
#'@import ggplot2
#'
#'@export
#'
ggplot_spaghetti <- function(y, id, time, alpha = 0.2, method = "loess",
                             jit = 0.0, group = NULL, wrap = NULL, bw = FALSE, scales = NULL){
  #necessary packages
  list.of.packages <- c("ggplot2")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  rm(new.packages, list.of.packages) #removing these objects after I use them

  require(ggplot2)
  if(is.null(scales) == F & is.null(wrap) == T){
    stop("scales parameter can only be specified when using the wrap option.", call. = F)
  }

  #setting the factor variable to 0 when jit equals 0 so that there is no jittering
  if(jit == 0){
    fact = 0
  } else{fact = 1}

  #this first section will product a ggplot without any grouping variables
  if(is.null(group) == T & is.null(wrap) == T){
    gg_dat <- data.frame(y, id, time)
    gg_dat <- subset(gg_dat, !is.na(y))
    ids <- as.character(unique(gg_dat$id))
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      ry<-runif(1, min = 0, max = jit)
      rx<-runif(1, min = 0, max = jit)
      gg_dat_ind <- gg_dat[gg_dat$id == i,]
      gg_dat_ind$time <- jitter(gg_dat_ind$time, factor = fact, amount = rx)
      gg_dat_ind$y <- jitter(gg_dat_ind$y, factor = fact, amount = ry)
      if(nrow(gg_dat_ind) >= 1){
        base <- base + geom_point(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)
      }
      if(nrow(gg_dat_ind) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
        base <- base + geom_line(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y, col = "red", lty = "dashed"), lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
  #Here we specifying just a Group Variable
  else if (is.null(group) == F & is.null(wrap) == T) {
    if(is.factor(group) == F){
      group <- factor(group)
    }
    gg_dat <- data.frame(y, id, time, group)
    gg_dat <- subset(gg_dat, !is.na(y))
    ids <- as.character(unique(gg_dat$id))
    groups <- unique(as.character(gg_dat$group))
    #adding a check to make sure that the number of groups is not too large for the linetype
    if(length(groups)>13){
      gg_dat$lty_group <- as.factor(rep("solid", nrow(gg_dat)))
    } else{
      gg_dat$lty_group <- gg_dat$group
    }
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      for (j in groups){
        ry <- runif(1, min = 0, max = jit)
        rx <- runif(1, min = 0, max = jit)
        gg_dat_grp <- gg_dat[gg_dat$id == i & gg_dat$group == j,]
        gg_dat_grp$time <- jitter(gg_dat_grp$time, factor = fact, amount = rx)
        gg_dat_grp$y <- jitter(gg_dat_grp$y, factor = fact, amount = ry)
        if(nrow(gg_dat_grp) >= 1){
          base <- base +
            geom_point(data = gg_dat_grp, aes(x = time, y = y, col = group, linetype = lty_group), alpha = alpha)
        }
        if(nrow(gg_dat_grp) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
          base <- base +
            geom_line(data = gg_dat_grp, aes(x = time, y = y, col = group, linetype = lty_group), alpha = alpha)
        }
      }
    }
    base <- base +
      stat_smooth(data = gg_dat, aes(x = time, y = y, group = group, col = group, linetype = lty_group),
                  lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
  #no grouping variable and just a facet wrap
  else if (is.null(group) == T & is.null(wrap) == F) {
    if(is.factor(wrap) == F){
      wrap <- factor(wrap)
    }
    gg_dat <- data.frame(y, id, time, wrap)
    gg_dat <- subset(gg_dat, !is.na(y))
    wraps <- unique(as.character(gg_dat$wrap))
    #adding a check to make sure that the number of groups is not too large for the linetype
    if(length(wraps)>13){
      gg_dat$lty_wrap <- as.factor(rep("solid", nrow(gg_dat)))
    } else{
      gg_dat$lty_wrap <- gg_dat$wrap
    }
    ids <- as.character(unique(gg_dat$id))
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      ry<-runif(1, min = 0, max = jit)
      rx<-runif(1, min = 0, max = jit)
      gg_dat_ind <- gg_dat[gg_dat$id == i,]
      gg_dat_ind$time <- jitter(gg_dat_ind$time, factor = fact, amount = rx)
      gg_dat_ind$y <- jitter(gg_dat_ind$y, factor = fact, amount = ry)
      if(nrow(gg_dat_ind) >= 1){
        base <- base + geom_point(data = gg_dat_ind, aes(x = time, y = y, linetype = lty_wrap, col = wrap), alpha = alpha)+
          facet_wrap( ~ wrap, scales = scales)
      }
      if(nrow(gg_dat_ind) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
        base <- base + geom_line(data = gg_dat_ind, aes(x = time, y = y, linetype = lty_wrap, col = wrap), alpha = alpha)+
          facet_wrap( ~ wrap, scales = scales)
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y, linetype = lty_wrap, col = wrap),
                               lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
  #now we are left with the case where both wrap and group are specified
  else {
    if(is.factor(group) == F){
      group <- factor(group)
    }
    if(is.factor(wrap) == F){
      wrap <- factor(wrap)
    }
    gg_dat <- data.frame(y, id, time, group, wrap)
    gg_dat <- subset(gg_dat, !is.na(y))
    ids <- as.character(unique(gg_dat$id))
    groups <- unique(as.character(gg_dat$group))
    wraps <- unique(as.character(gg_dat$wrap))
    #adding a check to make sure that the number of groups is not too large for the linetype
    if(length(wraps)>13){
      gg_dat$lty_wrap <- as.factor(rep("solid", nrow(gg_dat)))
    } else{
      gg_dat$lty_wrap <- gg_dat$wrap
    }
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      for (j in groups){
        ry <- runif(1, min = 0, max = jit)
        rx <- runif(1, min = 0, max = jit)
        gg_dat_grp <- gg_dat[gg_dat$id == i & gg_dat$group == j,]
        gg_dat_grp$time <- jitter(gg_dat_grp$time, factor = fact, amount = rx)
        gg_dat_grp$y <- jitter(gg_dat_grp$y, factor = fact, amount = ry)
        if(nrow(gg_dat_grp) >= 1){
          base <- base + geom_point(data = gg_dat_grp, aes(x = time, y = y, col = group, linetype = lty_wrap), alpha = alpha)+
            facet_wrap( ~ wrap, scales = scales)
        }
        if(nrow(gg_dat_grp) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
          base <- base + geom_line(data = gg_dat_grp, aes(x = time, y = y, col = group, linetype = lty_wrap), alpha = alpha)+
            facet_wrap( ~ wrap, scales = scales)
        }
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y, col = group, linetype = lty_wrap),
                               lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
}
