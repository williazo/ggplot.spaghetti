#black and white version for manuscripts
ggplot_spaghetti_bw <- function(y, id, time, alpha = 0.0, group = NULL, wrap = NULL , method = "loess", jit = 0.0){
  if(!require("ggplot2")) {
    install.packages("ggplot2")
  }
  require(ggplot2)
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
      gg_dat_ind$time <- jitter(gg_dat_ind$time, amount = rx)
      gg_dat_ind$y <- jitter(gg_dat_ind$y, amount = ry)
      if(nrow(gg_dat_ind) >= 1){
        base <- base + geom_point(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)
      }
      if(nrow(gg_dat_ind) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
        base <- base + geom_line(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y),lty = 2, lwd = 2.5, col = "red",
                               method = method, se = FALSE)
    return(base)
  }
  #otherwise if a grouping variable is specified then it will plot each group in separate color
  else if (is.null(group) == F & is.null(wrap) == T) {
    if(is.factor(group) == F){
      group <- factor(group)
    }
    gg_dat <- data.frame(y, id, time, group)
    gg_dat <- subset(gg_dat, !is.na(y))
    ids <- as.character(unique(gg_dat$id))
    groups <- unique(as.character(gg_dat$group))
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      for (j in groups){
        ry <- runif(1, min = 0, max = jit)
        rx <- runif(1, min = 0, max = jit)
        gg_dat_grp <- gg_dat[gg_dat$id == i & gg_dat$group == j,]
        gg_dat_grp$time <- jitter(gg_dat_grp$time, amount = rx)
        gg_dat_grp$y <- jitter(gg_dat_grp$y, amount = ry)
        if(nrow(gg_dat_grp) >= 1){
          base <- base + geom_point(data = gg_dat_grp, aes(x = time, y = y, lty = group), col = "black", alpha = alpha)
        }
        if(nrow(gg_dat_grp) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
          base <- base + geom_line(data = gg_dat_grp, aes(x = time, y = y, lty = group), col = "black", alpha = alpha)
        }
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y, group = group, lty = group),
                               col = "black", lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
  #no grouping variable and just a facet wrap
  else if (is.null(group) == T & is.null(wrap) == F) {
    if(is.factor(wrap) == F){
      wrap <- factor(wrap)
    }
    gg_dat <- data.frame(y, id, time, wrap)
    gg_dat <- subset(gg_dat, !is.na(y))
    ids <- as.character(unique(gg_dat$id))
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      ry<-runif(1, min = 0, max = jit)
      rx<-runif(1, min = 0, max = jit)
      gg_dat_ind <- gg_dat[gg_dat$id == i,]
      gg_dat_ind$time <- jitter(gg_dat_ind$time, amount = rx)
      gg_dat_ind$y <- jitter(gg_dat_ind$y, amount = ry)
      if(nrow(gg_dat_ind) >= 1){
        base <- base + geom_point(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)+
          facet_wrap( ~ wrap)
      }
      if(nrow(gg_dat_ind) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
        base <- base + geom_line(data = gg_dat_ind, aes(x = time, y = y), alpha = alpha)+
          facet_wrap( ~ wrap)
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y),
                               lty = 2, lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
  #assuming that both a group and wrap variable are specified
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
    base <- ggplot() + xlab("") + ylab("")
    for(i in ids){
      for (j in groups){
        ry <- runif(1, min = 0, max = jit)
        rx <- runif(1, min = 0, max = jit)
        gg_dat_grp <- gg_dat[gg_dat$id == i & gg_dat$group == j,]
        gg_dat_grp$time <- jitter(gg_dat_grp$time, amount = rx)
        gg_dat_grp$y <- jitter(gg_dat_grp$y, amount = ry)
        if(nrow(gg_dat_grp) >= 1){
          base <- base + geom_point(data = gg_dat_grp, aes(x = time, y = y, lty = group), col = "black", alpha = alpha)+
            facet_wrap( ~ wrap)
        }
        if(nrow(gg_dat_grp) >= 2){ #I am adding in this condition so that I don't get errros if there is only one data point
          base <- base + geom_line(data = gg_dat_grp, aes(x = time, y = y, lty = group), col = "black", alpha = alpha)+
            facet_wrap( ~ wrap)
        }
      }
    }
    base <- base + stat_smooth(data = gg_dat, aes(x = time, y = y, lty = group),
                               col = "black", lwd = 2.5, method = method, se = FALSE)
    return(base)
  }
}
