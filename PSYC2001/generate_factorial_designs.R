## written by K. Garner, 2023
## plot the various ways you can get main effects
library(tidyverse)

#### coffee + stress experiment 
x <- c(1, 2)

plot_me <- function(x, low_stess, hi_stress, title){
  
  stress_cols <- c("#1b9e77", "#7570b3")
  
  plot(x, lo_stress, type="l", lwd=3, xlab="Coffee",
       ylab="Test Result", cex.lab = 1.5,
       col=stress_cols[1], axes=F, ylim=c(30, 70))
  axis(side=1, at=c(1, 2), labels=c("low", "high"), cex.axis=1.5)
  axis(side=2, at=c(30, 40, 50, 60, 70), cex.axis=1.5)
  points(x, hi_stress, type="l", lwd=3, lty=2)
  legend(1, 70, lty=c(1,2), lwd =3, col=stress_cols, legend=c("low", "high"),
         title="stress")
  
  filename <- paste("factorial/", title, ".png")
  dev.copy(png, filename, width = 350, height = 350)
  dev.off()
}

lo_stress <- c(40, 55)
hi_stress <- c(40, 55)
plot_me(x, lo_stress, hi_stress, "ME_coffee")

lo_stress <- c(40, 40)
hi_stress <- c(57, 57)
plot_me(x, lo_stress, hi_stress, "ME_stress")

lo_stress <- c(40, 55)
hi_stress <- c(50, 65)
plot_me(x, lo_stress, hi_stress, "ME_both")

lo_stress <- c(42, 60)
hi_stress <- c(50, 55)
plot_me(x, lo_stress, hi_stress, "interaction")

lo_stress <- c(42, 60)
hi_stress <- c(55, 35)
plot_me(x, lo_stress, hi_stress, "opposites")


### scenario 2 - practice effects 3-way interaction
plot_4_3way <- function(x, fx_a, fx_b, mn, title){
  
  task_cols <- c("#1b9e77", "#7570b3")
  
  plot(x, fx_a, type="l", lwd=3, xlab="Time",
       ylab="Performance", cex.lab = 1.5,
       col=task_cols[1], axes=F, ylim=c(30, 70), main=mn)
  axis(side=1, at=c(1, 2), labels=c("pre", "post"), cex.axis=1.5)
  axis(side=2, at=c(30, 40, 50, 60, 70), cex.axis=1.5)
  points(x, fx_b, type="l", lwd=3, lty=2)
  legend(1, 70, lty=c(1,2), lwd =3, col=task_cols, legend=c("piano", "guitar"),
         title="task")
  
  filename <- paste("factorial/", title, ".png")
  dev.copy(png, filename, width = 350, height = 350)
  dev.off()
}

x <- c(1, 2)
fx_a <-c(50, 65)
fx_b <-c(50, 35)
mn <- "piano practice group"
plot_4_3way(x, fx_a, fx_b, mn, "3way_Pgrp")

fx_a <- c(50, 45)
fx_b <- c(50, 57)
mn <- "crossword puzzels group"
plot_4_3way(x, fx_a, fx_b, mn, "3way_Cgrp")
