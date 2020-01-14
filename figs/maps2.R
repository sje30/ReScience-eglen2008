library(sjedist)
## This file draws the maps; see hor_bdmin.R for the code that
## runs the simulations.

load("fa.Rda")  ## version for paper.
load('fa_csn.Rda')

load("fb.Rda")
load("fc.Rda")



real.sim.plot <- function(fit, name, soma.rad=2, bar,
                          bg.h1="white", bg.h2="black") {
  ## Plot real data (left) and simulation (right)

  ## H1 = open; H2 = filled.
  ##bg.h1 = "white"
  ##bg.h2 = "black"

  plot.biv <- function(t1, t2, w, soma.rad) {
    ## Plot one group of neurons.
    symbols(x=t1[,1], y=t1[,2],
            circles=rep(soma.rad, nrow(t1)), bg=bg.h1,
            inch=F, asp=1, lwd=0.1,
            xlim=w[1:2], ylim=w[3:4],
            xaxt="n", yaxt="n", xlab="", ylab="")
    symbols(x=t2[,1], y=t2[,2],
            circles=rep(soma.rad, nrow(t2)),
            bg=bg.h2, lwd=0.1, inch=F, add=T)
    rect(w[1], w[3], w[2], w[4])
  }

  with(fit$allpar, plot.biv(pts.1, pts.2, w, soma.rad)   )
  segments(bar[1], bar[2], bar[1]+bar[3], bar[2], lwd=2)
  
  with(fit, plot.biv(t1.sim, t2.sim, allpar$w, soma.rad))

  if (!is.null(name))
    title(name)

}

pdf("hor_fieldA.pdf", width=inch(17), height=inch(8.3),
           horiz=F, onefile=F)
par(mar=c(0.1,.1,.1,.1), bty='n', mfrow=c(1,2))
real.sim.plot(fa.fit, name='', soma.rad=5, bar=c(50, 30, 100))
dev.off()


## original file: ~/papers/2007_bihor/figs/maps2.R
