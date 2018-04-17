library(circlize) # biblioteca da ferramenta

# Read data ----
setwd("C:/Users/Renan/Desktop/Chord Diagrams")
#setwd("/home/renansantos/Área de Trabalho/Online Algothim tests/OCLR-NSGA-II")
#setwd("/home/renansantos/Área de Trabalho/r100")
d1 <- read.csv("CL_NSGAII.csv", header = FALSE, sep=";")
d2 <- read.csv("CL_NSGAII.csv", header = FALSE, sep=";")
# d1 <- read.csv("cl_nsga.csv", header = FALSE)
# d2 <- read.csv("nsga.csv", header = FALSE)

npop_d1 <- nrow(d1) # número de pontos
nobj_d1 <- ncol(d1) # dimensao
na_d1 <- matrix(data = NA, nrow = npop_d1, ncol = 3)
colnames(na_d1) <- c('norma', 'angulo', 'eixo')

npop_d2 <- nrow(d2) # número de pontos
nobj_d2 <- ncol(d2) # dimensao
na_d2 <- matrix(data = NA, nrow = npop_d2, ncol = 3)
colnames(na_d2) <- c('norma', 'angulo', 'eixo')

c1 <- d1
c2 <- d2
apply(rbind(c1, c2), 2, max) -> colmax2
apply(rbind(c1, c2), 2, min) -> colmin2
colmin2 <- as.numeric(format(colmin2, format = "f", digits = 4))
colmax2 <- as.numeric(format(colmax2, format = "f", digits = 4))


# Transformations ----
# normalization the data on [0,1] interval
if (max(d1, d2) > 1) {
  apply(rbind(d1, d2), 2, max) -> colmax
  apply(rbind(d1, d2), 2, min) -> colmin
  for (i in 1:npop_d1) {
    d1[i, ] <- (d1[i, ] - colmin) / (colmax - colmin)
  }
  for (i in 1:npop_d2) {
    d2[i, ] <- (d2[i, ] - colmin) / (colmax - colmin)
  }
}

# calculate the norm, smallest angle and do the association
for (i in 1:npop_d1) {
  na_d1[i, "norma"] <- norm(d1[i, ], type = "2")
  aux <- acos(d1[i, ] / na_d1[i, "norma"])
  na_d1[i, "angulo"] <- min(aux)
  na_d1[i, "eixo"] <- which.min(aux)
}
for (i in 1:npop_d2) {
  na_d2[i, "norma"] <- norm(d2[i, ], type = "2")
  aux <- acos(d2[i, ] / na_d2[i, "norma"])
  na_d2[i, "angulo"] <- min(aux)
  na_d2[i, "eixo"] <- which.min(aux)
}

# a ferramenta utiliza data frames
na_d1 <- as.data.frame(na_d1)
df_d1 = data.frame(factors = na_d1$eixo,
                   x = na_d1$angulo,
                   y = na_d1$norma)

na_d2 <- as.data.frame(na_d2)
df_d2 = data.frame(factors = na_d2$eixo,
                   x = na_d2$angulo,
                   y = na_d2$norma)





#--------------------------------------------------------------------------------------------------------------------

#  draw the arcs ----
circos.clear()
circos.par(
  gap.after = c(rep(2, nobj_d1 - 1), 10),
  start.degree = 90,
  track.height = 0.05
)
par(new = TRUE) # <- magic
circos.par("canvas.xlim" = c(-2, 2),
           "canvas.ylim" = c(-2, 2))
circos.initialize(factors = rep(1:nobj_d1, length.out = npop_d1),
                  xlim = c(0, min(max(d1, d2), 1)))

circos.track(
  ylim = c(0, 1),
  panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(1.5, "mm"),paste("Obj. ", CELL_META$sector.index))
    circos.text(0.03, CELL_META$cell.ylim[2] + uy(0.5, "mm"),as.character(colmin2[as.numeric(CELL_META$sector.index)]),cex = 0.5)
    circos.text(0.93, CELL_META$cell.ylim[2] + uy(0.5, "mm"),as.character(colmax2[as.numeric(CELL_META$sector.index)]),cex = 0.5)
  },bg.col="red"
)

#--------------------------------------------------------------------------------------------------------------------
for (i in 1:npop_d1) {
  for (j in 1:(nobj_d1 - 1)) {
    for (k in (j + 1):nobj_d1) {
      circos.link(j,
                  as.numeric(d1[i, j]),
                  k,
                  as.numeric((d1[i, k])),
                  col = "blue",
                  lwd = 0.1)
    }
  }
}

#for (i in 1:npop_d2) {
#  for (j in 1:(nobj_d2 - 1)) {
#    for (k in (j + 1):nobj_d2) {
#      circos.link(
#        j,
#        as.numeric(d2[i, j]),
#        k,
#        as.numeric((d2[i, k])),
#        col = "#006400",
#        lty = 4,
#        lwd = 0.1
#      )
#    }
#  }
#}

legend(x=0.8, y=-0.8, legend=c("NSGA-II", "SPEA 2"), fill=c("blue", "#006400"), bty="n")

