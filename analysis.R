# Enron Analysis

# load packages
library(plyr)
library(tidyverse)
library(lubridate)
library(igraph)
library(matrixcalc)

# read in adjacency matrices
adj.mats <- readRDS("enronAdjMats.RDS")

# read in github functions
source("functinos_needed.R")

# creating a test graph laplacian matrix using one of adjacency matrices
# A <- adj.mats[[29]]
# D <- diag(colSums(A)) # degree matrix
# L = D-A # graph Laplacian matrix: L=D-A
# 
# # standardize graph Laplacian by diving by the trace
# L.std = L/sum(diag(L))
# 
# # creating list of standardized graph Laplacians
# L.std.list = list()
# for (i in 1:length(adj.mats)){
#   A <- adj.mats[[i]]
#   D <- diag(colSums(A))
#   L = D-A
#   L.std.list[[i]] = L/sum(diag(L))
#   print(isSymmetric(L.std.list[[i]]))
# }
# 
# saveRDS(L.std.list, "enronGraphLaplacianMats.RDS")

# Graph from adjacency matrix
# graphA <- graph_from_adjacency_matrix(
#   A,
#   mode = "undirected",
#   weighted = TRUE,
#   diag = FALSE,
#   add.colnames = NULL
# )
# plot(graphA, vertex.size=3, vertex.label=NA)

# spectral decomposition L = UZU'
# spectral <- eigen(L.std)
# U <- spectral$vectors
# lam <- spectral$values
# Z = diag(lam)
# F_a_L = U %*% Z %*% t(U)

# tangent coordinates
# H = F_a_L[2:147,] # Helmert submatrix (https://people.stat.sc.edu/dryden/STAT718A/notes/shape-chap2B.pdf)
# HF_a_LHt = H %*% F_a_L %*% t(H)
# v = vech(HF_a_LHt) # tangent coordinates


# distances between consecutive month graph Laplacian using Euclidean distance
# test between month 1 and 2
# A1 = adj.mats[[1]]
# D1 <- diag(colSums(A1)) # degree matrix
# L1 = D1-A1 # graph Laplacian matrix: L=D-A
# A2 = adj.mats[[2]]
# D2 <- diag(colSums(A2)) # degree matrix
# L2 = D2-A2 # graph Laplacian matrix: L=D-A
# 
# L1.std = L1/sum(diag(L1))
# L2.std = L2/sum(diag(L2))
# 
# L1.std_L2.std = L1.std - L2.std
# dist = sqrt(sum(diag(t(L1.std_L2.std)%*%L1.std_L2.std)))

# computing distances between all consecutive months
dist <- c()
for (i in 2:length(adj.mats)){
  Ai_1 = adj.mats[[i-1]]
  Di_1 = diag(colSums(Ai_1))
  Li_1 = Di_1-Ai_1
  Li_1.std = Li_1/sum(diag(Li_1))
  Ai = adj.mats[[i]]
  Di = diag(colSums(Ai))
  Li = Di-Ai
  Li.std = Li/sum(diag(Li))
  diff = Li_1.std - Li.std
  dist[i-1] = sqrt(sum(diag(t(diff)%*%(diff))))
}

# plot of euclidean distances
dist.df <- data.frame(Month = 2:36,
                      "Euclidean Distance" = as.numeric(dist))
ggplot(dist.df, aes(x=Month, y=Euclidean.Distance)) +
  geom_line() + ylab("Euclidean Distance")

# same thing but now for square root Euclidean distance - returning NA on sqrt of eigenvalues bc too small
# dist.sqrt <- c()
# for (i in 2:length(adj.mats)){
#   Ai_1 = adj.mats[[i-1]]
#   Di_1 = diag(colSums(Ai_1))
#   Li_1 = Di_1-Ai_1
#   Li_1.std = Li_1/sum(diag(Li_1))
#   spectral.i_1 <- eigen(Li_1.std)
#   Ui_1 <- spectral.i_1$vectors
#   lam.i_1 <- sqrt(spectral.i_1$values)
#   Zi_1 = diag(lam.i_1)
#   Li_1.std.sqrt = Ui_1 %*% Zi_1 %*% t(Ui_1)
#   
#   Ai = adj.mats[[i]]
#   Di = diag(colSums(Ai))
#   Li = Di-Ai
#   Li.std = Li/sum(diag(Li))
#   spectral.i <- eigen(Li.std)
#   Ui <- spectral.i$vectors
#   lam.i <- sqrt(spectral.i$values)
#   Zi = diag(lam.i)
#   Li.std.sqrt = Ui %*% Zi %*% t(Ui)
#   diff.sqrt = Li_1.std.sqrt - Li.std.sqrt
#   dist.sqrt[i-1] = sqrt(sum(diag(t(diff.sqrt)%*%(diff.sqrt))))
# }

# testing function lnr
source("lnr.R")
source("kerFctn.R")
L.std.list <- readRDS("enronGraphLaplacianMats.RDS")

enron.stock.prices = c(39.72, 40.97, 44, 41.63, 38.25, 42.31, 37.38,
                       56.38, 68.94, 68.44, 69.75, 76.94, 71.25, 71.88,
                       82.13, 89.44, 80, 80.38, 77.56, 68.44, 77.90, 
                       66.53, 59.44, 56.99, 47.26, 49.12, 40.25, 30.67, 
                       33.17, 9.48, 0.63, 0.12, 0.1, 0.1, 0.1, 0.1)

Xmat = data.frame(month = 1:36,
                  stock.price = enron.stock.prices)
Xmat = as.matrix(Xmat)

lnr.test1 <- lnr(gl = L.std.list, x=1:36)
lnr.test2 <- lnr(gl = L.std.list, x=enron.stock.prices)
lnr.test <- lnr(gl = L.std.list, x=Xmat)

df.resids <- data.frame(month = 1:36,
                        resid = lnr.test$residuals)

ggplot(df.resids, aes(x=month, y=resid)) +
  geom_line() + ylab("Residual") + xlab("Month")

lnr.df <- data.frame(month = 1:36,
                     residual = lnr.test$residuals,
                     stock.price = enron.stock.prices)

max_price <- max(lnr.df$stock.price)
max_residuals <- max(lnr.df$residual)
scaling_factor <- max_price / max_residuals
lnr.df$residuals_scaled <- lnr.df$residual * scaling_factor

ggplot(lnr.df, aes(x=month, y=residual)) + geom_line()

# 2 y-axis plot for residual and stock price
coeff <- 150

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

ggplot(lnr.df, aes(x=month)) +
  geom_line( aes(y=residual), linewidth=2, color=temperatureColor) + 
  geom_line( aes(y=stock.price / coeff), linewidth=2, color=priceColor) +
  scale_y_continuous(
    # Features of the first axis
    name = "Residual",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name=" Stock Price ($)")
  ) + 
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) + xlab("Month")

# overlayed graph of consecutive month distance and residuals
overlayed.df <- data.frame(month = 1:36,
                           distance = c(NA, dist))
overlayed.df2 <- data.frame(month = 1:36,
                           residual = lnr.test1$residuals)

ggplot() + 
  geom_line(data = overlayed.df, aes(x = month, y = distance), color = "blue") +
  geom_line(data = overlayed.df2, aes(x = month, y = residual), color = "red") +
  xlab('Month') 
