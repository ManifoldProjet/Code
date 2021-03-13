#Manifold projet
#rm(list = ls())
library(rgl) 
library(vegan)
library(kernlab)
library(FactoMineR)
library(factoextra)

n <- 1000 
u <- matrix(runif(4 * n), ncol = 2)
v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])

x <- -cos(v) * v
y <- 20 * u[, 2]
z <- sin(v) * v

swissroll <- cbind(x, y , z)
plot3d(swissroll[order(v), ], col = rainbow(n), size = 5)

####Test d'une autre figure
# a <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*cos(v)
# b <- (3 + cos(v/2)*sin(u) - sin(v/2)*sin(2*u))*sin(v)
# c <- sin(v/2)*sin(u) + cos(v/2)*sin(2*u)
# weirdshape <- cbind(a, b, c)
# plot3d(weirdshape, colvar = c, col = rainbow(n), size = 10)

#ACP Classique

res.pca <- prcomp(swissroll, scale = TRUE)
fviz_eig(res.pca)
plot(res.pca$rotation)
plot(res.pca$sdev)
plot(res.pca$x) #ici on peut voir qu'on arrive a aplatir le swissroll


#Kernel ACP
k_acp <- kpca(as.matrix(swissroll), kernel="rbfdot", kpar = 
                    list(sigma = 0.01), th = 1e-4, na.action = na.omit)

plot(eig(k_acp),  xlim = c(0,10))

xkpca_v <- kpca(swissroll, kernel = "vanilladot", kpar = list())

plot(pcv(xkpca_v), col = rainbow(nrow(swissroll)), pch = 10)



#mds classique
swissroll.mds <- cmdscale(dist(swissroll), k = 3, eig = FALSE, add = FALSE, 
                      x.ret = T)

