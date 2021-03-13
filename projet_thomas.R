#Generation des donnees---

## creation 3d sphere
library("ggm")
n <- 1000
sphere <- rsphere(n,3)
save(sphere,file='sphere.RData')

library("rgl")
plot3d(sphere[order(sphere[,1]),],col = rainbow(n))
plot(sphere[order(sphere[,1]),],col = rainbow(n), pch = 19, asp = 1)


## Swiss roll
n <- 1000 # Random position on the parameteric domain.
u <- matrix(runif(2 * n), ncol = 2)
v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
x <- -cos(v) * v
y <- 20 * u[, 2]
z <- sin(v) * v
swissroll <- cbind(x, y , z)
plot3d(swissroll[order(v), ], col = rainbow(n), size = 10)


## helix 

t = seq(0, 40*pi, by=pi/50)
#       VARIABLES
R=3;# MAJOR RADIUS
r=1;# MINOR RADIUS
n=6;# No. of loops

xt = (R + r*cos(n*t))*cos(t)
yt = r*sin(n*t)
zt = (R + r*cos(n*t))*sin(t)
scatter3D(xt,yt,zt, theta = 15, phi = 20, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
helix <- cbind(xt, yt, zt)
plot3d(helix[order(zt), ],col=rainbow(n))
### 6) helix in a torus ####
n = 10000
u = seq(0, 40*pi, by=pi/30)
v = u * 0.1 #number of loops in the torus
a=3
b=1
x=(a + (b * cos(u)) ) * cos(v) 
y=(a + (b * cos(u)) ) * sin(v) 
z= (b * sin(u)) 
#labels
print(length(z))
print(length(z[z<0.5]))

scatter3D(x,y,z, theta = 100, phi = 30, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
scatter3D(x,y,z, theta = 100, phi = 100, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
helix_in_torus <- cbind(x, y, z)
plot3d(helix_in_torus[order(x), ],col=rainbow(n))
#reduction de dimension


## perform LLE ----
library("lle")

## Pour la sphere
# Calcul le nb de voisins optimaux : prendre le rho le plus petit
calc_k( sphere, m=2, kmin=1, kmax=20 )

library(dimRed)
# https://www.rdocumentation.org/packages/dimRed/versions/0.2.3/topics/LLE-class

emb_sphere_lle <- embed(sphere, "LLE", ndim=2, knn=16)
plot(emb_sphere_lle@data@data[order(sphere[,1]),], col =rainbow(n))

## Swiss roll
# Calcul le nb de voisins optimaux : prendre le rho le plus petit
kppv <- calc_k( swissroll, m=2, kmin=26, kmax=35 )
# on prends k = 35
emb_swiss_lle <- embed(swissroll, "LLE", ndim=2, knn=35)
plot(emb_swiss_lle@data@data[order(swissroll[,1]),], col =rainbow(n))

## helix
# Calcul le nb de voisins optimaux : prendre le rho le plus petit
kppv <- calc_k(helix, m=2, kmin=3, kmax=15)
# on choisi k=3 maix rho tres eleve : pas ouf
emb_helix_lle <- embed(helix, "LLE", ndim=2, knn=3)
plot(emb_helix_lle@data@data[order(helix[,1]),], col =rainbow(n))


## helix thorus
# Calcul le nb de voisins optimaux : prendre le rho le plus petit
kppv <- calc_k(helix_in_torus, m=2, kmin=3, kmax=15)
# on choisi k=6
emb_helix_thorus_lle <- embed(helix_in_torus, "LLE", ndim=2, knn=6)
plot(emb_helix_thorus_lle@data@data[order(helix_in_torus[,1]),], col =rainbow(n))


# evaluation ---
### sphere
#### cophenetic correlation

cr <- cophenetic_correlation(emb_sphere_lle)
print(cr)
# 0 : pas de corre, 1 : corre forte positive
#### AUC : proche de 1 bonne repre 
#library(coRanking)
auc <- AUC_lnK_R_NX(emb_sphere_lle)

### Swiss roll
#### cophenetic correlation

cr <- cophenetic_correlation(emb_swiss_lle)
# 0 : pas de corre, 1 : corre forte positive
#### AUC : proche de 1 bonne repre 
#library(coRanking)
auc <- AUC_lnK_R_NX(emb_swiss_lle)


## helix
#### cophenetic correlation

cr <- cophenetic_correlation(emb_helix_lle)
# 0 : pas de corre, 1 : corre forte positive
#### AUC : proche de 1 bonne repre 
#library(coRanking)
auc <- AUC_lnK_R_NX(emb_helix_lle)
# mauvaise repre du lle pour helix


## helix thorus --
#### cophenetic correlation

cr <- cophenetic_correlation(emb_helix_thorus_lle)
# 0 : pas de corre, 1 : corre forte positive
#### AUC : proche de 1 bonne repre 
#library(coRanking)
auc <- AUC_lnK_R_NX(emb_helix_thorus_lle)
# bien meilleur repre du lle pour helix thorus




