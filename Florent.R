# packages ####
install.packages("dimRed")
install.packages("coRanking")
install.packages('optimx')
install.packages('energy')
library(optimx)
library(energy)
library(rgl) 
library(vegan)
library(plot3D)
library(rgl)
library(Rtsne)
library(dimRed)
library(coRanking)
# I. Datasets ####
## I.A. Datasets synthétiques ####
### 1) swiss roll ####
#library(rgl) 
#library(vegan)
#library(plot3D)
n <- 1000 # Random position on the parameteric domain.
u <- matrix(runif(2 * n), ncol = 2)
v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
x <- -cos(v) * v
y <- 20 * u[, 2]
z <- sin(v) * v
swissroll <- cbind(x, y , z)
plot3d(swissroll[order(v), ], col = rainbow(n), size = 10)
#http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization
scatter3D(x,y,z, theta = 15, phi = 20, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")


### 2) helix ####
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
### 2bis) helix in a torus ####
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
### 3) Sphere Thomas ####
#hypersphere
install.packages("BiocManager")
library(BiocManager)
BiocManager::install("graph")
install.packages("ggm")
library("ggm")
install.packages("rgl")
library(rgl)
n <- 1000
sphere <- rsphere(n,3)
#plot3d(sphere[order(sphere[,1]),],col = rainbow(n))
#plot3D(sphere[order(sphere[,1]),],col = rainbow(n))
#scatter3D(sphere[order(sphere[,1]),],col = rainbow(n))
#scatter3D(sphere,col = rainbow(n))
## I.B. Datasets IRL ####
### 1) Mnist #### 
setwd("~/Documents/Manifold Learning/Manifold_Projet/Manifold_Learning_Projet")
all    <- as.matrix(read.table("data.txt"))
labels <- read.table("labels.txt", colClasses = 'integer')
# II. Algorithmes ####
## II.A. KACP (Cintia) ####
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
swissroll.mds <- cmdscale(dist(swissroll), k = 3, eig = FALSE, add = FALSE, x.ret = T)
## II.B. lle (Thomas) ####
library("lle")
# perform LLE
results <- lle( X=sphere, m=3, k=10, reg=2, ss=FALSE, id=TRUE, v=0.9 )
str( results )
# plot results and intrinsic dimension (manually)
plot( results$Y[order(sphere[,1]),],col=rainbow(n), main="embedded data", xlab=expression(y[1]), ylab=expression(y[2]) )
plot( results$id, main="intrinsic dimension", type="l", xlab=expression(x[i]), ylab="id", lwd=2 )
## II.C. t-SNE (Florent) ####
install.packages("Rtsne")
library(Rtsne)
### 1) tSNE sur swissRoll ####
fit_swissroll <- Rtsne(swissroll,               # données
                       pca = FALSE,           # initialisation
                       perplexity = 100,       # paramètre à regler
                       theta = 0.0)           # acceleration de l'algorithme
print(fit_swissroll)
plot(fit_swissroll$Y[order(swissroll[,3]),], col = jet.col(100), pch = 19)

### 2) tSNE sur helix ####
set.seed(1)
fit_helix <- Rtsne(helix,               # données
                   pca = FALSE,           # initialisation
                   perplexity = 30,       # paramètre à regler
                   theta = 0.0, # acceleration de l'algorithme
                   check_duplicates = FALSE)           
print(fit_helix)
plot(fit_helix$Y, col = jet.col(100), pch = 19)
### 2bis) tSNE sur helix in torus ####
fit_helix_in_torus <- Rtsne(helix_in_torus,               # données
                            pca = FALSE,           # initialisation
                            perplexity = 50,       # paramètre à regler
                            theta = 0.0,
                            check_duplicates = FALSE)           # acceleration de l'algorithme
print(fit_helix_in_torus)
plot(fit_helix_in_torus$Y[order(helix_in_torus[,3]),], col = jet.col(100), pch = 19)
### 3) t-SNE sphere ####
fit_sphere <- Rtsne(sphere,               # données
                    pca = FALSE,           # initialisation
                    perplexity = 30,       # paramètre à regler
                    theta = 0.0,
                    check_duplicates = FALSE) 
print(fit_sphere)
plot(fit_sphere$Y, col = rainbow(n), pch = 19)
### 4) t-SNE sur un échantillon de 1000 images de MNIST ####
fit_MNIST <- Rtsne(all,               # données
                   pca = FALSE,           # initialisation
                   perplexity = 30,       # paramètre à regler
                   theta = 0.0,
                   check_duplicates = FALSE) 
print(fit_MNIST)fl
plot(fit_MNIST$Y, color = rainbow(n), pch = 19)

# III. Comparaison méthodes de réduction de dimension ####
library(dimRed)
library(coRanking)
## load test data set
data_swissroll <- loadDataSet("Swiss Roll", n = 1000)
data_helix <- loadDataSet("helix", n = 1000)
data_sphere <- loadDataSet("Sphere", n = 1000)
RDD_apply <- function(dataset){
  embed_methods <- c("tSNE", "LLE", "kPCA")
  ## apply dimensionality reduction
  data_emb <- lapply(embed_methods, function(x) embed(dataset, x))
  names(data_emb) <- embed_methods
  ## figure \ref{fig:plotexample}a, the data set
  #plot(dataset, type = "3vars")
  ## figures \ref{fig:plotexample}b (Isomap) and \ref{fig:plotexample}d (PCA)
  lapply(data_emb, plot, type = "2vars")
  ## figure \ref{fig:plotexample}c, quality analysis
  #plot_R_NX(data_emb)
  return(data_emb)
}
data_emb_swiss = RDD_apply(data_swissroll)
data_emb_helix = RDD_apply(data_helix)
data_emb_sphere = RDD_apply(data_sphere)
data_emb_mnsit= RDD_apply(all)

## III.A. Co-ranking matrix (Florent) ####
coranking_analysis <- function(dataset, data_emb_method){
  Q = coranking(
    dataset@data,
    data_emb_method@data@data,
    input_Xi = "data",
    input_X = "data",
    use = "C"
  )
  imageplot(
    Q = Q,
    lwd = 2,
    bty = "n",
    main = "co-ranking matrix",
    xlab = expression(R),
    ylab = expression(Ro),
    col = colorRampPalette(colors = c("gray85", "red", "yellow", "green", "blue"))(100),
    axes = FALSE,
    legend = TRUE,
  )
}
corank_analysis_methods <- function(dataset, data_emb){
  data_emb_methods = c(data_emb$tSNE, data_emb$LLE, data_emb$kPCA)
  lapply(data_emb_methods, function(x) coranking_analysis(dataset, x))
}
### coranking swissroll ####
corank_analysis_methods(data_swissroll, data_emb_swiss)
### coranking helix  ####
corank_analysis_methods(data_helix, data_emb_helix)
### coranking sphere ####
corank_analysis_methods(data_sphere, data_emb_sphere)
### coraning mnist ####
corank_analysis_methods(all, data_emb_mnsit)
coranking_analysis_mnist <- function(dataset, data_emb_method){
  Q = coranking(
    dataset,
    data_emb_method@data@data,
    input_Xi = "data",
    input_X = "data",
    use = "C"
  )
  imageplot(
    Q = Q,
    lwd = 2,
    bty = "n",
    main = "co-ranking matrix",
    xlab = expression(R),
    ylab = expression(Ro),
    col = colorRampPalette(colors = c("gray85", "red", "yellow", "green", "blue"))(100),
    axes = FALSE,
    legend = TRUE,
  )
}
corank_analysis_methods_mnist <- function(dataset, data_emb){
  data_emb_methods = c(data_emb$tSNE, data_emb$LLE, data_emb$kPCA)
  lapply(data_emb_methods, function(x) coranking_analysis_mnist(dataset, x))
}
corank_analysis_methods_mnist(all, data_emb_mnsit)

mode(all) = "double"
Q_mnist_tsne = coranking(
  all,
  data_emb_mnsit$tSNE@data@data,
  input_Xi = "data",
  input_X = "data",
  use = "C"
)
Q_mnist_lle = coranking(
  all,
  data_emb_mnsit$LLE@data@data,
  input_Xi = "data",
  input_X = "data",
  use = "C"
)
Q_mnist_kpca = coranking(
  all,
  data_emb_mnsit$kPCA@data@data,
  input_Xi = "data",
  input_X = "data",
  use = "C"
)
vect_Q_mnist = c(Q_mnist_tsne, Q_mnist_lle, Q_mnist_kpca )
for(Q in vect_Q_mnist){
  imageplot(
    Q = Q,
    lwd = 2,
    bty = "n",
    main = "co-ranking matrix",
    xlab = expression(R),
    ylab = expression(Ro),
    col = colorRampPalette(colors = c("gray85", "red", "yellow", "green", "blue"))(100),
    axes = FALSE,
    legend = TRUE,
  )
}
## III.B. Q_global et Q_local (Cintia) ####
## III.C. AUC + Cophenetic correlation (Thomas) ####
## III.D. Reconstruction error Bonus####

#  calcul critères de qualité #### 
calcul_quality = function(emb_data){
  for(emb in emb_data){
    print(emb)
    for (q in dimRedQualityList()[-c(7,8)] ){
      print(q)
      res = quality( emb , q)
      print(res)
    }
  }
  
}
calcul_quality_mnist = function(emb_data){
  for(emb in emb_data){
    print(emb)
    for (q in dimRedQualityList()[-c(5,7,8)] ){
      print(q)
      res = quality( emb , q)
      print(res)
    }
  }
  
}


calcul_quality(data_emb_swiss)
calcul_quality(data_emb_helix)
calcul_quality(data_emb_sphere)
calcul_quality_mnist(data_emb_mnsit)

                    