library(tidyverse)
library(Matrix)
library(pracma)

set.seed(20200202)

###### ----------------------- helper function ----------------------- ######

## (1) matrix norm
matrix_norm <- function(v, A) {
  # check inputs
  stopifnot(
    dim(A)[1] == dim(A)[2], 
    dim(v)[1] == dim(A)[1],
    dim(v)[2] == 1
  )
  
  sqrt( t(v) %*% A %*% v )[1, 1]
}


###### ----------------------- read data ----------------------- ######

load("ATTFaces")
#ATTFaces <- readRDS('shrink_ATTFaces.rds')
dim(ATTFaces)

# split into test and train
Y <- ATTFaces[ , , 1:8, ]
Y_test <- ATTFaces[ , , 9:10, ]

# reshape
nr <- dim(ATTFaces)[1]
nc <- dim(ATTFaces)[2]

dim(Y) <- c(nr * nc, 8* 40)  
Y <- t(Y)
dim(Y_test) <- c(nr * nc, 2 * 40)  
Y_test <- t(Y_test)
dim(Y)
dim(Y_test)

n <- dim(Y)[1]  # number of individuals (in training set) -- 320
p <- dim(Y)[2]  # number of image pixels -- 644



# check
# mean(c(ATTFaces[,,1,1]) == Y[1,])

# centering
Yc <- Y - colMeans(Y)



# ###### ----------------------- Correlation matrix ----------------------- ######
# 
# ###### correlation across rows (individuals)
# 
# # (1) same individual
# cor_same_individual <- c()
# for (i in 1:8) {
#   cor_same_individual <- c( cor_same_individual , 
#                             cor(t( Y[1:8, ] ))[i, -i] %>% mean() )
# }
# mean(cor_same_individual)   # -- 0.5716092
# 
# # (2) different individuals
# cor_diff_individual <- c()
# for (i in 1:8) {
#   cor_diff_individual <- c( cor_diff_individual , 
#                             cor(t( Y[ seq(i, by = 8, length.out = 40), ] ))[1,-1] %>% mean() )
# }
# mean(cor_diff_individual)   # -- 0.3711984

# ## start easy: ignore row correlation at first ##
# 
# 
# ###### correlation across columns (pixels)
# theta <- .95




## start easy
# using sparseMatrix
Phi   <- sparseMatrix(1:n, 1:n, x = rep(1, n))
# Sigma <- sparseMatrix(1:p, 1:p, x = rep(1, p))


mesh <- meshgrid(1:p, 1:p)
xx   <- mesh$X
yy   <- mesh$Y

theta <- .95
Sigma <- theta ^ abs(xx - yy)



###### ----------------------- GMD on Yc ----------------------- ######

K <- 50   # number of principal components


## initialize
Yhat <- Yc
U    <- matrix(nr = n, nc = K)
V    <- matrix(nr = p, nc = K)
d    <- rep(NA, K)

## timing -- start
start_time <- Sys.time()

## iteratively compute K components
for (k in 1:K) {
  
  # print current stage
  cat(paste0('Working on ', k, 'th GMD Principal Componet...\n'))
  
  ## (a) repeat until convergence
  u_new <- rnorm(n)
  v_new <- rnorm(p)
  u_diff <- 100
  v_diff <- 100
  
  eps <- 0.01
  iter <- 1
  max_iter <- 100
  
  while ( (u_diff > eps) & (v_diff > eps) & (iter < max_iter) ) {
    u_old  <- u_new
    u_temp <- Yhat %*% Sigma %*% v_new
    u_new  <- u_temp / matrix_norm(u_temp, Phi)
    u_diff <- sqrt( t(u_new - u_old) %*% (u_new - u_old) )[1,1]
    
    v_old  <- v_new
    v_temp <- t(Yhat) %*% Phi %*% u_new
    v_new  <- v_temp / matrix_norm(v_temp, Sigma)
    v_diff <- sqrt( t(v_new - v_old) %*% (v_new - v_old) )[1,1]
    
    iter = iter + 1
  }
  cat(paste0('... ', iter, ' iterations before convergence \n\n'))
  
  # save uk and vk
  U[ , k] <- as.vector(u_new)
  V[ , k] <- as.vector(v_new)
  
  
  ## (b) Compute and save dk (k-th largest Singular Value)
  dk <- (t(u_new) %*% Phi %*% Yhat %*% Sigma %*% v_new)[1, 1]
  d[k] <- dk 
  
  ## (c) Compute new Yhat
  Yhat <- Yhat - u_new %*% dk %*% t(v_new) 
}

end_time   <- Sys.time()
time_taken <- end_time - start_time
time_taken


# approximate
Uk <- U
Vk <- V
Dk <- diag(d) 
mean((Uk %*% Dk %*% t(Vk) - Yc) ^ 2)


# projection
P = Vk %*% t(Vk)
# Y1_approx <- Yc[1,] %*% P + colMeans(Y)  
# Y1_approx <- matrix(Y1_approx, nr = nr, nc = nc)
# image(Y1_approx, axes = FALSE, col = grey(seq(0, 1, length = 256)))


# compute coordinate for each class
cor <- matrix(NA, nr = K, nc = 40)
for(i in 1:40) {
  Yi <- Yc[(8*i-7):(8*i), ]
  cor[,i] <- rowMeans( t(Vk) %*% t(Yi) )
}

# for a new image input
Ystar <- Yc[1, ]
Ystar_cor <- t(Vk) %*% Ystar
# image(V %*% Ystar_cor, axes = FALSE, col = grey(seq(0, 1, length = 256)))

# distance
distance <- colMeans((cor - matrix(rep(Ystar_cor, 40), nr = K, nc = 40)) ^ 2)
# which.min(distance)


# test
predicted <- rep(NA, 80)
true <- rep(1:40, each = 2)
for (i in 1:80) {
  Ystar <- Y_test[i, ] - colMeans(Y)
  Ystar_cor <- t(Vk) %*% Ystar
  distance <- colMeans((cor - matrix(rep(Ystar_cor, 40), nr = K, nc = 40)) ^ 2)
  predicted[i] = which.min(distance)
}
predicted
mean(predicted == true)

# train
train_predicted <- rep(NA, 320)
train_true <- rep(1:40, each = 8)
for (i in 1:320) {
  Ystar <- Yc[i, ]
  Ystar_cor <- t(Vk) %*% Ystar
  distance <- colMeans((cor - matrix(rep(Ystar_cor, 40), nr = K, nc = 40)) ^ 2)
  train_predicted[i] = which.min(distance)
}
#predicted
mean(train_predicted == train_true)
 

# plot
v1 <- matrix(Vk[,1], nr = nr, nc = nc)
v2 <- matrix(Vk[,2], nr = nr, nc = nc)
v3 <- matrix(Vk[,3], nr = nr, nc = nc)
v4 <- matrix(Vk[,4], nr = nr, nc = nc)
par(mar = rep(0, 4))
par(mfrow = c(2,2))
image(v1, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(v2, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(v3, axes = FALSE, col = grey(seq(0, 1, length = 256)))
image(v4, axes = FALSE, col = grey(seq(0, 1, length = 256)))
