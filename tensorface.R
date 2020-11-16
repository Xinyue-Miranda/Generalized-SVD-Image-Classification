library(tidyverse)

load("ATTFaces")   # original image
#ATTFaces <- readRDS('shrink_ATTFaces.rds')   # shrinked image

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

#write.csv(Y, 'shrink_ATTFaces_train.csv', row.names = F)
#write.csv(Y_test, 'shrink_ATTFaces_test.csv', row.names = F)

# check
# mean(c(ATTFaces[,,1,1]) == Y[1,])

# centering
Yc <- Y - colMeans(Y)

# SVD on Yc
  

# start with left eigenvectors
YYT <- Yc %*% t(Yc)
dim(YYT)

lambda <- eigen(YYT)$values
D <- diag(sqrt(lambda))
U <- eigen(YYT)$vectors
V <- t(Yc) %*% U %*% diag(1/sqrt(lambda))


# dimension reduction

start_time <- Sys.time()

K  <- 100
Uk <- U[ , 1:K]
Dk <- D[1:K, 1:K]
Vk <- V[ , 1:K]

# approximate
mean((Uk %*% Dk %*% t(Vk) - Yc) ^ 2)


# # plot eigenface
# v1 <- matrix(Vk[,1], nr = nr, nc = nc)
# v2 <- matrix(Vk[,2], nr = nr, nc = nc)
# v3 <- matrix(Vk[,3], nr = nr, nc = nc)
# v4 <- matrix(Vk[,4], nr = nr, nc = nc)
# par(mar = rep(0, 4))
# par(mfrow = c(2,2))
# image(v1, axes = FALSE, col = grey(seq(0, 1, length = 256)))
# image(v2, axes = FALSE, col = grey(seq(0, 1, length = 256)))
# image(v3, axes = FALSE, col = grey(seq(0, 1, length = 256)))
# image(v4, axes = FALSE, col = grey(seq(0, 1, length = 256)))


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


# running time
end_time <- Sys.time()
end_time - start_time
