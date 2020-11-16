Vk_GMD <- read.csv('foo_100.csv', header = F)
Vk_GMD <- as.matrix(Vk_GMD)


# projection
P = Vk_GMD %*% t(Vk_GMD)
# Y1_approx <- Yc[1,] %*% P + colMeans(Y)  
# Y1_approx <- matrix(Y1_approx, nr = nr, nc = nc)
# image(Y1_approx, axes = FALSE, col = grey(seq(0, 1, length = 256)))


# compute coordinate for each class
cor <- matrix(NA, nr = K, nc = 40)
for(i in 1:40) {
  Yi <- Yc[(8*i-7):(8*i), ]
  cor[,i] <- rowMeans( t(Vk_GMD) %*% t(Yi) )
}

# for a new image input
Ystar <- Yc[1, ]
Ystar_cor <- t(Vk_GMD) %*% Ystar
# image(V %*% Ystar_cor, axes = FALSE, col = grey(seq(0, 1, length = 256)))

# distance
distance <- colMeans((cor - matrix(rep(Ystar_cor, 40), nr = K, nc = 40)) ^ 2)
# which.min(distance)


# test
predicted <- rep(NA, 80)
true <- rep(1:40, each = 2)
for (i in 1:80) {
  Ystar <- Y_test[i, ] - colMeans(Y)
  Ystar_cor <- t(Vk_GMD) %*% Ystar
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
  Ystar_cor <- t(Vk_GMD) %*% Ystar
  distance <- colMeans((cor - matrix(rep(Ystar_cor, 40), nr = K, nc = 40)) ^ 2)
  train_predicted[i] = which.min(distance)
}
#predicted
mean(train_predicted == train_true)





