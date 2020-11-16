library(tidyverse)


# read data
load("ATTFaces")
dim(ATTFaces)


# shrink the image by half * half
shrink <- function(img) {
  a = img[ , 1:ncol(img)-1][ , c(TRUE, FALSE)] + img[ , 2:ncol(img)][ , c(TRUE, FALSE)]
  b = a[1:nrow(a)-1,  ][c(TRUE, FALSE), ] + a[2:nrow(a), ][c(TRUE, FALSE), ]
  return(b)
}


# plot the shrinked image for different people
par(mfrow = c(3,4))
for (i in 1:12) {
  img <- ATTFaces[ , , 1, i]
  image(shrink(shrink(img)), axes = FALSE, col = grey(seq(0, 1, length = 256)))
}


# shrink the ATTFaces dataset
shrink_ATTFaces <- array(dim = c(23, 28, 10, 40))
for(i in 1:40) {
  cat(paste('Processing individual'), i, '...\n')
  for(j in 1:10){
    img <- ATTFaces[ , , j, i]
    shrink_ATTFaces[ , , j, i] <- shrink( shrink(img) )
  }
}


# store the shrinked dataset
saveRDS(shrink_ATTFaces, 'shrink_ATTFaces.rds')


# shrink_ATTFaces <- readRDS('shrink_ATTFaces.rds')
par(mfrow = c(3,4))
for (i in 1:3) {
  for (j in 1:4) {
    image(shrink_ATTFaces[ , , j, i], axes = FALSE, col = grey(seq(0, 1, length = 256)))
  }
}




