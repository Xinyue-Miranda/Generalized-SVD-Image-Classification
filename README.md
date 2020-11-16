# Generalized-SVD-Image-Classification

## Overview
This is an independent study project where I proposed an innovative generalized SVD method and applied it to image recognition. The purpose of this project is to identify the gap of using SVD for feature extraction from high dimension data, especially image data with complex structures.

## Background
Although SVD is a powerful technology for feature extraction and dimension reduction, the traditional SVD simply ignores structural relationships like spatial and temporal dependency. Therefore, it has poor performance when dealing with imaging and time series. In the paper [A Generalized Least-Square Matrix Decomposition](https://www.tandfonline.com/doi/full/10.1080/01621459.2013.852978?casa_token=sd5-IA-aaHYAAAAA%3AvUkhCA_yMLrq70rOlNOn0XcjWev_h9cTLsVWIWzNw4k9_x-eqKnaKwxcoJS_lvR7jsgfZLhFt-XL)[1], the author Allen introduced a generalized SVD model which shows promising results for borrowing spatial knowledge through using covariance matrices. Though Allen’s paper provides promising results for this generalized tensor decomposition method, few studies have been performed to evaluate applications of this method. 

## Data
The AT&T Face Dataset contains 400 face images of 40 different individuals. 10 images were taken for everyone with different facial expressions and positions. Below are several examples in the dataset.

## Methodologies
Inspired by the paper by Allen, my intention is to identify if introducing the spatial knowledge by using correlation information according to Allen’s methodology will improve the results of image recognition. The idea is to find the best low-rank approximation of the data with spatial information introduced. 
I divide the dataset with 80% to be training set and 20% to be test set. I first perform SVD on the training set and derive 50 eigenfaces, then calculate the coordinates for each of the 40 classes.

## Results
After calculating the coordinates of each image in the eigenface subspace, I can classify a new input image by determining the class label which minimizes the distance from the new image to each known class. The classification accuracy on the training set is 99.38%, while the accuracy on the test set is 76.5%.

## Reference
[1] Allen G I, Grosenick L, Taylor J. A generalized least-square matrix decomposition[J]. Journal of the American Statistical Association, 2014, 109(505): 145-159.
