##################################################################################################
# Developer: Daewook Kwon
# Title: SVDIMAGE
# 
# Description:
# This program requires two arguemnts: 1) An image file (of Array format) 2) Desired number of SV's 
# This program attempts to approximate the given image using the method of SVD; the quality of 
# approximation will become better as you use more number of SV's 
# 
# To demostrate this program, you will need "jepg" library (can be installed through: install.packages("jpeg")) 
#######################################################################################################
rm(list = ls())

## Define the Function: SVDIMAGE

SVDIMAGE = function(Array, numSV){
	stopifnot(!missing(Array), !missing(numSV), class(Array) == "array")
	Mats = list(Array[,,1], Array[,,2], Array[,,3])
	svd.Mats = list()
	Apprx.Mats = list()
	
	for(i in 1:3){
		svd.Mats[[i]] = svd(Mats[[i]])
		Apprx.Mats[[i]] = svd.Mats[[i]]$u[,1:numSV] %*% diag(svd.Mats[[i]]$d[1:numSV],ncol=numSV) %*% t(svd.Mats[[i]]$v[,1:numSV])
		Apprx.Mats[[i]][Apprx.Mats[[i]] >= 1] = 1; Apprx.Mats[[i]][Apprx.Mats[[i]] <= 0] = 0
	}
	
	ReArray = array(c(Apprx.Mats[[1]], Apprx.Mats[[2]], Apprx.Mats[[3]]), dim = dim(Array))
	ratio = dim(img)[1]/dim(img)[2] 
	plot(1:2, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', ann = FALSE, asp = ratio)
	rasterImage(ReArray, 1, 1, 2, 2)
	text(1.5, 1.1, paste(numSV, "SV's"), col = "white", cex = 3)
	invisible(ReArray)
}

#######################################################################################################
##Demonstration

## Read the Image
require("jpeg")
img = readJPEG(system.file("img", "rubberduck1.jpg", package = "jpeg"))

## Run Our Function!!
windows(8, 12); layout(matrix(1:6, ncol = 2, nrow = 3))
par(oma = c(1, 1, 1, 1), mar = c(0, 0, 0, 0))
ratio = dim(img)[1]/dim(img)[2]
plot(1:2, type = 'n', xaxt = 'n', yaxt = 'n', bty = 'n', ann = FALSE, asp = ratio)
rasterImage(img, 1, 1, 2, 2); text(1.5, 1.1, "Original", cex = 3, col = "white")
SVDIMAGE(img, 1); SVDIMAGE(img, 5); SVDIMAGE(img, 10); SVDIMAGE(img, 30); SVDIMAGE(img, 100)

