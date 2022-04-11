library("jpeg")
library("tidyverse")

img <- readJPEG("img2.jpg")

# get red, green and blue values from img
r <- img[,,1]
g <- img[,,2]
b <-img[,,3]

img.r.svd <- svd(r)
img.g.svd <- svd(g)
img.b.svd <- svd(b)

img.svds <- list(img.r.svd, img.g.svd, img.b.svd)

for(j in seq.int(3, round(nrow(img)/8, -1), length.out = 8)) {
  a <- sapply(img.svds, function(i) {
    img.compress <- i$u[,1:j] %*% diag(i$d[1:j]) %*% t(i$v[,1:j]) 
    
    #write.csv(img.compress, paste("./svd-decomposition-results/svd-matrix", "_svd_rank_", round(j, 0), ".csv"))  
    
    
  }, simplify = "array")

  writeJPEG(a, paste("./svd-decomposition-results/img-compressed", "_svd_rank_", round(j, 0), 
                     ".jpg", sep=""))
}
