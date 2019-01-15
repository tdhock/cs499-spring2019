library(png)
fashion <- readPNG("fashion-mnist-sprite.png")
plot(0, xlim=0:1, ylim=0:1, asp=1)
## 28 * 3 * 40.
pixels <- 28
first.each <- rep(c(TRUE, FALSE, FALSE), each=pixels)
some <- fashion[first.each,first.each,]
rasterImage(some, 0, 0, 1, 1)
writePNG(some, "fashion-mnist-sprite-some.png")

class.name.vec <- if(TRUE)c( #my guess at class names
  "blouse",
  "pants",
  "sweater",
  "dress",
  "jacket",
  "sandal",
  "top",
  "sneaker",
  "bag",
  "boot") else c(# guesses from friends.
  "blouse", #tops #sport top ete short
  "pants", #pants 
  "sweater",  #gilet manche long
  "dress", #robe
  "jacket",
  "sandal",
  "top",  #hiver long
  "sneaker",
  "bag",
  "boot")

for(class.i in seq_along(class.name.vec)){
  to <- class.i*pixels
  from <- to-pixels+1
  class.name <- class.name.vec[[class.i]]
  out.png <- paste0("fashion-mnist-", class.name, ".png")
  writePNG(some[from:to,1:pixels,], out.png)
}
