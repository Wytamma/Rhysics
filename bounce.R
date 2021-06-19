source('~/programming/Rhysics/Rhysics.R')

rad <- 5
xspeed <- 2.5
yspeed <- 1

xdirection <- 1
ydirection <- -1

#S4 Ball Class
setClass("Ball", slots = list(position = "vector", r = "numeric"))

setMethod("plot", "Ball", function(self, x , y) {
  ellipse(self@position[1], self@position[2], self@r, self@r, col = '#3498db')
})

setGeneric("update", function(self)
  standardGeneric("update"))

setMethod("update", "Ball", function(self) {
  xpos <-  self@position[1] + xspeed * xdirection
  ypos <-  self@position[2] + yspeed * ydirection
  self@position <- c(xpos, ypos)
  self
})


setup <- function() {
  xpos <- width / 2

  ypos <- height / 2

  ball <<- new("Ball", position = c(xpos, ypos), r = rad)
}


draw <- function() {
  createCanvas(40, 80)
  plot(ball)
  ball <<- update(ball)
  if (ball@position[1] >= width - rad || ball@position[1] <= rad) {
    xdirection <<- xdirection * -1
  }
  if (ball@position[2] >= height - rad || ball@position[2] <= rad) {
    ydirection <<- ydirection * -1
  }
}

plots <- loop(number_of_frames = 100)
