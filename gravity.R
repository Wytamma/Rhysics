source('~/programming/Rhysics/Rhysics.R')

#S4 Mover Class
createMover <- setClass(
  "Mover",
  slots = list(
    position = "Vector",
    velocity = "Vector",
    acceleration = "Vector",
    mass = "numeric"
  )
)

setMethod("plot", "Mover", function(self, x , y) {
  ellipse(xpos(self), ypos(self), self@mass, self@mass, col = '#3498db')
})

setGeneric("ypos", function(self)
  standardGeneric("ypos"))

setMethod("ypos", "Mover", function(self) {
  self@position@y
})

setGeneric("ypos<-", function(self, value)
  standardGeneric("ypos<-"))

setMethod("ypos<-", "Mover", function(self, value) {
  self@position@y <- value
  validObject(self)
  self
})

setGeneric("xpos", function(self)
  standardGeneric("xpos"))

setMethod("xpos", "Mover", function(self) {
  self@position@x
})

setGeneric("xpos<-", function(self, value)
  standardGeneric("xpos<-"))

setMethod("xpos<-", "Mover", function(self, value) {
  self@position@x <- value
  validObject(self)
  self
})


setGeneric("applyForce", function(self, force)
  standardGeneric("applyForce"))

setMethod("applyForce", "Mover", function(self, force) {
  f <-  force / self@mass
  self@acceleration <- self@acceleration + f
  self
})


setGeneric("update", function(self)
  standardGeneric("update"))

setMethod("update", "Mover", function(self) {
  self@velocity <- self@velocity + self@acceleration
  self@position <- self@position + self@velocity
  self@acceleration <-  self@acceleration * 0
  self
})

setGeneric("checkEdges", function(self)
  standardGeneric("checkEdges"))

setMethod("checkEdges", "Mover", function(self) {
  if (ypos(self) <= self@mass) {
    self@velocity@y <- self@velocity@y * -0.5
    ypos(self) <- self@mass
  }
  self
})

number_of_balls <- 30
width <- 40
height <- 40

setup <- function() {
  balls <- c()
  for (i in 1:number_of_balls) {
    xpos <- width * runif(1, .05, .95)
    ypos <- height * runif(1, .1, 1)

    balls <- c(
      balls,
      createMover(
        position = createVector(x = xpos, y = ypos),
        velocity = createVector(x = 0, y = 0),
        acceleration = createVector(x = 0, y = 0),
        mass = runif(1, 0.1, 2.5)
      )
    )
  }
  balls <<- unlist(balls)
}

draw <- function() {
  createCanvas(width, height, border = TRUE)
  for (i in 1:length(balls)) {
    balls[[i]] <<- update(balls[[i]])
    balls[[i]] <<- checkEdges(balls[[i]])
    gravity <-  createVector(x = 0, y = -0.1 * balls[[i]]@mass)
    balls[[i]] <<- applyForce(balls[[i]], gravity)
    plot(balls[[i]])
  }
}

loop(number_of_frames = 90)

# # Save
# library(animation)
# saveGIF(loop(number_of_frames = 90), movie.name = "gravity.gif", interval = .05)
