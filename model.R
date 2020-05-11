

runAndPlot <- function(growthConstant, carryingCapacity, initialSize, length, dt) {
  time <- seq(0, length, dt)
  tumorSize <- runModel(growthConstant, carryingCapacity, initialSize, time)
  
  plot(time, tumorSize, "l")
}

runModel <- function(growthConstant, carryingCapacity, initialSize, time) {
  tumorSize <- list(initialSize)
  
  for (t in 2:length(time)) {
    prevSize <- tumorSize[t-1]
    tumorSize[t] <- as.numeric(prevSize) + dX.dt(growthConstant, carryingCapacity, prevSize)
  }
  
  tumorSize
}

dX.dt <- function(growthConstant, carryingCapacity, size) {
  as.numeric(growthConstant) * as.numeric(log(as.numeric(carryingCapacity) / as.numeric(size))) * as.numeric(size)
}
