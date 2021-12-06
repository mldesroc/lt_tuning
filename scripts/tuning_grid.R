tuning_grid <- expand.grid(
  maxSegments = c(5,6,7),
  spikeThreshold = c(1, 0.9, 0.8),
  vertexCountOvershoot = c(0,2,3),
  preventOneYearRecovery = c(TRUE, FALSE),
  recoveryThreshold = c(1, 0.75, 0.25),
  pvalThreshold = c(0.05, 0.1, 0.2),
  bestModelProportion = c(1, 1.25, 1.5),
  minObservationsNeeded = c(4, 5, 6, 7),
  delta = "loss",
  sort = 'newest',
  year = '1985',
  mag = c(25, 50, 75),
  dur = c(1, 2, 3, 4),
  preval = c(200, 300, 400),
  mmu = c(4, 7, 10)
)