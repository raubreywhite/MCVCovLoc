devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
MapCov(
  wdir = 'S:/Data/MCVCovLoc',
  StatusDate = '2020-01-01',
  BirthCohort = 2005,
  dose = 1
  )
Sys.time() - StartTime
