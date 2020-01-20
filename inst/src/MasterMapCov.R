devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
MapCov(
  wdir = 'S:/Data/MCVCovLoc_wdir',
  StatusDate = '2019-10-01',
  BirthCohort = 2005,
  dose = 2
  )
Sys.time() - StartTime
