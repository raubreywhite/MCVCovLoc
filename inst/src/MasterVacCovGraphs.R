devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
VacCovGraphs(
  wdir = 'S:/Data/MCVCovLoc',
  StatusDate = '2020-01-01',
  NUTSlevel = 2
)
Sys.time() - StartTime
