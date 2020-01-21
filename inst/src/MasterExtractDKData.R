devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
DKIndPop('2020-01-01', 'S:/Data/MCVCovLoc_wdir')
DKIndVac('2020-01-01', 'S:/Data/MCVCovLoc_wdir')
Sys.time() - StartTime
