devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
MCVCovLoc::InputData(
  wdir = 'S:/Data/MCVCovLoc_wdir',
  StatusDate = '2019-10-01',
  IndividuelPopulationFile = "DKIndPop20191001.csv",
  IndividuelMCVFile = 'DKIndVac20191001.csv',
  AggregatedPopulationFile = NA,
  AggregatedMCVFile = NA,
  AdministrativeMCVFile = NA
)
Sys.time() - StartTime
