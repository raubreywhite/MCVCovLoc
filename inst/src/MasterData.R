devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
MCVCovLoc::InputData(
  wdir = 'S:/Data/MCVCovLoc_wdir',
  StatusDate = '2020-01-01',
  IndividuelPopulationFile = "DKIndPop20200101.csv",
  IndividuelMCVFile = 'DKIndVac20200101.csv',
  AggregatedPopulationFile = NA,
  AggregatedMCVFile = NA,
  AdministrativeMCVFile = NA
)
Sys.time() - StartTime
