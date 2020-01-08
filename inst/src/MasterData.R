devtools::load_all('S:/Data/MCVCovLoc')

StartTime <- Sys.time()
InputData(
  wdir = 'S:/Data/MCVCovLoc',
  StatusDate = '2020-01-01',
  IndividuelPopulationFile = "DKIndPop20200101.csv",
  IndividuelMCVFile = 'DKIndVac20200101.csv',
  AggregatedPopulationFile = NA,
  AggregatedMCVFile = NA,
  AdministrativeMCVFile = NA
)
Sys.time() - StartTime
