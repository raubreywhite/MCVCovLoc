#' Check, combine and prepare input data.
#' Create NUTS0 coverage graphs
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividuelPopulationFile Individual input ;-separated population file
#' @param IndividuelMCVFile Individual input ;-separated vaccination file
#' @param AggregatedPopulationFile Aggregated input ;-separated population file
#' @param AggregatedMCVFile Aggregated input ;-separated vaccination file
#' @param AdministrativeMCVFile Administrative input ;-separated vaccination file
#' @import data.table
#' @return Create a subdirectory in \code{wdir}
#' @example
#' \dontrun{
#' InputData(
#'    wdir = 'S:/Data/MCVCovLoc_wdir',
#'    StatusDate = '2020-01-01',
#'    IndividuelPopulationFile = "DKIndPop20200101.csv",
#'    IndividuelMCVFile = 'DKIndVac20200101.csv',
#'    AggregatedPopulationFile = NA,
#'    AggregatedMCVFile = NA,
#'    AdministrativeMCVFile = NA
#'    )
#' }
InputData <- function(wdir, StatusDate,
                      IndividuelPopulationFile = NA,
                      IndividuelMCVFile = NA,
                      AggregatedPopulationFile = NA,
                      AggregatedMCVFile = NA,
                      AdministrativeMCVFile = NA) {

  # wdir = 'S:/Data/MCVCovLoc_wdir'
  # StatusDate = '2020-01-01'
  # IndividuelPopulationFile = "DKIndPop20200101.csv"
  # IndividuelMCVFile = 'DKIndVac20200101.csv'
  # AggregatedPopulationFile = NA
  # AggregatedMCVFile = NA
  # AdministrativeMCVFile = NA

  if (!is.na(IndividuelPopulationFile) & !is.na(IndividuelMCVFile)) {
    IndPop <- IndPopData(wdir, StatusDate, IndividuelPopulationFile)
    AggPop <- data.table::setDT(IndPop)[, .(count = .N),
                                        keyby = .(nuts, birthyear, gender)]
    IndVac <- IndVacData(wdir, StatusDate, IndividuelMCVFile)
    IndVac <- merge(IndVac, IndPop[, c("personid", "nuts")], by = "personid", all = FALSE)
    AggVac <- data.table::setDT(IndVac)[, .(count = .N),
                                        keyby = .(nuts, birthyear,
                                                  vacagemonth = TimeMonth(birthdate, vacdate), gender, doserecorded)]
  } else {
    if (!is.na(IndividuelPopulationFile) & !is.na(AggregatedMCVFile)) {
      IndPop <- IndPopData(wdir, StatusDate, IndividuelPopulationFile)
      AggPop <- data.table::setDT(IndPop)[, .(count = .N),
                                          keyby = .(nuts, birthyear, gender)]
      AggVac <- AggVacData(wdir, StatusDate, AggregatedMCVFile)
    } else {
      if (!is.na(AggregatedPopulationFile) & !is.na(AggregatedMCVFile)) {
        AggPop <- AggVacData(wdir, StatusDate, AggregatedPopulationFile)
        AggVac <- AggVacData(wdir, StatusDate, AggregatedMCVFile)
      } else {
        stop("No input data")
      }
    }
  }

  ### Coverage data by NUTS, BirthYear and month of age at vaccination ###
  VacCov <- function(wdir, StatusDate, AggPop, AggVac) {
    VacMonth <- merge(data.table::setDT(AggPop)[, .(N = sum(count)), keyby = .(nuts, birthyear)],
                      data.table::setDT(AggVac)[, .(VacD1 = sum(count*(doserecorded == 'D1')), VacD2 = sum(count*(doserecorded == 'D2'))),
                                                keyby = .(nuts, birthyear, vacagemonth)],
                      by = c('nuts', 'birthyear'), all = TRUE)
    VacMonth <- data.table::setDT(VacMonth)[, cVacD1 := cumsum(VacD1), by = .(nuts, birthyear)]
    VacMonth <- data.table::setDT(VacMonth)[, cVacD2 := cumsum(VacD2), by = .(nuts, birthyear)]
    return(VacMonth)
  }

  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))) {
    dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate)))
  }
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData"))) {
    dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData"))
  }
  for (f in c(IndividuelPopulationFile, IndividuelMCVFile, AggregatedPopulationFile, AggregatedMCVFile, AdministrativeMCVFile)) {
    if (!is.na(f)) {
      file.copy(from = paste0(wdir, "/data/", f), to = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/InputData/", f),
                overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
    }
  }
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  saveRDS(AggPop, file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/AggPop.RDS"))
  saveRDS(AggVac, file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/AggVac.RDS"))
  saveRDS(VacCov(wdir, StatusDate, AggPop, AggVac), file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output"))) {
    dir.create(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output"))
  }

  VacCovGraphs(wdir, StatusDate, 0)
}

#' Read and prepare individual population input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividuelPopulationFile Individual input ;-separated population file
#' @return IndPop
IndPopData <- function(wdir, StatusDate, IndividuelPopulationFile) {
  if (!file.exists(paste0(wdir, "/data/", IndividuelPopulationFile))) {
    # Check if exist
    stop(paste0("NOT FOUND: Individual population file ", wdir, "/data/", IndividuelPopulationFile))
  }
  # Read
  IndPop <- read.csv(paste0(wdir, "/data/", IndividuelPopulationFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndPop) <- tolower(colnames(IndPop))

  if (sum(colnames(IndPop) %in% c("personid", "birthdate", "nuts")) != 3) {
    stop(paste0("ERROR: The variables PersonID, BirthDate or NUTS not in Individual population file: ", paste(colnames(IndPop), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(IndPop)) {
    if (!is.character(IndPop[, v])) { IndPop[, v] <- as.character(IndPop[, v]) }
  }

  if (sum(is.na(IndPop$personid)) > 0) {
    stop("ERROR: Missing PersonID")
  }

  if (sum(is.na(as.Date(IndPop$birthdate))) >0) {
    stop("ERROR: In Birthdate")
  }
  if (sum(IndPop$birthdate > StatusDate) > 0) {
    stop("ERROR: Birthdate after StatusDate")
  }
  IndPop$birthyear <- as.numeric(format(as.Date(IndPop$birthdate),'%Y'))
  if (sum(IndPop$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (nrow(IndPop[duplicated(IndPop[, c("personid", "birthdate")]),]) > 0) {
    stop("ERROR: Duplicated PersonID, BirthDate")
  }

  if (sum(!(IndPop$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(IndPop$nuts)) != max(nchar(IndPop$nuts))) {
    IndPop$nuts <- substr(IndPop$nuts, 1, min(nchar(IndPop$nuts)))
  }

  return(IndPop)
}

#' Read and prepare individual vaccination input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param IndividuelMCVFile Individual input ;-separated vaccination file
#' @import data.table
#' @return IndVac
IndVacData <- function(wdir, StatusDate, IndividuelMCVFile) {

  if (!file.exists(paste0(wdir, "/data/", IndividuelMCVFile))) {
    stop(paste0("NOT FOUND: Individual vaccination file ", wdir, "/data/", IndividuelMCVFile))
  }

  # Read
  IndVac <- read.csv(paste0(wdir, "/data/", IndividuelMCVFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndVac) <- tolower(colnames(IndVac))

  if (sum(colnames(IndVac) %in% c("personid", "birthdate", "vacdate", "doserecorded")) != 4) {
    stop(paste0("ERROR: The variables PersonID, BirthDate, VacDate or DoseRecorded not in Individual vaccination file: ", paste(colnames(IndVac), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(IndVac)) {
    if (!is.character(IndVac[, v])) { IndVac[, v] <- as.character(IndVac[, v]) }
  }

  if (sum(is.na(IndVac$personid)) > 0) {
    stop("ERROR: Missing PersonID")
  }

  if (sum(is.na(as.Date(IndVac$birthdate))) >0) {
    stop("ERROR: In Birthdate")
  }
  if (sum(IndVac$birthdate > StatusDate) > 0) {
    stop("ERROR: Birthdate after StatusDate")
  }
  IndVac$birthyear <- as.numeric(format(as.Date(IndVac$birthdate),'%Y'))
  if (sum(IndVac$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }

  if (nrow(IndVac[!(IndVac$doserecorded %in% cbind("D1", "D2")),]) > 0) {
    stop("ERROR: Wrong or missing doserecorded")
  }
  if (nrow(IndVac[duplicated(IndVac[, c('personid', 'doserecorded')]),]) > 0) {
    stop("ERROR: The same recorded dose more times")
  }

  IndVac$vacdate <- as.Date(IndVac$vacdate)
  if (nrow(IndVac[is.na(IndVac$vacdate),]) > 0) {
    stop("ERROR: Wrong or missing VacDate")
  }
  if (nrow(IndVac[(IndVac$birthdate > IndVac$vacdate),]) > 0) {
    stop("ERROR: VacDate before BirthDate")
  }
  if (nrow(IndVac[(IndVac$vacdate > StatusDate),]) > 0) {
    stop("ERROR: VacDate after StatusDate")
  }

  if (nrow(data.table::setDT(IndVac)[order(personid, vacdate), .SD[2], by = personid][doserecorded == 'D1']) > 0) {
    stop("ERROR: Dose 1 after dose 2")
  }

  return(IndVac)
}

#' Read and prepare aggregated population input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param AggregatedPopulationFile Aggregated input ;-separated population file
#' @return AggPop
AggPopData <- function(wdir, StatusDate, AggregatedPopulationFile) {

  if (!file.exists(paste0(wdir, "/data/", AggregatedPopulationFile))) {
    stop(paste0("NOT FOUND: Individual vaccination file ", wdir, "/data/", AggregatedPopulationFile))
  }

  # Read
  AggPop <- read.csv(paste0(wdir, "/data/", AggregatedPopulationFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndVac) <- tolower(colnames(IndVac))

  if (sum(colnames(AggPop) %in% c("nuts", "birthyear", "count")) != 3) {
    stop(paste0("ERROR: The variables NUTS, BirthYear or Count not in Aggregated population file: ", paste(colnames(AggPop), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(AggPop)) {
    if (!is.character(AggPop[, v])) { AggPop[, v] <- as.character(AggPop[, v]) }
  }

  if (sum(AggPop$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (AggPop$birthyear > as.numeric(format(as.Date(StatusDate),'%Y'))) {
    warning("Warning: BirthYear after year of StatusDate. These are excluded")
    AggPop <- subset(AggPop, birthyear <= as.numeric(format(as.Date(StatusDate),'%Y')))
  }

  if (sum(!(AggPop$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(AggPop$nuts)) != max(nchar(AggPop$nuts))) {
    AggPop$nuts <- substr(AggPop$nuts, 1, min(nchar(AggPop$nuts)))
  }

  return(AggPop)
}

#' Read and prepare aggregated vaccination input.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param AggregatedMCVFile Aggregated input ;-separated vaccination file
#' @return AggVac
AggVacData <- function(wdir, StatusDate, AggregatedMCVFile) {

  if (!file.exists(paste0(wdir, "/data/", AggregatedMCVFile))) {
    stop(paste0("NOT FOUND: Individual vaccination file ", wdir, "/data/", AggregatedMCVFile))
  }

  # Read
  AggVac <- read.csv(paste0(wdir, "/data/", AggregatedMCVFile), header = TRUE, sep = ';', as.is = TRUE)
  # Change all column names to lower case
  colnames(IndVac) <- tolower(colnames(IndVac))

  if (sum(colnames(AggVac) %in% c("nuts", "birthyear", "vacagemonth", "doserecorded", "count")) != 5) {
    stop(paste0("ERROR: The variables NUTS, BirthYear, VacAgeMonth, DoseRecorded or Count not in Aggregated vaccination file: ", paste(colnames(AggVac), collapse = ", ")))
  }

  # Set all variables to character
  for (v in colnames(AggVac)) {
    if (!is.character(AggVac[, v])) { AggVac[, v] <- as.character(AggVac[, v]) }
  }

  if (nrow(AggVac[!(AggVac$doserecorded %in% cbind("D1", "D2")),]) > 0) {
    stop("ERROR: Wrong or missing doserecorded")
  }

  if (sum(AggVac$birthyear < 2005) > 0) {
    stop("ERROR: BirthYear before 2005")
  }
  if (AggVac$birthyear > as.numeric(format(as.Date(StatusDate),'%Y'))) {
    warning("Warning: BirthYear after year of StatusDate. These are excluded")
    AggVac <- subset(VacPop, birthyear <= as.numeric(format(as.Date(StatusDate),'%Y')))
  }

  if (sum(!(AggVac$nuts %in% readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS"))$NUTS_ID)) > 0) {
    stop("ERROR: Unknown NUTS")
  }
  if (min(nchar(AggVac$nuts)) != max(nchar(AggVac$nuts))) {
    AggVac$nuts <- substr(AggVac$nuts, 1, min(nchar(AggVac$nuts)))
  }

  return(AggVac)
}

#' Calculate time-difference in month.
#'
#' @param StartDate Starting date
#' @param EndDate End date
#' @return time-diff in month
TimeMonth <- function(StartDate, EndDate) {
  StartDate <- as.Date(StartDate)
  EndDate <- as.Date(EndDate)
  m <- 12*(as.numeric(format(EndDate, '%Y')) - as.numeric(format(StartDate, '%Y'))) +
    (as.numeric(format(EndDate, '%m')) - as.numeric(format(StartDate, '%m'))) -
    (as.numeric(format(EndDate, '%d')) < as.numeric(format(StartDate, '%d')))
  return(m)
}

#' Create coverage graphs at NUTS-level.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param NUTSlevel NUTS level
#' @import data.table
#' @import ggplot2
#' @return Graphs
#' @example
#' \dontrun{
#' VacCovGraphs(
#'   wdir = 'S:/Data/MCVCovLoc',
#'   StatusDate = '2020-01-01',
#'   NUTSlevel = 2
#' )
#' }
VacCovGraphs <- function(wdir, StatusDate, NUTSlevel) {

  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    stop(paste0("ERROR: Directory don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  if (!file.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))) {
    stop(paste0("ERROR: Data don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }

  VacMonth <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))

  VacMonth$nuts <- substr(VacMonth$nuts, 1, NUTSlevel + 2)
  VacMonth <- data.table::setDT(VacMonth)[, .(N = sum(N), cVacD1 = sum(cVacD1), cVacD2 = sum(cVacD2)), keyby = .(nuts, birthyear, vacagemonth)]
  VacMonth$CovD1 <- 100*VacMonth$cVacD1/VacMonth$N
  VacMonth$CovD2 <- 100*VacMonth$cVacD2/VacMonth$N

  VacMonth$birthyear <- as.factor(VacMonth$birthyear)

  # library(ggplot2)

  for (n in unique(VacMonth$nuts)) {

    text <- subset(readRDS(paste0(system.file("ShapeFile", package="MCVCovLoc"), "/NutsNames.RDS")), NUTS_ID == n)$NUTS_NAME

    ### Coverage by vac-age in months and BirthYear ###
    pdata <- subset(VacMonth, (nuts == n) & (vacagemonth <= 150))
    # Dose 1
    CovMonthD1 <- ggplot(pdata, aes(x = vacagemonth)) +
      geom_line(aes(y = CovD1, group = birthyear, linetype = birthyear)) +
      ggtitle(paste(text, "- D1 coverage")) + theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Age in months") + ylab("Percent vaccinated") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
      scale_x_continuous(breaks = c(seq(0, 150, by = 6))) +
      labs(caption = paste("Status", StatusDate))
    suppressWarnings(
      ggsave(file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovMonthD1_", n, ".png"), CovMonthD1, width = 30, height = 21, units = "cm")
    )
    # Dose 2
    CovMonthD2 <- ggplot(pdata, aes(x = vacagemonth)) +
      geom_line(aes(y = CovD2, group = birthyear, linetype = birthyear)) +
      ggtitle(paste(text, "- D2 coverage")) + theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Age in months") + ylab("Percent vaccinated") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
      scale_x_continuous(breaks = c(seq(0, 150, by = 6))) +
      labs(caption = paste("Status", StatusDate))
    suppressWarnings(
      ggsave(file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovMonthD2_", n, ".png"), CovMonthD2, width = 30, height = 21, units = "cm")
    )
    ### Coverage by BirthYear ###
    # Dose 1
    CovBirthCohortD1 <- ggplot(data.table::setDT(VacMonth)[nuts == n, .SD[.N], by = birthyear]) +
      geom_bar(aes(x = birthyear, y = CovD1), stat = "identity") +
      xlab("Birth year") + ylab("Percent vaccinated") +
      ggtitle(paste(text, "- D1 coverage")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(caption = paste("Status", StatusDate))
    suppressWarnings(
      ggsave(file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovBirthCohortD1_", n, ".png"), CovBirthCohortD1, width = 30, height = 21, units = "cm")
    )
    # Dose 2
    CovBirthCohortD2 <- ggplot(data.table::setDT(VacMonth)[nuts == n, .SD[.N], by = birthyear]) +
      geom_bar(aes(x = birthyear, y = CovD2), stat = "identity") +
      xlab("Birth year") + ylab("Percent vaccinated") +
      ggtitle(paste(text, "- D2 coverage")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(caption = paste("Status", StatusDate))
    suppressWarnings(
      ggsave(file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/CovBirthCohortD2_", n, ".png"), CovBirthCohortD2, width = 30, height = 21, units = "cm")
    )
  }
}

#' Create coverage map at NUTS-level by Birth Cohort.
#'
#' @param wdir Working directory.
#' @param StatusDate Date of status calculation.
#' @param BirthCohort Birth year of cohort
#' @param dose First or second dose
#' @import data.table
#' @import sf
#' @import raster
#' @import tmap
#' @return map
#' @example
#' \dontrun{
#' MapCov(
#'   wdir = 'S:/Data/MCVCovLoc',
#'   StatusDate = '2020-01-01',
#'   BirthCohort = 2005,
#'   dose = 1
#' )
#' }
MapCov <- function(wdir, StatusDate, BirthCohort, dose) {

  if (!dir.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))) {
    stop(paste0("ERROR: Directory don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data"))
  }
  if (!file.exists(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))) {
    stop(paste0("ERROR: Data don't exist: ", wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  }

  X <- readRDS(paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Data/NUTSVacMonth.RDS"))
  X$CovD1 <- 100*X$cVacD1/X$N
  X$CovD2 <- 100*X$cVacD2/X$N
  X <- setDT(X)[birthyear == BirthCohort, .SD[.N], by = nuts]

  # library(sf)
  # library(raster)
  # library(tmap)
  tmap_options(show.messages = FALSE)
  NUTS_spdf <- rgdal::readOGR(system.file("ShapeFile", package="MCVCovLoc"), "NUTS_RG_01M_2016_4326", verbose = FALSE)
  # map1 <- merge(NUTS_spdf, X, by.x = "NUTS_ID", by.y = "nuts", duplicateGeoms = TRUE) # duplicateGeoms to include area without data
  map1 <- merge(NUTS_spdf, X, by.x = "NUTS_ID", by.y = "nuts", all = FALSE)

  tmap_mode("plot") # Fixed map
  # tmap_mode("view") # Zoomable map

  map <- tm_shape(map1) +
    tm_borders() +
    tm_fill(col = paste0("CovD",dose), breaks = c(0, 50, seq(80, 90, by = 5), seq(90, 100, by = 1)), palette = "Blues") +
    tm_text("NUTS_NAME", size = 0.7) +
    tm_layout(title = paste("Birth cohort", BirthCohort)) +
    tm_layout(bg.color = "lightblue", earth.boundary = TRUE, space.color="grey90") +
    tm_credits(paste("Status", StatusDate), align = "left")

  tmap_save(map, file = paste0(wdir, "/Status_", gsub("-", "", StatusDate), "/Output/map_BirthCohort", BirthCohort, "_D", dose, ".png"),
            width = 1920, height = 1080, asp = 0, verbose = )
}
