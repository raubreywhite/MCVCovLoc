#' Extract Danish individuel population file.
#'
#' @param ExtractDate Date of extraction.
#' @param wdir Working directory.
#' @return ;-seperated file
DKIndPop <- function(ExtractDate, wdir) {
  con <- RODBC::odbcConnect("DKMOMO")
  IndPop <- RODBC::sqlQuery(con, paste0("
    		select cprnr as PersonID, dob as BirthDate,
      		case when substring(cprnr,10,1) in('0', '2', '4', '6', '8') then 'F'
      		     when substring(cprnr,10,1) in('1', '3', '5', '7', '9') then 'M'
      		     end as Gender,
    		  case when region = '1081' then 'DK05'
    		       when region = '1082' then 'DK04'
    		       when region = '1083' then 'DK03'
    		       when region = '1084' then 'DK01'
    		       when region = '1085' then 'DK02'
    		       end as NUTS
    		from IB_DKMOMO.DKMOMO.DKpopulation with(nolock)
    		where (entryd <= '", ExtractDate ,"') and ('", ExtractDate ,"' < exitd) and
    		(dob >= '2005-01-01') and (region in('1081', '1082', '1083', '1084', '1085'))
    		order by cprnr
    "), stringsAsFactors = FALSE, as.is = TRUE)
  RODBC::odbcClose(con)
  rm(con)
  IndPop$ExtractDate <- ExtractDate

  write.table(IndPop[, c("ExtractDate", "PersonID", "BirthDate", "Gender", "NUTS")],
              file = paste0(wdir, "/data/DKIndPop", gsub("-", "", ExtractDate), ".csv"), sep = ";", row.names = FALSE)
}

#' Extract Danish individuel vaccination file.
#'
#' @param ExtractDate Date of extraction.
#' @param wdir Working directory.
#' @import data.table
#' @return ;-seperated file
DKIndVac <- function(ExtractDate, wdir) {
  con <- RODBC::odbcConnect("DDV")
  IndVac <- RODBC::sqlQuery(con, paste0("
      select cpr as PersonID, DateOfBirth as BirthDate,
        case when substring(cpr,10,1) in('0', '2', '4', '6', '8') then 'F'
        	 when substring(cpr,10,1) in('1', '3', '5', '7', '9') then 'M'
        	 end as Gender,
        vaccination_Effectuationdate as VacDate,
        case when VaccineIdentifier = 1617178192 then 'MEA-MUM-RUB'
             when VaccineIdentifier = 1617178190 then 'MEA-MUM-RUB-VAR'
             end as VacType,
        case when VaccineIdentifier = 1617178192 then 'J07BD52'
             when VaccineIdentifier = 1617178190 then 'J07BD54'
             end as ATC,
        case when datediff(day, DateOfBirth, vaccination_effectuationdate) > 1430 then 'D2'
             else 'D1' end as DoseRecorded
      from Ib_EpiDDv.dbo.MFR_SLIM with(nolock)
      where (datepart(year, vaccination_created) >= 2005) and
            (vitalstatus NOT in('90', '70', '80')) and
            (datediff(day, DateOfBirth, vaccination_effectuationdate) > 344) and
            ('2005-01-01' <= DateOfBirth) and (DateOfBirth <= '", ExtractDate, "') and
            ('2005-01-01' <= vaccination_effectuationdate) and (vaccination_effectuationdate <= '", ExtractDate, "')
      order by cpr, vaccination_effectuationdate
  "), stringsAsFactors = FALSE, as.is = TRUE)
  RODBC::odbcClose(con)
  rm(con)
  IndVac <- data.table::setDT(IndVac)[, .SD[.N], keyby = .(PersonID, DoseRecorded)]
  IndVac$ExtractDate <- ExtractDate

  write.table(IndVac[, c("ExtractDate", "PersonID", "BirthDate", "Gender", "VacDate", "VacType", "ATC", "DoseRecorded")],
              file = paste0(wdir, "/data/DKIndVac", gsub("-", "", ExtractDate), ".csv"), sep = ";", row.names = FALSE)
}

# DKIndPop('2020-01-01', 'S:/Data/MCVCovLoc')
# DKIndVac('2020-01-01', 'S:/Data/MCVCovLoc')
