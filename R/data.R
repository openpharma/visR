#' Cancer survival data
#'
#' Creation script in data-raw
#'
"brca_cohort"

#' @title adtte - CDISC ADaM compliant time to event data set
#'
#' @description ADTTE data copied from the 2013 CDISC Pilot
#' @source CDISC SDTM/ADAM Pilot Project.
#'   \url{https://github.com/phuse-org/phuse-scripts/tree/master/data}
#'
#' @format A data frame with 254 rows and 26 variables:
#' \describe{
#'  \item{STUDYID}{Study Identifier}
#'  \item{SITEID}{Study Site Identifier}
#'  \item{USUBJID}{Unique Subject Identifier}
#'  \item{AGE}{Age}
#'  \item{AGEGR1}{Pooled Age Group 1}
#'  \item{AGEGR1N}{Pooled Age Group 1 (N)}
#'  \item{RACE}{Race}
#'  \item{RACEN}{Race (N)}
#'  \item{SEX}{Sex}
#'  \item{TRTSDT}{Date of First Exposure to Treatment}
#'  \item{TRTEDT}{Date of Last Exposure to Treatment}
#'  \item{TRTDUR}{Duration of treatment (days)}
#'  \item{TRTP}{Planned Treatment}
#'  \item{TRTA}{Actual Treatment}
#'  \item{TRTAN}{Actual Treatment (N)}
#'  \item{PARAM}{Parameter Description}
#'  \item{PARAMCD}{Parameter Code}
#'  \item{AVAL}{Analysis Value}
#'  \item{STARTDT}{Time to Event Origin Date for Subject}
#'  \item{ADT}{Analysis Date}
#'  \item{CNSR}{Censor}
#'  \item{EVNTDESC}{Event or Censoring Description}
#'  \item{SRCDOM}{Source Domain}
#'  \item{SRCVAR}{Source Variable}
#'  \item{SRCSEQ}{Source Sequence Number}
#'  \item{SAFFL}{Safety Population Flag}
#' }
#' @keywords datasets CDISC adtte
#' @name adtte
#' @examples
#' data("adtte")
"adtte"
