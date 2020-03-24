#' Generate cohort attrition table 
#' 
#' @description This function calculates the subjects counts for each step of the cohort selection.
#'
#' By using the description tag you'll notice that I
#' can have multiple paragraphs in the description section
#' 
#' @param data r table. It is used as the input data to count the subjects that meets the criteria of interest
#' @param criteria_descriptions character vector. It contains the descriptions of the inclusion/exclusion criteria. 
#' Each element of the vector corresponds to the description of each criterion.
#' @param criteria_conditions character vector. It contains the corresponding conditions of the criteria.
#' These conditions will be used in the table to compute the counts of the subjects.
#' @param subject_column_name character. The column name of the table that contains the subject id.
#' @usage vr_attrition_table(data, criteria_descriptions, criteria_conditions, subject_column_name)
#' @return The counts and percentages of the remaining and excluded subjects for each step of the cohort selection in a table format.
#' @details The vector criteria_descriptions must have the same length with the vector criteria_descriptions
#' 
#' @examples
#' # NOT RUN: TODO: This example is throwing an error. Check the issue. 
#' # create 500 patient ids, with 'lung' or 'breast' as cancer type, aged between 5 and 105 years old
#' # calculate how many patients have lung cancer patients and subsequently which of them are at least 18 years old.
#' # cohort <-  dplyr::tibble(patient_id=base::sample(x = 1:1000, size=500), cancer_type=base::sample(c('lung','breast'), 500, replace=T), age=base::sample(x = 5:105, size=500, replace=T))
#' 
#' # Run function
#' #vr_attrition_table(
#' # data                  = cohort, 
#' # criteria_descriptions = c("1. lung cancer diagnosis", "2. Be 18 years of age or older."),
#' # criteria_conditions   = c("cancer_type=='lung'","age>=18"),
#' # subject_column_name   = 'patient_id'
#' #) 
#' @export
vr_attrition_table <- function(
  data, 
  criteria_descriptions, 
  criteria_conditions, 
  subject_column_name){
 

   if ( !subject_column_name %in% names(data) ) {
   
   stop("The 'subject_column_name' argument doesn't correspond to a column name. Please correct the 'subject_column_name' ans re-run the function")
     
  } 
 
   if ( length(criteria_descriptions)!=length(criteria_conditions) ){
     
   stop("Vectors 'criteria_descriptions' and 'criteria_conditions' must have the same length.")
  
  } 
  
 if (!NA %in% criteria_conditions) {
   
   criteria_map <- data.frame(cbind(criteria_descriptions, criteria_conditions), stringsAsFactors=F)
   
   final_cond          <- c()
   person_count_master <- c()
   
   for (each_cond in criteria_map$criteria_conditions) { 
   
   final_cond          <- ifelse(is.null(final_cond), each_cond, paste(final_cond, each_cond, sep=" & "))   
   person_count_temp   <- data %>% filter(eval(parse(text=final_cond))) %>% select(!!subject_column_name) %>% n_distinct
   person_count_master <- c(person_count_master, person_count_temp)
   
   }
   
   if (length(person_count_master)>0) {
     
    count_master_table <- dplyr::tibble("Remaining N"=person_count_master)
    criterion_0 <- dplyr::tibble(criteria_conditions='none', criteria_descriptions='Total cohort size', `Remaining N`=select(data, !!subject_column_name) %>% n_distinct)

    # generate attrition table
    attrition_table <-
    criterion_0 %>% 
      dplyr::bind_rows(cbind(criteria_map, count_master_table)) %>%
      dplyr::mutate(`Remaining %`= 100*`Remaining N`/max(`Remaining N`),
           `Excluded N` = lag(`Remaining N`, n=1L, default=max(`Remaining N`))-`Remaining N`,
           `Excluded %` = 100*`Excluded N`/ max(`Remaining N`)) %>%
       # rename columns
       dplyr::rename(Condition = criteria_conditions,
                     Criteria = criteria_descriptions) %>% 
      # fix formatting
      dplyr::select(Criteria, Condition, dplyr::everything())
      # dplyr::mutate_at(vars(matches(' N')), list(~format(., big.mark=','))) %>%
      # dplyr::mutate_at(vars(matches(' %')), list(~round(., 2)))  
    return(attrition_table)
    
   }}
}


# cohort <-  dplyr::tibble(
#   # create 500 patient ids
#   patient_id=base::sample(x = 1:1000, size=500), 
#  # with 'lung' or 'breast' as cancer type
#   cancer_type=base::sample(c('lung','breast'), 500, replace=T),
#   # aged between 5 and 105 years old
#   age=base::sample(x = 5:105, size=500, replace=T)
# )
 
# Run function

# vr_attrition_table(
# data                  = cohort, 
# criteria_descriptions = c("1. lung cancer diagnosis", "2. Be 18 years of age or older."),
# criteria_conditions   = c("cancer_type=='lung'","age>=18"),
# subject_column_name   = 'patient_id'
# )
