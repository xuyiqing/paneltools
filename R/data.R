#' @title gs2020  
#'  
#' @description data from Grumbach and Sahn (2020)
#' which uses district-election year level TSCS data from US House general elections between 1980 and 2012  
#'  
#' @docType data  
#' @name gs2020  
#' @format A dataframe containing 7007 rows and 9 variables  
#' \itemize{
#'   \item cycle time 
#'   \item district_final individual 
#'   \item general_sharetotal_A_all outcome 
#'   \item general_sharetotal_B_all other variable 
#'   \item general_sharetotal_H_all other variable 
#'   \item cand_A_all treatment
#'   \item cand_B_all covariate 1
#'   \item cand_H_all covariate 2
#'   \item dfe id 
#' } 
#' @source Grumbach, Jacob M., and Alexander Sahn. 2020.
#'  “Race and Representation in Campaign Finance.” 
#'  American Political Science Review 114 (1): 206–21. https://doi.org/10.1017/S0003055419000637. 
#' @usage data(paneltools)  
NULL  

#' @title hh2019  
#'  
#' @description an empirical example from Hainmueller and Hopkins (2019), 
#' in which the authors investigate the effects of indirect democracy (versus direct democracy) 
#' on naturalization rates in Switzerland using municipality-year level panel data from 1991 to 2009. 
#'  
#' @docType data  
#' @name hh2019  
#' @format A dataframe containing 22971 rows and 4 variables 
#' \itemize{  
#'   \item bfs id  
#'   \item year time  
#'   \item nat_rate_ord outcome  
#'   \item indirect treatment  
#' }
#' @source Hainmueller, Jens, and Dominik Hangartner. 2019.
#'  “Does Direct Democracy Hurt Immigrant Minorities? Evidence from Naturalization Decisions in Switzerland.” 
#'  American Journal of Political Science 63 (3): 530–47.  
#' @usage data(paneltools)  
NULL
