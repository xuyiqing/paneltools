#' @title matching 
#'  
#' @description \code{matching} generate the episode or cohort according to the tuning parameter  
#'  
#' @param data A long form panel, can be missing or un-balanced
#' @param Y name of outcome
#' @param D name of treatment
#' @param X name of covariates
#' @param index name of id and time
#' @param c tuning parameter of carryover
#' @param a tuning parameter of pre-treatment
#' @param b tuning parameter of post-treatment
#' @param type "episode" or "cohort"
#'  
#' @return \code{matching} return a list containing the wide panel, true id and unit, 
#' tuning parameters, and each episode/cohort. 
#' @author Hongyi Jiang <hyjiang2017@nsd.pku.edu.cn> 
#'  
#' @export
matching <- function(data, # data in long form
    Y, # outcome
    D, # treatment
    X = NULL, # covariates
    index, # unit and time
    c, # number of carryover effect period 
    a, # number of pre-treatment period, lag
    b, # number of post-treatment period, lead
    type = "episode" # 2 options, "episode" by default: for each unit i time t, separately estimate
    # "cohort": for each fixed time t, allow multiple treatment unit i
    ){
    
    # library the packages needed outside match
    # library(tidyr)
    # library(dplyr)
    # library(rlang)
    
    ## store variable names
    varY <- Y
    varD <- D
    varX <- X
    varInd <- index[1];
    varTime <- index[2];
    
    ## check balanced panel
    id_list <- unique(data[[varInd]])[order(unique(data[[varInd]]))] # variable for unit, ordered 
    time_list <- unique(data[[varTime]])[order(unique(data[[varTime]]))] # variable for time, ordered
    
    TT <- length(time_list) # length of time
    N <- length(id_list) # length of unit
    ldata <- length(data[[varInd]]) # length of the initial panel, it balanced, should equal to N*T
    
    if (N*TT == ldata){ # The panel is balanced.
      balanced_data <- data %>%  
        arrange(!!!syms(c(varInd,varTime))) # first sort by T, then sort by N, so that it is convenient for reshape as a T-by-N wide panel
    } else{ # The panel is un-balanced.
      balanced_data <- data %>%  
        complete(!!!syms(c(varInd,varTime))) # first sort by T, then sort by N, so that it is convenient for reshape as a T-by-N wide panel
    }
    
    ## reshape the data from long to wide
    if (is.null(X)){ # The panel does not include covariates.
      Ylong <- as.matrix(balanced_data[, varY], nrow = N*TT, ncol = 1) # long panel for Y
      Dlong <- as.matrix(balanced_data[, varD], nrow = N*TT, ncol = 1) # long panel for D
      
      Ywide <- matrix(Ylong,nrow = TT, ncol = N) # wide panel for Y
      Dwide <- matrix(Dlong,nrow = TT, ncol = N) # wide panel for D
    } else{ # The panel includes covariates.
      Ylong <- as.matrix(balanced_data[, varY], nrow = N*TT, ncol = 1) # long panel for Y
      Dlong <- as.matrix(balanced_data[, varD], nrow = N*TT, ncol = 1) # long panel for D
      Xlong <- as.matrix(balanced_data[, varX], nrow = N*TT, ncol = length(varX)) # long panel for X
      
      Ywide <- matrix(Ylong,nrow = TT, ncol = N) # wide panel for Y
      Dwide <- matrix(Dlong,nrow = TT, ncol = N) # wide panel for D
      Xwide <- list()  
      for (p in 1:length(varX)) {  
        Xwide[[paste0("Xwide", p)]] <- matrix(Xlong[, p], nrow = TT, ncol = N)  
      } # an empty list, since the array cannot be a data, we use the list to store Xwide 
    }
    
    ## check whether there is missing value in Y, D and X according to different values of c, a, and b
    
    if (c + a + b + 1 > TT) {
      stop("Length of \"period\" should be no more than T!")
    } # else {print("The period is valid.")}
    
    ## generate the non-missing dummy matrix, where 1 denotes non-missing 
    non_missing = matrix(0, nrow = TT, ncol = N)
    if (is.null(X)){ # The panel does not include covariates.
      for (t in (c+a+1):(TT-b)){
        for (i in 1:N){
          if (all(!is.na(Ywide[(t-c-a):(t+b), i])) & # whether missing in Y
              all(!is.na(Dwide[(t-c-a):(t+b), i])) # whether missing in D
          ){
            non_missing[t, i] <- 1 # if Y and D not missing, we temporarily denote 1
          } 
        }
      }
    } else{ # The panel includes covariates.
      for (t in (c+a+1):(TT-b)){
        for (i in 1:N){
          if (all(!is.na(Ywide[(t-c-a):(t+b), i])) & # whether missing in Y
              all(!is.na(Dwide[(t-c-a):(t+b), i])) # whether missing in D
          ){
            non_missing[t, i] <- 1 # if Y and D not missing, we temporarily denote 1
          } 
          for (p in length(Xwide)){
            if (any(is.na(Xwide[[p]][(t-c-a):(t+b), i])) # whether missing in p-th dimension of X
            ){
              non_missing[t, i] <- 0 # if exist missing, back to 0
            } 
          }
        }
      }
    }
    
    # get the valid treated unit it
    Treated = matrix(0, nrow = TT, ncol = N)
    for (t in (c+a+1):(TT-b)){
      for (i in 1:N){
        if (non_missing[t, i] == 1 & # no missing value for unit i in time t
            Dwide[t, i] == 1 & # unit i is treated at time t
            all(Dwide[(t-c-a):(t-1), i] == 0) # unit i in not treated before time t
        ){
          Treated[t, i] <- 1
        } else {Treated[t, i] <- 0}
      }
    }
    
    # generate the treated unit it vector
    Treated_long <- matrix(Treated, nrow = TT*N, ncol = 1) # reshape to long vector
    time_long <- matrix(as.matrix(replicate(N, c(1:TT))), nrow = TT*N, ncol = 1) # time candidate
    unit_long <- matrix(as.matrix(t(replicate(TT, c(1:N)))), nrow = TT*N, ncol = 1) # unit candidate
    treat_id <- cbind(unit_long[(Treated_long==1), ], time_long[(Treated_long==1), ]) # generate episodes
    
    if (nrow(treat_id) == 0) {
      stop("No Treated Unit is Valid")
    } # else(print("There exists valid treated units."))
    
    # for each treated unit, get its valid control unit
    Control <- matrix(0, nrow = N, ncol = nrow(treat_id))
    for (j in 1:nrow(treat_id)){ # treat_id[j,2] denote the treatment time for this episode
      for (i in 1:N){
        if (non_missing[treat_id[j, 2], i] == 1 & # no missing value for unit i in time t
            all(Dwide[(treat_id[j, 2]-c-a):(treat_id[j, 2]+b), i] == 0) # unit i is not treated at all
        ){
          Control[i, j] <- 1
        }
      }
    }
    Control <- t(Control)
    
    # combine the treated unit it and its valid control units
    # the control unit is denoted by id i, since t is pinned down by the treated unit
    # for a certain treated unit it, if there is no valid control unit, delete it
    
    # update the treated id from sorted proxy to actual id
    # treat_id_true <- cbind(id_list[treat_id[, 1]],time_list[treat_id[, 2]])
    matching_set <- as.data.frame(cbind(treat_id, Control))
    colnames(matching_set) = c("ind", "time", as.character(matrix(id_list, nrow = 1, ncol = N)))
    
    # change the matrix of matching set to a list
    if (type == "episode"){
      episode <- list()
      if (is.null(X)){
        episode[["wide panel"]] <- list(
          Ywide = Ywide,
          Dwide = Dwide,
          id = varInd,
          time = varTime,
          Yname = varY,
          Dname = varD
        )
      } else{
        episode[["wide panel"]] <- list(
          Ywide = Ywide,
          Dwide = Dwide,
          Xwide = Xwide,
          id = varInd,
          time = varTime,
          Yname = varY,
          Dname = varD,
          Xname = varX
        )
      }
      episode[["matrix of match"]] <- list(
        M_match = matching_set
      )
      episode[["unit and time"]] <- list(
        unit = id_list,
        time = time_list
      )
      episode[["tuning parameters"]] <- list(
        carryover = c,
        pre_treatment = a,
        post_treatment = b
      )
      for (i in 1:dim(matching_set)[1]){
        INDtreat <- as.vector(matching_set[i, 1])
        INDtime <- as.vector(matching_set[i, 2])
        INDcontrol <- as.vector(matching_set[i, -1:-2])
        # treat <- as.character(id_list[INDtreat])
        treat <- as.character(INDtreat)
        # time <- as.character(time_list[INDtime])
        time <- as.character(INDtime)
        # control <- as.character(matrix(id_list, nrow = 1, ncol = length(INDcontrol)))[(INDcontrol==1)]
        control <- as.character(matrix(1:length(INDcontrol),nrow=1,ncol=length(INDcontrol)))[(INDcontrol==1)]
        episode[[paste0("episode", i)]] <- list(
          treated_unit = treat,
          treated_time = time,
          control_units = control)
      }
      return(episode)
    } else if (type == "cohort"){
      group <- split(matching_set[, 1], matching_set[, 2]) 
      B <- unique(matching_set$time)[order(unique(matching_set$time))]
      cohort <- list()
      if (is.null(X)){
        cohort[["wide panel"]] <- list(
          Ywide = Ywide,
          Dwide = Dwide,
          id = varInd,
          time = varTime,
          Yname = varY,
          Dname = varD
        )
      } else{
        cohort[["wide panel"]] <- list(
          Ywide = Ywide,
          Dwide = Dwide,
          Xwide = Xwide,
          id = varInd,
          time = varTime,
          Yname = varY,
          Dname = varD,
          Xname = varX
        )
      }
      cohort[["matrix of match"]] <- list(
        M_match = matching_set
      )
      cohort[["unit and time"]] <- list(
        unit = id_list,
        time = time_list
      )
      cohort[["tuning parameters"]] <- list(
        carryover = c,
        pre_treatment = a,
        post_treatment = b
      )
      for (t in 1:length(B)){
        # treat <- as.character(id_list[group[[as.character(B[t])]]])
        treat <- as.character(group[[as.character(B[t])]])
        # time <- as.character(time_list[B[t]])
        time <- as.character(B[t])
        INDcontrol <- as.vector(matching_set[t, -1:-2])
        # control <- as.character(matrix(id_list, nrow = 1, ncol = length(INDcontrol)))[(INDcontrol==1)]
        control <- as.character(matrix(1:length(INDcontrol),nrow=1,ncol=length(INDcontrol)))[(INDcontrol==1)]
        cohort[[paste0("cohort", t)]] <- list(
          treated_units = treat,
          treated_time = time,
          control_units = control)
      }
      return(cohort)
    } else {
      stop("The match type only contains episode or cohort.")
    }
    }