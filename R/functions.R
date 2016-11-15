#' Find inter-event intervals
#'
#' Transform a vector of event times to inter-event intervals at a specific factor, or factor combination.
#' The final inter-event value at each factor level is NA.
#' @param data A data frame containing event times along with optional descriptive information
#' @param group A character vector of the name(s) of one or more factor combinations for
#' each unit of analysis, e.g  \code{c(“Subject”, “Session”)}.
#' @param time The name of the event time column
#' @return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#' consiting of input data with new column, "Inter_Event_Intervals", appended. The last value at
#' each factor level combination is NA.
#' @examples
#' data(lickometer)
#' event_intervals(lickometer, group = c(“Subject”, “Session”), time = "Time")
#' @seealso \code{\link{event_episodes}} for finding episodes.
#' @export
event_intervals <- function(data, group, time = "Time") {

  by_factor <- lapply(group, as.symbol)
  mutate_call <- lazyeval::interp(~c(diff(a), NA), a = as.name(time))

  data %>% group_by_(.dots = by_factor) %>%
    mutate_(.dots = setNames(list(mutate_call), "Inter_Event_Intervals")) %>%
    ungroup(data)

}


#native R fucntions can also be used but is aproximatly 20 times slower. Returns vector instead of dataframe.
#event_intervals2 <- function(data, sessionID, time = "Time") {
#
#  data <- tapply(licks.df$Time, licks.df$Session.Identifier, function(x) c(diff(x),NA))
#   <- unlist(x)
#  return(x)
#}



#'Add time bins
#'
#'Add time bins to data frame from a vector of event times
#'@param data A data frame containing a vector event times along with optional descriptive information
#'@param time The name of the event time vector
#'@param samples_per_session the maximum time units units, or samples in a session
#'@param samples_per_bin number of samples, or time units per bin. Must be a dividend  of samples_per_session
#'@param bins A vector containg bin lables. The the lengh of the vector must corrspond to the quotient of
#'samples_per_bin into samples_per_session. If the argument bin lables are from 0:number of bins are included.
#'@param bin_name Lable for bin vecotr. Default is "Bins"
#'@examples
#'data(lickometer)
#'add_bins(lickometer, time = "Time", samples_per_session = 50 *60 * 60 * 23, samples_per_bin = 50 *60 * 60,
#'bin_labels = 0:22)
#'@export
add_bins <- function(data, time,  samples_per_session, samples_per_bin, bin_labels, bin_name = "Bins")  {

  if(missing(bin_labels)) {
    bin_labels <- (samples_per_session/samples_per_bin) - 1
    bin_labels <- 0:bin_labels
  }

  bins_to_add <- cut(data[[time]],  seq(0, samples_per_session, samples_per_bin),
         labels = bin_labels)
   data[, bin_name] <-  bins_to_add
  #data$Bins <- bins_to_add
  return(data)
}

#can auto add lables,



#' Identify events meeting an inter-interval episode criterion
#'
#'Identify events conforming to a minimum episode inter-event interval criterion. Enumerate and label
#'sequence of events with each episode,
#'@param data A data frame containing inter-event intervals along with additional descriptive information.
#'@param IEI A minimum inter-interval episode criterion value.
#'@param intervals name of column containing inter-event intervals.
#'@param filter fillters observations that do not correspond to episode criterion
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'consiting of input data plus 3 columns of episode information.
#'Episode_Crit: A logical vector corresponding to event runs meeting the IEI criterion.
#'Episode_Events: An integer vector corresponding to the number of events per episode
#'Within_Episode_Order: An integer vector of the order of events within each episode
#What is the utility of Within_Episode_Order???
#'@examples
#' data(lickometer)
#' interval_data <- event_intervals(lickometer, group = c("Subject”, “Session"), time = "Time")
#' episode_lable(interval_data, 12, intervals = "Inter_Event_Intervals")
#'@export
#'@examples
#' data(lickometer)
#' event_intervals(lickometer, group = c(“Subject”, “Session”), time = "Time")
#' episode_lable(lickometer, 12, intervals = "Inter_Event_Intervals")
#'@export
episode_lable <- function(data, group, EPE, IEI, intervals = "Inter_Event_Intervals", filter = TRUE)  {

  by_factor <- lapply(group, as.symbol)

  event_runs <- rle(unlist(data[ , which(names(data) %in% intervals)], use.names = FALSE) <= IEI)

  data$Episode_Crit <- rep(event_runs$values, event_runs$lengths)

  data$Episode_Events <- rep(event_runs$lengths, event_runs$lengths)
  data$Within_Episode_Order <- sequence(event_runs$lengths)


  if (filter == TRUE) {

    mutate_call = lazyeval::interp(~run_sequence(x), x = as.name("Episode_Crit"))


    data <- data %>% group_by_(.dots = by_factor ) %>%
      mutate_(.dots = setNames(list(mutate_call), "Episode_Order_")) %>%
      ungroup



     mutate_call = lazyeval::interp(~run_sequence(x), x = as.name("Episode_Order_"))
    data <- data %>% group_by_(.dots = by_factor ) %>%
      mutate_(.dots = setNames(list(mutate_call), "Episode_Order")) %>%
      ungroup


    data$Episode_Crit <- NULL
    data$Episode_Order_ <- NULL
    return(data)
   }else{
    return(data)}
}




#' Generate sequence of vector[i] repeated vector[i] times
#'
#'  sequence considitng  for each element of vector repeated  element times.
#' Usefull along incombination with for   \code{\link{rle}}
#' for number repeating event
#'@param x A vector integers
#' @seealso \code{\link{sequence}} and \code{\link{seq}}
#'@export
seq_seq <- function(x) {
  rep(seq(x), x)
}




#' Order of runs of equal values in a vector
#'
#'Generate ordered sequnce of intergers for each run of equal values
#'in a vector
#'for number repeating event
#'@param x A vector containg runs of equal values
#'@return a vector equal in length to x, consiting an incrementing
#'integers corespoidng to each run of equal values in x
#'@seealso \code{\link{rle}}
#'@export
run_sequence <- function(x){
  run_lengths <- rle(x)[[1]]
  rep(seq(run_lengths), run_lengths)
}



#depricated
#' Number vector of repeating elements
#'
#'   sequence of repeating elmemns where x is data frame vector of episode runs. This operation is expensive with larger opperation dplyr impmentation
# for speed
episode_order <- function(data,  group, episode_crit = "Episode_Crit") {
  by_factor <- lapply(group, as.symbol)
  mutate_call = lazyeval::interp(~run_sequence(x), x = as.name(episode_crit))

  data %>% group_by_(.dots = by_factor ) %>%
  mutate_(.dots = setNames(list(mutate_call), "Episode_Order")) %>%
  ungroup
  return(data)
}



## episode order implementation in R base funtion. Orders of magnitude slower slower.
episode.order2 <- function(x) {
  runs <- tapply(x$Exceeds, x$Session.Identifier, rle)
  lenghts.runs <- lapply(runs, (function(m) m[[1]]))
  x$Episode.order <- unlist(lapply(lenghts.runs, seq_seq))
}



#' Generate summary of episode parameters
#'
#' Create data frame summary of episode paramters from event time data. Includes summary
#'@param data A data frame containing event times along with additional descriptive information
#'@param group A character vector of the name(s) of one or more factor combinations for
#'@param time The name of the event time column
#'@param IEI A minimum inter-interval episode criterion value.
#'@param EPE Mximum Events per Episode value
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'of episodes and episode parmaters. Rows corespond to episodes.
#'Episode_Events: An integer vector corresponding to the number of events per episode.
#'Episode_End: An integer vector of episode end times.
#'Event_Duration: Episdoe duration.
# creates data frame of episodes paramters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
#'@examples
#'data(lickometer)
#'episode_summarize(lickometer, group = c("Subject", "Session"), IEI= 12, EPE = 10 ,time ="Time")
#'@export
episode_summarize <- function(data, group,  IEI, EPE, intervals = "Inter_Event_Intervals", time = "Time") {


  event_runs <- rle(unlist(data[ , which(names(data) %in% intervals)], use.names = FALSE) <= IEI)


  #event_runs <- rle(intervals <= IEI) #rle object
  data$Episode_Crit <- rep(event_runs$values, event_runs$lengths)  #Logical vector correspondign to episodes exceeding interepisde criterion == TRUE
  data$Episode_Events <- rep(event_runs$lengths, event_runs$lengths)


  episode_starts <- data[head(c(1, cumsum (event_runs$length) + 1), length(event_runs$length)), ]  #bout start time
  Episode_End <- data[[time]]
  episode_starts$Episode_End <-  Episode_End [tail(c(1, cumsum(event_runs$length)), length(event_runs$length))]  #bout end times


  # subset episodes that meets episode criteria
  episode_data <- filter(episode_starts, Episode_Events >= EPE & Episode_Crit)

  # calcualte episode duraton
  episode_data$Event_Duration <- episode_data$Episode_End - episode_data$Time
  episode_data$Episode_Crit <- NULL
  return(episode_data)

}


#intervals = "Inter_Event_Intervals"
#group = c("Subject", "Session")
#eps <- event_intervals(lickometer, group = c("Subject", "Session") )
#episode_summarize(eps, group = c("Subject", "Session"), intervals = "Inter_Event_Intervals",IEI = 12,EPE = 10, time = "Time" )



#' Generate summary of episode parameters (Secondary)
#'
#'Create data frame summary of episode paramters from labled
#'@param data A data frame containing event times along with additional descriptive information
#'@param group A character vector of the name(s) of one or more factor combinations for
#'@param summarize_by A vector labling events to summarize
#'@param time Name of event time vector
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'of episodes and episode parmaters. Rows corespond to episodes.
#'Episode_Events: An integer vector corresponding to the number of events per episode.
#'Episode_End: An integer vector of episode end times.
#'Event_Duration: Episdoe duration.
# creates data frame of episodes paramters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
#'@examples
#'data(lickometer)
#'episode_summarize(lickometer, group = c("Subject", "Session"), IEI= 12, EPE = 10 ,time ="Time")
#'@export
episode_summarize2 <- function(data, group, summarize_by, time = "Time") {

  event_runs <- rle(unlist(data[ , which(names(data) %in% summarize_by)], use.names = FALSE))
  data$Episode_Events <- rep(event_runs$lengths, event_runs$lengths)


  episode_starts <- data[head(c(1, cumsum (event_runs$length) + 1), length(event_runs$length)), ]  #bout start time
  Episode_End <- data[[time]]
  episode_starts$Episode_End <-  Episode_End [tail(c(1, cumsum(event_runs$length)), length(event_runs$length))]  #bout end times

  episode_data <-   episode_starts
  episode_data$Event_Duration <- episode_data$Episode_End - episode_data$Time
  episode_data$Episode_Crit <- NULL
  return(episode_data)

}




