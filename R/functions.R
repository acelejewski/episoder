#' episoder: A package for episode and event interval analysis
#'
#' A common approach to analyzing high-resolution behavioural event data, eg. lickometer or
#' feeder recordings, is by grouping individual events into episodes  according to inter-event-interval
#' distributions. The episoder package provides several functions for obtaining inter-event-interval and
#' episode information form large, event time-series datasets.
#'
##' The episoder funtions \code{\link{event_intervals}}, \code{\link{event_episodes}} provide highresilutoin
##' while \code{\link{episode_summarize}} generates a summary of episode information according
##' specified in
#'
#' @section See Also:
#' For further infrmoanton see viggnet("episodes") for further informaiton
#'
#' @docType package
#' @name episoder
#'
#'


library(dplyr)
library(lazyeval)



#' Calculate inter-event intervals
#'
#' Transform a data frame event time column to inter-event intervals at a
#' specified factor, or factor combination. The final inter-event value at each factor level is NA.
#' @param data A data frame containing event times along with additional descriptive information
#' @param group A character vector of the name(s) of one or more factor combinations for
#' each unit of analysis, e.g  \code{c(“Subject”, “Session”)}.
#' @param time The name of the event time column
#' @return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#' consiting of input data with new column, "Inter_Event_Intervals", appended. The last value at
#' each factor level combination is NA.
#' @examples
#' data(lick_data)
#' event_intervals(licks_data, group = c(“Subject”, “Session”), time = "Time")
#' @seealso \code{\link{event_episodes}} for finding episodes.
#' @export
event_intervals <- function(data, group, time = "Time") {

  by_factor <- lapply(group, as.symbol)
  mutate_call <- lazyeval::interp(~c(diff(a), NA), a = as.name(time))

  data %>% group_by_(.dots = by_factor) %>%
    mutate_(.dots = setNames(list(mutate_call), "Inter_Event_Intervals")) %>%
    ungroup(data)

}

#'Add event time bins
#'
#'add event time bins
#'@param data A data frame containing event times along with additional descriptive information
#'@param time The name of the event time column
#'@param bins A vector containg bin lables
#'@param samples_per_session the maximum time units units, or samples in a session
#'@param samples_per_bin number of samples, or time units per bin
#'@examples
#'data(lick_data)
#'event_bins(licks_df, time = "Time",
#'samples_per_session = 50 *60 * 60 * 23,
#'samples_per_bin = 50 *60 * 60, bins = c(0:22))
#'@export
add_bins <- function(data, time,  bins, samples_per_session, samples_per_bin)  {

  bins_to_add <- cut(data[[time]],  seq(0, samples_per_session, samples_per_bin),
         labels = bins)

  data$Bins <- bins_to_add
  return(data)
}




#' Identify events meeting an inter-interval episode criterion
#'
#'Identify events conforming to a minimum episode inter-event interval criterion. Enumerate and label
#'sequence of events with each episode,
#'@param data A data frame containing inter-event intervals along with additional descriptive information.
#'@param IEI A minimum inter-interval episode criterion value.
#'@param intervals name of column containing inter-event intervals.
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'consiting of input data plus 3 columns of episode information.
#'Episode_Crit: A logical vector corresponding to event runs meeting the IEI criterion.
#'Episode_Events: An integer vector corresponding to the number of events per episode
#'Within_Episode_Order: An integer vector of the order of events within each episode
#What is the utility of Within_Episode_Order???
#'@examples
#' data(lick_data)
#' interval_data <- event_intervals(licks_data, group = c(“Subject”, “Session”), time = "Time")
#' event_episodes(interval_data, 12, intervals = "Inter_Event_Intervals")
#'@export
#'@examples
#' data(lick_data)
#' event_intervals(licks_data, group = c(“Subject”, “Session”), time = "Time")
#' event_episodes(licks_data, 12, intervals = "Inter_Event_Intervals")
#'@export
event_episodes <- function(data, IEI, intervals = "Inter_Event_Intervals") {
  event_runs <- rle(unlist(data[ , which(names(data) %in% intervals)], use.names = FALSE) <= IEI) #rle object
  Episode_Crit <- rep(event_runs$values, event_runs$lengths)        #
  Episode_Events <- rep(event_runs$lengths, event_runs$lengths)
  Within_Episode_Order <- sequence(event_runs$lengths)
  as.tbl(data.frame(data, Episode_Crit, Episode_Events, Within_Episode_Order))

}





#' Generate sequence of vector[i] repeated vector[i] times
#'
#'  sequence considitng  for each element of vector repeated  element times.
#'  Usefull along incombination with for   \code{\link{rle}}
#'for number repeating event
#'@param x A vector integers
#'  #' @seealso \code{\link{sequence}} and \code{\link{seq}}
# not exported
seq_seq <- function(x) {
  (rep(seq(x), x))
}

#' Number vector of repeating ellements
#'
#'   sequence of repeating elmemns where x is data frame vector of episode runs. This operation is expensive with larger opperation dplyr impmentation
# for speed

episoder_order <- function(data,  group, episode_crit = "Episode_crit") {
  by_factor <- lapply(group, as.symbol)
  mutate_call = lazyeval::interp(~seq_seq(rle(a)[[1]]), a = as.name(episode_crit))

  data %>% group_by_(.dots = by_factor ) %>%
  mutate_(.dots = setNames(list(mutate_call), "Episode_order")) %>%
  ungroup
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
#'data(licks_df)
#'episode_summarize(licks_df, group = c("Subject", "Session"), IEI= 12, EPE = 10 ,time ="Time")
#'@export
episode_summarize <- function(data, group, time, IEI, EPE) {

  data <- event_intervals(data, group, time)


  event.runs <- rle(data$Inter_Event_Intervals <= IEI) #rle object
  #logical vector of events that macthc or edataceed inter-event cirterion (TRUE)
  data$Episode_crit <- rep(event.runs$values, event.runs$lengths)  #Logical vector correspondign to episodes exceeding interepisde criterion == TRUE
  # duration of episode edataceeds ILI
  data$Episode_Events <- rep(event.runs$lengths, event.runs$lengths)

  # starts
  episode.starts <- data[head(c(1, cumsum(event.runs$length) + 1), length(event.runs$length)), ]  #bout start time
  episode.starts$Episode_End <- data$Time[tail(c(1, cumsum(event.runs$length)), length(event.runs$length))]  #bout end times

  # subset episodes that meets episode criteria
  episode_data <- filter(episode.starts, Episode_Events >= EPE & Episode_crit)

  # calcualte episode duraton
  episode_data$Event.duration <- episode_data$Episode_End - episode_data$Time
  episode_data$Episode_Crit <- NULL
  return(episode_data)

}
