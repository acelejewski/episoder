#' episoder: A package for episode and event interval analysis
#'
#' A common approach to analyzing high-resolution behavioural event data, eg. lickometer or
#' feeder recordings, is by grouping individual events into episodes  according to inter-event-interval
#' distributions. The episoder package provides several functions for obtaining inter-event-interval and
#' episode information form large, event time-series datasets.
#'
#' The episoder futnions \code{\link{event_intervals}}, \code{\link{event_episodes}},
#' while \code{\link{episode_summarize}},
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
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles}
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



#' Lable events minimum inter-episode interval criterion
#'
#'Identify episodes conforming to a minimum interepisode duration criterion
#'@param data  A data frame containing inter-event intervals along with additional descriptive information
#'@param IEI An inter-event value less than or equal to desired episode criterion
#'@param intervals the name of column containg inter-event
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles}
#'consiting of input data with 3 new colums of episode information
#'Episode_crit: A logical vector correspondign to event runs meeting the IEI criterion
#'Episode.events
#'@export
event_episodes <- function(data, IEI, intervals = "Inter_Event_Intervals") {
  event.runs <- rle(unlist(data[ , which(names(data) %in% intervals)], use.names = FALSE) <= IEI) #rle object
  Episode_crit <- rep(event.runs$values, event.runs$lengths)        #
  Episode.events <- rep(event.runs$lengths, event.runs$lengths)
  Within.episode.order <- sequence(event.runs$lengths)
  as.tbl(data.frame(data, Episode_crit, Episode.events, Within.episode.order))

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



#' Create data frame summary episode paramters
#'
#' Create data frame summary of episode paramters from event time data. Includes summary
#'@param data A data frame
#'@param group A characther vector of unique data frame vector factor combinations for each c("Session", "Subject")
#'@param IEI A value of time less than or equal the minimum inter-event interval duration criterion for an episode
#'@param EPE A value specifiing maximum Event
# creates data frame of episodes paramters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
#'@export
#'
episode_summarize <- function(data, group, IEI, EPE, time) {

   data <- event_intervals(data, group, time)


  # create vector of inter-event durtions
  #by_factor <- lapply(group, as.symbol)
  #mutate_call = lazyeval::interp(~c(diff(a), NA), a = as.name("Time"))
  #data<- data %>% group_by_(.dots = by_factor ) %>%
  #  mutate_(.dots = setNames(list(mutate_call), "Inter_Event_Intervals")) %>%
  #  ungroup

  event.runs <- rle(data$Inter_Event_Intervals <= IEI) #rle object
  #logical vector of events that macthc or edataceed inter-event cirterion (TRUE)
  data$Episode_crit <- rep(event.runs$values, event.runs$lengths)  #Logical vector correspondign to episodes exceeding interepisde criterion == TRUE
  # duration of episode edataceeds ILI
  data$Episode.events <- rep(event.runs$lengths, event.runs$lengths)
  #Within.episode.order <- sequence(event.runs$lengths)

  # starts
  episode.starts <- data[head(c(1, cumsum(event.runs$length) + 1), length(event.runs$length)), ]  #bout start time
  episode.starts$Event.end <- data$Time[tail(c(1, cumsum(event.runs$length)), length(event.runs$length))]  #bout end times

  # subset episodes that meets episode criteria
  episode_data <- filter(episode.starts, Episode.events >= EPE & Episode_crit)

  # calcualte episode duraton
  episode.data$Event.duration <- episode.data$Event.end - episode.data$Time
  return(episode_data)

}




##create bins

#episoder_bin<-   function(data, bins, resolution, max_time, time_units, time)  {
#              cut(data$time,   seq(0, 50 * 60 * 60 *24 , 180000),
#                         labels = bins)
#}
