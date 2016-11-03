#' episoder: A package for computating time series event data into episode
#'
#' The episoder package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name episoder
#'
#'
library(lazyeval)
library(dplyr)


#' Calcualte inter-event intervals
#'
#' Transform a vector of event time data into inter-eventduraton interval according to specified factor. The final
#' inter-event value for each factor level is NA.
#' @param df A data frame
#' @param group A characther vector of unique data frame vector factor combinations for each c("Session", "Subject")
#' @param time The name of data frame vector containg event times
##  @param dataframe Reutrns df as data frame if TRUE, returns df as tibble if FALSE.
#' @return New vector "Inter_Episode_Intervals" consiting inter episode intervals appended to input existing df
# @examples an example of the code
#' @family aggregate functions
#' @seealso \code{\link{episoder_identify}} for finding episodes.

episoder_intervals <- function(df, group, time = "Time") {

  by_factor <- lapply(group, as.symbol)
  mutate_call = lazyeval::interp(~c(diff(a), NA), a = as.name(time))

  df <- dplyr::group_by_(.data = df ,.dots = by_factor )
  df <- dplyr::mutate_(.data = df, .dots = setNames(list(mutate_call), "Inter_Event_Intervals")) #%>%
  df <- dplyr::ungroup(df)
  return(df)

# futre code controling data output format
#  if (dataframe == TRUE) {as.data.frame(df)   #return as data frame, else return as tibble. see package tibble
#  } else {
#     return(df)
#   }

}


#' Identify episodes by minimum inter-episode duration
#'
#'Identify episodes conforming to a minimum interepisode duration criterion
#'@param df  A data frame containing episode data
#'@param IEI a value of time less than or equal the minimum inter-event interval duration criterion for an episode
#'@param name of data frame vector containg interepisode
#'@return a data frame or tible of the same lengh as the input data frame.
episoder_identify <- function(df, IEI, intervals = "Inter_Event_Intervals") {
  event.runs <- rle(unlist(df[ , which(names(df) %in% intervals)], use.names = FALSE) <= IEI) #rle object

  Episode_crit <- rep(event.runs$values, event.runs$lengths)  #Logical vector correspondign to episodes exceeding interepisde criterion == TRUE
  Episode.events <- rep(event.runs$lengths, event.runs$lengths)

  Within.episode.order <- sequence(event.runs$lengths)
  as.tbl(data.frame(Episode_crit, Episode.events, Within.episode.order))

}


#' Repeat each element of of vector (vector[i]), vector[i] times
#'
#'  sequence for each element the value of that element times. Usefull along with wiht  \code{\link{rle}}
#'    for number repeating event
#'@param x A vector integers
#'  #' @seealso \code{\link{sequence}} and \code{\link{seq}}

seq_seq <- function(x) {
  (rep(seq(x), x))
}

#' Number vector of repeating ellements
#'
#'   sequence of repeating elmemns where x is data frame vector of episode runs. This operation is expensive with larger opperation dplyr impmentation
# for speed

episoder_order <- function(df,  group, episode_crit = "Episode_crit") {
  by_factor <- lapply(group, as.symbol)
  mutate_call = lazyeval::interp(~seq_seq(rle(a)[[1]]), a = as.name(episode_crit))

  df %>% group_by_(.dots = by_factor ) %>%
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
#'@param df A data frame
#'@param group A characther vector of unique data frame vector factor combinations for each c("Session", "Subject")
#'@param IEI A value of time less than or equal the minimum inter-event interval duration criterion for an episode
#'@param EPE A value specifiing maximum Event


# creates data frame of episodes paramters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
episoder.summarize <- function(df, group, IEI, EPE ) {

  # create vector of inter-event durtions
  by_factor <- lapply(group, as.symbol)
  mutate_call = lazyeval::interp(~c(diff(a), NA), a = as.name("Time"))
  df<- df %>% group_by_(.dots = by_factor ) %>%
    mutate_(.dots = setNames(list(mutate_call), "Inter_Event_Intervals")) %>%
    ungroup
  event.runs <- rle(df$Inter_Event_Intervals <= IEI) #rle object
  #logical vector of events that macthc or edfceed inter-event cirterion (TRUE)
  df$Episode_crit <- rep(event.runs$values, event.runs$lengths)  #Logical vector correspondign to episodes exceeding interepisde criterion == TRUE
  # duration of episode edfceeds ILI
  df$Episode.events <- rep(event.runs$lengths, event.runs$lengths)
  #Within.episode.order <- sequence(event.runs$lengths)

  # starts
  episode.starts <- df[head(c(1, cumsum(event.runs$length) + 1), length(event.runs$length)), ]  #bout start time
  episode.starts$Event.end <- df$Time[tail(c(1, cumsum(event.runs$length)), length(event.runs$length))]  #bout end times

  # subset episodes that meets episode criteria
  episode.data <- filter(episode.starts, Episode.events >= EPE & Episode_crit)

  # calcualte episode duraton
  episode.data$Event.duration <- episode.data$Event.end - episode.data$Time
  episode.data

}




#create bins

episoder_bin<-   function(df, bins, resolution, max_time, time_units, time)  {
              cut(df$time,   seq(0, 50 * 60 * 60 *24 , 180000),
                         labels = bins)
}
