#' Calculate inter-event intervals
#'
#' Transform a vector of event times to inter-event intervals grouped by a factor, or combination of factors.
#' The final inter-event interval value at each factor level is NA padded.
#' @param data A data frame containing event times along with optional descriptive information.
#' @param group A character vector containing the name(s) of one or more grouping factor combinations.
#'  e.g. \code{c("Subject", "Session")}
#' @param time The name of the event time vector (as a characther string).
#' @return A data frame (tbl_data class; see
#'   \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'   consisting of input data with, "Inter_Event_Intervals" vector appended. The last inter-event
#'   interval value at each factor level combination is NA padded.
#' @examples
#' data(lickometer)
#' event_intervals(lickometer, group = c(“Subject”, “Session”), time = "Time")
#' @seealso \code{\link{episode_label}} and  \code{\link{episode_summarize}}.
#' @export
event_intervals <- function(data, group, time = "Time") {

  by_factor <- lapply(group, as.symbol)
  mutate_call <- lazyeval::interp(~c(diff(a), NA), a = as.name(time))

  data %>% group_by_(.dots = by_factor) %>%
    mutate_(.dots = setNames(list(mutate_call), "Inter_Event_Intervals")) %>%
    ungroup(data)

}


#native R fucntions can also be used but is aproximatly 20 times slower. Returns vector instead of dataframe.
#event_intervals2 <- function(x, sessionID, time = "Time") {
#
#  x <- tapply(licks.df$Time, licks.df$Session.Identifier, function(x) c(diff(x),NA))
#   <- unlist(x)
#  return(x)
#}



#'Add time bins
#'
#'Add a binning vector for event-time data.
#'@param data A data frame containing a vector of event-times along and optional descriptive information.
#'@param time The name of the event-time vector.
#'@param samples_per_session The total number of samples, or highest possible time unit of a session.
#'@param samples_per_bin The number of samples, or time units per bin. Must be a dividend of
#'  samples_per_session.
#'@param bin_labels A vector containing bin labels. Length of the vector must correspond
#'  to the number of bins. If missing, bin labels are added automatically (0:number of bins).
#'@param samples_per_bin The number of samples per bin. Must be a dividend of samples_per_session.
#'@param bin_name Name of binning vector. Default name is "Bins"
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
  return(data)
}



#'Label events meeting episode criteria
#'
#'Identify events conforming to a specified inter-event intervals, the number of
#'events per episode. Provide order of both events and episodes.
#'@param data A data frame containing event times along with optional descriptive information.
#'@param IEI A minimum inter-event interval criterion value.
#'@param group A character vector containing the name(s) of one or more grouping factor combinations.
#' e.g. \code{c("Subject", "Session")}.
#'@param intervals The name of the inter-event interval vector
#'@param tag A string identifying episode type. "Episode" is the is default.
#'@return A data frame (tbl_data class; see
#'\href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#' consisting of input data and 4 columns of episode information:
#'\itemize{
#'\item Episode_Interval_Criterion: A logical vector corresponding to inter_event_interval runs meeting
#'the IEI criterion.
#'\item Episode_Events: An integer vector containing to the number of events within episode.
#'\item Within_Episode_Order: An integer vector containing the order of events within each episode.
#'\item Episode_Order: An integer vector containing the order of episodes within each factor combination
#'}
#'@seealso \code{\link{episode_summarize}}.
#`@examples
#`data(lickometer)
#`interval_data <- event_intervals(lickometer, group = c("Subject", "Session"), time = "Time")
#`episode_label(interval_data, IEI = 12, , intervals = "Inter_Event_Intervals", tag = "Episode")
#'@export
episode_label <- function(data, group, IEI, intervals = "Inter_Event_Intervals", tag = "Episode")  {

  by_factor <- lapply(group, as.symbol)

  event_runs <- rle(data[[intervals]] <= IEI)


  data[paste(tag, "_Interval_Criterion", sep = "")]  <- rep(event_runs$values, event_runs$lengths)
  data[paste(tag, "_Events", sep = "")] <- rep(event_runs$lengths, event_runs$lengths)
  data[paste("Within_", tag, "_Order", sep = "")] <- sequence(event_runs$lengths)

  mutate_call = lazyeval::interp(~run_sequence(x), x = as.name(paste(tag, "_Interval_Criterion", sep = "")))

  data <- data %>% group_by_(.dots = by_factor ) %>%
      mutate_(.dots = setNames(list(mutate_call), paste(tag, "_Order", sep = ""))) %>%
      ungroup

  return(data)
}



#
# #' Label events meeting episode criteria with base
# #'
# #'Label events conforming to a minimum episode inter-event interval, the number of
# #'events per episode, the order of events and episodes
# #'@param data A data frame containing event times along with optional descriptive information.
# #'@param IEI A minimum inter-event interval criterion value.
# #'@param group A character vector containing the name(s) of one or more grouping factor combinations.
# #'  e.g.  \code{c(“Subject”, “Session”)}.
# #'@param intervals The name of the inter-event interval vector (as a characther string).
# #'@param tag A character string identifying episode type. "Episode" the is default.
# #'@return A data frame (tbl_data class; see
# #'\href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
# #' consisting of input data and 4 columns of episode information:
# #'\itemize{
# #'\item Episode_Interval_Criterion: A logical vector corresponding to inter_event_interval runs meeting
# #'the IEI criterion.
# #'\item Episode_Events: An integer vector containing to the number of events within episode.
# #'\item Within_Episode_Order: An integer vector containing the order of events within each episode.
# #'\item Episode_Order: An integer vector containing the order of episodes within each factor combintation
# #'}
# #'@seealso \code{\link{episode_summarize}}.
# #'@examples
# #'data(lickometer)
# #'interval_data <- event_intervals(lickometer, group = c("Subject”, “Session"), time = "Time")
# #'episode_label(interval_data, IEI = 12, , intervals = "Inter_Event_Intervals", tag = "Episode")
# #'@export
# episode_label_base <- function(data, IEI, intervals = "Inter_Event_Intervals", tag = "Episode")  {
#
#   event_runs <- rle(data[[intervals]] <= IEI)
#
#   data[paste(tag, "_Interval_Criterion", sep = "")]  <- rep(event_runs$values, event_runs$lengths)
#   data[paste(tag, "_Events", sep = "")] <- rep(event_runs$lengths, event_runs$lengths)
#   data[paste("Within_", tag, "_Order", sep = "")] <- sequence(event_runs$lengths)
#
#   return(data)
# }



#' Sequence of equal value runs in a vector
#'
#'Generate an ordered sequence of integers corresponding to each run of equal values
#'in a vector.
#'@param x A vector containing runs of equal values.
#'@return A vector, equal in length to x, consisting of integers incrementing for each run
#'of equal values in x.
#'@seealso \code{\link{rle}},  \code{\link{renumber}}
#'@export
run_sequence <- function(x){

  if (class(x) == "factor") {
    x <- as.character(x)
  }

  run_lengths <- rle(x)[[1]]
  rep(seq(run_lengths), run_lengths)
}



#' Generate summary of episode parameters
#'
#'Generate a summary of episode parameters from event time data according
#'to minimum inter-event interval duration and minimum number of events per episode criteria.
#'@param data A data frame containing event and inter-event interval times and with optional
#' descriptive information.
#'@param group A character vector containing the name(s) of one or more grouping factor combinations.
#'   e.g.  \code{c("Subject", "Session")}.
#'@param IEI A minimum Inter-Event Interval criterion value.
#'@param EPE A minimum Events Per Episode criterion value.
#'@param time The name of the event time vector (as a character string).
#'@param intervals The name of the event time vector (as a character string). If argument is not
#'included, intervals will be calculated from event time vector.
#'@return A data frame (tbl_data class; see
#'\href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#' consisting of input data and 4 columns of episode information:
#'\itemize{
#'\item Episode_Interval_Criterion: A logical vector corresponding to inter_event_interval runs meeting
#'the IEI criterion.
#'\item Episode_Events: An integer vector containing to the number of events within episode.
#'\item Within_Episode_Order: An integer vector containing the order of events within each episode.
#'\item Episode_Order: An integer vector containing the order of episodes within each factor combination
#'}
#'@seealso \code{\link{episode_summarize}}.
#'@examples
#'data(lickometer)
#'interval_data <- event_intervals(lickometer, group = c("Subject", "Session"), time = "Time")
#'episode_label(interval_data, IEI = 12, , intervals = "Inter_Event_Intervals", tag = "Episode")
#'@export
episode_summarize <- function(data, group,  IEI, EPE, time = "Time", intervals) {


  if (missing(intervals)) {
    data <- event_intervals(data = data, group = group, time = time)
    intervals <- "Inter_Event_Intervals"
    }


  event_runs <- rle(data[[intervals]] <= IEI)

  data$Episode_Crit <- rep(event_runs$values, event_runs$lengths)
  data$Episode_Events <- rep(event_runs$lengths, event_runs$lengths)



  data$Episode_Start <- data[[time]]

  episode_starts <- data[head(c(1, cumsum (event_runs$length) + 1), length(event_runs$length)), ]  #bout start time
  Episode_End <- data[[time]]
  episode_starts$Episode_End <-  Episode_End [tail(c(1, cumsum(event_runs$length)), length(event_runs$length))]  #bout end times

  episode_data <- filter(episode_starts, Episode_Events >= EPE & Episode_Crit)

  episode_data$Episode_Duration <- episode_data$Episode_End - episode_data$Time
  episode_data$Episode_Rate <-   episode_data$Episode_Events /  episode_data$Episode_Duration

  episode_data$Episode_Crit  <- NULL
  episode_data[[time]] <- NULL

  return(episode_data)
}




#' Renumber a vector of event runs
#'
#'Sequentially number runs of equal values in a vector within a data frame
#'@param data A data frame containing a vector of event runs
#'@param group A character vector of the name(s) of one or more factor combinations for
#'@param name_in Name of the vector to renumber
#'@param name_out Name of renumbered vector. If left blank, the name is unchanged
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'of episodes and episode parameters. Rows correspond to episodes.
#'Episode_Events: An integer vector corresponding to the number of events per episode.
#'Episode_End: An integer vector of episode end times.
#'Event_Duration: Episode duration.
# creates data frame of episodes parameters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
#'@examples
#'data(lickometer)
#'episode_summarize(lickometer, group = c("Subject", "Session"), IEI= 12, EPE = 10 ,time ="Time")
#'@export
renumber <- function(data, group, name_in, name_out) {

   if (missing(name_out)) {name_out <- name_in}

    by_factor <- lapply(group, as.symbol)
    mutate_call = lazyeval::interp(~run_sequence(x), x = as.name(name_in))

    x <- suppressMessages(data %>% group_by_(.dots = by_factor ) %>%
           transmute_(.dots = setNames(list(mutate_call), name_out)) %>%
           ungroup)

    x <-  unlist(x[ , name_out], use.names = FALSE)

    data[, name_in] <-   x
    names(data)[which(names(data) == name_in) ] <- name_out

    return(data)
}


#' Prepare events for survival analysis
#'
#'Order and summat events for survival analsys
#'@param data A data frame containing a vector of event runs
#'@param IEI_range Lower cut off inter-event interval duration
#'@param time_range Upper cut off inter-event interval duration
#'@param time_cutoff Upper cut for time
#'@param group  A character vector containing the name(s) of one or more grouping factor combinations.
#'@return A data frame (tbl_data class; see
#' \href{https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html}{Tibbles})
#'of episodes and episode parameters. Rows correspond to episodes.
#'Episode_Events: An integer vector corresponding to the number of events per episode.
#'Episode_End: An integer vector of episode end times.
#'Event_Duration: Episode duration.
# creates data frame of episodes parameters corresponding to a defined inter-episode interval(IEI) and minimum events per episode
#'@export
event_survival <- function(data, IEI_min, IEI_max, time_start, time_end, group = c("Treamtment")) {
  #apply filters
  if (!(missing(IEI_min))) {data <- data[data$Inter_Event_Intervals >= IEI_min, ]}
  if (!(missing(IEI_max))) {data <- data[data$Inter_Event_Intervals >= IEI_max, ]}
  if (!(missing(time_start))) {data <- data[data$Time >= time_start, ]}
  if (!(missing(time_end))) {data <- data[data$Time >= time_end, ]}


  by_factor <- lapply(group, as.symbol)
  n <- "n"
  mutate_call <- lazyeval::interp(~cumsum(a), a = as.name(n))

  data  <-data %>% group_by_( .dots = by_factor) %>%
              count_( ~Inter_Event_Intervals) %>%
              arrange_(~desc(Inter_Event_Intervals)) %>%
              mutate_(.dots = setNames(list(mutate_call), "cum")) %>%
              arrange_(~Inter_Event_Intervals)

  return(data)
}

