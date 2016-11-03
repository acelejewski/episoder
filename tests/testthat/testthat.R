library(testthat)
library(episoder)

test_check("episoder")

data("licks_df")  ### create smaller dataframe for testing

episoder_interval_test <- episoder_intervals(licks.df, group = c("Session", "Subject"), time = "Time")

unique_sessions <- unique(episoder_interval_test$Session.Identifier)

inter_interval_NAs <- episoder_interval_test$Inter_Event_Intervals[which(is.na(episoder_interval_test$Inter_Event_Intervals))]
expect_equal(length(inter_interval_NAs), length(unique_sessions))

