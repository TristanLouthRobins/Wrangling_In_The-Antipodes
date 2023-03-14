library(tidyverse)

# start time: actual time that recording begins - i.e. 14:30pm = 14.5
# the time time compressed minute mark that we want the actual time of - i.e. 8 (minute mark)
# interval: the interval for the AudioMoth to record - i.e. 5 minute interval = 5
# edit_amt: how long each clip in the time compression is - i.e. 15 seconds

time_mark <- function(start_time, mark, interval, edit_amt) {
  print(paste("Start time is:", start_time))
  n = (60 / edit_amt)
  a.time = interval * n
  a.time.secs = a.time * 60
  
  print(paste("AudioMoth interval was every:", interval, "minutes"))
  print(paste("Edit amount was:", edit_amt, "seconds"))
  print(paste("For every compressed minute, this is", a.time, "minutes of actual time"))
  
  n = 60 / a.time
  hour = (a.time * n) * 60.0
  A.s = start_time * hour
  A.d = mark * a.time.secs
  A.m = A.s + A.d
  A.m = A.m / 3600
  A.m.h = floor(A.m)
  A.m.m = A.m %% 1
  A.m.m = round(A.m.m * 60.0, 2)
  A.m.s = A.m.m %% 1
  A.m.s = round(A.m.s * 60.0, 0)
  
  print(paste("Actual time of audio mark ", mark, " is: ", A.m.h, ":", A.m.m, ":", A.m.s, sep = ""))
}

time_mark(14.5, 2, 5, 15)
