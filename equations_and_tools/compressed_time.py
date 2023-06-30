# Version 1.2 Tristan Louth-Robins
# Latest update: 1/07/23
# Additional prompt included to handle crossfaded files.

import pandas as pd
import numpy as np
import math

# start time: actual time that recording begins - i.e. 14:30pm = 14.5
# the time time compressed minute mark that we want the actual time of - i.e. 8 (minute mark)
# interval: the interval for the AudioMoth to record - i.e. 5 minute interval = 5
# edit_amt: how long each clip in the time compression is - i.e. 15 seconds

def time_mark(start_time, duration, interval, edit_amt):
    print("Real-world start time was: ", start_time, ":00")
    n = (60 / edit_amt)
    a_time = interval * n
    a_time_secs = a_time * 60

    print("AudioMoth recording interval was every ", interval, " minutes.")
    print("Time compression edit amount was: ", edit_amt, " seconds.")
    print("For every compressed minute, this is approximately ", round(a_time, 0), " minutes of real-world time.\n")

    df_marks = pd.DataFrame()

    for i in range(duration):
        n = 60 / a_time
        hour = (a_time * n) * 60.0
        A_s = start_time * hour
        A_d = i * a_time_secs
        A_m = A_s + A_d
        A_m = A_m / 3600
        A_m_h = np.floor(A_m)
        A_m_h = math.trunc(A_m)
        A_m_m = A_m % 1
        A_m_m = A_m_m * 60
        A_m_m = math.trunc(A_m_m)
        if A_m_m < 10:
            A_m_m = str(A_m_m).zfill(2) # add a leading zero to the minutes variable.
        else:
            A_m_m = A_m_m            
#       A_m_s = A_m_m % 1
#       A_m_s = round(A_m_s * 60, 0)
        r_w = str(A_m_h) + ":" + str(A_m_m) 
        row = i
        df_marks.at[i, "minute mark"] = row
        df_marks.at[i, "real-world"] = r_w

    print(df_marks)

start = int(input("Enter the starting real_world hour mark of the audio (24 hr time - e.g 1 or 15): "))
dur = int(input("Enter the total duration of the compressed audio file (in minutes): "))
dur = dur + 1
interval = int(input("Enter the recording interval of the AudioMoth (in minutes): "))
amt = int(input("Enter length in seconds of the audio file edits (in seconds): "))
cf = str(input("Did the file have crossfades applied to it? (y/n) "))
if cf == "y" or cf == "Y":
    cf_amt = int(input("What was the crossfade amount applied? (s): "))
    amt = amt - (cf_amt/2)
    print("A crossfade of " + str(cf_amt) + " seconds was applied during batch processing,\nThe actual edit length is: " + str(amt) + " s.\n\n#################\n\n")
else:
    amt = amt

time_mark(start, dur, interval, amt)
