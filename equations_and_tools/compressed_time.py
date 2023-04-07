import pandas as pd
import numpy as np

def time_mark(start_time, duration, interval, edit_amt):
    print("Real-world start time is: ", start_time, " hour")
    n = (60 / edit_amt)
    a_time = interval * n
    a_time_secs = a_time * 60

    print("AudioMoth recording interval was every: ", interval, " minutes")
    print("Time compression edit amount was: ", edit_amt, " seconds")
    print("For every compressed minute, this is ", a_time, " minutes of r-w time")

    df_marks = pd.DataFrame()

    for x in range(duration):
        n = 60 / a_time
        hour = (a_time * n) * 60.0
        A_s = start_time * hour
        A_d = x * a_time_secs
        A_m = A_s + A_d
        A_m = A_m / 3600
        A_m_h = np.floor(A_m)
        A_m_m = A_m % 1
        A_m_m = round(A_m_m * 60.0, 2)
        A_m_s = A_m_m % 1
        A_m_s = round(A_m_s * 60.0, 0)

        r_w = str(A_m_h) + str(A_m_m) + str(A_m_s)
        row = x

        df_marks.at[x, 1] = row
        df_marks.at[x, 2] = r_w

    print(df_marks)

time_mark(0,12,10,5)



