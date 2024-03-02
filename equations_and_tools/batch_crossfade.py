#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Tests the audacity pipe.

Keep pipe_test.py short!!
You can make more complicated longer tests to test other functionality
or to generate screenshots etc in other scripts.

Make sure Audacity is running first and that mod-script-pipe is enabled
before running this script.

Requires Python 2.7 or later. Python 3 is strongly recommended.

"""
import os
import sys

if sys.platform == 'win32':
    print("pipe-test.py, running on windows")
    TONAME = '\\\\.\\pipe\\ToSrvPipe'
    FROMNAME = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    print("pipe-test.py, running on linux or mac")
    TONAME = '/tmp/audacity_script_pipe.to.' + str(os.getuid())
    FROMNAME = '/tmp/audacity_script_pipe.from.' + str(os.getuid())
    EOL = '\n'

print("Write to  \"" + TONAME +"\"")
if not os.path.exists(TONAME):
    print(" ..does not exist.  Ensure Audacity is running with mod-script-pipe.")
    sys.exit()

print("Read from \"" + FROMNAME +"\"")
if not os.path.exists(FROMNAME):
    print(" ..does not exist.  Ensure Audacity is running with mod-script-pipe.")
    sys.exit()

print("-- Both pipes exist.  Good.")

TOFILE = open(TONAME, 'w')
print("-- File to write to has been opened")
FROMFILE = open(FROMNAME, 'rt')
print("-- File to read from has now been opened too\r\n")


def send_command(command):
    """Send a single command."""
    print("Send: >>> \n"+command)
    TOFILE.write(command + EOL)
    TOFILE.flush()

def get_response():
    """Return the command response."""
    result = ''
    line = ''
    while True:
        result += line
        line = FROMFILE.readline()
        if line == '\n' and len(result) > 0:
            break
    return result

def do_command(command):
    """Send one command, and return the response."""
    send_command(command)
    response = get_response()
    print("Rcvd: <<< \n" + response)
    return response

def quick_test():
    """Example list of commands."""
    do_command('Help: Command=Help')
    do_command('Help: Command="GetInfo"')
    #do_command('SetPreference: Name=GUI/Theme Value=classic Reload=1')

# Batch crossfade process
# Ensure that Audacity is launched and that audio files have been imported into the session.

import tkinter as tk
from tkinter import messagebox
from tkinter import simpledialog

def batch_crossfade(num_files, file_start, file_end, fadelen):
    i = 1
    while i < num_files:
        print("File " + str(i) + " end: " + str(file_end))
        do_command('SelectAll:')
        fade_begin = file_end - (fadelen/2)
        fade_end = file_end + (fadelen/2)
        print("File: " + str(i) + "\nFade begin: " + str(fade_begin) + "s" + "\nFade end: " + str(fade_end) + "s")
        do_command('SelectAll:')
        do_command(f'SelectTime:End="{fade_begin}" RelativeTo="ProjectStart" Start="{fade_end}"')
        print("Region selected.")
        do_command('CrossfadeClips:Use_Preset="<Factory Defaults>"')
        print("Crossfade for audio files " + str(i) + " to " + str(i + 1) + "completed!")
        file_start = file_start + increment
        file_end = file_end + increment
        i += 1
        print("Next File = " + str(i) + "\nStart: " + str(file_start) + "\nEnd: " + str(file_end))

    print("Processing complete.")

    # Create a root window
    root = tk.Tk()
    root.withdraw()  # Hide the root window

    # Show a pop-up message box
    result = messagebox.showinfo("Pop-up Message", "Processing of crossfade is complete.\nRemember to save and export your file!")

    # Check the result and exit the program if OK is clicked
    if result == "ok":
        sys.exit()

    # Run the main event loop
    root.mainloop()

# Create a root window for the user input

print("BATCH CROSSFADE FOR AUDACITY\n")
print("Remember to count the files you inported into Audacity!\n\n")
num_files = int(input("enter the number of files imported into Audacity: "))
file_start = 0
file_end = int(input("length of each of the files (in seconds): "))
fadelen = int(input("crossfade length (in seconds): "))
increment = file_end - (fadelen/2)

do_command('SelectAll:')
do_command(f'SelectTime:End="{file_end}" RelativeTo="ProjectStart" Start="0"')
do_command('Trim:')
do_command('Align_EndToEnd')
do_command('MixAndRender')

batch_crossfade(num_files, file_start, file_end, fadelen)




