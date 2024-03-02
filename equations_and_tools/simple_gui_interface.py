import tkinter as tk
from tkinter import filedialog, Text
import os, sys, subprocess # allows us to run the app.

root = tk.Tk() # like HTML, this holds the whole structure.
apps = []

if os.path.isfile('save.txt'):
     with open('save.txt', 'r') as f:
          tempApps = f.read()
          tempApps = tempApps.split(',')
          apps = [x for x in tempApps if x.strip()]

def addApp():

    for widget in frame.winfo_children():
        widget.destroy()

    filename= filedialog.askopenfilename(initialdir="/", title="Select File",
                                         filetypes=(("executables", "*.app"), ("all files", "*.*")))
    apps.append(filename)
    print(filename)
    for app in apps:
        label = tk.Label(frame, text=app, bg="gray")
        label.pack()

def open_file(filename):
    if sys.platform == "win32":
        os.startfile(filename)
    else:
        opener = "open" if sys.platform == "darwin" else "xdg-open"
        subprocess.call([opener, filename])

def runApps():
    for app in apps:
        open_file(app)

canvas = tk.Canvas(root, height=700, width=700, bg="#FFF000")
canvas.pack()

frame = tk.Frame(root, bg="#FFFFFF")
frame.place(relwidth=0.8, relheight=0.8, relx = 0.1, rely = 0.1)

openFile = tk.Button(root, text="Open File", padx=10,
                     pady=5, fg="#000000", bg="#263D42", command=addApp)

openFile.pack()

runApps = tk.Button(root, text="Run Apps", padx=10,
                     pady=5, fg="#000000", bg="#263D42", command=runApps)

runApps.pack()

for app in apps:
     label = tk.Label(frame, text=app)
     label.pack()

root.mainloop()

with open('save.txt', 'w') as f:
          for app in apps:
             f.write(app + ',')