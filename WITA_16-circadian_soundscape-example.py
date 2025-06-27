import glob
import os
import matplotlib.pyplot as plt
from maad import sound, util
from scipy.io.wavfile import write
import numpy as np
import csv
from datetime import datetime

# Set base directory containing daily subfolders
base_path = '/Users/your_directory_here/Moth Temp/WITA16/'
sample_len = 15  # sample length for each of the audio files in seconds
start = 0 # start hour for daily observation (e.g., 00:00)
end = 24 # end hour for daily observation (e.g., 24:00)
freq_max = 50 # upper threshold of frequency range (e.g., 50kHz)

# Create CSV metadata file
metadata_file = 'outputs-log/processing_log.csv'
os.makedirs('outputs-log', exist_ok=True)
write_header = not os.path.exists(metadata_file)

with open(metadata_file, mode='a', newline='') as log_file:
    log_writer = csv.writer(log_file)
    if write_header:
        log_writer.writerow(['Timestamp', 'Observation', 'Num Files', 'Sample Length (s)', 'Total Duration (s)', 'Spectrogram File', 'Audio File'])

# Loop through all subdirectories
subfolders = [f.path for f in os.scandir(base_path) if f.is_dir()]

for folder in subfolders:
    observation = os.path.basename(folder)
    print(f"\nProcessing folder: {observation}")

    flist = glob.glob(os.path.join(folder, '*.WAV'))
    flist.sort()
    if not flist:
        print(f"  Skipped {observation}: no WAV files found.")
        continue

    long_wav = []
    for fname in flist:
        s, fs = sound.load(fname)
        s = sound.trim(s, fs, 0, sample_len)
        long_wav.append(s)

    print("  Files loaded. Combining audio...")
    long_wav = util.crossfade_list(long_wav, fs, fade_len=0.5)

    Sxx, tn, fn, ext = sound.spectrogram(long_wav, fs, window='hann', nperseg=1024, noverlap=512)

    fig, ax = plt.subplots(1,1, figsize=(10,3))
    util.plot_spectrogram(Sxx, extent=[start, end, 0, freq_max],
                          ax=ax, db_range=80, gain=25, colorbar=False)
    ax.set_xlabel('Time [Hours]')
    ax.set_xticks(range(0,25,1))

    os.makedirs("outputs-spectral", exist_ok=True)
    spec_file = f"outputs-spectral/{observation}.png"
    plt.savefig(spec_file)
    plt.close()
    print(f"  Saved spectrogram to {spec_file}")

    os.makedirs("outputs-audio", exist_ok=True)
    long_wav_norm = np.int16(long_wav / np.max(np.abs(long_wav)) * 32767)
    audio_file = f"outputs-audio/{observation}.wav"
    write(audio_file, fs, long_wav_norm)
    print(f"  Exported audio to {audio_file}")

    # Log metadata
    with open(metadata_file, mode='a', newline='') as log_file:
        log_writer = csv.writer(log_file)
        log_writer.writerow([
            datetime.now().isoformat(timespec='seconds'),
            observation,
            len(flist),
            sample_len,
            len(long_wav) / fs,
            spec_file,
            audio_file
        ])

