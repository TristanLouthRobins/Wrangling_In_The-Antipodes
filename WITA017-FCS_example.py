# ~ WRANGLING IN THE ANTIPODES ~            
# 17: Exploring scikit-maad - False Colour Spectrum analysis
# Tristan Louth-Robins, January 2026

import numpy as np
import matplotlib.pyplot as plt
from maad import sound, features
from maad.util import power2dB, plot2d
from skimage import transform
from sklearn.preprocessing import MinMaxScaler
from sklearn.decomposition import NMF

print("***********\nDependencies retrieved.\n***********\n\n")

s, fs = sound.load('your_file_path_here/audio_file.wav') # <<<< Enter your file path here.
Sxx, tn, fn, ext = sound.spectrogram(s, fs, nperseg=1024, noverlap=512, fscale='log')

print("***********\nAudio loaded.\n***********\n\n")

Sxx_db = power2dB(Sxx, db_range=70)
Sxx_db = transform.rescale(Sxx_db, 0.5, anti_aliasing=True, channel_axis=None)  # rescale for faster computation # multichannel=False change by  channel_axis=None
plot2d(Sxx_db, figsize=(4,10), extent=ext)

shape_im, params = features.shape_features_raw(Sxx_db, resolution='low')

# Format the output as an array for decomposition
print("\n\nFormatting the output as an array for decomposition...\n\n")
X = np.array(shape_im).reshape([len(shape_im), Sxx_db.size]).transpose()

# Decompose signal using non-negative matrix factorization
print("\n\nDecomposing signal...\n\n")
Y = NMF(n_components=3, init='random', random_state=0).fit_transform(X)

Y = MinMaxScaler(feature_range=(0,1)).fit_transform(Y)
intensity = 1 - (Sxx_db - Sxx_db.min()) / (Sxx_db.max() - Sxx_db.min())
plt_data = Y.reshape([Sxx_db.shape[0], Sxx_db.shape[1], 3])
plt_data = np.dstack((plt_data, intensity))

fig, axes = plt.subplots(3,1, figsize=(10,8))
for idx, ax in enumerate(axes):
    ax.imshow(plt_data[:,:,idx], origin='lower', aspect='auto',
              interpolation='bilinear')
    ax.set_axis_off()
    ax.set_title('Basis ' + str(idx+1))

print("\n\nPrinting the plots...\n\n")
plt.show()

fig, ax = plt.subplots(2,1, figsize=(10,6))
ax[0].imshow(Sxx_db, origin='lower', aspect='auto', interpolation='bilinear', cmap='gray')
ax[0].set_axis_off()
ax[0].set_title('Spectrogram')
ax[1].imshow(plt_data, origin='lower', aspect='auto', interpolation='bilinear')
ax[1].set_axis_off()
ax[1].set_title('False-color spectrogram')

plt.show()