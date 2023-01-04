# Python workflow for computing acoustic indices using the scikit-maad package.
# Tristan Louth-Robins. 2023

# Version 0.1 (Jan 2023): 'Real novice pass / I'm an R user, please be gentle trial'

# Resources: https://scikit-maad.github.io/quickstart.html
# Specifically: https://scikit-maad.github.io/_auto_examples/2_advanced/plot_extract_alpha_indices.html

# STEP 1: Read in acoustic data and compute acoustic indices -----------------------
# Required packages -
import pandas as pd
import os
import maad.sound
from maad import sound, features
from maad.util import (date_parser, plot_correlation_map,
                       plot_features_map, plot_features, false_Color_Spectro)

# Set variables --------------------------------------------------------------------
SPECTRAL_FEATURES=['MEANf','VARf','SKEWf','KURTf','NBPEAKS','LEQf',
'ENRf','BGNf','SNRf','Hf', 'EAS','ECU','ECV','EPS','EPS_KURT','EPS_SKEW','ACI',
'NDSI','rBA','AnthroEnergy','BioEnergy','BI','ROU','ADI','AEI','LFC','MFC','HFC',
'ACTspFract','ACTspCount','ACTspMean', 'EVNspFract','EVNspMean','EVNspCount',
'TFSD','H_Havrda','H_Renyi','H_pairedShannon', 'H_gamma', 'H_GiniSimpson','RAOQ',
'AGI','ROItotal','ROIcover']

TEMPORAL_FEATURES=['ZCR','MEANt', 'VARt', 'SKEWt', 'KURTt',
               'LEQt','BGNt', 'SNRt','MED', 'Ht','ACTtFraction', 'ACTtCount',
               'ACTtMean','EVNtFraction', 'EVNtMean', 'EVNtCount']

df = date_parser("../../data/", dateformat='Hex', verbose=True)

#####################################################################################

# simple test with single file:
s, fs = sound.load('data/test.WAV')

# Examine audio file and regions of interest - spectrogram and amplitude --------
# rois.find_rois_cwt(s, fs, flims=(4500,8000), tlen=2, th=0, display=True)

Sxx, tn, fn, ext = maad.sound.spectrogram(s, fs, mode='amplitude')

# Bioacoustic index --
BI = maad.features.bioacoustics_index(Sxx, fn)

print('BI Soundecology : %2.2f ' %BI)
# BI Soundecology : 9.43

#######################################################################################

df_indices = pd.DataFrame()
df_indices_per_bin = pd.DataFrame()

for index, row in df.iterrows() :

    # get the full filename of the corresponding row
    fullfilename = row['file']
    # Save file basename
    path, filename = os.path.split(fullfilename)
    print ('\n**************************************************************')
    print (filename)

    #### Load the original sound (16bits) and get the sampling frequency fs
    try :
        wave,fs = sound.load(filename=fullfilename, channel='left', detrend=True, verbose=False)

    except:
        # Delete the row if the file does not exist or raise a value error (i.e. no EOF)
        df.drop(index, inplace=True)
        continue

    """ =======================================================================
                     Computation in the time domain
    ========================================================================"""

    # Parameters of the audio recorder. This is not a mandatory but it allows
    # to compute the sound pressure level of the audio file (dB SPL) as a
    # sonometer would do.
    S = -35         # Sensbility microphone-35dBV (SM4) / -18dBV (Audiomoth)
    G = 26+16       # Amplification gain (26dB (SM4 preamplifier))

    # compute all the audio indices and store them into a DataFrame
    # dB_threshold and rejectDuration are used to select audio events.
    df_audio_ind = features.all_temporal_alpha_indices(wave, fs,
                                          gain = G, sensibility = S,
                                          dB_threshold = 3, rejectDuration = 0.01,
                                          verbose = False, display = False)

    """ =======================================================================
                     Computation in the frequency domain
    ========================================================================"""

    # Compute the Power Spectrogram Density (PSD) : Sxx_power
    Sxx_power,tn,fn,ext = sound.spectrogram (wave, fs, window='hanning',
                                             nperseg = 1024, noverlap=1024//2,
                                             verbose = False, display = False,
                                             savefig = None)

    # compute all the spectral indices and store them into a DataFrame
    # flim_low, flim_mid, flim_hi corresponds to the frequency limits in Hz
    # that are required to compute somes indices (i.e. NDSI)
    # if R_compatible is set to 'soundecology', then the output are similar to
    # soundecology R package.
    # mask_param1 and mask_param2 are two parameters to find the regions of
    # interest (ROIs). These parameters need to be adapted to the dataset in
    # order to select ROIs
    df_spec_ind, df_spec_ind_per_bin = features.all_spectral_alpha_indices(Sxx_power,
                                                            tn,fn,
                                                            flim_low = [0,1500],
                                                            flim_mid = [1500,8000],
                                                            flim_hi  = [8000,20000],
                                                            gain = G, sensitivity = S,
                                                            verbose = False,
                                                            R_compatible = 'soundecology',
                                                            mask_param1 = 6,
                                                            mask_param2=0.5,
                                                            display = False)

    """ =======================================================================
                     Create a dataframe
    ========================================================================"""
    # First, we create a dataframe from row that contains the date and the
    # full filename. This is done by creating a DataFrame from row (ie. TimeSeries)
    # then transposing the DataFrame.
    df_row = pd.DataFrame(row)
    df_row =df_row.T
    df_row.index.name = 'Date'
    df_row = df_row.reset_index()

    # add scalar indices into the df_indices dataframe
    df_indices = df_indices.append(pd.concat([df_row,
                                              df_audio_ind,
                                              df_spec_ind], axis=1))
    # add vector indices into the df_indices_per_bin dataframe
    df_indices_per_bin = df_indices_per_bin.append(pd.concat([df_row,
                                                              df_spec_ind_per_bin], axis=1))
# Set back Date as index
#df_indices = df_indices.set_index('Date') <--- throws an error, can't find Date variable
#df_indices_per_bin = df_indices_per_bin.set_index('Date')

print(df_indices)
