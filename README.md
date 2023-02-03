# Narwhals-Prey-Capture-Efficiency

This repository contains code and data necessary to generate the analyses and figures associated with the manuscript entitled "Extremely low seasonal prey capture efficiency in a deep-diving whale, the narwhal" , published in Biology Letters in 2023. 
Manuscript authors: Philippine Chambault, Susanna Blackwell, Mads Peter Heide-Jørgensen.

The repository is organised as follows:

- Sub-directory "Scripts": all scripts needed to conduct the analysis, the figures and tables described in the associated Manuscript.


The filtered dataset needed to run the scripts will be made available on DRYAD soon. It includes four *.txt files containing the stomach temperature data recorded by both the STP tags (n=7) and the Acousondes (n=3). The files are organised as follow:

- The high resolution data for each of the 3 narwhals equipped with both an Acousonde and a STP tag (1 file per whale); each file (sampling rate: 1-sec) contains the following columns: date and time, depth (in m), buzz, (0 vs 1) , click (0 vs 1), days (since departure), surf (at the surface or not), raw ST values (in °C), interpolated ST values every second, type (NA, feeding start, drop, feeding end), feeding (feeding vs. no feeding), event (ST drop number);

- The low resolution data  collected by the STP tags of the 7 narwhals not equipped with an Acousonde ("STP_clean_7ids.csv") and contains the following columns: ptt (individual ID), stp (stomach temperature values in °C), dateTime, depth (in m), type2 (NA, start, end of feeding event) and event.



 
