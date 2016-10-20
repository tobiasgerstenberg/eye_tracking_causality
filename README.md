# Eye-tracking causality #

This repository contains materials of the project Eye-tracking Causality. Please contact [me](mailto:tger@mit.edu)  in case you have any questions. 

## Code ##

This folder contains the experiment and analysis code. 

### Matlab ###

- Code that was used to run the experiment in MATLAB using the Psychophysics toolbox: 
	+ `tracking_counterfactual.m`: Used to run the counterfactual condition
	+ `tracking_causal.m`: Used to run the causal condition
	+ `tracking_outcome.m`: Used to run the outcome condition

### R ###

- Code to run the analysis and make plots in R.
- `analysis.R` performs statistical analysis and plots the results, the file also creates a Markdown report of the results which may be accessed [here](https://rawgit.com/tobiasgerstenberg/eye_tracking_causality/master/code/R/analysis.html)  
- `HMM_model.R` runs the HMM and Viterbi algorithm on an individual participant
- `readData.R` reads in the behavioral data and classification of the eye-movements to create a summary file of the data (which `analysis.R` uses) 
- `ballPositions.txt` contains the positions of each ball on the screen over time (as outputted by the physics simulator)

### Flash ###

- Creates the different clips that participants viewed in the experiment using the physics engine Box2D. 

## Data ##

- Contains the raw data files as well as summary data files. 

## Figures ##

This folder contains the following figures.

### Diagrams ###

- Diagrams of the 18 test clips that were shown to participants in the experiment (both as pdf and png).

### Paper

- Figures contained in the paper. 

### Plots ###

#### Counterfactual looks ####

- Plots showing the results of the static classification for each clip and condition separately. 

#### Heat maps ####

- Heat maps of participants' looking patterns separated by clip and condition. 

#### Hidden Markov Model over time ####

- Plots showing how the posterior over states changes over time for the different clips and conditions. 

### Screenshots ###

- Screenshots of the instructions for the three different conditions. 

## Videos ##

This folder contains different videos, showing just the clips, participants' eye-movements, the Hidden Markov Model, and an example for how the model classifies a participants' looks. 

### Classification ###

- Videos showing how the HMM classifies a participants' looks in the causal condition for each clip and instance. The videos are slowed down and play at 10 fps (rather than 30 fps) so that it's easier to see how the model classifies the different looks. 
- The label at the top of the screen indicates what state the model believes that the participant is in (using a Viterbi algorithm as described in the paper).

### Clips ###

- Videos of the 18 test clips and 2 practice clips. 

### HMM ###

- Clips that show how the Hidden Markov Model's classification regions change over time for each clip. 

### Tracking ###

- Clips showing each participants' eye-movements, organized by experimental condition (and the eye-movements of those participants who were excluded from the experiment based on track loss).  





