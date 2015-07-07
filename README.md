# EcoDAS-scicomm-survey-analysis
This repository contains the code used to analyse the data for the science communication chapter of the 2014 EcoDAS.

> ####**WARNING**: when commiting triple check that you are not uploading sensitive personal information to the repository! (_i.e._ add the any data file to .gitignore) 

#Instructions:
These instructions assume you have already installed [R](http://www.r-project.org/) and [RStudio](http://www.rstudio.com/). Also, if you are not already using the GitHub app, you should really consider using Git or GitHub in the future! Version control will improve your life.

1. If you have the GitHub app installed for [Windows](https://windows.github.com/) or [Mac](https://mac.github.com/) you can click "Clone in Desktop" on the [website](https://github.com/remi-daigle/EcoDAS-scicomm-survey-analysis), otherwise click "[Download ZIP](https://github.com/remi-daigle/EcoDAS-scicomm-survey-analysis/archive/master.zip)" and extract the folder.

2. Unfortunately, this repository **DOES NOT CONTAIN THE DATA** since the survey data contains personal information. Please get the data from Elisha's email and put it in the main working directory for this project (_e.g._ `C:/.../EcoDAS-scicomm-survey-analysis`).

3. Open `EcoDAS-scicomm-survey-analysis.Rproj` from the main working directory with Rstudio.

4. To keep things tidy, I've organized the separate tasks into discrete scripts:
	* `master.R` is the main script that sources all the other tasks
	* `function.R` is a storage space for any new custom functions we may create
	* `load_data.R` loads the data and transforms it to a more R friendly format

5. If you create another script, please remember to add it to this [readme](https://github.com/remi-daigle/EcoDAS-scicomm-survey-analysis/blob/master/README.md) file
6.  Sync, write code, commit, sync, repeat!

For more help with Git and GitHub see:
https://help.github.com/articles/good-resources-for-learning-git-and-github/
http://swcarpentry.github.io/git-novice/
or Google!
