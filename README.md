# MapBiomas Accuracy Source Code

This repository organizes the MapBiomas accuracy codes.

**Requisites**:

  * Python 3.7 or above
  
  * scikit-learn library for Python
  
  * R 3.6.2 o above
  
  * R Studio 1.1.2.5 or above
  
  * tidyverse , ggplot2 , stringr , reshape2 , shiny , htmlwidgets , highcharter and dplyr packages for R
  
  **Recommendations**: 
   * For Windows, install [Miniconda - Python 3.7](https://docs.conda.io/en/latest/miniconda.html) or above and add it to the system variable PATH like:
      
      * PATH =  C:\ProgramData\Miniconda3; C:\ProgramData\Miniconda3\Library\bin; C:\ProgramData\Miniconda3\Scripts;
  
  * Install scikit-learn library using **conda install -c intel scikit-learn** on system terminal (windows prompt)   

# How to use

## 1. Export reference and classification matrix from Google Earth Engine (GEE)

  Copy and paste the code **src/estimates_codes/1_export_gee_input.js** on your GEE code editor and click on Run. After that, click on Task and run every task.

## 2. Getting Accuracy Assessment information with Python

  Open your system terminal and run the following line like **python3 src/estimates_codes/2_accuracy_estimates.py <INPUT_DIR> <OUTPUT_DIR>**. Enter the directory address of the exported files as INPUT_DIR and the output files as OUTPUT_DIR. Sit in a comfortable chair, grab a book, a coffee and wait ... this process can take a while.
  
  When the process ends, run the code **3_format_toShinny_for_Unix.sh** (For Windows: 3_format_toShinny_for_Windows.bat) inside the output directory. This code prepares the file names and directory hierarchy for the Shiny reading format.
  
  One last change needs to be made. Download the [Sublime](https://www.sublimetext.com/) code editor, drag and drop all .CSV files from the *lv1*, *lv2* and *lv3* output directories, then press Ctrl + Shift + F and replace all the "NA" for NaN (without quotes).
  
## 3. Visualize accuracy information using R/Shiny

To visualize the accuracy information that you have process you need to:
* Copy the directories *lv1*, *lv2* and *lv3* to the directory **src/visualization_codes/integracao/**
* Open the src/visualization_codes/App.R code with RStudio
* Click on Run App (at the top center of the screen)
