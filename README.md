# MapBiomas Accuracy Source Code

This repository organizes the MapBiomas accuracy codes.

If you want to dig more deep about the accuracy approach, you could acess this [publication](https://www.researchsquare.com/article/rs-819697/v1)

**Requisites**:

  * Python 3.7 or above
  
  * scikit-learn library for Python

  * pandas library for Python

  * pyarrow and fastparquet library for Python
  
  **Recommendations**: 
   * For Windows, install [Miniconda - Python 3.7](https://docs.conda.io/en/latest/miniconda.html) or above and add it to the system variable PATH like:
      
      * PATH =  C:\ProgramData\Miniconda3; C:\ProgramData\Miniconda3\Library\bin; C:\ProgramData\Miniconda3\Scripts;
  
  * Install scikit-learn library using **conda install -c intel scikit-learn** on system terminal (windows prompt)
  * Install pandas, pyarrow and fastparquet libraries using **conda install pandas pyarrow fastparquet** on system terminal (windows prompt) 

# How to use

## 1. Export reference and classification matrix from Google Earth Engine (GEE)

  Copy and paste the code **src/estimates_codes/1_export_gee_input.js** on your GEE code editor and click on Run. After that, click on Task and run every task.

## 2. Getting Accuracy Assessment information with Python

  Open your system terminal and run the following line like **python3 src/estimates_codes/2_accuracy_estimates.py <INPUT_DIR> <OUTPUT_DIR> <COLLECTION_NAME> <OUTPUT_FILENAME>**. Enter the directory address of the exported files as INPUT_DIR and the output files as OUTPUT_DIR. Also, give a name for your data collection in COLLECTION_NAME (e.g. "c8") and file name (e.g. "accuracy_mapbiomas_col8") in OUTPUT_FILENAME. Sit in a comfortable chair, grab a book, a coffee and wait ... this process can take a while.
