# mdib-hd

This repository contains analysis code for this project on the Open Science Framework (OSF): https://osf.io/bsft6/.

Author: Jeremy W. Eberle

# Data

The present scripts import `final HD Aim 1 data_deid_OSF.csv` stored in the `data/bot_cleaned` folder of the OSF project. The data were collected 6/2021-1/2023 by Dr. Jessie Gibson via RedCap for the MDIB Development Study. The file was obtained from Dr. Gibson on 2/28/2024.

Bot responses were already cleaned from this file by Dr. Gibson and a graduate research assistant. Dr. Gibson also removed certain columns for deidentification or irrelevance to the present manuscript. For the original raw data, please contact Dr. Gibson.

# Code

To run the scripts, create a parent folder (with any desired name, indicated by `.` below) with two subfolders: `data/bot_cleaned` and `code`. The working directory must be set to the parent folder for the scripts to import and export files correctly using relative file paths.

```
.                                # Parent folder (i.e., working directory)
├── data                         # Data subfolder
├── └── bot_cleaned              # 1 CSV file from OSF project
└── code                         # Code subfolder
```

Put the CSV file from OSF above in the `bot_cleaned` folder, and put the scripts in the `code` folder. The scripts are to be run in the order listed. At the top of each script, restart R (CTRL+SHIFT+F10 on Windows) and set your working directory to the parent folder (CTRL+SHIFT+H).

The scripts will create additional folders within the `data` folder, and a `results` folder with various subfolders. For the final structure of the `data` and `results` folders (and files outputted to these folders), see the OSF project.