This repository contains code for the manuscript 'Characterising a species-rich and understudied tropical insect fauna using DNA barcoding' by Hemprich-Bennett et al. (2026).

The repository's analyses took place in Rstudio server in a docker container, pulled from dockerhub using the two scripts in the container_files directory. This image was built using the script in 'container_files/image_build.sh'. However the analyses should work in any R environment with the required packages installed.

Raw data files are available in the data/raw_data directory. One large raw file, `BOLD_Public.11-Jul-2025.tsv` (29 GB), used in scripts/07_big_public_dataset_processing.R, is not included here due to its size. It has been uploaded to GigaScience’s GigaDB repository, along with all processed data and results, and this repository, and the full dataset and repository will be downloadable from GigaScience's GigaDB portal.

Script 14 must be ran interactively, as it uses the taxize package which requires user input in some cases. See comments in the script for more details. As a result of this querying 'live' data, it may generate slightly different results from those submitted with this manuscript, due to any changes in the public dataset made in the meantime.
