This repository contains code for the preprint and submitted manuscript 'Characterising a species-rich and understudied tropical insect fauna using DNA barcoding' by Hemprich-Bennett et al. (2025).

The repository's analyses took place in Rstudio server in a docker container, pulled from dockerhub using the two scripts in the container_files directory. This image was built using the script in 'container_files/image_build.sh'. However the analyses should work in any R environment with the required packages installed.

This repository DOES NOT contain one raw file, file `BOLD_Public.11-Jul-2025.tsv`, used in script `scripts/07_big_public_dataset_processing.R` as it is too large. A similar file can be obtained from https://v4.boldsystems.org/index.php/datapackages, although the exact data version will be different as this file was a snapshot of all BOLD's public data on July 11th 2025. 
