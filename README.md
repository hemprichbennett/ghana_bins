This repository contains code for the preprint and submitted manuscript 'Characterising a species-rich and understudied tropical insect fauna using DNA barcoding' by Hemprich-Bennett et al. (2025).

# Software environment

The repository's analyses took place in Rstudio server in a docker container, pulled from dockerhub using the two scripts in the container_files directory. This image was built using the script in 'container_files/image_build.sh'. However the analyses should work in any R environment with the required packages installed.

This repository DOES NOT contain one raw file, used in script `scripts/07_big_public_dataset_processing.R` as it is too large. This file can be obtained from https://v4.boldsystems.org/index.php/datapackages. This is why there are currently two 'rstudio_server' scripts in the container_files directory. The first, `rstudio_server_no_big_data.sh` can be used to build the docker image without this large file, and the second, `rstudio_server_with_big_data.sh` can be used to build the docker image if you have downloaded this large file and placed it in the container_files directory.