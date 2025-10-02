This repository contains code for the preprint and submitted manuscript 'Characterising a species-rich and understudied tropical insect fauna using DNA barcoding' by Hemprich-Bennett et al. (2025).

All scripts are to be ran in Rstudio using the Docker image built from the files in the container_files directory.

Some scripts require you to provide a file 'bold_key.R' in the repository's base directory. This file should contain your BOLD API key as follows:

```R
bold_key <- 'your_bold_api_key_here'
```

This key can be obtained by registering for a BOLD account at https://www.boldsystems.org and then copying the API Key found in your account settings.

The repository's analyses took place in Rstudio server in a docker container, pulled from dockerhub using the two scripts in the container_files directory. This image was built using the script in 'container_files/image_build.sh'.

This repository DOES NOT contain one raw file, used in script `scripts/07_big_public_dataset_processing.R` as it is too large. This file can be obtained from https://v4.boldsystems.org/index.php/datapackages.