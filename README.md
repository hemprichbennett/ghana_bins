This repository contains code for the preprint and submitted manuscript 'Characterising a species-rich and understudied tropical insect fauna using DNA barcoding' by Hemprich-Bennett et al. (2025).

All scripts are to be ran in Rstudio using the Docker image built from the files in the container_files directory.

Some scripts require you to provide a file 'bold_key.R' in the repository's base directory. This file should contain your BOLD API key as follows:

```R
bold_key <- 'your_bold_api_key_here'
```

This key can be obtained by registering for a BOLD account at https://www.boldsystems.org/index.php/Account/Login and then copying the API Key found in your account settings.

While all data being used in this repository is publicly available on the BOLD projects 'GCEP', 'TMGHA' and 'TMGHB', I believe some necessary files (e.g. BIN availability) are only available through the web console for dataset owners. There is also additional sampling metadata that was impossible or impractical to provide to BOLD. Therefore, rather than beginning the scripts by querying using the new `BOLDconnectR` package to query BOLD, I've manually loaded in the data files. They are too large to be stored via git, but will be made available in the manuscript's Dryad repository etc on publication.
