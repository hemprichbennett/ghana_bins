docker run --rm -ti -e PASSWORD=password -p 8781:8787 \
  -v .:/home/rstudio/bin_accumulation \
  -v /Users/dhb/.config/rstudio/rstudio-prefs.json:/home/rstudio/.config/rstudio/rstudio-prefs.json \
  -v ~/data/manual_bold_queries/BOLD_Public.11-Jul-2025/BOLD_Public.11-Jul-2025.tsv:/home/rstudio/bin_accumulation/data/raw_data/BOLD_Public.11-Jul-2025.tsv \
  hemprichbennett/bin_accumulation_img:2025-07-11


