docker run --rm -ti -e PASSWORD=password -p 8781:8787 \
  -v .:/home/rstudio/bin_accumulation \
  -v /Users/dhb/.config/rstudio/rstudio-prefs.json:/home/rstudio/.config/rstudio/rstudio-prefs.json \
  hemprichbennett/bin_accumulation_img:2025-06-10

