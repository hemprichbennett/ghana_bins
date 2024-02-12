docker run --rm -ti -e PASSWORD=password -p 8781:8787 \
  -v .:/home/rstudio/bin_accumulation \
  -v /Users/dhb/.config/rstudio/rstudio-prefs.json:/home/rstudio/.config/rstudio/rstudio-prefs.json \
  hemprichbennett/bin_accumulation_img:2024-02-05

    #hemprichbennett/op1_net_img:2024-01-29