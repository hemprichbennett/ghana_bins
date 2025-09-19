date=$(date '+%Y-%m-%d')

docker build --platform=linux/amd64 --pull --rm -f "container_files/DOCKERFILE" -t hemprichbennett/bin_accumulation_img:$date "."

docker push hemprichbennett/bin_accumulation_img:$date
