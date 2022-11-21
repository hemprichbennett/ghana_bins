FROM rocker/rstudio:latest

ENV TZ=Europe/Moscow \
    DEBIAN_FRONTEND=noninteractive

RUN apt update
RUN apt upgrade -yq
RUN apt-get -yq install python3-pip git

RUN pip install -U pip
RUN pip install pymssql pandas sqlalchemy

COPY Rprofile.site /etc/R

RUN install.r remotes
COPY DESCRIPTION .
RUN Rscript -e "remotes::install_deps()"
