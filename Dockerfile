
FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y --no-install-recommends 
    libgdal-dev 
    gdal-bin 
    libgeos-dev 
    libproj-dev 
    proj-bin 
    proj-data 
    libudunits2-dev 
    libssl-dev 
    libcurl4-openssl-dev 
    libxml2-dev 
    libfontconfig1-dev 
    libfreetype6-dev 
    libpng-dev 
    libtiff5-dev 
    libjpeg-dev 
    pkg-config 
    make 
    g++ 
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c(
    \"shiny\",
    \"mapgl\",
    \"sf\",
    \"terra\",
    \"dplyr\",
    \"readr\",
    \"tidyr\",
    \"stringr\",
    \"janitor\",
    \"htmltools\",
    \"geojsonsf\",
    \"classInt\",
    \"viridisLite\",
    \"png\",
    \"jsonlite\",
    \"httpuv\"
), repos=\"https://cloud.r-project.org\")"

WORKDIR /app

COPY app/ /app/

EXPOSE 7860

CMD ["R", "-e", "shiny::runApp(\"/app\", host=\"0.0.0.0\", port=7860, launch.browser=FALSE)"]

