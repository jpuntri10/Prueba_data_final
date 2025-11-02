FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev

RUN R -e 'install.packages(c("shiny","ggplot2","dplyr","openxlsx","scales","data.table"))'

COPY . /app
WORKDIR /app

CMD ["R", "-e", "shiny::runApp('/app', port=as.numeric(Sys.getenv('PORT')), host='0.0.0.0')"]
