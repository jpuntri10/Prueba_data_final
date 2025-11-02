FROM rocker/shiny:latest

# Instala los paquetes necesarios
RUN R -e "install.packages(c('shiny','ggplot2','dplyr','scales','openxlsx'))"

# Copia todo el contenido del repo al contenedor
COPY . /srv/shiny-server/

# Expone el puerto 8080
EXPOSE 8080

# Comando para iniciar Shiny Server
CMD ["/usr/bin/shiny-server"]
