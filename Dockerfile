FROM r-base:latest

ENV NUM_CORES 4
WORKDIR /app

RUN Rscript -e "install.packages('data.table')"
RUN Rscript -e "install.packages('dplyr')"
RUN Rscript -e "install.packages('geosphere')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('gridExtra')"
RUN Rscript -e "install.packages('maps')"
RUN Rscript -e "install.packages('mapproj')"
RUN Rscript -e "install.packages('purrr')"
RUN Rscript -e "install.packages('raster')"
RUN Rscript -e "install.packages('sp')"

ADD . /app

CMD ["Rscript", "visualise_globe.R"] 