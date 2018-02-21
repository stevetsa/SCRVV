FROM r-base:latest

MAINTAINER Steve Tsang "mylagimail2004@yahoo.com"
# Original Dockerfile obtained from Winston Chang "winston@rstudio.com"
# https://github.com/rocker-org/shiny

# Install dependencies and Download and install shiny server
RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    git-core && \
    wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'plotly', 'tidyr', 'ggplot2', 'dplyr', 'DT'), repos='https://cran.rstudio.com/')" && \

#    git clone --recurse-submodules https://github.com/NCBI-Hackathons/ViRGo    
#RUN cp -R /ViRGo/* /srv/shiny-server/. && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
