FROM rocker/shiny-verse:latest

# RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#  libudunits2-dev \
#  libgeos-dev \
#  libgeos++-dev \
#  libgdal-dev \
#  && install2.r --error \
RUN install2.r --error \
  ggplot2 \
  sqldf \
  viridis \
  plotly \
  lubridate

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libnss-wrapper \
  gettext-base

COPY /app /srv/shiny-server/
COPY /shiny-server.conf /etc/shiny-server/shiny-server.conf

# --------------------------------------------------------
#
# copy over the startup script
#
# --------------------------------------------------------
COPY passwd.template /passwd.template
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+x /usr/bin/shiny-server.sh && mkdir -p /var/log/shiny-server && chmod a+rwx /var/log/shiny-server && mkdir -p /var/lib/shiny-server/bookmarks/shiny

