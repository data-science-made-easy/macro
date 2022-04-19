library(shiny)
library(shinyWidgets)
library(DT)
# library(shinybusy) # use ::
source('R/dropdown-generator.R')
source('R/initialialisation-functions.R')
source('R/functions.R')
source('R/settings.R')
source('R/thermometer.R')
source('R/traffic-light.R')
source('R/disable-tabs.R')

# LOAD JAMES
if ("linux-gnu" == version$os) {
  source('/home/mdk@cpb.nl/james/saffier/load-james.R')
} else {
  source('~/Dropbox/cpb/git/james/initialize-james-anywhere.R')  
}