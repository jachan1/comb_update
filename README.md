## Setup
You will probably have to authorize googlesheets first:       
require(googlesheets)             
gs_auth()

## Run App
Shiny runs apps in the local environment, so the extra wrapper below runs the app in an empty environment.

source(textConnection("shiny::runGitHub('comb_update', 'jachan1')"), local=attach(NULL))
