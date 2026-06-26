# =============================================================================
# GLASSBOX  app.R
# Thin launcher that fixes the load order. It sources the data and helpers,
# then the act modules, then builds the UI and the server from their files, so
# every module function exists before the navbar is built. The teammate's
# global.R, ui.R, server.R and the R folder are left unchanged.
# =============================================================================

library(shiny)

# 1. data, palette, objects, helpers
source("global.R")

# 2. act modules, so caseUI and the rest are defined
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(.f)

# 3. build the UI and the server from their files
ui     <- source("ui.R",     local = FALSE)$value
server <- source("server.R", local = FALSE)$value

shinyApp(ui, server)
