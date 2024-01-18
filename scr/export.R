library(shinylive)
shinylive::export(appdir = "smogstripes-appv2", destdir = "docs")

shinyuieditor::launch_editor(app_loc = "smogstripes-appv2/")
