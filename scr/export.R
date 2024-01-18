library(shinylive)
shinylive::export(appdir = "smogstripes-appv2", destdir = "docs")
httpuv::runStaticServer("docs/", port=8008)

