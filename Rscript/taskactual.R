library(pacman)
p_load("readr", "ggplot2", "cowplot", "rstudioapi")
print("hello Joel")

#set active document context

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)