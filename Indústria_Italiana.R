library(tidytuesdayR)
library(dplyr)
library(shiny)
library(bs4Dash)
library(ggplot2)
library(fresh)
library(DT)
library(tidyverse)
library(fmsb)
library(bsicons)

tuesdata <- tidytuesdayR::tt_load(2026, week = 18)

food_beverages <- tuesdata$food_beverages
textiles <- tuesdata$textiles
transport <- tuesdata$transport

ui <- dashboardPage(
  freshTheme = Indústria Italiana ,
  scrollToTop = TRUE,
  fullscreen = TRUE,
  dark = TRUE,
  help = NULL,
  title = "Indústria Italiana",

    header = dashboardHeader(
    title = dashboardBrand(
      title = "Indústria Italiana",
      color = "primary",
      image = ""
    )
  ),
