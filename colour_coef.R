#' ---
#' title: "Calculating colour coefficients"
#' subtitle: 'All coefficients are calculated using the colour science package, Illuminant A, 10 deg observer and CIE 1964' 
#' author: "Alex Koiter"
#' date: "July 21, 2021"
#' output:
#'   html_document:
#'     theme: cosmo
#'     toc: true
#'---
#' # Load libraries 
#+ message = FALSE
library(colorscience)
library(tidyverse)
library(readxl)
library(purrr)

#' # Read in data files  
filenames <- list.files(path = "./Example data", # Location of data
                        pattern = "*.txt", # will grab any thing with the .txt extension
                        recursive = TRUE, # It will search all subdirectories
                        full.name = TRUE) # Will include full path name

get_file <- function(f) {
  read.delim(f, fileEncoding = guess_encoding(f)$encoding[1], header = FALSE) %>%
    as_tibble() %>%
    gather(A, B) %>%
    separate(B, into = c("wl", "value"), sep = ", ") %>%
    mutate(id = str_remove(basename(f), ".txt")) %>% # Uses the name of the .txt file as the ID
    select(-A)
}

allData <- lapply(filenames, get_file)

data <- do.call(rbind, allData)

#' # Average spectra across the 10 scans  
#' 
avg.data <- data %>%
  mutate(wl = as.numeric(wl)) %>%
  filter(wl >= 360 & wl <= 830) %>%
  droplevels() %>%
  group_by(wl,id) %>%
  summarise(value = mean(as.numeric(value), na.rm = T)*100) %>%
  print() 

#' # Spectra to XYZ 
#' 

my_XYZ <- function(x) {
  z <- spectra2XYZ(as.matrix(x), illuminantIn = illuminantA, ciexyzIn = ciexyz64) * 100
  data.frame(X = z[1], Y = z[2], Z = z[3])
}

XYZ <- avg.data %>%
  #gather(id, value, -wl) %>%
  ungroup() %>%
  nest(data = c(wl, value)) %>%
  mutate(XYZ = map(data, my_XYZ)) %>%
  unnest(XYZ) %>%
  select(-data) %>%
  print()

#' # XYZ to xyY 

my_xyY <- function(x) {
  z <- XYZ2xyY(c(x[1], x[2], x[3]))
  as.data.frame(z) %>%
    rename(x = V1, y = V2, Y = V3)
}

xyY <- XYZ %>%
  mutate(v = pmap(.l = list(X, Y,  Z), 
                  .f = ~ my_xyY(x = c(..1, ..2, ..3)))) %>%
  select(-X, -Y, -Z) %>%
  unnest(cols = c(v)) %>%
  select(-Y) %>%
  print()

#' # XYZ to lab 

d <- XYZperfectreflectingdiffuser %>%
  mutate_at(.vars = vars(-Illuminant), .funs = ~ . * 100)

my_lab <- function(x) {
  z <- XYZ2Lab(c(x[1], x[2], x[3]), illuminant = "A", observer = 10, RefWhite = d)
  as.data.frame(z)
}

lab <- XYZ %>%
  mutate(v = pmap(.l = list(X, Y,  Z), 
                  .f = ~ my_lab(x = c(..1, ..2, ..3)))) %>%
  unnest(cols = c(v)) %>%
  select(-X, -Y, -Z) %>%
  print()


#' # XYZ to Luv 

my_luv <- function(x) {
  z <- XYZ2Luv(c(x[1]/100, x[2]/100, x[3]/100), illuminant = "A", observer = 10, RefWhite = d)
  as.data.frame(z)
}

luv <- XYZ %>%
  mutate(v = pmap(.l = list(X, Y,  Z), 
                  .f = ~ my_luv(x = c(..1, ..2, ..3)))) %>%
  unnest(cols = c(v))%>%
  select(-X, -Y, -Z, -L) %>%
  print()


#' # XYZ to RGB 

my_rgb <- function(x) {
  z <- XYZ2RGB(c(x[1], x[2], x[3])/100, illuminant = "A", observer = 10) * 255
  as.data.frame(z) %>%
    rename(R = V1, G = V2, B = V3)
}

RGB <- XYZ %>%
  mutate(v = pmap(.l = list(X, Y,  Z), 
                  .f = ~ my_rgb(x = c(..1, ..2, ..3)))) %>%
  unnest(cols = c(v)) %>%
  select(-X, -Y, -Z) %>%
  print()

#' # lab to lch  
# No function in the colour science package to calculate these 
# You can report h in radians or degrees. Work by the Lobb et al. group generally reports in degrees; However work by Martínez-Carreras et als reports in radians. Choose wisely :)

lch <- lab %>%
  group_by(id) %>%
  summarise(
    h = atan((b*pi/180)/(a*pi/180)), # provides h in radians
    #h = (atan(b/a))*180/pi, # provides h in degrees
    c = (a^2 + b^2)^0.5
    ) %>%
  print()

#' # Combine and export 
#' 
colour.coef <- XYZ %>%
  inner_join(xyY) %>%
  inner_join(luv) %>%
  inner_join(lab) %>%
  inner_join(lch) %>%
  inner_join(RGB) %>%
  print()

#' At this stage you could also merge the data, by the file id, with other meta data e.g., location, depth etc.

#+ eval = FALSE
write.csv(x = colour.coef, file = "Results.csv", row.names = F)

#' # Session Info and Data

#' My Packages
#+ R.options = list(width = 100)
devtools::session_info()
