#' ---
#' title: "Calculating colour coefficients"
#' subtitle: 'All coefficients are calculated based on: Barthod L, Kui L, Lobb DA, Owens PN, Martínez-Carreras N, Koiter AJ, Petticrew EL, McCullough GK, Cenwei L, Gaspar L (2015) Selecting color-based tracers and classifying sediment sources in the assessment of sediment dynamics using sediment source fingerprinting. Journal of Environment Quality 44:1605.'
#' author: "Alex Koiter"
#' date: "July 21, 2021"
#' output:
#'   html_document:
#'     theme: cosmo
#'     toc: true
#'---
#' # Load libraries 
#+ message = FALSE
library(tidyverse)
library(readxl)
library(purrr)

#' # Read in data files  
filenames <- list.files(path = "./Example data", # Location of data
                        pattern = "*.txt", # will grab any thing with the .txt extension
                        recursive = TRUE, # It will search all subdirectories
                        full.name = TRUE) # Will include full path name


allData <- lapply(filenames, 
                  function(.file){
                    dat <- read.table(textConnection(gsub(",", "\t", readLines(.file))))
                    dat$id <- as.character(gsub(pattern = ".txt", replacement = "", .file)) # removes extension
                    dat$id <- as.factor(gsub(pattern = ".*/", replacement = "", dat$id)) # removes path name
                    dat    # return the dataframe
                  })

data <- do.call(rbind, allData)

#' # Average spectra across the 10 scans  
#' 
avg.data <- data %>%
  filter(V1 >= 360 & V1 <= 830) %>%
  droplevels() %>%
  select(V1, V2, V4, V6, V8, V10, V12, V14, V16, V18, V20, id) %>%
  rename(wl = V1) %>%
  gather(sample_n, value, -id, -wl) %>%
  group_by(wl,id) %>%
  summarise(value = mean(value, na.rm = T)*100) %>%
  spread(key = id, value = value) %>%
  print() 

#' # Spectra to XYZ 
#' Load colour matching function
wts <- read_excel("/home/alex/Dropbox/TCLS Sample Inventory - 2015 to 2017/R colour/weights_1nm_360_830.xls", col_names = F) %>%
  rename(wx = ...1, wy = ...2, wz = ...3) 

XYZ <- avg.data %>%
  bind_cols(wts) %>%
  gather(id, value, -wl, -wx, -wy, -wz) %>%
  group_by(id) %>%
  summarise(X = sum(value/100 * wx * 100/(sum(wy))), Y = sum(value/100 * wy * 100/(sum(wy))), Z = sum(value/100 * wz * 100/(sum(wy)))) %>%
  print() 

#' # XYZ to xyY 

xyY <- XYZ %>%
  group_by(id) %>%
  summarise(x = X / (X + Y + Z), y = Y / (X + Y + Z)) %>%
  print.data.frame()

#' # XYZ to lab  
white = c(111.144, 100.00, 35.200)

lab <- XYZ %>%
  group_by(id) %>%
  summarise(
    l = ifelse(Y/white[2] > 0.008856, (116*(Y/white[2])^(1/3)-16), (903.3*(Y/white[2])+16) - 16),
    a = 500 * (ifelse(X/white[1] > 0.008856, ((X/white[1])^(1/3)), ((903.3*(X/white[1]) + 16)/116)) - ifelse(X/white[1] > 0.008856, ((Y/white[2])^(1/3)), (903.3*(Y/white[2]) + 16)/116)),
    b = 200 * (ifelse(Z/white[3] > 0.008856, ((Y/white[2])^(1/3)), (903.3*(Y/white[2]) + 16)/116) - ifelse(Z/white[3] > 0.008856, ((Z/white[3])^(1/3)), ((903.3*(Z/white[3]) + 16)/116)))
    ) %>%
  print()

#' # XYZ to luv   

luv <- XYZ %>%
  group_by(id) %>%
  summarise(
    u = 13 * (ifelse(Y/white[2] > 0.008856, (116*(Y/white[2])^(1/3)-16), (903.3*(Y/white[2])+16) - 16)) * ( (4 * X/(X + 15 * Y + 3 * Z)) - (4 * white[1]/(white[1] + 15 * white[2] + 3 * white[3]))),
    v = 13 * (ifelse(Y/white[2] > 0.008856, (116*(Y/white[2])^(1/3)-16), (903.3*(Y/white[2])+16) - 16)) * ( (9 * Y/(X + 15 * Y + 3 * Z))- (9 * white[2]/(white[1] + 15 * white[2] + 3 * white[3])))
  ) %>%
  print()


#' # spectra to RGB 

RGB <- data %>%
  filter(V1 >= 450 & V1 <= 600 | V1 >= 630 & V1 <= 690) %>%
  droplevels() %>%
  select(V1, V2, V4, V6, V8, V10, V12, V14, V16, V18, V20, id) %>%
  rename(wl = V1) %>%
  #rename_at(.vars = vars(-contains("wl")), ~1:10) %>%
  mutate(RGB = ifelse(wl < 520, "B", ifelse(wl > 630, "R", "G"))) %>%
  gather(sample_n, value, -wl, -RGB, -id) %>%
  group_by(RGB, id) %>%
  summarise(value = mean(value)*255) %>%
  spread(key = RGB, value = value) %>%
  mutate(id = as.character(id)) %>%
  print()

#' # lab to lch
# 
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

#' At this stage yo ucould also merge the data, by the file id, with other meta data e.g., location, depth etc.
#+ eval = FALSE
write.csv(x = colour.coef, file = "testing.csv")

#' # Session Info and Data

#' My Packages
#+ R.options = list(width = 100)
devtools::session_info()