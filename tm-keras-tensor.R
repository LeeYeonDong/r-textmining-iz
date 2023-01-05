install.packages("keras")  # keras 패키지 설치
install.packages("tensorflow")  # tensorflow 패키지 설치
library(keras)  # keras 라이브러리 불러오기
use_condaenv("tf")
remotes::install_github("rstudio/reticulate")
library(reticulate)
reticulate::install_miniconda()
install_keras(method = c("conda"), conda = "auto", version = "default", tensorflow = "gpu")
library(tensorflow)  # tensorflow 라이브러리 불러오기
install_tensorflow()  # tensorlfow 설치

