      
      options(repos=structure(c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))
      # A demonstration of clicking, hovering, and brushing
      #options(repos=structure(c(CRAN="http://mirrors.opencas.cn/cran/")))
    ## global required 
    if (!suppressWarnings(require(devtools))) install.packages("devtools")
    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
    
    
    
    ## rstudio
    if (!require("rstudioapi")) install.packages("rstudioapi")
    
    
    ## shiny associate pkgs
    
    if (!require("shiny")) install.packages("shiny") 
    
    if (!require("shinyjqui")) install.packages("shinyjqui")
    if (!require("shinyWidgets")) install.packages("shinyWidgets")
    if (!require("shinyjs")) install.packages("shinyjs")
    if (!require("shinydashboard")) install.packages("shinydashboard")
    
    if (!require("highcharter")) remotes::install_github("jbkunst/highcharter")
   #   if (!require("highcharter")) install.packages("highcharter") 
    
    
    ## common package 
    if (!require("grid")) install.packages("grid")
    if (!require("gridExtra")) install.packages("gridExtra")
    if (!require("scales")) install.packages("scales") 
    
    ## color 
    
    if (!require("RColorBrewer")) install.packages("RColorBrewer")
    if (!require("colourpicker")) install.packages("colourpicker")
    if (!require("png")) install.packages("png")     
    if (!require("tiff")) install.packages("tiff")
    if (!require("OpenImageR")) install.packages("OpenImageR")
    
    
    ## data frame
    if (!require("tidyr")) install.packages("tidyr")
    if (!require("bedr")) install_github('davetang/bedr')  #install.packages("bedr")
    if (!require("data.table")) install.packages("data.table")
    if (!require(tidyverse)) install.packages("tidyverse")
    
    ## tools
    if (!require("reshape2")) install.packages("reshape2")
    if (!require("stringr")) install.packages("stringr")
    if (!require("sitools")) install.packages("sitools")
    if (!require("xtable")) install.packages("xtable")
    
    
    
    ### comprihansive 
    if (!require("ggforce")) install.packages("ggforce")
    if (!require("pheatmap")) install.packages("pheatmap")
    if (!require("ggplot2")) install.packages("ggplot2")
    if (!require("ggrepel")) install.packages("ggrepel")
    if (!require("cowplot")) install.packages("cowplot")
    if (!require("plotly")) install.packages("plotly")
    if (!require("gggenes")) install.packages("gggenes")
    if (!require("ggpubr")) install.packages("ggpubr")
      
      
      if (!require(viridis)) install.packages("viridis")
    #  if (!require(ggplot2)) install.packages("ggplot2")
      if (!require(readr)) install.packages("readr")
   #   system("pip install --user magic-impute")
      if (!require(Rmagic)) install.packages("Rmagic")
      if (!require(phateR)) install.packages("phateR")
    #  devtools::install_github("KrishnaswamyLab/phateR", ref="dev")
      
    #  library(phateR)
    ## cluster 
    if (!require("Rtsne")) install.packages("Rtsne")
    if (!require("ica"))  install.packages("ica")
    if (!require("ruta")) install.packages("ruta")
   # if (!require("phateR"))  devtools::install_github("KrishnaswamyLab/phateR", dependencies=TRUE)
    if (!require("factoextra")) install.packages("factoextra")
    if (!require("dbscan")) install.packages("dbscan")
    if (!require("FactoMineR")) install.packages("FactoMineR")
    if (!require("mclust")) install.packages("mclust")  
    if (!require("cluster")) install.packages("cluster")
    if (!require("Matrix"))  install.packages("Matrix", repos="http://R-Forge.R-project.org")
    if (!require("uwot")) install.packages("uwot")
    ### genome
    if (!require("Gviz"))   BiocManager::install("Gviz") ##XX
    if (!require("GenomeInfoDb"))  BiocManager::install("GenomeInfoDb") ##XX 
    if (!require("GenomicRanges"))  BiocManager::install("GenomicRanges") ##XX 
    ### rare packages
    if (!require("rgdal")) install.packages("rgdal")
    
    if (!require("sjmisc")) install.packages("sjmisc")
    if (!require("broom")) install.packages("broom")
    if (!require("DT")) install.packages('DT')
    if (!require("leaflet")) devtools::install_github("rstudio/leaflet") #install.packages("leaflet")
    if (!require("sp")) install.packages("sp")
    if (!require("lemon")) install.packages("lemon")
    if (!require("caroline")) install.packages("caroline")
    if (!require("rlist")) install.packages("rlist")
    library(qdapTools)
