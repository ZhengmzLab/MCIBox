
options(shiny.maxRequestSize=1920*1920^2)

#options(shiny.maxRequestSize=1000*1024^2)
#library(ggvenn)
current_file <- rstudioapi::getActiveDocumentContext()$path 
current_path=dirname(current_file)
source(paste0(current_path,"/PKG.r"))
setwd(current_path)
#print(current_file )
FLIST <- list.files(pattern = "\\.*$",path = "./DATA/genome/chrband/")
#DPATH <- list.dirs(dirname(rstudioapi::getActiveDocumentContext()$path),full.names = FALSE)
#FLIST <- list.files(pattern = "\\.txt$",path = dirname(rstudioapi::getActiveDocumentContext()$path))
#FLIST <- list.files(path = dirname(rstudioapi::getActiveDocumentContext()$path))

#print(FLIST)
#########################
LIBLIST=list.dirs(path = "./LIB/",full.names = FALSE)


#####
shinyUI(fluidPage(
  conditionalPanel(
    condition = "input.password != 'mci'",  
    titlePanel(
      fluidRow(
        column(1,),
        column(11,"Multiplex Chromatin Interaction VIEW"),
      ),
    ),
    textInput("password", "Password:", value = "mci"),

  ),  
  conditionalPanel(
    condition = "input.password == 'mci'",  
    
    useShinyjs(),
    
    titlePanel(
      fluidRow( 
        column(1,),
        column(11,"MCI-view"),
      ),
    ),
    ###################################
    # jqui_sortable(div(id="SORT0",
   # fixedRow(id="FR_TIAN"   ,style = "background-color:#FAFAFA;",
  #           column(1,"TIAN"),
  #           column(4,current_file),
#
#             column(3,format(Sys.time(), "%a %b %d %X %Y"))
#             
#    ),
   
    fixedRow(id="FR_FIN"   ,style = "background-color:#FAFAFA;",
             
             
             column(2, 
                    dropdown(inputId = "DPD_FIN",label="FIN" ,width = "300", offset=6,
                             # dropdown(inputId = "DPD_MAINFIN",  label = "MAINFIN",width = "300", 
                             selectInput("MAINFIN", label = NULL, ##HTML('<FONT color="yellow"><em><h4>input file</em></h4></FONT>'), 
                                         #    choices = list("Demo_chiadrop", "Demo_p2chiadrop", "GAM001", "SPRITE" ), 
                                         choices = LIBLIST,
                                         selected = "dm3.P2CDP.NATURE.CMB12"),
                             #F.Demo_CDP    F.SCRNA    F.scRNA_P_OSCAR  M.GAM001  F.Demo_P2CDP  F.scRNA_P  H.SPRITE001
                             
                             #),
                        
                            numericInput("SEED", label = "seed",min =-999999,   max =999999  ,value = 123),
                            radioButtons("HTYPE", label = "HTYPE", 
                                         choices = list("binary", "digital"),
                                         selected ="binary"),
                            
                             radioButtons("SCRNA_EXP_YES", label = "SCRNA_EXP_YES", 
                                          choices = list("Yes", "No"),
                                          selected ="Yes"),
                          
                             numericInput("SCRNA_EXP", "SCRNA_EXP_MIN",min = 0, max = 2000, value = 0.1, step = 0.001), 
                             numericInput("SCRNA_EXP_MAX", "SCRNA_EXP_MAX",min = 0, max = 2000, value = 2, step = 0.001),  
                             
                             
                             actionButton("GO_BIN", "GO_BIN",icon = shiny::icon("angellist")),
                             numericInput("BINNUM_M01", "bin_num",min = 1, max = 200, value = 40, step = 1),
                             numericInput("FNMIN_M01", "fn_min",min = 1, max = 10000, value = 2, step = 1),
                             numericInput("FNMAX_M01", "fn_max",min = 1, max = 10000, value = 1000, step = 1),
                    ),
                    
             ),
             column(2, 
                    checkboxInput("CHK_IPT", "display", TRUE),),
             column(2, checkboxInput("CHK_HD_SLCT", "HD-view", FALSE),),
             column(2, checkboxInput("CHK_R01", "LD-view",FALSE),),
             column(2, checkboxInput("CHKGENE", "gene", FALSE),),
    ),
    
   
    fixedRow(id="FR_GENE_SEARCH", style = "background-color:#FAFAFA;",
      column(1, actionButton("GO_INPUT_GENE_NAME", "GS")),
      column(3, textInput("INPUT_GENE_NAME",label =NULL, value = NULL)),
      column(3, selectInput("LIST_GENE", label = NULL,  choices = NULL, multiple = FALSE, selected = NULL,),),
      column(2, numericInput("MIN_GENE_RGN", label=NULL,min = 1, max = 100000000, value = 0, step = 1),),#100000
    ),
    
    

    fixedRow( id="FR_RGN",  style = "background-color:#FAFAFA;",
      
      
      column(1,
             actionButton("GO_INPUT_RGN_TEXT", "R") #shiny::icon("angellist"))
      ),      
      column(5,
             textInput("INPUT_RGN_TEXT",label =NULL, value = "chrX:20100000-20400000")),
      #  textInput("INPUT_RGN_TEXT",label =NULL, value = "chrX:5749229-5927949")),
      
      
      column(3,
             selectInput("INPUT_REFGNM", label = NULL, 
                         choices = FLIST, 
                         selected = "dm3")),
      
      column(3,
             selectInput("INPUT_CHROMOSOME", label =NULL, 
                         choices = list("chr2L","chr2R","chr3L","chr3R","chrX","chr4"), 
                         selected = "chrX")),
      
      
      
    ),
    fixedRow(id="FR_RGNSLD",  style = "background-color:#FAFAFA;",
               column(1, actionButton("GO_INPUT_RGN_SLIDE", "S"),),
               column(5,
                      setSliderColor("#FF4500",1),
                      chooseSliderSkin("Modern"), #
                      sliderInput("INPUT_RGN_SLIDE", label = NULL, min = 0,   max = 21000000, value = c(20100000,20400000), width='100%'),
               ) ,
           
               column(1,actionButton("ZMSM","><")),
               column(1,actionButton("ZMLG","<>")),
               column(1, actionButton("MVLT","<<")),
               column(1, actionButton("MVRT",">>")),
             column(2,   numericInput("ZMPCT", label = NULL, min = 0,   max = 1, value = 0.1,step=0.1),),
              
    ),
    #  ),),
    ## <<<<<<<<<<<<<<

  ## <<<<<<<<<<<<<<
  fixedRow(id="FR_CYTOBAND",
    column(1,
          
           
    ),
    column(11,
           plotOutput("CYTOBAND",height="60",  
                      brush = brushOpts( id = "CYTOBAND_BRUSH",
                                         direction = "x", clip = TRUE, fill = "blue",   
                                         stroke = "#036", opacity = 0.3, resetOnNew = TRUE)),
           
    ),
    
  ),
    
    ##>>>>>>>>>>
    jqui_sortable(div(id="SORT",#tyle = "overflow-y:scroll;max-height: 2500px; padding:0 0 0 0;background: white",  
 
                      fluidRow( id="FR_GENETRACK",
                        column(1,
                               
                               dropdown(inputId = "DPDGENE", label="GT",width=200,
                                        sliderInput("GENESIZE", label = "name size", min = 0,   max = 100, value = 20,step=1),
                                        
                                        sliderInput("HT_GENE", label = "height", min = 0,   max = 1000, value = 1000,step=5),
                                        numericInput("PWGENE", "plot width",min = 0, max = 1, value =0.98, step = 0.001),
                                        selectInput("CLPS", label = ("CLPS"), 
                                                    choices = list(TRUE, "longgest","shortest","meta", FALSE), 
                                                    selected = FALSE),
                                        numericInput("WD_GENE", label = "width", min = 50,   max = 150, value = 100.01,step=0.001),
                                        radioButtons("ISOSTYLE", label = "isotype", 
                                                     choices = list("longest", "all","merged"),
                                                     selected ="all"),
                                        selectInput("GENESTYLE", label = ("gene style"), 
                                                    choices = list("dense", "full","hide","squish", "pack"), 
                                                    selected = "full"),#STK="full"   #c(hide, dense, squish, pack,full),
                               )),
                        
                        
                        column(11, 
                               
                               #/* top | right | bottom | left */
                               #  margin: 2px 1em 0 auto;
                               wellPanel(id = "WP_GENETRACK",  style = "overflow-y:scroll;max-height: 150px; padding:0 0 0 0;background: white",  #style = "overflow-y:scroll; max-height: 150px;background: white;margin: auto 0 auto 0;",
                                         uiOutput("GENETRACK")
                               ),
                        ),
                      ),

                      fluidRow(id="FR_SELECT",
                               column(1,"HD"),    
                     
                        column(2,  
                               checkboxInput("CHK_M01", "Cluster view", FALSE),),
                        column(2,
                               checkboxInput("CHK_F01", "Fragment view", FALSE),),
                        column(2,
                               checkboxInput("CHK_B01", "Coverage", FALSE),),
                        column(2,       
                               checkboxInput("CHK_T01", "Heatmap",FALSE),),
                        column(2,
                               checkboxInput("CHK_LP01", "Loop", FALSE),),

                      ),### select
                      
                      fluidRow(id="FR_COVERAGE",### coverage
                        column(1, 
                               dropdown(inputId = "DPD_B01", label="C",width = "300", offset=6,
                                           sliderInput("HT_B01", label = "covHT", min = 0,   max = 1000, value = 1,step=1),
                                           numericInput("WD_B01", label = "covWD", min = 50,   max = 150, value = 100,step=0.5),
                                           
                                           radioButtons("YTK_B01", label = "show_yticks", 
                                                        choices = list("Yes","No"),
                                                        selected = "No"),
                                           
                                          ),
  
                               uiOutput("UI_T00"),
                        ),
                        column(11,   uiOutput("UI_B01")), 
                      ),### coverage
                      
                      fluidRow(id="FR_HD",## HD.cluster
                        column(1,
                               dropdown(inputId = "DPD_M01", width = "300", offset=6, icon="HC",
                                     
                                        sliderInput("HT_M01", label = "height", min = 0,   max = 1000, value = 1,step=5),
                                        numericInput("WD_M01", label = "width", min = 50,   max = 150, value = 100,step=0.5),
                                        numericInput("TH_M01", label = "treeHeight", min = 0,   max = 1000, value = 50,step=1),
                                        
                                        radioButtons("CLUORNOT", label = "cluster?", 
                                                     choices = list("CLU","NOT"),
                                                     selected = "CLU"),
                                        
                                        textInput("HD_CUTREE", label = "cluster num",value = 4),
                                        
                                        colourpicker::colourInput("CC1_M01", "backcolor", "#DCE9F5"),
                                        colourpicker::colourInput("CC2_M01", "color", "darkred"),
                                        
                                        selectInput("CLST", label = ("hclustering method"), 
                                                    choices = list("ward.D", "ward.D2", "single", "complete", "average" , "mcquitty" , "median" ,"centroid" ), 
                                                    selected = "ward.D2"),
                                        selectInput("DIST", label = ("distance function"), 
                                                    choices = list("euclidean", "maximum","manhattan", "canberra", "binary" , "minkowski"), 
                                                    selected = "euclidean"),
                               ),
                              # numericInput("HD_CUTREE", label = "HD_CUTREE", min = 1,   max = 50, value = 1,step=1),
                             

                        ),
                        column(11,
                               
                               uiOutput("UI_M01"),
                        ),
                      ),## HD.cluster
                      fluidRow(id="FR_FRAGMENT",
                               column(1, dropdown(inputId = "DPD_F01", width = "300", offset=6,icon="HF",
                                                  sliderInput("HT_F01", label = "HT_F01", min = 0,   max = 1000, value = 1,step=1),
                                                  numericInput("WD_F01", label = "WD_F01", min = 50,   max = 150, value = 100,step=0.5),
                                                  selectInput("SHAPE_F01", label = ("fragshape"), 
                                                              choices = list("butt", "round","square"), 
                                                              selected = "round"),
                                                 # sliderInput("FSIZE_F01", label = "fragsize", min = 0,   max =40, value = 0.5,step=0.1),
                                                #  sliderInput("LSIZE_F01", label = "linesize", min = 0,   max =40, value = 0.2,step=0.1),
                                                  numericInput("FSIZE_F01", label = "fragsize", min = 0,   max = 40, value = 0.5,step=0.01),
                                                  numericInput("LSIZE_F01",label = "linesize", min = 0,   max = 40, value = 0.2,step=0.01),
                                                  
                                                  
                                                  
                                                  
                                                  
                               )),
                               column(11,   uiOutput("UI_F01")), 
                      ),
                      fluidRow(id="FR_HEATMAP",
                        
                        column(1, dropdown(inputId = "DPD_T01", width = "300", label="HM",
                                           sliderInput("HT_T01", label = "heatmapHT", min = 0,   max = 1000, value = 1,step=1),
                                           numericInput("WD_T01", label = "heatmapWD", min = 50,   max = 150, value = 100,step=0.5),
                                           colourpicker::colourInput("heat_low", "heat_na", "white"),
                                           colourpicker::colourInput("heat_mid", "heat-", "white"),
                                           colourpicker::colourInput("heat_high", "heat+", "red"),
                                           checkboxInput("HEATMAP_REVERSE", "reverse", FALSE),
                                           checkboxInput("BOTTOM", "bottom", TRUE),
                                           
                                           sliderInput("HEATMAP_COLMAX", label = "HEATMAP_COLMAX", min =0,   max = 200, value = c(0,6),step=0.1),
                                           #  sliderInput("HEATMAP_COLMIN", label = "HEATMAP_COLMIN", min =-10,   max = 200, value = 0,step=1),
                                           
                                           sliderInput("heat_midpoint", label = "heat_midpoint", min =-10,   max = 10, value = 0,step=0.5),
                                           
                                           # numericInput("heat_midpoint", label = "heat_midpoint", min = 50,   max = 1500, value = 100,step=5),
                                           
                                           
                                           
                                           
                                           sliderInput("LA_T01", label = "l-adjust", min = -200,   max = 200, value = -30,step=5),
                                           
                                           sliderInput("YRATIO_T01", label = "yratio", min = 1,   max =40, value = 40,step=1),
                                           
                                           
                                           
                        )),
                        
                        
                        column(11,   uiOutput("UI_T01")), 
                        
                      ),
                      
                      
                      fluidRow(id="FR_LOOP",
                        
                        column(1, dropdown(inputId = "DPD_LP01", width = "300", offset=6,label="LP",
                                           sliderInput("HT_LP01", label = "loopHT", min = 0,   max = 1000, value = 1,step=1),
                                           numericInput("WD_LP01", label = "loopWD", min = 50,   max = 150, value = 100,step=0.5),
                                           # numericInput("PET_LP01", label = "loopPET", min = 1,   max = 100, value = 1,step=1),
                                           sliderInput("LOOP_PET", label = "LOOP_PET", min =0,   max = 2000, value = c(0,100)),
                                           sliderInput("LOOP_LEN", label = "LOOP_LEN", min =0,   max = 40, value = c(0,40)),
                                           selectInput("YTK_LP01","loop_yaxis",choices=c("Yes","No"),selected="No"),
                                           selectInput("TXT_LP01","loop_text",choices=c("Yes","No"),selected="No"),
                                           
                                           selectInput("LP_FLT_METHOD","loop_filter_method",choices=c("AND","OR"),selected="AND"),
                                           actionButton("AND_LOOP", NULL,icon = shiny::icon("angellist")),
                                           textInput("LOC_LOOP", label =  "loop bin select", value = ""),
                                           
                        )),
                        column(11,   uiOutput("UI_LP01")), 
                      ),


                      
                      fluidRow(  id="FR_2LOW",
                        
                        column(1,id="FR_2LOW_1",
                               dropdown(inputId = "DPD_R01", width = "300", offset=6,icon="LD",
                                        # actionButton("GO_M04","go cluster",icon("arrow-up")),
                                        sliderInput("HT_R01", label = "heightR", min = 0,   max = 1000, value = 250,step=1),
                                        numericInput("WD_R01", label = "widthR", min = 50,   max = 150, value = 100,step=0.5), 
                                        
                                        numericInput("CSL_C04", label = "select cluster", min = 0,   max = 150, value =0,step=1 ),
                                        sliderInput("SLD_D04_D", label = "domain", min = 0,   max = 1, value = c(0.5,0.6),step=0.001),
                                      #  sliderInput("INIT_C04", label = "init_height", min = 0,   max = 1000, value = 250,step=50),
                                        sliderInput("SLD_D04_G", label = "subDomain", min = 0,   max = 1, value = c(0.5,0.6),step=0.001),
                                        actionButton("FACET","facet",icon("arrow-right")),
                                        downloadButton("downloadData", "Download"),
                                        
                                        sliderInput("HT_C04", label = "height C", min = 0,   max = 1000, value = 250,step=1),
                                        numericInput("WD_C04", label = "width C", min = 50,   max = 150, value = 100,step=0.5),
                                        
                                    
                                        selectInput("SHAPE_C04", label = ("fragshape"), 
                                                    choices = list("butt", "round","square"), 
                                                    selected = "round"),
                                     #   sliderInput("FSIZE_C04", label = "fragsize", min = 0,   max =40, value = 0.5,step=0.1),
                                      #  sliderInput("LSIZE_C04", label = "linesize", min = 0,   max =40, value = 0.2,step=0.1),
                                      numericInput("FSIZE_C04", label = "fragsize", min = 0,   max = 40, value = 0.5,step=0.01),
                                      numericInput("LSIZE_C04", label = "linesize", min = 0,   max = 40, value = 0.2,step=0.01),
                                      
                                      
                                      checkboxInput("CHK_D04", "FRGM", TRUE),
                                      checkboxInput("CHK_M04", "SCTR", TRUE),
                                        ),##dropdown
                                     
                       
                               checkboxInput("CHK_CLU04", "Fragment", FALSE),
                               checkboxInput("CHK_C04LP", "Cluster",FALSE),
                               checkboxInput("CHK_R02", "Loci",FALSE),
                               checkboxInput("CHK_DM04", "2kde",FALSE),
                               
                               
                               uiOutput("UI_INFO_M04"),   
                               uiOutput("UI_M04"), 

  
                                ),##
                        
                        column(3,id="FR_2LOW_2",
                               
                             fluidRow(
                                         column(12,
                                             selectInput("ddmethod", label = ("ddmethod"), choices = list("TSNE", "UMAP","PCA","MDS","ICA","RUTA","PHATE"),   selected = "UMAP"), ###,"RUTA","PHATE"
                                        ),
                                       
                                        
                                     ),
                             fluidRow(
                                          column(6,
                                                 dropdown(inputId = "PARA_UMAP", icon="PARA_UMAP",
                                                           numericInput("NNEIG", label = "UMAP_n_neigbours", min = 0,   max = 500, value = 15,step=1),
                                                           sliderInput("UNAPSOMR", label = "UMAP set_op_mix_ratio",min =0,   max =1  ,value = 1,step=0.01),
                                                           selectInput("UMAPM", label = ("UMAP metric"), 
                                                                       choices = list( "euclidean", "cosine", "manhattan", "hamming", "correlation", "categorical"), 
                                                                       selected = "euclidean"),
                                                           selectInput("UMAPNN", label = ("UMAP nn_method"), 
                                                                      choices = list("fnn","annoy"), 
                                                                      selected = "annoy"),
                                                           selectInput("UMAPI", label = ("UMAP init"), 
                                                                       choices = list("spectral","random","laplacian","pca","spca","agspectral" ), 
                                                                       selected = "spectral"),
                                                           sliderInput("UMAPNC", label = "UMAP n_components",min =2,   max =100  ,value = 2,step=1),
                                                         ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_TSNE", icon="PARA_TSNE",
                                                          sliderInput("PERP", label = "TSNE perplexity",min =0,   max =300  ,value = 30,step=5), 
                                                          sliderInput("TSNE_TH", label = "TSNE theta",min =0.0,   max =1  ,value = 0.5,step=0.01),
                                                          sliderInput("TSNE_EF", label = "TSNE exaggeration_factor",min =0,   max =300  ,value = 12,step=1),
                                                          sliderInput("TSNE_ET", label = "TSNE eta",min =0,   max =500  ,value = 200,step=10),
                                                          sliderInput("TSNE_MO", label = "TSNE momentum",min =0,   max =1  ,value = 0.5,step=0.01),
                                                          sliderInput("TSNE_FM", label = "TSNE final_momentum",min =0,   max =1  ,value = 0.8,step=0.01),
                                                          sliderInput("TSNE_MI", label = "TSNE max_iter",min =0,   max =10000  ,value = 1000,step=20),
                                                          radioButtons("TSNE_PC", label = "TSNE PCA",  choices = list(TRUE,FALSE), selected = TRUE),
                                                          radioButtons("TSNE_PP", label = "TSNE partial_pca",  choices = list(TRUE,FALSE), selected = FALSE),
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_PCA", icon="PARA_PCA",
                                                          sliderInput("PC", label = "PCA PC_selection",min =1,   max =10  ,value = c(1,2)), 
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_MDS", icon="PARA_MDS",
                                                          selectInput("MDSM", label = ("MDS dist"), 
                                                                      choices = list("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" , "kendall"), 
                                                                      selected = "euclidean"),
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_ICA", icon="PARA_ICA",
                                                          radioButtons("ICAC", label = "ICA center",  choices = list(TRUE,FALSE), selected = TRUE),
                                                          sliderInput("ICAM", label = "ICA maxit",min =0,   max =1000  ,value = 100,step=10), 
                                                 ),# dropdown(
                                                 ####################################################################
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_RUTA", icon="PARA_RUTA",
                                                          selectInput("RUTAT", label = "RUTA type",  choices = list("basic", "sparse", "contractive", "denoising", "robust" , "variational"), 
                                                                      selected = "basic"),
                                                          selectInput("RUTAA", label = "RUTA activation",  choices = list("linear","tanh", "sigmoid", "relu", "elu" , "selu"), 
                                                                      selected = "linear"),
                                                          sliderInput("RUTAE", label = "RUTA epochs",min =1,   max =200  ,value = 20,step=1), 
                                                          
                                                 ),# dropdown(
                                                 ####################################################################
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_PHATE", icon="PARA_PHATE",
                                                          
                                                          sliderInput("PHATEG", label = "PHATE gamma",min =-1,   max =1  ,value = 1,step=0.1), 
                                                          sliderInput("PHATET", label = "PHATE t",min =0,   max =10  ,value = 0,step=0.1), 
                                                          
                                                 ),# dropdown(
                                                 ####################################################################
                                                 
                                          ),#column(,
                                          column(12,
                                                 selectInput("DDC_SELECT", label = "clumethod", 
                                                             choices = list("gaumix","hdensity","density","hkmeans", "kmeans","hpca","fuzzy"),
                                                             selected = "hkmeans"),
                                          ),
                                          
                                          column(12,id="PARA_CLUSTER",
                                              
                                                                                                  
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_DENSITY", icon="PARA_DBSCAN",
                                                          numericInput("MINPTS_M01", label = "minPts", min = 1,   max = 50, value = 5,step=1),#hdensity,density
                                                          numericInput("EPS_M01", label = "eps", min = 0,   max = 10, value =1.5,step=0.1),#density
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_HDENSITY", icon="PARA_HDBSCAN",
                                                          numericInput("MINPTS_M01H", label = "minPts", min = 10,   max = 500, value = 50,step=10),#hdensity,density
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_GAUMIX", icon="PARA_GAUMIX",
                                                          numericInput("glow", label = "gaumix_low", min = 0,   max = 50, value = 1,step=1), #kmeans,hkmeans
                                                          numericInput("ghigh", label = "gaumix_high", min = 0,   max = 100, value = 20,step=1), #kmeans,hkmeans
                                                          
                                                          selectInput("GAUMIX_MN", label = ("modelNames"), 
                                                                      choices = list("EII", "VII", "EEI", "EVI", "VEI", "VVI"), 
                                                                      selected = NULL),
                                                          
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_KMEANS", icon="PARA_KMEANS",
                                                          
                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_HKMEANS", icon="PARA_HKMEANS",

                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_HPCA", icon="PARA_HPCA",

                                                 ),# dropdown(
                                                 ####################################################################
                                                 dropdown(inputId = "PARA_FUNNY", icon="PARA_FUZZY",
                                                          selectInput("FUNNY_METRIX", label = ("metrix"), 
                                                                      choices = list("euclidean", "manhattan",  "SqEuclidean"), 
                                                                      selected = "euclidean"),
                                                 ),# dropdown(
                                                 ####################################################################
                                                 ),### column(6,id="PARA_CLUSTER",
                                          column(12,id="FR_CLU_NUM",
                                          numericInput("CLUNUM_M04", label = "set cluster num", min = 0,   max = 30, value = 4,step=1), #kmeans,hkmeans
                                          ),
                                          column(12,id="FR_CLU_AUTO","auto cluster number",
                                          ),
                                          ),
                      
                                          
                              ),# column(6,id="FR_2LOW_2",
                        
                        column(4, id="FR_2LOW_3",
                               uiOutput("UI_R01"),    
                               ),
                        column(4, id="FR_2LOW_4",
                               uiOutput("UI_C04"),    
                        ),
                        
                        
                     
                      ),# fluidRow(  id="FR_2LOW",
                      
                      
                      
  
                      
                     
                      
                      

                      
                      ##~~~~~~~~~
                      fluidRow(id="LD_CLUS",
                        column(1,
                               
                               
                               dropdown(inputId = "DPD_M04", width = "300",label="LC",
              
                                        
                                        actionButton("GAP_M04","gap",icon("arrow-right")),
                                        sliderInput("HT_M04", label = "height", min = 0,   max = 1000, value = 1,step=1),
                                        numericInput("WD_M04", label = "width", min = 50,   max = 150, value = 100,step=0.5),
                                        selectInput("M04_HIT_COL","hit_color",choices=c("color","mono"),selected="color"),
                                        colourpicker::colourInput("MONO_COLOR_NAME", "MONO_COLOR_NAME", "gray"),
                                        colourpicker::colourInput("BC_M04", "background", "#DCF1F5"),
                               ),
                               
                        ),
                        column(11,  
                               uiOutput("UI_R04"),
                        ),
                      ),
                      

                      
                      fluidRow(  id="FL_C04LP",
                                 column(1,
                                        dropdown(inputId = "DPD_C04LP", width = "300",label="LF",
                                              
                                          actionButton("BUTTON_CLU", "drop cluster",icon = shiny::icon("angellist")),
                                                 
                                               
                                           textInput("ID_CLU",label =NULL, value = ""),
                                          sliderInput("HT_C04LP", label = "height", min = 0,   max = 1000, value = 300,step=1),
                                          numericInput("WD_C04LP", label = "width", min = 50,   max = 150, value = 100,step=0.5),
                                            selectInput("SHAPE_C04LP", label = ("fragshape"), 
                                                        choices = list("butt", "round","square"), 
                                                        selected = "round"),
                                           # sliderInput("FSIZE_C04LP", label = "fragsize", min = 0,   max =40, value = 0.5,step=0.1),
                                          #  sliderInput("LSIZE_C04LP", label = "linesize", min = 0,   max =40, value = 0.2,step=0.1),
                                          numericInput("FSIZE_C04LP", label = "fragsize", min = 0,   max = 40, value = 0.5,step=0.01),
                                          numericInput("LSIZE_C04LP",label = "linesize", min = 0,   max = 40, value = 0.2,step=0.01),
                                        ),
                                 ),
                                 column(11,
                                        uiOutput("UI_C04LP"),   
                                 ),
                                 
                      ), ##fluidRow(  id="FL_C04LP",
                    #  fixedRow(id="FR_TV"   ,
                           #    column(1,),#style = "background-color:#FAFAFA;",
                           #    column(11,
                         #             radioButtons("TV", label = "Please select type of view", 
                           #                        choices = list(
                         #                                         "Target loci view"=1,
                       #                                           "Chromatin organization pattern view (CTCF)"=2,
                       #                                           "Transcription pattern view (RNAPII)"=3,
                       #                                           "Transcription regulation pattern view (Super-enhancers)"=4),
                      #                             selected =1,inline=TRUE),
                     #          ),
                    #  ),
                      
                      fluidRow(id="FR_VENN",
                        column(1, 
                               actionButton("AND01", "LF"),
                              
                               uiOutput("UI_VENN"), 
                               uiOutput("UI_VENN3"), 
                        ),
                        
                        column(5,
                               textInput("LOC01", label =  "LOC01:bin selection", value = "15,33,26,26,18,18,18,18,"),
                        ),
                        column(1, 
                               actionButton("AND02", NULL,icon = shiny::icon("angellist")),
                        ),
                        
                        column(5,
                               textInput("LOC02", label =  "LOC02:rgn selection", value = ""),
                        ),
                        
                      ),#fluidRow(id="FR_VENN",
                      
                      
                      
                      fluidRow(id="FR_ALL",
                        column(1,
                               actionButton("DATA_SLCT_ALL", "all",icon = NULL),
                               
                        ),  
                        
                        
                        column(5,
                               checkboxGroupInput("DATA_SLCT", label = "DATA_SLCT", inline=TRUE,
                                                  choices = NULL,
                                                  selected = NULL), 
                        ),
                        column(2,
                               # actionButton("INEXCLUDE","include/exclude"),
                               radioButtons("INEXCLUDE", label = NULL, 
                                            choices =c("include","exclude"), 
                                            selected = "exclude"), 
                        ),
                        column(2,
                               radioButtons("LOC_HDLD", label = "", 
                                            choices = list("HD", "LD"),
                                            selected ="LD"),
                        ),
                        
                        column(2,
                               selectInput("VENN_SLCT", label = "plot selection", 
                                           choices =NULL, 
                                           selected = NULL), 
                        ),
                     #   column(3,
                       #     
                        #       uiOutput("UI_INFO_VENN"),
                        #),
                        #column(3,
                        #       uiOutput("UI_INFO_VENN2"), 
                        #),
                        
                      ), ##VVVVVVVVVVVVVVVV
                      
                      fluidRow(id="FR_VENN2",column(1,
                                      dropdown(inputId = "DPD_VENN2", label="",
                                               sliderInput("HT_VENN2", label = "height", min = 0,   max = 3000, value = 1,step=5),
                                               numericInput("WD_VENN2", label = "width", min = 50,   max = 150, value = 100,step=0.5),
                                               sliderInput("VENN_ONE_PLOT_HEIGHT2", label = "one", min = 0,   max = 3000, value = 200,step=5),
                                               selectInput("SHAPE_VENN", label = ("fragshape"), 
                                                           choices = list("butt", "round","square"), 
                                                           selected = "round"),
                                               sliderInput("FSIZE_VENN", label = "fragsize", min = 1,   max =40, value = 2,step=0.1),
                                               sliderInput("LSIZE_VENN", label = "linesize", min = 0,   max =40, value = 1,step=0.1),
                                               
                                      ),   
                      ),
                      
                      column(11, style = "right: -15px; padding-left: 0px;  padding-right: 15px;left: 15px;",
                             
                             #/* top | right | bottom | left */
                             #  margin: 2px 1em 0 auto;
                             # wellPanel(id = "WP_VENN2",  style = "overflow-y:scroll;max-height: 610px; padding:0 0 0 0;background: #green",  #style = "overflow-y:scroll; max-height: 150px;background: white;margin: auto 0 auto 0;",
                             uiOutput("UI_VENN2"), 
                             #),
                      ),
                      ),#fluidRow
                      
                      fluidRow(id="FR_VENN4",column(1,
                                      dropdown(inputId = "DPD_VENN4", label="",
                                               
                                               sliderInput("HT_VENN4", label = "height", min = 0,   max = 3000, value = 1,step=5),
                                               numericInput("WD_VENN4", label = "width", min = 50,   max = 150, value = 100,step=0.5),
                                               
                                      ),   
                      ),
                      
                      column(11, style = "right: -15px; padding-left: 0px;  padding-right: 15px;left: 15px;",
                             
                             #/* top | right | bottom | left */
                             #  margin: 2px 1em 0 auto;
                             #  wellPanel(id = "WP_VENN3",  style = "overflow-y:scroll;max-height: 610px; padding:0 0 0 0;background: #green",  #style = "overflow-y:scroll; max-height: 150px;background: white;margin: auto 0 auto 0;",
                             uiOutput("UI_VENN4"), 
                             
                             #),
                      ),
                      ),#fluidRow
                      
                      fluidRow(id="FR_DOMAIN",
                        column(1,   
                       
                               uiOutput("UI_INFO04"),
                               dropdown(inputId = "DPD_D04B", width = "300",label="2K",
                                        sliderInput("HT_DM04", label = "height_domain", min = 0,   max = 3000, value = 1,step=25),
                                        numericInput("WD_DM04", label = "width_domain", min = 50,   max = 150, value = 100,step=0.5),
                                        dropdown(inputId = "DPD_DMN_ITEM", icon="domain item",width = "300",
                                              checkboxGroupInput("CKP_D04", label = h3("Checkbox group"), 
                                                                 choices = list( "contour map" = 1,"domain box"=2,"box label"=3,"select line"=4,"parameter"=5),
                                                                 selected = list(1,2,3,4,5)), 
                                        ),
                                       # input$Detail_Adjust
                                         checkboxInput("Detail_Adjust", "Detail_Adjustw", FALSE),
                                       selectInput("DMN_CUT", label = ("DMN_CUT"), choices = list("40%", "20%","90%" ,"60%","100%"),  selected = "40%"),
                                        sliderInput("DOMAIN", label = "domain", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                        dropdown(inputId = "DPD_DMN_PARA", icon="domain para",width = "300",
                                            sliderInput("DOMAIN01", label = "domain01", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN02", label = "domain02", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN03", label = "domain03", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN04", label = "domain04", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN05", label = "domain05", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN06", label = "domain06", min = 0,   max = 1,value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN07", label = "domain07", min = 0,   max = 1,value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN08", label = "domain08", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN09", label = "domain09", min = 0,   max = 1,value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN10", label = "domain10", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN11", label = "domain11", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN12", label = "domain12", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN13", label = "domain13", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN14", label = "domain14", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN15", label = "domain15", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN16", label = "domain16", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN17", label = "domain17", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN18", label = "domain18", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN19", label = "domain19", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                            sliderInput("DOMAIN20", label = "domain20", min = 0,   max = 1, value = c(0.45,0.60),step=0.05),
                                        ),
                                        selectInput("SHAPE_DMN", label = ("fragshape"), choices = list("butt", "round","square"),  selected = "round"),
                                        sliderInput("FSIZE_DMN", label = "fragsize", min = 0,   max =40, value = 0.5,step=0.1),
                                        sliderInput("LSIZE_DMN", label = "linesize", min = 0,   max =40, value = 0.2,step=0.1),
                                        
                                        downloadButton("download_domain", "DM_DOWN"),
                                        
                                        
                               ),
                        ),  
                        column(11,  
                               uiOutput("UI_DM04"),
                        ),
                      ),
                      ##~~~~~~~~~ 
                   
                     ##~~~~~~~~~ 
                  #   fixedRow(id="FR_SIL", style = "background-color:#FAFAFA;",
                  #            
                  #            column(12,
                 #                    # wellPanel(id = "WP_UI_DATA2",  style = "overflow-y:scroll;max-height: 200px; padding:0 0 0 0;background: white",
                   #                  uiOutput("UI_SIL")
                    #                 #  ),
                                     
                     #         ),
                              
                    # ),##FR_SIL
                      ####
                     fixedRow(id="FR_BOTTOM", style = "background-color:#FAFAFA;",
                              column(1, 
                                     dropdown(inputId = "DPD_LAST", label="IF",
                                              sliderInput("HT_LAST", label = "height", min = 0,   max = 2000, value = 200,step=1),
                                     ),
                              ),
                              column(6,
                                     # wellPanel(id = "WP_UI_DATA",  style = "overflow-y:scroll;max-height: 200px; padding:0 0 0 0;background: white",
                                     uiOutput("UI_DATA")
                                     #),
                              ),
                              column(5,
                                     # wellPanel(id = "WP_UI_DATA2",  style = "overflow-y:scroll;max-height: 200px; padding:0 0 0 0;background: white",
                                     uiOutput("UI_DATA4")
                                     #  ),
                                     
                              ),
                              
                     ),###FR_BOTTOM

############################################LOC
                    
                        
                      
                      
    )),##quryui

    
  )##conditionalPanel(   #2       
))##shinyUI(fluidPage(
