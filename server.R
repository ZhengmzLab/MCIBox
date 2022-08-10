

current_file <- rstudioapi::getActiveDocumentContext()$path 
current_path=dirname(current_file)
source(paste0(current_path,"/PKG.r"))
setwd(current_path)
#print(current_file )
FLIST <- list.files(pattern = "\\.*$",path = "./DATA/genome/chrband/")
# Define server logic required to draw a histogram
shinyServer(function(input, output,session,clientDatamci) {

  observe({
    
  #  toggle(id = "VENN_SLCT", condition =  input$CHK_R02)
  #  toggle(id = "DATA_SLCT_ALL", condition =  input$CHK_R02)
  #  toggle(id = "DATA_SLCT", condition =  input$CHK_R02)
    
    
    toggle(id = "FR_VENN4", condition =  input$CHK_R02&input$AND01)
    toggle(id = "FR_VENN2", condition =  input$CHK_R02&input$AND01)
    toggle(id = "FR_VENN", condition =  input$CHK_R02)
    toggle(id = "FR_ALL", condition =  input$CHK_R02)
    toggle(id = "FR_TV", condition =  input$CHK_R02)
    toggle(id = "TRK_VENN", condition =  input$CHK_R02&input$AND01)
    toggle(id = "TRK_VENN2", condition =  input$CHK_R02&input$AND01)
    toggle(id = "TRK_VENN3", condition =  (input$CHK_R02)&input$AND01)
    toggle(id = "TRK_VENN4", condition =  input$CHK_R02&input$AND01)

    
  #  toggle(id = "FR_CYTOBAND", condition =  1==1)
    toggle(id = "FR_GENE_SEARCH", condition =  input$CHKGENE& input$CHK_IPT)

    
          
    
              toggle(id = "PARA_TSNE", condition =  input$ddmethod == "TSNE")
              toggle(id = "PARA_UMAP", condition =  input$ddmethod == "UMAP")
              toggle(id = "PARA_PCA", condition =  input$ddmethod == "PCA")
              toggle(id = "PARA_MDS", condition =  input$ddmethod == "MDS")
              toggle(id = "PARA_ICA", condition =  input$ddmethod == "ICA")
              toggle(id = "PARA_RUTA", condition =  input$ddmethod == "RUTA")
              toggle(id = "PARA_PHATE", condition =  input$ddmethod == "PHATE")
              
              toggle(id = "PARA_KMEANS", condition =  input$DDC_SELECT == "kmeans")
              toggle(id = "PARA_HKMEANS", condition =  input$DDC_SELECT == "hkmeans")
              toggle(id = "PARA_DENSITY", condition =  input$DDC_SELECT == "density")
              toggle(id = "PARA_HDENSITY", condition =  input$DDC_SELECT == "hdensity")
              toggle(id = "PARA_GAUMIX", condition =  input$DDC_SELECT == "gaumix")
              toggle(id = "PARA_HPCA", condition =  input$DDC_SELECT == "hpca")
              toggle(id = "PARA_FUNNY", condition =  input$DDC_SELECT == "fuzzy")
              
          
              toggle(id = "FR_CLU_AUTO", condition =  input$DDC_SELECT == "density" || input$DDC_SELECT == "hdensity" || input$DDC_SELECT == "gaumix")
          
              toggle(id = "FR_CLU_NUM", condition =  input$DDC_SELECT == "hpca"|| input$DDC_SELECT == "fuzzy"|| input$DDC_SELECT == "kmeans"||  input$DDC_SELECT == "hkmeans")
             
              
              toggle(id = "LD_CLUS", condition =  input$CHK_CLU04)
               toggle(id = "FL_C04LP", condition =  input$CHK_C04LP)
               
               
               toggle(id = "CHK_SIL", condition =  input$CHK_R01)
               toggle(id = "TRK_SIL", condition =  input$CHK_R01 & input$CHK_SIL  )
               toggle(id = "FR_SIL", condition =  input$CHK_R01 & input$CHK_SIL  )
               
               
              toggle(id = "FR_2LOW", condition =  input$CHK_R01)
              toggle(id = "FR_GENETRACK", condition =  input$CHKGENE)
              
              toggle(id = "FR_SELECT", condition =input$CHK_IPT&input$CHK_HD_SLCT)
              toggle(id = "FR_BOTTOM", condition =input$CHK_IPT)
              
              toggle(id = "FR_COVERAGE", condition =input$CHK_IPT)
              toggle(id = "TRACKGENE", condition =input$CHKGENE)
              toggle(id = "DPDGENE", condition =input$CHKGENE)
              toggle(id = "FR_HD", condition =input$CHK_M01& input$CHK_IPT)
              toggle(id = "TRK_M01", condition =input$CHK_M01& input$CHK_IPT)
              toggle(id = "FR_FRAGMENT", condition =input$CHK_F01& input$CHK_IPT)
              toggle(id = "TRK_F01", condition =input$CHK_F01& input$CHK_IPT)
              # toggle(id = "TRK_R01", condition =input$CHK_R01& input$CHK_IPT)
              # toggle(id = "DPD_R01", condition =input$CHK_R01& input$CHK_IPT)
              toggle(id = "TRK_T00", condition =input$CHK_T01& input$CHK_IPT)
              toggle(id = "TRK_T01", condition =input$CHK_T01& input$CHK_IPT)
              toggle(id = "FR_HEATMAP", condition =input$CHK_T01& input$CHK_IPT)
              
              toggle(id = "TRK_B01", condition =input$CHK_B01& input$CHK_IPT)
              toggle(id = "FR_COVERAGE", condition =input$CHK_B01& input$CHK_IPT)
              
              toggle(id = "TRK_LP01", condition =input$CHK_LP01& input$CHK_IPT)
              toggle(id = "FR_LOOP", condition =input$CHK_LP01& input$CHK_IPT)
              
              #  toggle(id = "CHK_M04", condition =input$CHK_R01& input$CHK_IPT)
              # toggle(id = "DPD_M04", condition =input$CHK_R01& input$CHK_IPT)
              #toggle(id = "DPD_D04", condition =input$CHK_R01& input$CHK_IPT)
              
              
              toggle(id = "TRK_M04", condition =input$CHK_R01& input$CHK_M04& input$CHK_IPT)
              toggle(id = "DPD_M04", condition =input$CHK_R01& input$CHK_M04& input$CHK_IPT)
              toggle(id = "TRK_INFO04", condition =input$CHK_R01& input$CHK_M04& input$CHK_IPT)
              
              
              
              
            #  toggle(id = "CHK_C04", condition =  input$CHK_R01& input$CHK_D04& input$CHK_IPT)
              toggle(id = "SLD_D04_D", condition =input$CHK_R01& input$CHK_D04& input$CHK_IPT)
              toggle(id = "SLD_D04_G", condition =input$CHK_R01& input$CHK_D04& input$CHK_IPT)
              toggle(id = "DPD_D04",   condition =input$CHK_R01& input$CHK_D04& input$CHK_IPT)
              toggle(id = "TRK_C04",  condition = input$CHK_R01& input$CHK_D04& input$CHK_IPT)
              
              toggle(id = "FR_DOMAIN", condition =input$CHK_R01& input$CHK_D04& input$CHK_IPT)
          
              
              toggle(id = "TRK_DM04",  condition = input$CHK_R01& input$CHK_DM04& input$CHK_IPT)
              
              toggle(id = "DPD_D04B", condition =input$CHK_R01& input$CHK_DM04& input$CHK_IPT)
              
              #  toggle(id = "EPS_M01", condition =  (input$DDC_SELECT=="density"))
              # toggle(id = "MINPTS_M01", condition =  (input$DDC_SELECT=="density"|input$DDC_SELECT=="hdensity"))
              #  toggle(id = "CLUNUM_M04", condition =  (input$DDC_SELECT=="kmeans"|input$DDC_SELECT=="hkmeans"|input$DDC_SELECT=="hpca"|input$DDC_SELECT=="fuzzy"))
              
              
    
     })
  
  ###################################
  
  
  D=current_path
  
  # getwd()
  #setwd(D)
  
  
  ###################################
  
  ###################################
  #RESIZE <- reactiveValues(xmin = 0, xmax = SUBGNMSIZE$size)
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #  RESIZE <- reactiveValues(xmin = 5749229, xmax = 5927949, gnm="dm3",chr="chrX",left=1,right=5927949,gene="",strand="+",prom=0,probe=0)
  RESIZE <- reactiveValues(xmin = 20100000, xmax = 20400000, gnm="dm3",chr="chrX",left=0,right=22422827,gene="",strand="+",prom=0,probe=0)
  
  
  BTN_BIN<-reactiveValues(i=0)
  

  
  FLT_EXPR_LOOP=reactiveValues(i=NULL)
  observeEvent({
    input$AND_LOOP
    input$CHK_LP01
  },{
    BINS=input$LOC_LOOP
    
    if (str_detect(BINS, ",")){
      BIN_LIST= gsub(",", ",",trimws(BINS,which = "both", whitespace = ","))
    }else{
      BIN_LIST=BINS
    }
    FLT_EXPR_LOOP$i=BIN_LIST
    
  })
  
  
  FLT_EXPR=reactiveValues(i=NULL)
  GLOB_A=reactiveValues(i=NULL)
  observeEvent(input$AND01,{
    BINS=input$LOC01

    if (str_detect(BINS, ",")){
      BIN_LIST= gsub(",", ",",trimws(BINS,which = "both", whitespace = ","))
    }else{
      BIN_LIST=BINS
    }
    #print(BIN_LIST);#print("BIN_LIST-257")
    
    FLT_EXPR$i=BIN_LIST
    BINLIST=FLT_EXPR$i;#print(FLT_EXPR$i);#print("FLT_EXPR$i")
    DF_BINLIST=data.frame(unique(unlist(strsplit(BINLIST, ","))))
    colnames(DF_BINLIST)<-c("BIN_SELECTED")
    ODR_DF_BINLIST<-data.frame(DF_BINLIST[order(as.numeric(DF_BINLIST$BIN_SELECTED)),])
    colnames(ODR_DF_BINLIST)<-c("BIN_SELECTED")
    UNISTR_BINLIST<-paste(ODR_DF_BINLIST$BIN_SELECTED,collapse = ",")
    #print(ODR_DF_BINLIST); #print("ODR_DF_BINLIST:257")
    GLOB_A$i=ODR_DF_BINLIST$BIN_SELECTED
    updateTextInput(session,"LOC01",value=UNISTR_BINLIST)
    #print(GLOB_A$i); #print("GLOB_A$i:257")
    BINSIZE=round((RESIZE$xmax-RESIZE$xmin)/as.numeric(GLOB_BINNUM$i));#print(BINSIZE)
    UNIQ_BINS<-unlist(strsplit(UNISTR_BINLIST, "[,]"))
    #print(UNIQ_BINS)
    NEW_RGN_LIST=""
    for(i in 1:length(UNIQ_BINS)){
      #print(UNIQ_BINS[i]);#print(as.integer(UNIQ_BINS[i]));#print("UNIQ_BINS[i]")
      NUM_UNIQ_BIN=as.integer(UNIQ_BINS[i])
      NEWS=RESIZE$xmin+NUM_UNIQ_BIN*BINSIZE;#print(NEWS)
      NEWE=RESIZE$xmin+(NUM_UNIQ_BIN+1)*BINSIZE-1;#print(NEWE)
      #print("BIN-SE")
      NEWC=RESIZE$chr
      NEW_RGN=paste0(NEWC,":",NEWS,"-",NEWE)
      if (NEW_RGN_LIST==""){
        NEW_RGN_LIST=NEW_RGN
      }else{
        NEW_RGN_LIST=paste(NEW_RGN_LIST,NEW_RGN,sep=",")
      }
    }
    #print(NEW_RGN_LIST)
    if(BINS==""){
      updateTextInput(session,"LOC02",value="")
    }else{
      updateTextInput(session,"LOC02",value=NEW_RGN_LIST)
    }
    

    
    
  })##  observeEvent(input$AND01,{
  
  observeEvent(input$AND02,{
 
  RGNS02=input$LOC02
  #print(input$LOC02)
  if(RGNS02!=""){
    if (str_detect(RGNS02, ",")){
      RGNS02_LIST= gsub(",", ",",trimws(RGNS02,which = "both", whitespace = ","))
    }else{
      RGNS02_LIST=RGNS02
    }
    #print(RGNS02_LIST);#print("RGNS02_LIST-289")
    DF_RGN02LIST=data.frame(unique(unlist(strsplit(RGNS02_LIST, ","))))
    colnames(DF_RGN02LIST)<-c("RGN_SELECTED")
    ODR_DF_RGN02LIST<-data.frame(DF_RGN02LIST[order((DF_RGN02LIST$RGN_SELECTED)),])
    colnames(ODR_DF_RGN02LIST)<-c("RGN_SELECTED")
    UNISTR_RGN02LIST<-paste(ODR_DF_RGN02LIST$RGN_SELECTED,collapse = ",")
    #print(UNISTR_RGN02LIST); #print("UNISTR_RGN02LIST:295")
    UNIQ_RGNS02_LIST=unlist(strsplit(UNISTR_RGN02LIST, "[,]"))
    #print(UNIQ_RGNS02_LIST); #print("UNIQ_RGNS02")
    RGN2BIN=""
    for(i in 1:length(UNIQ_RGNS02_LIST)){
      UNIQ_RGNS02=UNIQ_RGNS02_LIST[[i]];#print(UNIQ_RGNS02)
      LINE02=unlist(strsplit(UNIQ_RGNS02, "[:-]"));#print(LINE02)
      C0200=unlist(strsplit(LINE02[[1]], "hr"))[[2]];#print(C0200)
      C02=LINE02[[1]];#print(C02)
      S02=round(as.numeric(LINE02[[2]]));#print(S02)
      E02=round(as.numeric(LINE02[[3]]));#print(E02)
      #print(RESIZE$xmin);#print(RESIZE$xmax);#print(RESIZE$chr)
      if(C02!=RESIZE$chr || S02<RESIZE$xmin || E02 > RESIZE$xmax){
        #print("Selected Loci not in current genomic region") ##  chrX:30100000-30400000
      }else{
        #print(GLOB_BINNUM$i)
        #print(S02);#print(E02) # chrX:20111000-20111100
        BINSIZE=round((RESIZE$xmax-RESIZE$xmin)/as.numeric(GLOB_BINNUM$i));#print(BINSIZE)
        RGN_START_BIN<-round((S02-RESIZE$xmin)/BINSIZE);#print(RGN_START_BIN)
        RGN_END_BIN<-round((E02-RESIZE$xmin)/BINSIZE);#print(RGN_END_BIN)
        for (m in RGN_START_BIN:RGN_END_BIN){
          if (RGN2BIN==""){
            RGN2BIN=m
          }else{
            RGN2BIN=paste(RGN2BIN,m,sep=",");#print(RGN2BIN)
          }
        }
      }
      #print(RGN2BIN)
      updateTextInput(session,"LOC01",value=RGN2BIN)
      #print("YES")
  
    } ## for(i in 1:length(UNIQ_RGNS02)){
  }else{
    updateTextInput(session,"LOC01",value="")
  } 
    
    
  })##observeEvent(input$AND02,{
  ##  chrX:30100000-30400000
 # chrX:20111000-20111100
  ##   chrX:20100000-20400000,chrX:20100000-20400000,chrX:30100000-30400000
  
  RDM0=reactiveValues(i=1,j=0)
  observeEvent({
    input$DIST
    input$CLST
    input$CHK_IPT
    input$M04_HIT_COL
    input$HTYPE
  }, {
    RDM0$j=RDM0$j+1
    RDM0$i=ifelse (RDM0$j%%2==0,1,0)
  })
  
  RDM1=reactiveValues(i=1,j=0)
  observeEvent({
    input$AND01
  #  input$VENN_SLCT
    input$LOC01
  }, {
    RDM1$j=RDM1$j+1
    RDM1$i=ifelse (RDM1$j%%2==0,1,0)
  }) 
  
  
  RDM=reactiveValues(i=1,j=0)
  observeEvent({
    input$CHK_IPT
    input$SEED
    input$CLUNUM_M04
    input$HEATMAP_REVERSE
    input$DDC_SELECT
    input$M04_HIT_COL
    input$HTYPE
    input$GAP_M04
    input$DPD_DDC
  }, {
    RDM$j=RDM$j+1
    RDM$i=ifelse (RDM$j%%2==0,1,0)
  })     
  
  
  observeEvent(input$CHK02,{
    reset("FIN02")
  })
  observeEvent(input$CHK02b,{
    reset("FIN02b")
  })
  observeEvent(input$CHK01b,{
    reset("FIN01b")
  })
  observeEvent(input$CHK01,{
    reset("FIN01")
  })
  
 # ZMPCT
  
  observeEvent(input$ZMSM, { 
    ADJ<-round((RESIZE$xmax-RESIZE$xmin)*input$ZMPCT)
    MIDV=round(RESIZE$xmin+((RESIZE$xmax-RESIZE$xmin)*0.5))
    
    W <- ifelse(RESIZE$xmin+ADJ>=MIDV,MIDV,RESIZE$xmin+ADJ)
    M <- ifelse(RESIZE$xmax-ADJ<=MIDV,MIDV+1,RESIZE$xmax-ADJ)
    REGION=paste0(RESIZE$chr,":",W,"-",M)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
  })
  observeEvent(input$ZMLG, { 
    ADJ<-round((RESIZE$xmax-RESIZE$xmin)*input$ZMPCT)
    W<- ifelse(RESIZE$xmin-ADJ<=0,1,RESIZE$xmin-ADJ)
    M<- ifelse(RESIZE$xmax+ADJ>RESIZE$right,RESIZE$right,RESIZE$xmax+ADJ)
    REGION=paste0(RESIZE$chr,":",W,"-",M)
    
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
  })
  observeEvent(input$MVLT, { 
    ADJ<-round((RESIZE$xmax-RESIZE$xmin)*input$ZMPCT)
    
    W <- ifelse(RESIZE$xmin-ADJ >= RESIZE$left , RESIZE$xmin-ADJ  , RESIZE$left )
    M <- ifelse(RESIZE$xmax-ADJ >= RESIZE$left , RESIZE$xmax-ADJ  , RESIZE$left )
    REGION=paste0(RESIZE$chr,":",W,"-",M)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
  })
  observeEvent(input$MVRT, { 
    ADJ<-round((RESIZE$xmax-RESIZE$xmin)*input$ZMPCT)
    
    W <- ifelse(RESIZE$xmin+ADJ <= RESIZE$right , RESIZE$xmin+ADJ  , RESIZE$right )
    M<- ifelse(RESIZE$xmax+ADJ <= RESIZE$right , RESIZE$xmax+ADJ  , RESIZE$right )
    REGION=paste0(RESIZE$chr,":",W,"-",M)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
    #print(REGION)
    
  })
  

  ####

  ###################################
  observeEvent(input$INPUT_REFGNM, {
    
    
    cat("INPUT_REFGNM")
    gnm <- input$INPUT_REFGNM
    GNMSIZE=read.table(paste0(current_path,"/DATA/genome/size/",gnm,".sze"))
    colnames(GNMSIZE)<-c("chr","size")
    SUBGNMSIZE=GNMSIZE%>%dplyr::filter(chr==RESIZE$chr)
    RESIZE$right=SUBGNMSIZE$size
    RESIZE$gnm <- gnm
    updateSelectInput(session,"INPUT_CHROMOSOME",choices=GNMSIZE$chr,selected="chrX")
    #updateTextInput(session,"INPUT_RGN_TEXT",value="chrX:5749229-5927949")
    updateTextInput(session,"INPUT_RGN_TEXT",value="chrX:100000-400000")
    
    updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(100000,400000))
    
  })
  
  observeEvent(input$INPUT_CHROMOSOME, {
    RESIZE$chr=input$INPUT_CHROMOSOME
    GNMSIZE=read.table(paste0(current_path,"/DATA/genome/size/",RESIZE$gnm,".sze"))
    colnames(GNMSIZE)<-c("chr","size")
    SUBGNMSIZE=GNMSIZE%>%dplyr::filter(chr==RESIZE$chr)
    RESIZE$right=SUBGNMSIZE$size
    RESIZE$xmin<-ifelse(RESIZE$xmin<=0,1,RESIZE$xmin)
    RESIZE$xmax<-ifelse(RESIZE$xmax > RESIZE$right, RESIZE$right, RESIZE$xmax)
    updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(RESIZE$xmin,RESIZE$xmax))
    REGION=paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
  })
  
  ###################################
 # if(2==1){
          observeEvent(input$CYTOBAND_BRUSH, {
            cat("CYTOBAND_BRUSH")
            
            brush <- input$CYTOBAND_BRUSH
         #   RESIZE$xmin <- round(brush$xmin)
         #   RESIZE$xmax <- round(brush$xmax)
            #RESIZE$xmin<-ifelse(RESIZE$xmin<=0, 1,RESIZE$xmin)
            #RESIZE$xmax<-ifelse(RESIZE$xmax > RESIZE$right, RESIZE$right, RESIZE$xmax)
            REGION=paste0(RESIZE$chr,":",round(brush$xmin),"-", round(brush$xmax))
            updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
         #   updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(RESIZE$xmin,RESIZE$xmax))
            
          })
 # }#
  ####################
  observeEvent( input$GO_INPUT_RGN_TEXT, {
    cat("GO_INPUT_RGN_TEXT")
    
    RGN=input$INPUT_RGN_TEXT
    RC0=unlist(strsplit(RGN, "[:-]"))[[1]]
    
    RRC0=unlist(strsplit(RC0, "hr"))[[2]]
    
    RC=paste0("chr",RRC0)
    
    RS=as.integer(gsub(",", "",unlist(strsplit(RGN, "[:-]"))[[2]]))
    RE=as.integer(gsub(",", "",unlist(strsplit(RGN, "[:-]"))[[3]]))
    RLEN=RE-RS
    #print(RS);#print(RE);#print(RLEN)
    if(input$CHKGENE==TRUE){
      if (RLEN < input$MIN_GENE_RGN){
        RS=RS-input$MIN_GENE_RGN
        RE=RE+input$MIN_GENE_RGN
      }
    }
    #print(RS);#print(RE)
    RESIZE$xmin <- RS
    RESIZE$xmax <- RE
    RESIZE$chr <- RC

    RESIZE$xmin<-ifelse(RESIZE$xmin<=0,1,RESIZE$xmin)
    RESIZE$xmax<-ifelse(RESIZE$xmax > RESIZE$right, RESIZE$right, RESIZE$xmax)
    updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(RESIZE$xmin,RESIZE$xmax))
    REGION=paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax)
    #print(REGION)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
    updateSelectInput(session,"INPUT_CHROMOSOME",selected=RESIZE$chr)
    reset("DOMAIN01")
    reset("DOMAIN02")
    reset("DOMAIN03")
    reset("DOMAIN04")
    reset("DOMAIN05")
    reset("DOMAIN06")
    reset("DOMAIN07")
    reset("DOMAIN08")
    reset("DOMAIN09")
    reset("DOMAIN10")
    reset("DOMAIN11")
    reset("DOMAIN12")
    reset("DOMAIN13")
    reset("DOMAIN14")
    reset("DOMAIN15")
    reset("DOMAIN16")
    reset("DOMAIN17")
    reset("DOMAIN18")
    reset("DOMAIN19")
    reset("DOMAIN20")
    
    # reset("PC")
    #  reset("PERP")
    #  reset("NNEIG")
    #reset("")
    # reset("ddmethod")
   # updateSelectInput(session,"ddmethod",selected="UMAP")
  #  updateSelectInput(session,"DDC_SELECT",selected="hkmeans")
  #  updateSliderInput(session,"PC",value = c(1,2))
  #  updateSliderInput(session,"PERP",value = 30)
  #  updateSliderInput(session,"NNEIG",value = 15)
    
    # updateSliderInput(session,"TA01SLIDE",min=0,max=SUBGNMSIZE$size,value=c(RS,RE))
  })
  observeEvent(input$GO_INPUT_RGN_SLIDE, {
    cat("GO_INPUT_RGN_SLIDE")
    slide <- input$INPUT_RGN_SLIDE
    RESIZE$xmin <- slide[[1]]
    RESIZE$xmax <- slide[[2]]
    RESIZE$xmin<-ifelse(RESIZE$xmin<=0,1,RESIZE$xmin)
    RESIZE$xmax<-ifelse(RESIZE$xmax > RESIZE$right, RESIZE$right, RESIZE$xmax)
    REGION=paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
    updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(RESIZE$xmin,RESIZE$xmax))
    
  })
  
  ###################################
  
  # observeEvent(input$BINNUM_M01,{
  #  updateNumericInput(session,"YRATIO_T01",max=input$BINNUM_M01,value=input$BINNUM_M01)  
  #})
  
  
  # numericInput("BINNUM_M01", "bin_num",min = 1, max = 200, value = 40, step = 1),
  #numericInput("FNMIN_M01", "fn_min",min = 1, max = 10000, value = 2, step = 1),
  #numericInput("FNMAX_M01", "fn_max",min = 1, max = 10000, value = 1000, step = 1),
  
  GLOB_BINNUM=reactiveValues(i=40)
  GLOB_FNMIN=reactiveValues(i=2)
  GLOB_FNMAX=reactiveValues(i=1000)
  observeEvent(input$GO_BIN,{
    GLOB_BINNUM$i<-input$BINNUM_M01
    GLOB_FNMIN$i<-input$FNMIN_M01
    GLOB_FNMAX$i<-input$FNMAX_M01
    updateNumericInput(session,"YRATIO_T01",max=input$BINNUM_M01,value=input$BINNUM_M01)
    #print(GLOB_BINNUM$i)
    #print(GLOB_FNMIN$i)
    #print(GLOB_FNMAX$i)
    #print("BO_BIN-----1055")
    #print(input$GO_BIN[1])
    #print("input$GO_BIN[1]--0")
    #  shinyjs::disable("GO_BIN")
    BTN_BIN$i=1
  })
  
  
  INRG<-100
  ADJ<-5
  
  
  
  
  ############
  YBRKNUM<-5
  XBRKNUM<-5
  COLOR_GRAY="gray"
  ###################################
  FTSNEPNG =reactiveValues(i=FALSE)
  TSNEDATA =reactiveValues(i=FALSE)
  FILTERDATA =reactiveValues(i=FALSE)
  PUB_AIN2 =reactiveValues(i=FALSE)
  PUB_AIN3 =reactiveValues(i=FALSE)
  FDF2D=reactiveValues(i=FALSE)
  ###################################
  #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###################################
  ###################################
  output$CYTOBAND <- renderPlot({
    
    GNMBAND=read.table(paste0(current_path,"/DATA/genome/chrband/",RESIZE$gnm))
    colnames(GNMBAND)<-c("chr","start","end","region","col")
    CHR=RESIZE$chr
    SUBGNMBAND=GNMBAND%>%filter(chr==CHR)
    
    #print(RESIZE$gnm)
    XLIM=c(RESIZE$xmin,RESIZE$xmax)
    XBREAKS<-seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM)
    #scale_x_continuous(breaks=XBREAKS,labels=scales::comma)+
    
    XLIM<-c(RESIZE$xmin,RESIZE$xmax)
    YBREAKS<-5
    head(SUBGNMBAND)
    LABEL=data.frame("BRKS"=XBREAKS)
    
    LMID<-data.frame(LABEL[2:5,])
    colnames(LMID)<-"BRKS"
    LEND<-data.frame(LABEL[c(1,6),])
    colnames(LEND)<-"BRKS"
    #print(LEND)
    RGNLEN=RESIZE$xmax-RESIZE$xmin
    RGNLENX=(RESIZE$xmax-RESIZE$xmin)/2+RESIZE$xmin
    #geom_text(data=data.frame(),aes(x=RGNLENX,y=-3,label=format(round(as.numeric(RGNLEN), 1), nsmall=0, big.mark=",")),
    #          size=4,color="gray",fontface = "italic")+
    ggplot(SUBGNMBAND) + 
      geom_segment(data=LABEL,aes(x=BRKS,xend=BRKS+1,y=5,yend=8),color="blue")+
      #geom_rect(data=SUBGNMSIZE,aes(xmin=0,xmax=size,ymin=0,ymax=15), size=0, fill="white",col="white",alpha=0)+
      geom_rect(data=SUBGNMBAND,aes(xmin=start,xmax=end,ymin=0,ymax=6, fill=col),size=0.2,col="black")+
      scale_fill_manual(values=c("gpos100"= rgb(92/255.0, 92/255.0, 81/255.0),
                                 "gpos"= rgb(92/255.0, 92/255.0, 81/255.0),
                                 "gpos75"= rgb(199/255.0, 192/255.0, 113/255.0),
                                 "gpos66"= rgb(199/255.0, 192/255.0, 113/255.0),
                                 "gpos50"= rgb(134/255.0, 186/255.0, 204/255.0),
                                 "gpos33"= rgb(134/255.0, 186/255.0, 204/255.0),
                                 "gpos25"= rgb(221/255.0, 210/255.0, 180/255.0),
                                 "gvar"= rgb(241/255.0, 230/255.0, 200/255.0),
                                 "gneg"= rgb(250/255.0, 240/255.0, 230/255.0),
                                 "acen"= rgb(219/255.0, 98/255.0, 25/255.0),
                                 "stalk"= rgb(100/255.0, 127/255.0, 164/255.0),
                                 na.value = rgb(100/255.0, 100/255.0, 100/255.0)))+
      
      #geom_rect(data=SUBGNMSIZE,aes(xmin=0,xmax=size,ymin=0,ymax=10), size=0.3, fill="white",col="gray",alpha=0)+
      #scale_fill_grey(end=0.5)+
      theme_bw()+
      geom_text(data=LMID,aes(x=BRKS,y=12,
                              label=format(round(as.numeric(BRKS), 1), nsmall=0, big.mark=",")),
                size=3,color="blue")+
      geom_text_repel(data=LEND,aes(x=BRKS,y=12,
                                    label=format(round(as.numeric(BRKS), 1), nsmall=0, big.mark=",")),
                      size=3,color="blue")+
      geom_text(data=data.frame(),aes(x=RGNLENX,y=-3,label=format(round(as.numeric(RGNLEN), 1), nsmall=0, big.mark=",")),
                size=3,color="black",fontface = "italic")+
      theme(legend.position="none",
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_rect(fill="white"),
            panel.border =  element_rect(size=0.2),
            plot.title = element_blank(),
            #plot.margin =unit(c(t=5, r=30, b=5, l=30), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
            #axis.ticks.length.y.left=unit(-0.25, "cm"),
            #axis.text.y = element_text(margin=unit(c(-0.5,0.5,0.5,0.5), "cm"),),
            axis.ticks = element_blank(),
            axis.text = element_blank(), 
            axis.title = element_blank(),
            plot.background = element_blank())+          
      scale_x_continuous(breaks=XBREAKS,labels=scales::comma)+
      scale_y_continuous(breaks=YBREAKS,labels=CHR)+
      
      coord_cartesian(xlim = XLIM,expand = FALSE,ylim=c(-5,15))
  })
  #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###################################data_s2 <- sample_n(data, 3)
  
#KAISHI:TRACKGENE
  GLOB_2D_DFM=reactiveValues(i=NULL)
  GLB_GENE_FILE=reactiveValues(i=FALSE)
  
  output$TRACKGENE <- renderPlot({
    if(input$CHKGENE==FALSE){
      return(NULL)
    }
    ### genome
   # if (!require("Gviz"))   BiocManager::install("Gviz") ##XX
  #  if (!require("GenomeInfoDb"))  BiocManager::install("GenomeInfoDb") ##XX 
  #  if (!require("GenomicRanges"))  BiocManager::install("GenomicRanges") ##XX 
    GLB_GENE_FILE$i<-read.table(paste0(current_path,"/DATA/genome/gene/",RESIZE$gnm,".gvz"),header=TRUE,sep="|")
    colnames(GLB_GENE_FILE$i)<-c("chromosome","start","end","width","strand","feature","gene",
                                 "exon","transcript","symbol","nexon","isotype","color")
    
    GENENAME<- GLB_GENE_FILE$i
   # GENENAME<-read.table(paste0(current_path,"/DATA/genome/gene/",RESIZE$gnm,".gvz"),header=TRUE,sep="|")
    # colnames(GENENAME)<-c("chromosome","start","end","width","strand","feature","gene","exon","transcript","symbol","nexon","isotype","color")
    #print(head(GENENAME))
    DATA=GENENAME
    RGNC=RESIZE$chr
    RGNS=RESIZE$xmin
    RGNE=RESIZE$xmax
    COL01="darkgreen"
    STK=input$GENESTYLE
    OIN<-DATA %>% filter(chromosome==RGNC,start>=RGNS,end<=RGNE)
    OING<-data.frame(unique(OIN$gene))
    
    colnames(OING)<-"gene"
    #print("000000000000  gene number 000000000")
    #print(nrow(OING))
    MIN_GENE<-merge(DATA,OING,by="gene",all.x=FALSE,all.y=TRUE)
    #print(head(MIN))
    if (input$ISOSTYLE=="longest"){
      SIN_GENE<-MIN_GENE %>% filter(isotype=="LONGEST_ISOFORM")
      
    }else if(input$ISOSTYLE=="merged"){
      SIN_GENE<-MIN_GENE %>% mutate(transcript = gene)
      #print(head(SIN))
    }else{
      SIN_GENE<-MIN_GENE
    }
    WTH=input$PWGENE
   # SIN_GENE_01<-SIN_GENE %>% filter(strand=="+");#print(head(SIN_GENE_01))
  #SIN_GENE_02<-SIN_GENE %>% filter(isotype=="LONGEST_ISOFORM")%>%filter(chromosome==RGNC)%>%filter(start>=RGNS)%>%filter(end<=RGNE);#print(head(SIN_GENE_02))
  
                 
    HTSIN<-length(unique(SIN_GENE$transcript))*5
    HTSIN=ifelse(HTSIN<100,100,HTSIN)
    updateNumericInput(session,"HT_GENE",value=HTSIN)
    #print(head(SIN_GENE));#print("GGGGGG")
  #print(RESIZE$xmin);#print(RESIZE$xmax)
      grtrack02 <- GeneRegionTrack(SIN_GENE,
                                 strand=SIN_GENE$strand,
                                 rstarts = SIN_GENE$start,
                                 rends = SIN_GENE$end,
                                 col=NULL,
                                 #fill=SIN_GENE_02$color,
                                 fill="#78909C",
                                 col.line=NULL,
                                 lwd=0.5,
                                 stacking=input$GENESTYLE, #c(hide, dense, squish, pack,full). 
                                 add35=TRUE,add53=TRUE,
                                 collapseTranscripts=FALSE,
                                arrowHeadWidth=20,
                                 fontsize.group=input$GENESIZE,
                                 shape=c("smallArrow", "box"), ## box, arrow, ellipse, and smallArrow are implemented.
                                 transcriptAnnotation = "symbol")
 
  #  grid.newpage()
  #  pushViewport(viewport(height=1,width=WTH, y=1, just="top"))
  #  grid.rect(gp=gpar(col="grey",lineheight=0.1))
      plotTracks(grtrack02,from = RGNS,to = RGNE,
                 panel.only=TRUE,chromosome = RGNC, 
                 add = TRUE)
   # popViewport(1)


    
  })
  output$GENETRACK <- renderUI({
    plotOutput("TRACKGENE", height = input$HT_GENE,width=paste0(input$WD_GENE,"%"))
  })
#JIESHU:TRACKGENE
  ###################################data_s2 <- sample_n(data, 3)
  SEL_GENE_LIST=reactiveValues(i=FALSE)
  
  observeEvent(   input$GO_INPUT_GENE_NAME, 
                  {
                    GENE_NAME=input$INPUT_GENE_NAME
                    FILEGENE<-GLB_GENE_FILE$i
                    
                    #print(head(FILEGENE))
                    #print("~~~~~~~~~  !!!!!!!")
                    #GENE_LIST<-agrep(GENE_NAME,FILEGENE$gene,ignore.case=TRUE,max=0)
                    SELECTED_GENE_LIST <- dplyr::filter(FILEGENE, grepl(GENE_NAME, gene,ignore.case=TRUE))#%>%dplyr::filter(isotype == "LONGEST_ISOFORM")
                    SEL_GENE_LIST$i <-SELECTED_GENE_LIST 
                    #SELECTED_GENE <- dplyr::filter(FILEGENE, grepl(paste0('^',GENE_NAME,'$'), gene,ignore.case=TRUE))%>%filter(isotype == "LONGEST_ISOFORM")
                    updateSelectInput(session,"LIST_GENE",choices=SELECTED_GENE_LIST$gene,selected = head(SELECTED_GENE_LIST$gene, 1))
                    
                  })
  observeEvent(input$LIST_GENE, {
    if(is.null(input$LIST_GENE) | SEL_GENE_LIST$i==FALSE) return(NULL)
    #print(input$LIST_GENE)
    SELECTED_GENE <- dplyr::filter(SEL_GENE_LIST$i, grepl(paste0('^',input$LIST_GENE,'$'), gene,ignore.case=TRUE))%>%dplyr::filter(isotype == "LONGEST_ISOFORM")
    
    GENE_STRAND=SELECTED_GENE$strand
    GENE_NAME=SELECTED_GENE$gene
    #print(GENE_NAME)
    GENE_CHROM<-SELECTED_GENE[1,]$chromosome
    #print((SELECTED_GENE))
    EXT=0.1 #RESIZE$probe #==0
    #print(ifelse(length(unique(SELECTED_GENE$gene))>1,"Match More","Uniq Match"))
    #print(head(SELECTED_GENE));#print("405")
    GENE_MINX=min(SELECTED_GENE$start,SELECTED_GENE$end)
    GENE_MAXX=max(SELECTED_GENE$start,SELECTED_GENE$end)
    GENELEN=GENE_MAXX-GENE_MINX
    #print(GENELEN)
    #print(EXT)
    #print("399")
    #print(GENE_MINX)
    #print(GENE_MAXX)
    GENELEN=ifelse(GENELEN<input$MIN_GENE_RGN,input$MIN_GENE_RGN,GENELEN)
    GENE_MINX=GENE_MINX-GENELEN
    GENE_MAXX=GENE_MAXX+GENELEN
    #print(GENE_MINX)
    #print(GENE_MAXX)
    
    #RESIZE$gene <- GENE_NAME
    #  #print (paste0( RESIZE$chr,":", RESIZE$xmin,"-",RESIZE$xmax));#print("433")
   # updateRadioButtons(session,"ISOSTYLE",selected="longest")
    #  updateSliderInput(session,"INPUT_RGN_SLIDE",min=RESIZE$left,max=RESIZE$right,value=c(RESIZE$xmin,RESIZE$xmax))
    REGION=paste0(GENE_CHROM,":",GENE_MINX,"-",GENE_MAXX)
    #REGION=paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax)
    updateTextInput(session,"INPUT_RGN_TEXT",value=REGION)
    #print(REGION)
    #print("798")
  })
  ###################################
  

  
  MAINDATA=reactiveValues(i=NULL)
  GLOB_SIN_FREQ=reactiveValues(i=NULL)
  GLOB_PVAL=reactiveValues(i=NULL)
  GLOB_FIN=reactiveValues(i=NULL)
  GLOB_MDF=reactiveValues(i=NULL)
  LAST_FIN_NAME=reactiveValues(i="NO")
  MAXFNSUM=10000
  GLOB_PLOCF=reactiveValues(i=NULL)
  
  GLB_LOC=reactiveValues(lrgn=NULL,lc=NULL,ls=NULL,le=NULL,sig=NULL)
  CLU0=reactiveValues(i=NULL)
 
  observeEvent( 
    {
      input$BUTTON_CLU
    },{  
      if (input$ID_CLU==""){
        CLU0$i=NULL
      }else{
        CLU0$i=input$ID_CLU
      }
    }
  )# input$BUTTON_LOC, 
#KAISHI:MAINFIN
  observeEvent({input$MAINFIN
    input$BUTTON_LOC
    RESIZE$chr
    RESIZE$xmin
    RESIZE$xmax
    #        RESIZE <- reactiveValues(xmin = 5749229, xmax = 5927949, gnm="dm3",chr="chrX",left=1,right=5927949,gene="",strand="+",prom=0,probe=0)
#    RESIZE
    RESIZE$gnm
    RESIZE$left
    RESIZE$right
    BTN_BIN$i
    #   input$FNMIN_M01
    #    input$FNMAX_M01
  },{
    
    #print("807807807807807807807")
    FINGNM=(((strsplit(as.character(input$MAINFIN),'.',fixed=TRUE))))[[1]][1]
    #print(input$MAINFIN)
    #print("input$MAINFIN")
    #print(FINGNM)
    #print("FINGNM")
    updateSelectInput(session,"INPUT_REFGNM",selected=FINGNM)
    
    FILENAME=paste0("./LIB/",input$MAINFIN,"/",RESIZE$chr,".SUBRDS.smp");#print(FILENAME)
    FIN<-readRDS(FILENAME)
    colnames(FIN)[1] <- "freg_chrom"
    colnames(FIN)[2] <- "freg_start"
    colnames(FIN)[3] <- "freg_end" 
    colnames(FIN)[4] <- "frag_num"
    colnames(FIN)[5] <- "lib"
    colnames(FIN)[6] <- "GEMNUMID"
    #print(head(FIN,4));#print(str(FIN));#print(input$MAINFIN);#print("##MAINFIN")
    #print("head(FIN,2)  870")
    #FIN<-FIN%>%mutate(gem_id=paste("lib",GEMNUMID,sep="-"))
    FIN<-FIN%>%mutate(gem_id=paste(lib,GEMNUMID,sep="-"))
    #print(head(FIN));#print("head(FIN)")
    #print(RESIZE$xmax);#print(RESIZE$xmin);
    SIN<-FIN
    SIN<- dplyr::filter(SIN, freg_chrom == RESIZE$chr); #print(tail(SIN));
    SIN<- dplyr::filter(SIN, freg_end <= RESIZE$xmax); #print(tail(SIN));
    SIN<- dplyr::filter(SIN, freg_start >= RESIZE$xmin); #print(tail(SIN));

   

 
    
    ##################
    
    #print(">>>>");#print(nrow(SIN));#print("SINOUT<<<<");
    
    
    
    #
    
    
    
    DXT<-data.frame(tidy(summary(as.numeric(SIN$frag_num))))
    
    updateNumericInput(session,"SCRNA_EXP",min=DXT$minimum,max=DXT$maximum,value=DXT$median)
    updateNumericInput(session,"SCRNA_EXP_MAX",min=DXT$minimum,max=DXT$maximum,value=DXT$maximum)
    #print(head(SIN,2))
    #print(nrow(SIN))
    
    #print("head(SIN,2)---1")
    
    #print(head(SIN));#print("head(SIN)")
    LX<-data.frame("NUM"=t(tidy(summary(as.numeric(SIN$frag_num)))))
    LX$NAME<-row.names(LX)
    PLOCF=tableGrob(LX)
    GLOB_PLOCF$i=PLOCF
    CUR_FIN_NAME=input$MAINFIN
    if(CUR_FIN_NAME=="dm3.SCRNA" ){
      
      updateRadioButtons(session,"SCRNA_EXP_YES",selected ="Yes")
      SIN<-SIN%>%dplyr::filter(frag_num>=DXT$minimum,frag_num<=DXT$maximum)
    }else{
      updateRadioButtons(session,"SCRNA_EXP_YES",selected ="No")
    }
    
    
    
    # if(input$SCRNA_EXP_YES=="Yes"){
    #     SIN<-SIN%>%dplyr::filter(frag_num>=input$SCRNA_EXP,frag_num<=input$SCRNA_EXP_MAX)
    #    }
    #else if(input$SCRNA_EXP_YES=="No"){
    #      SIN<-MAINDATA$i
    #   }
    #print(head(SIN));#print("head(SIN)")
    SIN_FREQ<-data.frame(table(SIN$gem_id));   #print(head(SIN_FREQ));#print("SIN_FREQ");
    colnames(SIN_FREQ)<-c("gem_id","Freq")
 
   
    SIN<-merge(SIN,SIN_FREQ,by="gem_id",drop=FALSE)
    #print(head(SIN,2))
    #print(nrow(SIN))
    
    #print("head(SIN,2)---2")
    SIN<-SIN%>%mutate(frag_num=Freq)%>%
      dplyr::select(-Freq)
    #print(head(SIN,2))
    #print(nrow(SIN))
    
    #print(MAXFNSUM)
    
    #print(table(SIN_FREQ$Freq))
    #print("table(SIN_FREQ$Freq)")
    fit_fn = function(fragcount,maxfragcount){
      df=data.frame(table(fragcount))
      colnames(df)<-c("gemfn","gemnum")
      rdf<- df[seq(dim(df)[1],1),]
      rdf$gemfn<-as.numeric(rdf$gemfn)
      rdf$gemnum<-as.numeric(rdf$gemnum)
      mdf<-rdf%>% mutate(fragnum=gemfn*gemnum)
      mdf<-mdf %>%mutate(gemsum=cumsum(gemnum),freqsum=cumsum(fragnum))
      #print(mdf)
      #print("mdf")
      GLOB_MDF$i<-data.frame(mdf)
      #print(GLOB_MDF$i)
      #print("GLOB_MDF$i")
      # ggplot(mdf,aes(gemfn,fragsum))+
      #   geom_step()
      for (i in nrow(mdf):1){
        if(mdf$freqsum[i] <= maxfragcount){
          #print(paste("fragnum <=",maxfragcount))
          #print(mdf[i,])
          return(mdf$gemfn[i])
          break
        }
      }
    }
   #print(fit_fn(SIN_FREQ$Freq,MAXFNSUM))
    fitfulfn=as.integer(fit_fn(SIN_FREQ$Freq,MAXFNSUM))
    
    #print(paste("fitfulfn",fitfulfn))
    FRER_SIN<-as.numeric(SIN_FREQ$Freq)
    if(LAST_FIN_NAME$i != CUR_FIN_NAME ){
      BTN_BIN$i=0
      shinyjs::disable("GO_BIN")
      shinyjs::enable("GO_BIN")
    }
    #print(CUR_FIN_NAME)
    #print("CUR_FIN_NAME")
    #print(LAST_FIN_NAME$i)
    #print("LAST_FIN_NAME")
    
    
 #   if(BTN_BIN$i==0){
      
 #     mmm=fitfulfn
  #    MMM=fitfulfn+2
 #     SIN<-SIN%>% dplyr::filter(frag_num>=mmm)
      #  BTN_BIN$i=1
 #   }else{
      mmm=input$FNMIN_M01
      MMM=input$FNMAX_M01
      #  mmm=ifelse(min(FRER_SIN)<2,2,min(FRER_SIN))
      SIN<-SIN%>% dplyr::filter(frag_num>=mmm)%>% dplyr::filter(frag_num<=MMM)
      
 #   }
    mmm<-ifelse(mmm<2,2,mmm) 
  #  MMM<-ifelse(MMM<4,4,MMM)
    GLOB_FNMIN$i=mmm
    GLOB_FNMAX$i=max(FRER_SIN)
    LAST_FIN_NAME$i=CUR_FIN_NAME
    #print(head(SIN,2))
    #print(nrow(SIN))
    
    #print("head(SIN,2)---4")
    #print(input$GO_BIN[1])
    #print("input$GO_BIN[1]")
    #  if(input$SCRNA_EXP_YES=="Yes"){
    #     SIN<-SIN%>%dplyr::filter(frag_num>=input$SCRNA_EXP,frag_num<=input$SCRNA_EXP_MAX)
    #   }else if(input$SCRNA_EXP_YES=="No"){
    #     SIN<-MAINDATA$i
    #   }
    MAINDATA$i<-SIN
    
    updateNumericInput(session,"FNMIN_M01",min=min(FRER_SIN),max=max(FRER_SIN),value=mmm)
    updateNumericInput(session,"FNMAX_M01",min=min(FRER_SIN),max=max(FRER_SIN),value=MMM)
    GLOB_SIN_FREQ$i<-SIN_FREQ

  }) ##  observeEvent({input$MAINFIN
#print("#JIESHU:MAINFIN")
#KAISHI:DATA  
  ###############################
  GLOB_INFO01=reactiveValues(i=NULL)
  GLOB_INFO02=reactiveValues(i=NULL)
  output$TRK_DATA <- renderPlot({
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    
    # FIN<-MAINDATA()
    SIN<-MAINDATA$i
    
    #print(head(SIN))
    #print("head(SIN,2)  1549")
    #print(table(SIN$frag_num))
    
    
    
    
    
    
    #DX<-data.frame("NUM"=t(tidy(summary(as.numeric(SIN$frag_num)))))
    #DX$NAME<-row.names(DX)
    #print(DX)
    #PF<-tableGrob(DX)
    #KAISHI:DATA2    
    output$TRK_DATA2 <- renderTable({format(GLOB_MDF$i, digits=2, nsmall=0)})
    
    
    
    
    RC=RESIZE$chr
    RS=RESIZE$xmin
    RE=RESIZE$xmax
    RLEN=RE-RS
    BINNUM=GLOB_BINNUM$i
    BINSIZE=round(RLEN/BINNUM)  
    
    BIN_COOR_LIST<-seq(0,BINNUM,1)*BINSIZE+RS
    mmLCFN=GLOB_FNMIN$i
    MMLCFN=GLOB_FNMAX$i
    
    
    
    
    # AIN0<-FIN%>%filter(freg_chrom==RC, freg_start>=RS, freg_end<=RE)
    AIN0<-SIN
    
    AIN0_FREQ<-GLOB_SIN_FREQ$i
    
    # AIN0_FREQ<-data.frame(table(AIN0$gem_id))
    #print(head(AIN0_FREQ))
    #print("head(AIN0_FREQ)")
    
    
    #   LX<-data.frame("NUM"=t(tidy(summary(as.numeric(AIN0_FREQ$Freq)))))
    #    LX$NAME<-row.names(LX)
    #    PLOCF=tableGrob(LX)
    PLOCF=GLOB_PLOCF$i
    
    AIN0_FREQ_FILTER<-AIN0_FREQ%>%filter(Freq>=mmLCFN, Freq<=MMLCFN)  ## local FN filtering
    NDATA=nrow(AIN0_FREQ_FILTER)
    while (NDATA<2){
      mmLCFN=mmLCFN-1
      AIN0_FREQ_FILTER<-AIN0_FREQ%>%filter(Freq>=mmLCFN, Freq<=MMLCFN)  ## local FN filtering
      NDATA=nrow(AIN0_FREQ_FILTER)
    }
    
    
    colnames(AIN0_FREQ_FILTER)<-c("gem_id","LCFN")
    AIN2<-merge(AIN0,AIN0_FREQ_FILTER,by="gem_id")
    
    
    
    LOCAL_UNIQ_GEM<-unique(AIN2$GEMNUMID)
    LOCAL_UNIQ_GEM_NUM<-length(LOCAL_UNIQ_GEM)
    
    #print(BIN_COOR_LIST)
    #print("BIN_COOR_LIST")
    DFM<-data.frame(matrix(0,nrow=LOCAL_UNIQ_GEM_NUM,ncol=BINNUM+1))
    colnames(DFM)<-BIN_COOR_LIST
    rownames(DFM)<-LOCAL_UNIQ_GEM
    #####################################20200817 modify
    
    AIN3<-AIN2%>%mutate(BIN_POS_FNMID=RS+floor(((freg_end-freg_start)/2+freg_start-RS)/BINSIZE)*BINSIZE)
    # nrow(AIN2)
    #  #print(head(AIN3))
    #  #print("head(AIN3)")
    #  #print(DFM[1,])
    #  #print("^^^^^ 1 ^^^^^^^")
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  for (i in 1:nrow(AIN3)){
    #    ROWNAM=as.character(AIN3$GEMNUMID[i])
    #    COLNAM=as.character(AIN3$BIN_POS_FNMID[i])
    #    if(input$HTYPE=="binary"){ #binary
    #      DFM[ROWNAM,COLNAM]=1
    #    }else if(input$HTYPE=="digital"){ #digital
    ##      DFM[ROWNAM,COLNAM]=AIN3$frag_num[i]+DFM[ROWNAM,COLNAM]
    #    }
    #  }
    ##################
    
    TRYAIN3<-AIN2%>%mutate(BIN_N_S=floor((freg_start-RS)/BINSIZE),BIN_POS_S=RS+BINSIZE*floor((freg_start-RS)/BINSIZE))%>%
      mutate(BIN_N_E=floor((freg_end-RS)/BINSIZE),BIN_POS_E=RS+BINSIZE*floor((freg_end-RS)/BINSIZE))
    #print(paste("RS=",RS))
    #print(paste("BINSIZE=",BINSIZE))
    # #print(head(TRYAIN3))
    #  #print("head(TRYAIN3)")
    # #print("///////////")
    for (i in 1:nrow(TRYAIN3)){
      ROWNAM=as.character(TRYAIN3$GEMNUMID[i])
      # #print(paste("ROWNAM=",ROWNAM))
      SBIN=as.numeric(TRYAIN3$BIN_N_S[i])
      EBIN=as.numeric(TRYAIN3$BIN_N_E[i])
      #print(TRYAIN3[i,])
      #  #print(SBIN)
      # #print(EBIN)
      for(j in SBIN:EBIN){
        # #print(j)
        BINPOS=RS+BINSIZE*j
        COLNAM=as.character(BINPOS)
        #print(paste("COLNAM=",COLNAM))
        if(input$HTYPE=="binary"){ #binary
          DFM[ROWNAM,COLNAM]=1
        }else if(input$HTYPE=="digital"){ #digital
          DFM[ROWNAM,COLNAM]=TRYAIN3$frag_num[i]+DFM[ROWNAM,COLNAM]
        }
      }
      
    }
    #print("<<<<<<<<")
    #print(DFM[1,])
    #print("^^^^^ 1 ^^^^^^^") 
    
    ###############
    ROWNAM=AIN3$GEMNUMID[1]
    
    
    DFM<-DFM[,-ncol(DFM)]
    
    #print(head(DFM,2))
    #print("head(DFM)")
    
    #TSNEDATA$i<-DATATSNE
    FTSNEPNG$i<-pt
    FILTERDATA$i<-DFM
    PUB_AIN2$i <-AIN2
    PUB_AIN3$i <-AIN3
    
    #print(head(FTSNEPNG$i,2))
    #print("head(FTSNEPNG$i,2)")
    #print(head(FILTERDATA$i,2))
    #print("head(FILTERDATA$i,2)")
    #print(head(PUB_AIN2$i,2))
    #print("head(PUB_AIN2$i,2)")
    #print(head(PUB_AIN3$i,2))
    #print("head(PUB_AIN3$i,2)")
    #print("-------1227------")
    RGNSIZE=format(round(as.numeric(RESIZE$xmax-RESIZE$xmin), 1), nsmall=0, big.mark=",")
    BINSIZE=format(round(as.numeric((RESIZE$xmax-RESIZE$xmin)/GLOB_BINNUM$i), 1), nsmall=0, big.mark=",")
    INFO=data.frame(infomation=c(paste0(input$MAINFIN),
                                 paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax),
                                 paste0("regionSize=",RGNSIZE),
                                 paste0("complexNum=",nrow(DFM)),
                                 paste0("binNum=",GLOB_BINNUM$i),
                                 paste0("binSize=",BINSIZE),
                
                                 paste0("fragCount=",nrow(PUB_AIN3$i))
                                 
                                 
    ))
    INFO2=data.frame(infomation=c(paste0(GLOB_FNMIN$i,"<=fragNum<=",GLOB_FNMAX$i),
                                  paste0("scRNA filter by RPKM=",input$SCRNA_EXP_YES),
                                  paste0(format(input$SCRNA_EXP,  digits = 2,nsmall = 6),"<=geneRPKM<=",format(input$SCRNA_EXP_MAX,  digits = 2,nsmall = 6)),
                                  paste0(input$HEATMAP_COLMAX[[1]],"<=2DheatColor>=",input$HEATMAP_COLMAX[[2]]),
                                  paste0(input$LOOP_PET[[1]],"<=loopPet<=",input$LOOP_PET[[2]]),
                                  paste0(input$LOOP_LEN[[1]],"<=loopLen<=",input$LOOP_LEN[[2]]),
                                  paste0("HIT type=",input$HTYPE)
    ))
    
    DF_CLU_COR=data.frame(
      clustername=as.factor(seq(1,29)),
      colorcode=c(brewer.pal(8, "Dark2"),brewer.pal(9, "Set1"),brewer.pal(12, "Set3"))
    )
    
    #print(INFO)
    #PF<-ggplot(data.frame())+theme_void()
    PI<-tableGrob(INFO)
    PI2<-tableGrob(INFO2)
    
    GLOB_INFO01$i<-INFO
    GLOB_INFO02$i<-INFO2
    grid.arrange(arrangeGrob(PI,PI2,nrow=1))
    #grid.arrange(arrangeGrob(PI,PI2,PLOCF,nrow=2,ncol=2),nrow=1)
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    #print("before           grid.arrange(PF,PLOCF,PI,nrow=1)------")
    # grid.arrange(PF,PLOCF,PI,PI2,nrow=1)
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    #print("after           grid.arrange(PF,PLOCF,PI,nrow=1)------")
  })
  output$UI_DATA <- renderUI({
    plotOutput("TRK_DATA", height =input$HT_LAST)
  })
#print("#JIESHU:DATA")
  output$UI_DATA2 <- renderUI({
    tableOutput("TRK_DATA2")
  }) #
#print("#JIESHU:DATA2")
  ###########################################
  #KAISHI:M01
  RES=reactiveValues(i=FALSE)
  RES_CLU=reactiveValues(i=FALSE)
  DFM_CLU=reactiveValues(i=FALSE)
  NEW_DFM_CLU=reactiveValues(i=FALSE)
  
  output$TRK_M01<- renderPlot({
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    req(input$MAINFIN)
    if(input$CHK_M01==FALSE){
      return(NULL)
    }
    #DATATSNE <- TSNEDATA$i
    # pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
   # #print(head(DFM,3))
   # #print("head(DFM,3) -- M01 1124")

    PH<-pheatmap(DFM,
                 legend = FALSE,
                 cluster_rows=TRUE,
                 clustering_distance_rows =input$DIST, ##"euclidean",
                 clustering_method = input$CLST,##"complete",
                 cluster_cols=FALSE,
                 cutree_rows = input$HD_CUTREE,
                 border_color = NA,
                 treeheight_row = input$TH_M01, 
                 treeheight_col = 0,
                 annotation_legend = FALSE,
                 annotation_names_row = FALSE,
                 color =c(input$CC1_M01,input$CC2_M01),
                 show_rownames=FALSE,
                 show_colnames = FALSE,
                 silent = TRUE)
    
    PH_NOTCLU<-pheatmap(DFM,
                 legend = FALSE,
                 cluster_rows=FALSE,
                 clustering_distance_rows =input$DIST, ##"euclidean",
                 clustering_method = input$CLST,##"complete",
                 cluster_cols=FALSE,
                 cutree_rows = input$HD_CUTREE,
                 border_color = NA,
                 treeheight_row = input$TH_M01, 
                 treeheight_col = 0,
                 annotation_legend = FALSE,
                 annotation_names_row = FALSE,
                 color =c(input$CC1_M01,input$CC2_M01),
                 show_rownames=FALSE,
                 show_colnames = FALSE,
                 silent = TRUE)
     
      res_clu<-cutree(PH$tree_row, k = input$HD_CUTREE)
      RES_CLU$i<-res_clu
      attributes(res_clu) <- NULL
     #print(head((res_clu)))
     PH$tree_row$cluster<-res_clu
      #print(str(PH$tree_row))
      RES$i<-PH
      
      dfm_clu<-cbind(DFM,"HDCLU"=cutree(PH$tree_row, k = input$HD_CUTREE))
      DFM_CLU$i <- dfm_clu[order(dfm_clu$HDCLU),]
      
      new_dfm_clu<-cbind(DFM,"cluster_list"=cutree(PH$tree_row, k = input$HD_CUTREE))
      new_dfm_clu$GEMNUMID<-rownames(new_dfm_clu)
      NEW_DFM_CLU$i <- new_dfm_clu[order(new_dfm_clu$cluster_list),]
      #print(NEW_DFM_CLU$i[1:3,])
   if(input$LOC_HDLD=="HD"){
      CLUSTER_LIST=seq(1:input$HD_CUTREE)
      observe({
        # if(input$selectall == 0) return(NULL) 
        #  else 
        if (input$DATA_SLCT_ALL%%2==1)
        {
          updateCheckboxGroupInput(session,"DATA_SLCT",choices= CLUSTER_LIST,inline=TRUE)
        }
        else
        {
          updateCheckboxGroupInput(session,"DATA_SLCT",choices= CLUSTER_LIST,selected=CLUSTER_LIST,inline=TRUE)
        }
      })
   }
      if (input$CLUORNOT=="CLU"){
     grid.arrange(PH[[4]])
      }else{
        grid.arrange(PH_NOTCLU[[4]])
      }

  
    updateNumericInput(session,"HT_M01",value=300)
    ## <<<<<<<<<<<<< M
  })
  
  output$UI_M01 <- renderUI({
    plotOutput("TRK_M01", height = input$HT_M01+RDM0$i,width=paste0(input$WD_M01,"%"))
  })
  ##  F
  
  #JIESHU:M01
  
#KAISHI:F01
  ###########################################
  GLOB_NEW_WIN01_F01=reactiveValues(i=FALSE)
  output$TRK_F01<- renderPlot({
    if(input$CHK_F01==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    DATATSNE <- TSNEDATA$i
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    res=RES$i
    DF<-data.frame("A","B",stringsAsFactors = FALSE)
    for (i in 1:length(res$tree_row$order)){
      ROW_CLUSTER_ORDER=i
      ROW_CLUSTER_NAME=res$tree_row$labels[res$tree_row$order[i]]
      ROW_CLUSTER_CLU=res$tree_row$cluster[res$tree_row$order[i]]
      DF[ROW_CLUSTER_ORDER,] <- list(ROW_CLUSTER_CLU,ROW_CLUSTER_NAME)
     # DF[ROW_CLUSTER_ORDER,] <- list(ROW_CLUSTER_ORDER,ROW_CLUSTER_NAME)
    }
    colnames(DF)<-c("cluster","GEMNUMID")
  #  colnames(DF)<-c("related_plot_line_num","GEMNUMID")
    DF$related_plot_line_num<-rownames(DF)


    colourCount = length(unique(res$tree_row$cluster))
    CC=c(brewer.pal(8, "Dark2"),brewer.pal(9, "Set1"),brewer.pal(12, "Set3"),brewer.pal(8, "Accent"),
         brewer.pal(9, "YlOrRd"),brewer.pal(9, "YlOrBr"),brewer.pal(9, "YlGnBu"),brewer.pal(9, "YlGn"),brewer.pal(9, "Reds"),
         brewer.pal(9, "RdPu"),brewer.pal(9, "Purples"),brewer.pal(9, "PuRd"),brewer.pal(9, "PuBuGn"),brewer.pal(9, "PuBu"),
         brewer.pal(9, "Oranges"),brewer.pal(9, "Greys"),brewer.pal(9, "Greens"),brewer.pal(9, "Blues"),brewer.pal(11, "BrBG"),
         brewer.pal(11, "RdGy"))
    #print(length(CC))
    #print("length(CC)")
    df0=data.frame(
      GROUP=factor(1:length(CC)),
      color.codes=CC )
    df<-data.frame(head(df0,as.numeric(colourCount)))
    pal = unique(df)
    pal$color.codes=as.character(pal$color.codes)
    pal$GROUP=as.character(pal$GROUP)
    pval = pal$color.codes
    names(pval) = pal$GROUP
    
  
    
    MIN<-merge(AIN2,DF,by = "GEMNUMID")
    WIN01=MIN
    # str(WIN01)
    WIN01<-transform(WIN01, related_plot_line_num = as.numeric(related_plot_line_num))
    YBREAK=20 ## howmany scales in yaxis
    TEXTSIZE=16 ## text size in plot
    #LH=as.numeric(input$LSIZE)
    #  FH=as.numeric(input$FSIZE)
    #LH=1
    #FTP="butt"
    #FH=2
    #print(head(WIN01))
    NEW_WIN01_F01<-WIN01
    NEW_WIN01_F01$CLUSTERID<-NEW_WIN01_F01$cluster
    NEW_WIN01_F01<-select(NEW_WIN01_F01,-cluster)
    #print(head(NEW_WIN01_F01)); #print("HD-cluster head(NEW_WIN01_F01) -- 1346")
    GLOB_NEW_WIN01_F01$i<-NEW_WIN01_F01
    RS=RESIZE$xmin
    RE=RESIZE$xmax
    PF<-ggplot(WIN01,aes(group=GEMNUMID,fill=GEMNUMID,color=cluster,x=freg_start,y=-related_plot_line_num))+
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=input$FSIZE_F01,lineend = input$SHAPE_F01)+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=input$LSIZE_F01,alpha=0.4)+
      theme_bw()+
      scale_color_manual(values = pval)+
      
      theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.border = element_rect(size=0.2),
            plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
      scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))            
    PF_NOTCLU<-ggplot(WIN01,aes(group=GEMNUMID,fill=GEMNUMID,color=factor(GEMNUMID),x=freg_start,y=gem_id))+
      geom_segment(aes(x=freg_start,xend=freg_end,y=gem_id,yend=gem_id),
                   size=input$FSIZE_F01,lineend = input$SHAPE_F01)+
      geom_line(aes(x=freg_start,y=gem_id),size=input$LSIZE_F01,alpha=0.4)+
      theme_bw()+
   #   scale_color_manual(values = pval)+
      
      theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.border = element_rect(size=0.2),
            plot.title = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
      scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)
     # coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))            
    
    ## >>>>>>>>>>>>> M
    updateNumericInput(session,"HT_F01",value=350)
    # grid.arrange(PHF02[[4]])
  #  grid.arrange(PF)
    if (input$CLUORNOT=="CLU"){
      grid.arrange(PF)
    }else{
      grid.arrange(PF_NOTCLU)
    }
    ## <<<<<<<<<<<<< M
  })
  
  output$UI_F01 <- renderUI({
    plotOutput("TRK_F01", height = input$HT_F01+RDM0$i,width=paste0(input$WD_F01,"%"))
  })
#JIESHU:F01
  #KAISHI:B01  
  output$TRK_B01<- renderPlot({ ## coverage
    if(input$CHK_B01==FALSE){
      return(NULL)
    }
    DATATSNE <- TSNEDATA$i
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    DF2D <- FDF2D$i
    COV<-data.frame(y=colSums(DFM))
    COV$x<-1:nrow(COV)
    
    
    #print(head(COV))
    #print("head(COV)")
    PC<-ggplot(COV,aes(x,y))+
      geom_col(alpha=0.2,aes(color=y,fill=y))+
      geom_line(color="navy")+
      #geom_bspline2(color="navy")+
      #geom_smooth(span=0.1,color="red")+
      theme_bw()+
      theme(legend.position="none",
            
            panel.grid= element_blank(), 
            
            
            panel.background = element_rect(fill="white"),
            panel.border = element_rect(size=0.5,color="gray"),
            axis.title= element_blank(), 
            plot.margin =unit(c(t=5, r=3, b=5, l=3), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
            axis.text.y = element_blank(), 
            axis.ticks.y=element_blank(),
            
            plot.background = element_blank()
      )+
      scale_x_continuous(breaks=seq(1,GLOB_BINNUM$i,1))+
      coord_cartesian(xlim=c(0.5,GLOB_BINNUM$i+0.5), expand = FALSE,ylim=c(min(COV$y)-1,max(COV$y)+1))
    if(input$YTK_B01=="Yes"){ 
      PC2<-ggplot(COV,aes(x,y))+
        
        theme_bw()+
        theme(legend.position="none",
              panel.grid.minor = element_blank(), 
              panel.grid.major.x  = element_blank(), 
              panel.background = element_blank(),
              panel.border =  element_blank(),
              axis.title= element_blank(), 
              plot.margin =unit(c(t=0, r=0, b=0, l=0), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
              axis.text.x = element_blank(), 
              axis.ticks.x =element_blank(),
              axis.text.y = element_text(color="red"), 
              axis.ticks.y =element_line(color="red"),
              plot.background = element_blank()
        )+
        scale_x_continuous(breaks=seq(1,GLOB_BINNUM$i,1))+
        coord_cartesian(xlim=c(0.5,GLOB_BINNUM$i+0.5), expand = FALSE,ylim=c(min(COV$y)-1,max(COV$y)+1))
      
      PC<- PC+annotation_custom(
        grob = ggplotGrob(PC2),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) 
    }# if(input$YTK_B01="YES"){ 
    
    
    updateNumericInput(session,"HT_B01",value=100)
    
    grid.arrange(PC)
    
  })
  
  output$UI_B01 <- renderUI({
    plotOutput(
      click = "CLK_B01",
      "TRK_B01", 
      height = input$HT_B01,width=paste0(input$WD_B01,"%"))
  })
  #JIESHU:B01
  ## >>>>>>>>>>>>> T
#KAISHI:T00
  GLOB_RNM4=reactiveValues(i=NULL)
  output$TRK_T00<- renderPlot({ ## 2D heatmap
    
    if(input$CHK_T01==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    DATATSNE <- TSNEDATA$i
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    #DFM <- FILTERDATA$i
    
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    DF2D<- data.frame(matrix(0, ncol = 3, nrow = 0),stringsAsFactors = FALSE)
    #print(head(DF2D))
    #print("^^^^^ 4 ^^^^^^^")
    for (i in 1:ncol(DFM)){
      for (j in 1:ncol(DFM)){
        if(i<=j){
          newline=data.frame(i,j ,sum(DFM[,i]*DFM[,j]))
          #newline=data.frame(colnames(DFM)[i] ,colnames(DFM)[j] ,sum(DFM[,i]*DFM[,j]))
          DF2D<-rbind(DF2D,newline)
        }
      }
    }
    colnames(DF2D)<-c("x","y","z")
    #print(head(DF2D))
    df_loop_length<-DF2D%>%mutate(len=y-x)
    #print((min(df_loop_length$len)))
    #print("^^^^^ 5 ^^^^^^^")
    updateSliderInput(session,"LOOP_PET",min=min(DF2D$z),max=max(DF2D$z),value=c(min(DF2D$z),max(DF2D$z)))
    updateSliderInput(session,"LOOP_LEN",min=min(df_loop_length$len),max=max(df_loop_length$len),
                      value=c(min(df_loop_length$len),max(df_loop_length$len)))
    FDF2D$i <-DF2D
    n=ncol(DFM)
    NM <- data.frame(matrix(0, ncol = 3, nrow = 0),stringsAsFactors = FALSE)
    colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(42)
    
    NM4 <-data.frame(matrix(NA, ncol = 2*n, nrow =n),stringsAsFactors = FALSE)
    colnames(NM)<-c("x","y","z")
    head(NM)
    for(i in 1:n){
      for(j in 1:n){
        if(input$BOTTOM==FALSE){
          if(j>i){
            k=j-i
            newline01<-data.frame(i*2+k-1,k+1,filter(DF2D,x==i,y==j)$z)
            NM4[k+1,(i*2+k-1)]=ifelse(is.na(NM4[k+1,(i*2+k-1)]) , 0, NM4[k+1,(i*2+k-1)])
            NM4[k+1,(i*2+k-1)]=filter(DF2D,x==i,y==j)$z+ NM4[k+1,(i*2+k-1)]
            colnames(newline01)<-c("x","y","z")
            NM<-rbind(NM,newline01)
            newline02<-data.frame(i*2+k  ,k+1,filter(DF2D,x==i,y==j)$z)
            NM4[k+1,(i*2+k)]=ifelse(is.na(NM4[k+1,(i*2+k)]),0,NM4[k+1,(i*2+k)])
            NM4[k+1,(i*2+k)]=filter(DF2D,x==i,y==j)$z+NM4[k+1,(i*2+k)]
            colnames(newline02)<-c("x","y","z")
            NM<-rbind(NM,newline02)
          }
        }else{
          if(j>=i){
            k=j-i
            newline01<-data.frame(i*2+k-1,k+1,filter(DF2D,x==i,y==j)$z)
            NM4[k+1,(i*2+k-1)]=ifelse(is.na(NM4[k+1,(i*2+k-1)]) , 0, NM4[k+1,(i*2+k-1)])
            NM4[k+1,(i*2+k-1)]=filter(DF2D,x==i,y==j)$z+ NM4[k+1,(i*2+k-1)]
            colnames(newline01)<-c("x","y","z")
            NM<-rbind(NM,newline01)
            newline02<-data.frame(i*2+k  ,k+1,filter(DF2D,x==i,y==j)$z)
            NM4[k+1,(i*2+k)]=ifelse(is.na(NM4[k+1,(i*2+k)]),0,NM4[k+1,(i*2+k)])
            NM4[k+1,(i*2+k)]=filter(DF2D,x==i,y==j)$z+NM4[k+1,(i*2+k)]
            colnames(newline02)<-c("x","y","z")
            NM<-rbind(NM,newline02)
          }
        }
      }
    }
    #print(head(NM4))
    MAX2DVALUE=0
    MIN2DVALUE=999999999999
    for(i in 1:nrow(NM4)){
      for(j in 1:ncol(NM4)){
        if (! is.na(NM4[i,j])){
          MAX2DVALUE=max(MAX2DVALUE,NM4[i,j])
          MIN2DVALUE=min(MIN2DVALUE,NM4[i,j])
        }
      }
    }
    #print(MAX2DVALUE)
    #print("MAX2DVALUE")
    #print(MIN2DVALUE)
    #print("MIN2DVALUE")
    # breaksList = seq(MIN2DVALUE,MAX2DVALUE, by = GAP)
    updateSliderInput(session,"HEATMAP_COLMAX",min=MIN2DVALUE,max=MAX2DVALUE,value=c(MIN2DVALUE,round(MAX2DVALUE/3)),step=0.1)
    
    # breaksList = seq(0, input$HEATMAP_COLMAX, by = 1)
    #, min =0,   max = 200, value = 200,step=1)
    #print(input$YRATIO_T01)
    NM4<-NM4[1:input$YRATIO_T01,]
    if(input$HEATMAP_REVERSE==TRUE){
      RNM4=NM4[order(1:nrow(NM4)),]
    }else{
      RNM4=NM4[order(nrow(NM4):1),]
    }
    GLOB_RNM4$i<-RNM4
    #print(head(RNM4))
    #print("head(RNM4---------1111111)")
  })##TRK_T00
  output$UI_T00 <- renderUI({
    plotOutput("TRK_T00", height = 1,width="100%")
  })
#JIESHU:T00
#KAISHI:T01
  output$TRK_T01<- renderPlot({ ## 2D heatmap
    if(input$CHK_T01==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    if(is.null(GLOB_RNM4$i)) return(NULL)
    RNM4<-GLOB_RNM4$i
    #NM4a<-NM4 #[1:input$YRATIO_T01,]
    #print(head(RNM4))
    #print("head(RNM4)")
    #print(nrow(NM4a))
    #RNM4<-ifelse(input$HEATMAP_REVERSE==TRUE,(NM4[order(nrow(NM4):1),]),(NM4[order(1:nrow(NM4)),]))
    #print(RNM4)
    
    #colors <- colorRampPalette(c("white", "green", "yellow", "red"))(length(unique(NM$z)))
    #colors <- colorRampPalette(c("white", "green", "yellow", "red"))(125)
    #colors = colorRampPalette(c("white","green","green4","violet","purple"))(256)
    #colors = colorRampPalette(c("yellow","red"))(256)
    #print("@@@@@")
    HEAT_LOW=input$heat_low
    HEAT_MID=input$heat_mid
    
    HEAT_HIGH=input$heat_high
    HEAT_MINPOINT=input$heat_midpoint
    
    breaksList = seq( input$HEATMAP_COLMAX[[1]], input$HEATMAP_COLMAX[[2]], by =0.1)
    
    # colors = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList))
    color_na=HEAT_LOW
    colors = colorRampPalette(c(HEAT_MID,HEAT_HIGH))(length(breaksList))
    P2D4<-pheatmap(RNM4,
                   legend = FALSE,
                   cluster_rows=FALSE,
                   cluster_cols=FALSE,
                   border_color = FALSE,#"gray",
                   treeheight_row = input$TH_M01, 
                   treeheight_col = 0,
                   annotation_legend = FALSE,
                   annotation_names_row = FALSE,
                   # color =colors,
                   na_col = color_na,
                   show_rownames=FALSE,show_colnames = FALSE,
                   color=colors,
                   # color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), 
                   breaks = breaksList,
                   silent = TRUE)
    
    #HEAT_MINPOINT=median(NM$sz)
    #  head(NM)
    
    #print(paste(min(NM$x),max(NM$x),min(NM$y),max(NM$y),sep="\t"))
    #print("min(NM$x),max(NM$x),min(NM$y),max(NM$y)")
    # NM$sz<-scale(NM$z)
    #print(median(NM$sz))
    #print(">>> median<<<")
    #P2D<-ggplot(NM,aes(x,y,fill = sz))+
    # geom_tile()+
    #  theme_void()+
    #  theme(legend.position = "none",
    #        panel.grid = element_blank(),
    #        axis.text= element_blank(),
    #        axis.ticks = element_blank(),
    #plot.margin =unit(c(t=5, r=0, b=5, l=-30), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    #        plot.margin =unit(c(t=5, r=0, b=5, l=input$LA_T01), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    
    #       axis.title=element_blank(),
    #  )+
    # scale_fill_gradient2(low=HEAT_LOW,mid=HEAT_MID,high=HEAT_HIGH,midpoint = HEAT_MINPOINT)+
    # scale_fill_manual(values=colors)+
    #coord_cartesian(ylim=c(0,ncol(DFM)*input$YRATIO_T01),xlim=c(1,max(NM$x)))
    
    #print(head(DF2D))
    
    #print(nrow(DF2D)) 
    
    
    updateNumericInput(session,"HT_T01",value=400)
    # updateNumericInput(session,"WD_T01",value=103.5)
    #print("===before plot heatmap")
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    #grid.arrange(P2D)
    grid.arrange(P2D4[[4]])
    
    #print("===after plot heatmap")
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    
    # P2D
  })
  ## <<<<<<<<<<<<< T
  output$UI_T01 <- renderUI({
    plotOutput("TRK_T01", height = input$HT_T01+RDM$i,width=paste0(input$WD_T01,"%"))
  })
#JIESHU:T01

  ### =========================
#KAISHI:LP01  
  ## >>>>>>>>>>>>> T
  output$TRK_LP01<- renderPlot({ #BINLOOP
    if(input$CHK_LP01==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    updateCheckboxInput(session,"CHK_T01",value= "YES")
    
    
    DATATSNE <- TSNEDATA$i
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    DF2D <- FDF2D$i
    #print(head(DF2D))
    #print("head(DF2D)")
    
    #print(nrow(DF2D)) 
    FILTER_DF2D<-DF2D%>%
      dplyr::filter(z>=input$LOOP_PET[[1]],z<=input$LOOP_PET[[2]]) %>% 
      dplyr::filter(y-x>=input$LOOP_LEN[[1]],y-x<=input$LOOP_LEN[[2]])
    
    
    #################
    
    if(FLT_EXPR_LOOP$i !=""  ){#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*())  
      BINLIST=FLT_EXPR_LOOP$i
      #print(BINLIST)
      #print("BINLIST--LOOP")
      
      DF_BINLIST=data.frame(unique(unlist(strsplit(BINLIST, ","))))
      colnames(DF_BINLIST)<-c("BIN_SELECTED")
      ODR_DF_BINLIST<-data.frame(DF_BINLIST[order(as.numeric(DF_BINLIST$BIN_SELECTED)),])
      colnames(ODR_DF_BINLIST)<-c("BIN_SELECTED")
      UNISTR_BINLIST<-paste(ODR_DF_BINLIST$BIN_SELECTED,collapse = ",")
      #print(head(UNISTR_BINLIST,2))
      #print("UNISTR_BINLIST--LOOP")
      AAAA=ODR_DF_BINLIST$BIN_SELECTED
      #print(head(AAAA,2))
      #print("A--LOOP")
      
      
      updateTextInput(session,"LOC_LOOP",value=UNISTR_BINLIST)
      
      
      # FLTMTD="AND"
      FLTMTD=input$LP_FLT_METHOD
      if(FLTMTD=="OR"){
        NEW_ODR_DF_BINLIST<-FILTER_DF2D%>%
          dplyr::filter(x==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[1])|y==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[1]))
        if(nrow(ODR_DF_BINLIST)>1){
          for(i in 2:nrow(ODR_DF_BINLIST)){
            #print(ODR_DF_BINLIST$BIN_SELECTED[i])
            #print("ODR_DF_BINLIST$BIN_SELECTED[i]")
            TEMP01<-FILTER_DF2D%>%
              dplyr::filter(x==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[i]))
            NEW_ODR_DF_BINLIST<-rbind(NEW_ODR_DF_BINLIST,TEMP01)
          }
        }
        #print(nrow(FILTER_DF2D))
        #print("nrow(FILTER_DF2D)")
        #print(nrow(unique(FILTER_DF2D)))
        #print("unique(FILTER_DF2D)")
      }else if (FLTMTD=="AND"){
        
        for(i in 1:nrow(ODR_DF_BINLIST)){
          for(j in 1:nrow(ODR_DF_BINLIST)){
            if(i!=j){
              TEMP01<-FILTER_DF2D%>%
                dplyr::filter(x==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[i]),y==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[j]))
              if(i==1 & j==2){
                NEW_ODR_DF_BINLIST<-TEMP01
                
              }else{
                NEW_ODR_DF_BINLIST<-rbind(NEW_ODR_DF_BINLIST,TEMP01)
                
              }
              TEMP02<-FILTER_DF2D%>%
                dplyr::filter(y==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[i]),x==as.numeric(ODR_DF_BINLIST$BIN_SELECTED[j]))
              NEW_ODR_DF_BINLIST<-rbind(NEW_ODR_DF_BINLIST,TEMP02)
            }
          }
        }
        
      }
      FILTER_DF2D<-unique(NEW_ODR_DF_BINLIST)
      
      
    } ##############
    
    
    
    #print(head(FILTER_DF2D))
    #print("head(FILTER_DF2D)")
    DFLP<-data.frame(matrix(0,ncol=4,nrow=0),stringsAsFactors = FALSE)
    colnames(DFLP)<-c("A","B","ID","SCORE")
    
    for(i in 1:nrow(FILTER_DF2D)){
      
      X=FILTER_DF2D[i,1]
      Y=FILTER_DF2D[i,2]
      M=X+(Y-X)/2
      Z=FILTER_DF2D[i,3]
      if(X != Y ){
        ID=paste(factor(X),factor(Y),sep="_")
        newline1=data.frame(X,0,ID,Z)
        newline2=data.frame(M,Z,ID,Z)
        newline3=data.frame(Y,0,ID,Z)
        colnames(newline1)<-c("A","B","ID","SCORE")
        colnames(newline2)<-c("A","B","ID","SCORE")
        colnames(newline3)<-c("A","B","ID","SCORE")
        DFLP<-rbind(DFLP,newline1)
        DFLP<-rbind(DFLP,newline2)
        DFLP<-rbind(DFLP,newline3)
      }
    }
    #print(head(DFLP))
    #print("head(DFLP)")
    anchorX=seq(0,max(DFLP$A),1)
    anchorY= rep(0, each=length(anchorX))
    BOX<-data.frame(anchorX,anchorY)
    
    #print(head(DFLP))
    #print("head(DFLP)")
    #print(input$LOOP_PET)
    #print(input$LOOP_PET[[2]])
    #print(input$LOOP_LEN)
    #print(input$LOOP_LEN[[1]])
    
    
    
    PLP<-ggplot(DFLP,aes(A,B*2,color = ID,group=ID))+
      # geom_bspline(color = 'steelblue')+
      # geom_bspline2(color = 'darkred')+
      #geom_point()+
      # geom_line()+
      geom_bspline()+
      #  geom_point(data=BOX,aes(x=anchorX,y=anchorY),color="gray",shape=15)+
      # scale_color_gradient()+
      #  scale_color_manual(values=colors)+
      theme_bw()+
      theme(legend.position="none",
            panel.grid = element_blank(), 
            panel.background = element_rect(fill="white"),
            panel.border = element_rect(size=0.5,color="gray"),
            axis.title= element_blank(), 
            plot.margin =unit(c(t=5, r=3, b=5, l=3), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
            axis.text.y= element_blank(), 
            axis.ticks.y=element_blank(),
            
            plot.background = element_blank())+
      scale_x_continuous(breaks=seq(1,GLOB_BINNUM$i,1))+
      coord_cartesian(xlim=c(0.5,GLOB_BINNUM$i+0.5), expand = FALSE)
    if(input$TXT_LP01=="Yes"){ 
      PLP<-PLP+   geom_text(data=filter(DFLP,B>0),aes(A,B,label=B),vjust=1.2,hjust=0.6,color="black")
      
    }
    # PLP
    if(input$YTK_LP01=="Yes"){ 
      PLP2<-ggplot(DFLP,aes(A,2*B,color = ID,group=ID))+
        geom_bspline(alpha=0)+
        
        theme_bw()+
        theme(legend.position="none",
              panel.grid.minor = element_blank(), 
              panel.grid.major.x  = element_blank(), 
              panel.background = element_blank(),
              panel.border =  element_blank(),
              axis.title= element_blank(), 
              plot.margin =unit(c(t=0, r=0, b=0, l=0), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
              axis.text.x = element_blank(), 
              axis.ticks.x =element_blank(),
              axis.text.y = element_text(color="red"), 
              axis.ticks.y =element_line(color="red"),
              plot.background = element_blank()
        )+
        scale_x_continuous(breaks=seq(1,GLOB_BINNUM$i,1))+
        scale_y_continuous( breaks = pretty_breaks())+
        
        coord_cartesian(xlim=c(0.5,GLOB_BINNUM$i+0.5), expand = FALSE)
      
      
      
      PLP<- PLP+annotation_custom(
        grob = ggplotGrob(PLP2),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) 
    }# if(input$YTK_LP01="YES"){ 
    
    updateNumericInput(session,"HT_LP01",value=200)
    # updateNumericInput(session,"WD_T01",value=103.5)
    grid.arrange(PLP,top=paste(input$MAINFIN,paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax),paste0("(",nrow(DFM),")")))
    # P2D
  })
  output$UI_LP01 <- renderUI({
    plotOutput("TRK_LP01", height = input$HT_LP01,width=paste0(input$WD_LP01,"%"))
  })
#JIESHU:LP01
  ## <<<<<<<<<<<<< LP
  ################################
  
  
#KAISHI:R01  
  ## >>>>>>>>>>>>> R
  GLOB_R01=reactiveValues(i=FALSE,j=FALSE)
  ### =========================
  output$TRK_R01 <- renderPlot({
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    if(input$CHK_R01==FALSE){
      return(NULL)
    }
    
    set.seed(as.integer(input$SEED))
  #  set.seed(as.integer(input$SEED))
    
    
    
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    # set.seed(as.integer(input$SEED))
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    #print("before ddi------")
    #print(head(DFM,2))
    #print("head(DFM,2)")
    
    DDM=input$ddmethod
    if(DDM=="TSNE"){
      
    
      
      TSNE <- Rtsne(DFM,retx=TRUE, check_duplicates=FALSE,perplexity =input$PERP,initial_dims = input$BINNUM_M01,
                    theta=input$TSNE_TH,exaggeration_factor=input$TSNE_EF,eta=input$TSNE_ET,momentum=input$TSNE_MO,final_momentum=input$TSNE_FM,
                    pca=input$TSNE_PC,partial_pca=input$TSNE_PP,max_iter=input$TSNE_MI)
      # TSNE <- Rtsne(DFM,retx=TRUE, check_duplicates=FALSE)
      
      
      DATATSNE = as.data.frame(TSNE$Y)
      colnames(DATATSNE)<-c("V1","V2")  
      TITLE=DDM
     
    }else if (DDM == "PCA"){
      PCA  <- prcomp(DFM, retx=TRUE, scale. =FALSE) # scaled pca [exclude species col]
      
      #  pca
      #  scores <- pca$x[,1:4] x
      #                    PC1         PC2        PC3        PC4
      # Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
      #  Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
      DATATSNE = data.frame(PCA$x[,input$PC[[1]]:input$PC[[2]]])
      colnames(DATATSNE)<-c("V1","V2")   
      TITLE=paste0(DDM,": ",input$PC[[1]],"-",input$PC[[2]])
      
    }else if (DDM == "UMAP"){
      UMAP <- umap(DFM,n_neighbors = input$NNEIG,
                   metric=input$UMAPM,
                   nn_method=input$UMAPNN,
                   init=input$UMAPI,
                   n_components=input$UMAPNC,
                   )
      
      
      #> head(embedding)
      #Sepal.Length Sepal.Width Petal.Length Petal.Width    UMAP1    UMAP2
      #1          5.1         3.5          1.4         0.2 12.24735 9.599084
      #2          4.9         3.0          1.4         0.2 10.64287 9.093225
      # DATATSNE <- UMAP%>%dplyr::select(UMAP1,UMAP2)
      DATATSNE = as.data.frame(UMAP)
      colnames(DATATSNE)<-c("V1","V2")
      TITLE=DDM
    }else if (DDM == "MDS"){
      dist_cereals_num <- dist(DFM, method = input$MDSM)
      mds_cereals_num <- cmdscale(dist_cereals_num,eig = TRUE, k = 2)
      #  #print(head(mds_cereals_num$point))
      # #print('head(mds_cereals_num$point)')
      
      DATATSNE <- data.frame(mds_cereals_num$points[, 1],mds_cereals_num$points[, 2])
      colnames(DATATSNE)<-c("V1","V2")
      TITLE=DDM
    }else if (DDM == "ICA"){
      ica_cereals_num <- icafast(DFM, nc=2,center=input$ICAC,maxit=input$ICAM)
      #  center = TRUE, maxit = 100, tol = 1e-6
      
      #print(head(ica_cereals_num$Y))
      #print('head(ica_cereals_num$Y)')
      DATATSNE <- data.frame(ica_cereals_num$Y[, 1],ica_cereals_num$Y[, 2])
      colnames(DATATSNE)<-c("V1","V2")
      TITLE=DDM
    }else if (DDM == "RUTA"){
        #reticulate::use_condaenv("r-tensorflow")
      x <- as.matrix(DFM)
     # library(ruta)
     # library(purrr)
      ruta_cereals_num <- autoencode(x, 2, type = input$RUTAT,   activation = input$RUTAA, epochs = input$RUTAE)
   #  ruta_cereals_num <- ruta::autoencode(scale(x), 2, type = input$RUTAT,   activation = input$RUTAA, epochs = input$RUTAE)
     #print(head(ruta_cereals_num))
      #print('head(ruta_cereals_num)')
      DATATSNE <- data.frame(ruta_cereals_num[, 1],ruta_cereals_num[, 2])
      colnames(DATATSNE)<-c("V1","V2")
      TITLE=DDM
    }else if (DDM == "PHATE"){
       # install.packages("phateR")
    #  devtools::install_github("KrishnaswamyLab/phateR", ref="dev",force = TRUE)
      
      library(phateR)
 #     data(tree.data)
   #   tree.phate <- phate(tree.data$data)
  #    plot(tree.phate, col = tree.data$branches)
      x <- as.matrix(DFM)
      print(x[1:3,1:5]);print("x[1:3,1:5]")
      print(dim(x));print("dim(x)")
      if (input$PHATET==0){
        PHT="auto"
      }else{
        PHT=input$PHATET
      }
    #  reticulate::conda_remove("r-reticulate")
    #  reticulate::py_discover_config(required_module = "phate")
     # phate_cereals_num <- phate(data = scale(x),ndim = 2,gamma=input$PHATEG,t=PHT)
     # phate_cereals_num <- phate(data = scale(x),ndim = 2,gamma=1,t="auto")
     tree.phate <- phate(x,gamma=input$PHATEG,t=PHT)
      # plot(tree.phate)
      phate_cereals_num <-tree.phate
      #print(head(phate_cereals_num))
      #print('head(phate_cereals_num)')
      DATATSNE <- data.frame(phate_cereals_num$embedding[, 1],phate_cereals_num$embedding[, 2])
      colnames(DATATSNE)<-c("V1","V2")
      TITLE=DDM
    }
    
    
    TSNEDATA$i <-DATATSNE
    
    MINV<-min(DATATSNE$V1,DATATSNE$V2)
    MAXV<-max(DATATSNE$V1,DATATSNE$V2)

    pt2<-ggplot(DATATSNE,aes(V1,V2))+
      geom_point(size=1,shape=19,alpha=0.7) +
      guides(colour=guide_legend(override.aes=list(size=6))) +
      # ggtitle(name) +
      theme_bw(base_size=12)+
      scale_color_manual(values = GLOB_PVAL$i)+
      theme(axis.text=element_blank(),
            panel.grid = element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.position = "right",
            #legend.direction="horizontal",
            legend.title = element_blank(),
      )  
    
    pt<- ggplot(DATATSNE, aes_string(x="V1", y="V2")) +
      geom_point(size=1,shape=1,alpha=0.7) +
      guides(colour=guide_legend(override.aes=list(size=6))) +
      ggtitle(TITLE) +
      theme_bw(base_size=12)+
      theme(axis.text=element_blank(),
            panel.grid = element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.position = input$LGD_R04
      )  
    #coord_fixed()
    
    updateNumericInput(session,"HT_R01",value=input$HT_R01)
    # updateNumericInput(session,"WD_R01",value=100)
    # grid.arrange(FTSNEPNG$i)
    GLOB_R01$i<-pt2
    GLOB_R01$j<-DDM
    grid.arrange(pt2)
  })##END:TRK_R01
  output$UI_R01 <- renderUI({
    plotOutput("TRK_R01", height = input$HT_R01,width=paste0(input$WD_R01,"%"))
  })
#JIESHU:R01 
  #JIESHU:R01 
  ###############  
  ####~~~~~~~
  
  output$TRK_SIL <- renderPlot({
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    if(input$CHK_R01==FALSE){
      return(NULL)
    }
    
    

    set.seed(as.integer(input$SEED))
    
    pt <- FTSNEPNG$i
    DFM <- FILTERDATA$i
    AIN2 <- PUB_AIN2$i 
    AIN3 <- PUB_AIN3$i 
    # set.seed(as.integer(input$SEED))
    #print(format(Sys.time(), "%a %b %d %X %Y"))
    #print("before ddi------")
    #print(head(DFM,2))
    #print("head(DFM,2)")

    DFSIL=data.frame(matrix(ncol=4,nrow=0))
  colnames(DFSIL)<-c("dd","clu","clu_num","sil_score")
      #  DDM=input$ddmethod
  TIME=format(Sys.time(), "%Y%m%d-%H%M%S")
  
  F_SIL_NAME=paste(RESIZE$chr,RESIZE$xmin,RESIZE$xmin,TIME,"silhouette","score","tsv",sep=".")
  header=paste("dd","clu","clu_num","sil_score",sep="\t")
  write.table(header, F_SIL_NAME, row.names = FALSE,quote=FALSE,sep="\t",append = TRUE,col.names=FALSE)
    #  for (DDM in c("TSNE","UMAP","MDS","ICA","PCA")){ 
      for (DDM in c("UMAP")){
    
              if(DDM=="TSNE"){
                TSNE <- Rtsne(DFM,retx=TRUE, check_duplicates=FALSE,perplexity =input$PERP,initial_dims = input$BINNUM_M01,
                              theta=input$TSNE_TH,exaggeration_factor=input$TSNE_EF,eta=input$TSNE_ET,momentum=input$TSNE_MO,final_momentum=input$TSNE_FM,
                              pca=input$TSNE_PC,partial_pca=input$TSNE_PP,max_iter=input$TSNE_MI)
                # TSNE <- Rtsne(DFM,retx=TRUE, check_duplicates=FALSE)
             
                
                DATATSNE = as.data.frame(TSNE$Y)
                colnames(DATATSNE)<-c("V1","V2")  
                TITLE=DDM
                
              }else if (DDM == "PCA"){
                PCA  <- prcomp(DFM, retx=TRUE, scale. =FALSE) # scaled pca [exclude species col]
                
                #  pca
                #  scores <- pca$x[,1:4] x
                #                    PC1         PC2        PC3        PC4
                # Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
                #  Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
                DATATSNE = data.frame(PCA$x[,input$PC[[1]]:input$PC[[2]]])
                colnames(DATATSNE)<-c("V1","V2")   
                TITLE=paste0(DDM,": ",input$PC[[1]],"-",input$PC[[2]])
                
              }else if (DDM == "UMAP"){
                UMAP <- umap(DFM,n_neighbors = input$NNEIG,
                             metric=input$UMAPM,
                             nn_method=input$UMAPNN,
                             init=input$UMAPI,
                             n_components=input$UMAPNC,
                )
                
                
                #> head(embedding)
                #Sepal.Length Sepal.Width Petal.Length Petal.Width    UMAP1    UMAP2
                #1          5.1         3.5          1.4         0.2 12.24735 9.599084
                #2          4.9         3.0          1.4         0.2 10.64287 9.093225
                # DATATSNE <- UMAP%>%dplyr::select(UMAP1,UMAP2)
                DATATSNE = as.data.frame(UMAP)
                colnames(DATATSNE)<-c("V1","V2")
                TITLE=DDM
              }else if (DDM == "MDS"){
                dist_cereals_num <- dist(DFM, method = input$MDSM)
                mds_cereals_num <- cmdscale(dist_cereals_num,eig = TRUE, k = 2)
                #  #print(head(mds_cereals_num$point))
                # #print('head(mds_cereals_num$point)')
                
                DATATSNE <- data.frame(mds_cereals_num$points[, 1],mds_cereals_num$points[, 2])
                colnames(DATATSNE)<-c("V1","V2")
                TITLE=DDM
              }else if (DDM == "ICA"){
                ica_cereals_num <- icafast(DFM, nc=2,center=input$ICAC,maxit=input$ICAM)
                #  center = TRUE, maxit = 100, tol = 1e-6
                
                #print(head(ica_cereals_num$Y))
                #print('head(ica_cereals_num$Y)')
                DATATSNE <- data.frame(ica_cereals_num$Y[, 1],ica_cereals_num$Y[, 2])
                colnames(DATATSNE)<-c("V1","V2")
                TITLE=DDM
              }else if (DDM == "RUTA"){
                #reticulate::use_condaenv("r-tensorflow")
                x <- as.matrix(DFM)
                library(ruta)
                library(purrr)
                #   ruta_cereals_num <- autoencode((x), 2)
                ruta_cereals_num <- ruta::autoencode(scale(x), 2, type = input$RUTAT,   activation = input$RUTAA, epochs = input$RUTAE)
                #print(head(ruta_cereals_num))
                #print('head(ruta_cereals_num)')
                DATATSNE <- data.frame(ruta_cereals_num[, 1],ruta_cereals_num[, 2])
                colnames(DATATSNE)<-c("V1","V2")
                TITLE=DDM
              }else if (DDM == "PHATE"){
                #  install.packages("phateR")
                #  devtools::install_github("KrishnaswamyLab/phateR", ref="dev",force = TRUE)
                
               # library(phateR)
                x <-as.matrix(DFM)
                print(head(x));print("PHATE")
                if (input$PHATET==0){
                  PHT="auto"
                }else{
                  PHT=input$PHATET
                }
                #  reticulate::conda_remove("r-reticulate")
                #  reticulate::py_discover_config(required_module = "phate")
                # phate_cereals_num <- phate(data = scale(x),ndim = 2,gamma=input$PHATEG,t=PHT)
                so.system("np.all(np.isfinite(mat))")
                phate_cereals_num <- phate(data = scale(x),ndim = 2,gamma=1,t="auto")
                
                
                #print(head(phate_cereals_num))
                #print('head(phate_cereals_num)')
                DATATSNE <- data.frame(phate_cereals_num$embedding[, 1],phate_cereals_num$embedding[, 2])
                colnames(DATATSNE)<-c("V1","V2")
                TITLE=DDM
              }
      tsne_data=DATATSNE
      cluster_num=input$CLUNUM_M04 # 4
      for (CLU in c("hkmeans")){
       # for (CLU in c("gaumix","hdensity","density","hkmeans","kmeans","hpca","fuzzy")){
         # for (cluster_num in seq(3,6))  {
            
                     filterdata=FILTERDATA$i
          
                    if(CLU == "gaumix"){   
                      print("gaumix--start")
                      GL=input$glow
                      GH=input$ghigh
                      mc <- Mclust(tsne_data,  G = GL:GH,  modelNames=input$GAUMIX_MN)
                     # mc <- Mclust(tsne_data)
                   #   print(str(mc));   print("str(mc)")
                      cl=mc$classification
                      colourCount = length(unique(cl))
                      filterdata$cluster_list<-as.integer(cl)
                     print(filterdata$cluster_list); print("filterdata$cluster_list")
                      
                      ##>>>>>>>>>>>>>>>>
                  
                      ##>>>>>>>>>>>>>>>>
                      #print("gaumix--end")
                    }else if (CLU == "hkmeans"){
                      cl<-hkmeans(tsne_data, k=cluster_num)
                      colourCount = length(unique(cl$cluster))
                      filterdata$cluster_list<-cl$cluster
                      ##>>>>>>>>>>>>>>>>

                      ##>>>>>>>>>>>>>>>>
                    }else if (CLU == "kmeans"){
                      cl<-kmeans(tsne_data, cluster_num, iter.max=500)
                      colourCount = length(unique(cl$cluster))
                      filterdata$cluster_list<-cl$cluster

                      ##>>>>>>>>>>>>>>>>
                    }else if(CLU == "hpca"){ 
                      
                      fit_cluster_pca <- PCA(tsne_data, ncp=cluster_num,graph=FALSE)
                      fit_cluster_hcpc<-HCPC(fit_cluster_pca,graph=FALSE,nb.clust = cluster_num)
                      cl<-fit_cluster_hcpc$data.clust$clust
                      colourCount = length(unique(fit_cluster_hcpc$data.clust$clust))
                      filterdata$cluster_list<-as.integer(fit_cluster_hcpc$data.clust$clust)

                      ##>>>>>>>>>>>>>>>>
                      #name=paste0("hpca ","n=",cluster_num)
                    }else  if(CLU == "fuzzy"){  
                     # fit_cluster_fanny<-fanny(tsne_data,k=cluster_num,cluster.only=TRUE,metric=input$FUNNY_METRIX,maxit = 500)
                      fit_cluster_fanny<-fanny(tsne_data,k=cluster_num,cluster.only=TRUE,metric=input$FUNNY_METRIX)
                      
                      cl=fit_cluster_fanny$clustering
                      colourCount = length(unique(cl))
                      filterdata$cluster_list<-as.integer(cl)

                      ##>>>>>>>>>>>>>>>>
                    }else if(CLU == "hdensity"){
                  #    cl<-hdbscan(tsne_data, minPts = 50)
                      cl<-dbscan::hdbscan(tsne_data, minPts = input$MINPTS_M01H)
                      
                      colourCount = length(unique(cl$cluster))
                      
                      filterdata$cluster_list<-cl$cluster
                      # name=paste0("hdensity ","n=",cluster_num)
                      CLU_PAR_01=paste0("minPts=",input$MINPTS_M01)
                      ##>>>>>>>>>>>>>>>>
                   #   print(filterdata$cluster_list); print("filterdata$cluster_list")
                      
                      ##>>>>>>>>>>>>>>>>
                    }else if(CLU == "density"){
                      cl<-dbscan::dbscan(tsne_data, eps = input$EPS_M01, minPts = input$MINPTS_M01)
                      
                     # cl<-dbscan(tsne_data,minPts =5,eps=0.5)
                      colourCount = length(unique(cl$cluster))
                      filterdata$cluster_list<-cl$cluster
                      CLU_PAR_01=paste0("minPts=",input$MINPTS_M01)
                      CLU_PAR_02=paste0("eps=",input$EPS_M01)
                    #  print(filterdata$cluster_list); print("filterdata$cluster_list")
                      ##>>>>>>>>>>>>>>>>
                  
                    }
                     print("^^^^^^^^^^^^^^^^^^")
                  #   print(paste(DDM,CLU,cluster_num,colourCount));  print("paste(DDM,CLU,cluster_num,colourCount)");
                     
                    print(unique(filterdata$cluster_list));print("unique(filterdata$cluster_list)")
                    # #print(filterdata$cluster_list);#print("filterdata$cluster_list");#print(length(filterdata$cluster_list))
                    #  #print(head(tsne_data)) ;#print("head(tsne_data)");#print(nrow(tsne_data))
                    df_cluster_list<-data.frame("cluster"=filterdata$cluster_list)
                    # #print(head(df_cluster_list)) ;#print("head(df_cluster_list)");#print(nrow(df_cluster_list));#print(nrow(df_cluster_list))
                    df_tsne_data<-as.data.frame(tsne_data)
                    #  #print(head(df_tsne_data)) ;#print("head(df_tsne_data)");#print(nrow(df_tsne_data))
                    df_clustered_tsne_data<-cbind(df_tsne_data,df_cluster_list)
                    print(head(df_clustered_tsne_data)) ;print("head(df_clustered_tsne_data)");print(nrow(df_clustered_tsne_data))
                    colnames(df_clustered_tsne_data)<-c("x","y","cluster")
                    library(cluster)
                    library(tibble)
                    sample_df<-as_tibble(df_clustered_tsne_data)
                    #####t#########
                    #print(sample_df);    #print("sample_df")
                    print(head(sample_df)); print("head(sample_df)")
                    if (colourCount==1){
                      mean_silhouette_score=-1
                    }else{
                      
                   
                        silhouette_score <- silhouette(as.integer(sample_df$cluster), dist(select(sample_df,x,y)))
                        print(str(silhouette_score)); print("str(silhouette_score)")
                        
                        mean_silhouette_score=mean(silhouette_score[,3])
                    }
                    
                    if (CLU %in% c("hkmeans","kmeans","hpca","fuzzy")){
                      print(paste(DDM,CLU,cluster_num,mean_silhouette_score)); print("paste(DDM,cluster_num,mean_silhouette_score)")
                      DFTMP=data.frame("dd"=DDM,"clu"=CLU,"clu_num"=cluster_num,"sil_score"=mean_silhouette_score)
                      
                      newline=paste(DDM,CLU,cluster_num,mean_silhouette_score,sep="\t")
                      
                    }
                    if (CLU %in% c("gaumix","hdensity","density","hkmeans","kmeans","hpca","fuzzy")){
                      print(paste(DDM,CLU,colourCount,mean_silhouette_score)); print("paste(DDM,colourCount,mean_silhouette_score)")
                      newline=paste(DDM,CLU,colourCount,mean_silhouette_score,sep="\t")
                      DFTMP=data.frame("dd"=DDM,"clu"=CLU,"clu_num"=colourCount,"sil_score"=mean_silhouette_score)
                      
                    }
                    
                    
                    
                    
                    #  RESIZE <- reactiveValues(xmin = 20100000, xmax = 20400000, gnm="dm3",chr="chrX",left=0,right=22422827,gene="",strand="+",prom=0,probe=0)
                  
                    write.table(newline, F_SIL_NAME, row.names = FALSE,quote=FALSE,sep="\t",append = TRUE,col.names=FALSE)
                    DFSIL<-rbind(DFSIL,DFTMP)
                    Sys.sleep(2)
                    #"dd","clu","clu_num","sil_score"
        #  }## for (cluster_num in seq(2,9))

      }## for (CLU in c("
    }## for (*DDM in c("TSNE","UMAP","MDS","ICA","PCA")){

 #   grid.arrange(arrangeGrob(tableGrob(GLOB_INFO03$i),tableGrob(GLOB_INFO03$j),nrow=1))
 # grid.arrange(tableGrob(DFSIL),nrow=1)
  
  colnames(DFSIL)<-c("dd","clu","clu_num","sil_score")
  DFSIL%>%mutate(silhouette=na_if(sil_score,-1))->DFSIL
  
 # colnames(DFSIL)<-c("dd","clu","clu_num","sil_score")
 # DFSIL$sil_value<-as.numeric(DFSIL)
  

  DFSIL$DD<-factor(DFSIL$dd,levels=c("TSNE","UMAP","MDS","ICA","PCA"))
  DFSIL$CLUSTER<-factor(DFSIL$clu,levels=c("gaumix","hdensity","density","hkmeans","kmeans","hpca","fuzzy"))
  
  PLOT_SIL<-ggplot(DFSIL, aes(x=DD, y=CLUSTER)) +
    geom_tile(aes(fill = silhouette),size=0.2,color="black") +
    scale_fill_distiller(palette = "RdYlGn",direction = -1,  na.value = "grey30",
                         limits = c(-1,1) ) +
    geom_text(aes(label=round(silhouette,3)))+
    labs(title = "silhouette score",
         x="Dimensionality_reduction",
         y = "Clustering_method")+
    theme_bw()+
    theme(panel.grid = element_blank())

  
  PLOT_DD<-ggplot(DFSIL,aes(DD,y=silhouette,color=DD))+
    #  scale_y_log10()+
    geom_boxplot()+
    geom_jitter()+
    stat_compare_means()+
    labs(#title = IN,
      x="Dimensionality reduction algorithm",
      y = "mean silhouette score")+
   # scale_y_continuous(limits=c(0.1,0.7))+
    # coord_cartesian(ylim=c(0.1,0.8))+
    theme_bw()+
    theme(axis.text = element_text(angle=0),panel.grid = element_blank(),
          legend.position = "none")
  PLOT_CLU<-ggplot(DFSIL,aes(CLUSTER,y=silhouette,color=CLUSTER))+
    #  scale_y_log10()+
    geom_boxplot()+  
    geom_jitter()+
    stat_compare_means()+
    labs(#title = IN,
      x="Clustering algorithm",
      y = "mean silhouette score")+
  #  scale_y_continuous(limits=c(0.1,0.6))+
    # coord_cartesian(ylim=c(0.1,0.8))+
    theme_bw()+
    theme(axis.text = element_text(angle=0),panel.grid = element_blank(),
          legend.position = "none")
  
  
  
  grid.arrange(PLOT_SIL,PLOT_DD,PLOT_CLU,nrow=1,top=F_SIL_NAME)
  })##END:TRK_SIL
  output$UI_SIL <- renderUI({
    plotOutput("TRK_SIL", height = input$HT_R01,width=paste0(input$WD_R01,"%"))
  })
  #JIESHU:SIL
  ###############
  #KAISHI:C04
  GLOB_2D_hitColors=reactiveValues(i=NULL)
  GLOB_2D_WIN01=reactiveValues(i=NULL)
  
  GLOB_NEW2D_WIN01<-reactiveValues(i=NULL)
  
  output$TRK_C04<- renderPlot({
    if(input$CHK_R01==FALSE){
      return(NULL)
    }
    if(input$CHK_D04==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    
    sort_data<-M04_PARA$i
    clean_sort_data<- M04_PARA$clean
    colourCount<-M04_PARA$colornum
    col=colorRampPalette(brewer.pal(8, "Dark2"))(colourCount)
    #   col=colorRampPalette(c( "brown4","green","navy", "magenta", "cyan", "red","blue","darkslategray"))(colourCount)
    #  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    #  ann_colors = list(Clusterlass =getPalette(colourCount))[1]           
    
    
    
    AIN2<-PUB_AIN2$i
    AIN3<-PUB_AIN3$i
    
    add_sort_data<-sort_data
    add_sort_data$GEMNUMID<-rownames(add_sort_data)
    #print("2033>>>");#print(head(add_sort_data));#print("<<<2033")
    
    
    DF <- data.frame(matrix(0, ncol = 3, nrow = 0),stringsAsFactors = FALSE)
    for (i in 1:nrow(add_sort_data)){
      ROW_CLUSTER_ORDER=i
      ROW_CLUSTER_NAME=add_sort_data[i,ncol(clean_sort_data)+1]
      ROW_CLUSTER_GEMNUMID=add_sort_data[i,ncol(clean_sort_data)+2]
      DF[ROW_CLUSTER_ORDER,] <- list(ROW_CLUSTER_ORDER,ROW_CLUSTER_NAME,ROW_CLUSTER_GEMNUMID)
    }
    colnames(DF)<-c("related_plot_line_num","CLUSTERID","GEMNUMID")
    MIN<-merge(AIN2,DF,by = "GEMNUMID")
    WIN01=MIN
    WIN01<-transform(WIN01, related_plot_line_num = as.numeric(related_plot_line_num))
    
    #PN2
    #LH=1
    #FTP="butt"
    #FH=2
    
    FTP=input$SHAPE_C04
    LH=input$LSIZE_C04
    FH=input$FSIZE_C04
    
    RS=RESIZE$xmin
    RE=RESIZE$xmax
    GLOB_2D_WIN01$i<-WIN01
    
   # #print(head(WIN01))
  #  #print("head(WIN01)---1982")
    if(input$M04_HIT_COL=="color"){
      hitColors<- GLOB_PVAL$i
    }else if(input$M04_HIT_COL=="mono"){
      #hitColors<- colorRampPalette(c("black"))(colourCount)
      hitColors<-colorRampPalette(input$MONO_COLOR_NAME)(100)
      #hitColors<- "black"
    }
    GLOB_2D_hitColors$i<-hitColors
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("GEMCLU",input$MAINFIN,RESIZE$gene,RESIZE$chr,RESIZE$xmin,RESIZE$xmax, "csv", sep = ".")
      },
      content = function(file) {
         write.csv(DF, file, row.names = FALSE)
        write.table(DF, file, row.names = FALSE,quote=FALSE,sep=",")
        write.table(WIN01, file, row.names = FALSE,quote=FALSE,sep=",")
      }
    )
    
    if(input$CSL_C04==0){
      WIN01<-WIN01
    }else{
      WIN01<-WIN01%>%dplyr::filter(CLUSTERID==input$CSL_C04)
    }
    
   
    #WIN01<-CFWIN01
    GLOB_NEW2D_WIN01$i<-WIN01
    #print(as.character(input$CSL_C04[1]))
    #   grid.arrange(arrangeGrob(arrangeGrob(tableGrob(GLOB_INFO01$i),tableGrob(GLOB_INFO03$i),GLOB_CLU_PLOT$i,nrow=1),ncol=1))
    grid.arrange(GLOB_CLU_PLOT$i)
    #KAISHI:DATA4
    output$TRK_DATA4 <- renderPlot({
      grid.arrange(arrangeGrob(tableGrob(GLOB_INFO03$i),tableGrob(GLOB_INFO03$j),nrow=1))
    })
    if(input$LOC_HDLD=="LD"){
    CLUSTER_LIST=unlist(unique(GLOB_2D_DFM$i$cluster_list))
    #print(str(CLUSTER_LIST))
    observe({
      # if(input$selectall == 0) return(NULL) 
      #  else 
      if (input$DATA_SLCT_ALL%%2==1)
      {
        updateCheckboxGroupInput(session,"DATA_SLCT",choices= CLUSTER_LIST,inline=TRUE)
      }
      else
      {
        updateCheckboxGroupInput(session,"DATA_SLCT",choices= CLUSTER_LIST,selected=CLUSTER_LIST,inline=TRUE)
      }
    })
    }
    updateNumericInput(session,"HT_C04",value=input$HT_C04)
  })## END:TRK_C04
  output$UI_C04 <- renderUI({ 
    plotOutput("TRK_C04", height = input$HT_C04,width=paste0(input$WD_C04,"%"))   })    
  #JIESHU:C04
  output$UI_DATA4 <- renderUI({
    plotOutput("TRK_DATA4", height =input$HT_LAST)
  })
  #JIESHU:DATA4
  ################################
#KAISHI:M04
 # GLOB_SIL=reactiveValues(i=NULL)
  GLOB_CLU_PLOT=reactiveValues(i=NULL)
  GLOB_INFO03=reactiveValues(i=NULL,j=NULL)
  M04_PARA =reactiveValues(i=FALSE,clean=FALSE,colornum=0)
  GLOB_COL=reactiveValues(i=NULL)
  output$TRK_M04 <- renderPlot({
    if( input$CHK_M04==FALSE ) return(NULL)
    if(input$CHK_R01==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    filterdata=FILTERDATA$i
    tsne_data=TSNEDATA$i
    cluster_num=input$CLUNUM_M04
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #########################
    ##(2) hc-km (hierarchical-cluster--kmeans)
    #  library(factoextra)
    #fun_hk = function(data,tsne_data,name,cluster_num=6)
    #{
    CLU_PAR_01=""
    CLU_PAR_02=""
    GL=input$glow
    GH=input$ghigh
    if(input$DDC_SELECT == "gaumix"){   
      #print("gaumix--start")
      mc <- Mclust(tsne_data,  G = GL:GH,  modelNames=input$GAUMIX_MN)
      cl=mc$classification
      colourCount = length(unique(cl))
      cluster_num=colourCount
      filterdata$cluster_list<-factor(cl)
      
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        paste0("G=",GL,":",GH),
        paste0("modelNames=",input$GAUMIX_MN),
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
      #print("gaumix--end")
    }else if (input$DDC_SELECT == "hkmeans"){
      cl<-hkmeans(tsne_data, k=cluster_num)
      colourCount = length(unique(cl$cluster))
      filterdata$cluster_list<-cl$cluster
      cluster_num=colourCount
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
    }else if (input$DDC_SELECT == "kmeans"){
      cl<-kmeans(tsne_data, cluster_num, iter.max=500)
      colourCount = length(unique(cl$cluster))
      filterdata$cluster_list<-cl$cluster
      cluster_num=colourCount
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
    }else if(input$DDC_SELECT == "hpca"){ 
      
      fit_cluster_pca <- PCA(tsne_data, ncp=cluster_num,graph=FALSE)
      fit_cluster_hcpc<-HCPC(fit_cluster_pca,graph=FALSE,nb.clust = cluster_num)
      cl<-fit_cluster_hcpc$data.clust$clust
      colourCount = length(unique(fit_cluster_hcpc$data.clust$clust))
      #cluster_num=colourCount
      filterdata$cluster_list<-as.integer(fit_cluster_hcpc$data.clust$clust)
      cluster_num=colourCount
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
      #name=paste0("hpca ","n=",cluster_num)
    }else  if(input$DDC_SELECT == "fuzzy"){  
      fit_cluster_fanny<-fanny(tsne_data,k=cluster_num,cluster.only=TRUE,metric=input$FUNNY_METRIX)
      
      cl=fit_cluster_fanny$clustering
      colourCount = length(unique(cl))
      cluster_num=colourCount
      filterdata$cluster_list<-as.integer(cl)
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        paste0("metric=",input$FUNNY_METRIX),
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
    }else if(input$DDC_SELECT == "hdensity"){
      cl<-dbscan::hdbscan(tsne_data, minPts = input$MINPTS_M01H)
      colourCount = length(unique(cl$cluster))
      #   cluster_num=colourCount)
      cluster_num=colourCount
      filterdata$cluster_list<-cl$cluster
      # name=paste0("hdensity ","n=",cluster_num)
      CLU_PAR_01=paste0("minPts=",input$MINPTS_M01)
      ##>>>>>>>>>>>>>>>>
      cluname=data.frame(infomation=c((
        input$DDC_SELECT),
        paste0("minPts=",input$MINPTS_M01H),
        paste0("clu num=",cluster_num)   
      ))
      ##>>>>>>>>>>>>>>>>
    }else if(input$DDC_SELECT == "density"){
      
      cl<-dbscan::dbscan(tsne_data, eps = input$EPS_M01, minPts = input$MINPTS_M01)
      colourCount = length(unique(cl$cluster))
      # cluster_num=colourCount
      filterdata$cluster_list<-cl$cluster
      CLU_PAR_01=paste0("minPts=",input$MINPTS_M01)
      CLU_PAR_02=paste0("eps=",input$EPS_M01)
      cluster_num=colourCount
    #print(input$DDC_SELECT )
    ##>>>>>>>>>>>>>>>>
    cluname=data.frame(infomation=c((
      input$DDC_SELECT),
      paste0("minPts=",input$MINPTS_M01),
      paste0("eps=",input$EPS_M01),
      
      paste0("clu num=",cluster_num)   
    ))
    }

   # #print(filterdata$cluster_list);#print("filterdata$cluster_list");#print(length(filterdata$cluster_list))
  #  #print(head(tsne_data)) ;#print("head(tsne_data)");#print(nrow(tsne_data))
    df_cluster_list<-data.frame("cluster"=filterdata$cluster_list)
   # #print(head(df_cluster_list)) ;#print("head(df_cluster_list)");#print(nrow(df_cluster_list));#print(nrow(df_cluster_list))
    df_tsne_data<-as.data.frame(tsne_data)
  #  #print(head(df_tsne_data)) ;#print("head(df_tsne_data)");#print(nrow(df_tsne_data))
    df_clustered_tsne_data<-cbind(df_tsne_data,df_cluster_list)
   # #print(head(df_clustered_tsne_data)) ;#print("head(df_clustered_tsne_data)");#print(nrow(df_clustered_tsne_data))
    colnames(df_clustered_tsne_data)<-c("x","y","cluster")
     library(cluster)
    library(tibble)
    sample_df<-as_tibble(df_clustered_tsne_data)
    #####t#########
    #print(sample_df);    #print("sample_df")
    #print(str(sample_df)); #print("str(sample_df)")
    silhouette_score <- silhouette(sample_df$cluster, dist(select(sample_df,x,y)))
    #print(str(silhouette_score)); #print("str(silhouette_score)")

   mean_silhouette_score=mean(silhouette_score[,3])
    #print(mean_silhouette_score);#print("mean_silhouette_score")
    ############################
 #   gg_kmeans <- ggplot(sample_df, aes(x,y, color = silhouette_score[,3], shape = factor(cluster))) +
#      geom_point(size=3) +
#      theme_bw() +
#      scale_colour_viridis(name = "sil.width")+
#      scale_shape_discrete(name = "cluster")
#    #print(gg_kmeans)
   silline= paste0("silhouette=",round(mean_silhouette_score,4))
   cluname<-rbind(cluname,silline)
   GLOB_INFO03$j<-cluname
   
   # GLOB_SIL$i=mean_silhouette_score
    ##>>>>>>>>>>>>>>>> INFOMATION ONLY
    if(input$ddmethod=="PCA"){
      # name=paste0(paste0(input$ddmethod,": ",input$PC[[1]],"-",input$PC[[2]])," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(input$ddmethod,
                                   paste0(input$PC[[1]],"-",input$PC[[2]])
      ))
 
    }else if (input$ddmethod=="TSNE"){
      # name=paste0(paste0(input$ddmethod,": ","perp","=",input$PERP)," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(input$ddmethod,
                                   paste0("initial_dims","=",input$BINNUM_M01),
                                   paste0("perplexity","=",input$PERP),
                                   paste0("theta","=",input$TSNE_TH),
                                   paste0("exaggeration_factor","=",input$TSNE_EF),
                                   paste0("eta","=",input$TSNE_ET),
                                   paste0("momentum","=",input$TSNE_MO),
                                   paste0("final_momentum","=",input$TSNE_FM),
                                   paste0("pca","=",input$TSNE_PC),
                                   paste0("partial_pca","=",input$TSNE_PP),
                                   paste0("max_iter","=",input$TSNE_MI)
      ))
  
    }else if (input$ddmethod=="UMAP"){
      #name=paste0(paste0(input$ddmethod,": ","n_neig","=",input$NNEIG)," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(input$ddmethod,
                                   paste0("n_neighbours","=",input$NNEIG),
                                   paste0("nn_method","=",input$UMAPNN), 
                                   paste0("metric","=",input$UMAPM),
                                   paste0("n_components","=",input$UMAPNC)
      ))

    }else if (input$ddmethod=="ICA"){
      #name=paste0(input$ddmethod,": ","center","=",input$ICAC," maxit=",input$ICAM," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(input$ddmethod,
                                   paste0("center","=",input$ICAC),
                                   paste0("maxit=",input$ICAM)
      ))
   
    }else if (input$ddmethod=="RUTA"){
      # name=paste0(input$ddmethod,": ","type","=",input$RUTAT," activation=",input$RUTAA," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(input$ddmethod,
                                   paste0("type","=",input$RUTAT),
                                   paste0("activation=",input$RUTAA),
                                   paste0("epochs=",input$RUTAE)
      ))

    }else if (input$ddmethod=="PHATE"){

      #    name=paste0(input$ddmethod,": ","gamma","=",input$PHATEG," t=",input$PHATET," ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(#input$ddmethod,
                                   paste0("gamma","=",input$PHATEG),
                                   paste0("t=",input$PHATET)
      ))
      cluname=data.frame(infomation=c((input$DDC_SELECT),
                                      paste0("clu num=",cluster_num)
      ))
    }else{
      # name=paste0(input$ddmethod,": ",input$DDC_SELECT," n=",cluster_num)
      name=data.frame(infomation=c(#input$ddmethod,
                                 #  paste0(input$DDC_SELECT," n=",cluster_num)
      ))

    }
    
    GLOB_INFO03$i<-name
    # getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    # getPalette = colorRampPalette(brewer.pal(12, "Paired"))
    # getPalette = colorRampPalette(brewer.pal(8, "Dark2"))
    #  getPalette = colorRampPalette(brewer.pal(12, "Set3"))
    #  getPalette = colorRampPalette(brewer.pal(9, "Pastel1"))
    #  getPalette = colorRampPalette(c("red","green","blue"))
    # getPalette = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))
    col=colorRampPalette(brewer.pal(8, "Dark2"))(colourCount)
    
    # col=getPalette(colourCount)
    #col=colorRampPalette(c( "brown4","green","navy", "magenta", "cyan", "red","blue","DarkKhaki","darkslategray"))(colourCount)
    #ann_colors = list(Clusterlass =getPalette(colourCount))[1]
    # ann_colors = list(Clusterlass =col)
    #print("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    
    #print(head(filterdata$cluster_list))
    #print("filterdatafilterdatafilterdatafilterdatafilterdata")
    sort_data<-filterdata[order(filterdata$cluster_list),]
    #print(tail(sort_data))
    #print("tail(sort_data)")
    last_accu=0
    CLU_CNT<-as.data.frame(table(sort_data$cluster_list))
    for (i in 1:nrow(CLU_CNT)){
      
      CLU_CNT$Accu[i]<-CLU_CNT$Freq[i]+last_accu
      last_accu= CLU_CNT$Accu[i]
      
      
    }
  #  #print(CLU_CNT) 
  #  #print("CLU_CNT") 
    clean_sort_data<-dplyr::select(sort_data,-cluster_list)
    clean_sort_data<-clean_sort_data*as.numeric(sort_data$cluster_list)
    #   annotation_row = data.frame(Clusterlass = sort_data$cluster_list)
    #  rownames(annotation_row) = row.names(sort_data)
    #    annoColors<-col
    #    names(annoColors) <- unique(annotation_row$Clusterlass)
    #    annoColors <- list(Clusterlass = annoColors)
    CC=c(brewer.pal(8, "Dark2"),brewer.pal(9, "Set1"),brewer.pal(12, "Set3"),brewer.pal(8, "Accent"),
         brewer.pal(9, "YlOrRd"),brewer.pal(9, "YlOrBr"),brewer.pal(9, "YlGnBu"),brewer.pal(9, "YlGn"),brewer.pal(9, "Reds"),
         brewer.pal(9, "RdPu"),brewer.pal(9, "Purples"),brewer.pal(9, "PuRd"),brewer.pal(9, "PuBuGn"),brewer.pal(9, "PuBu"),
         brewer.pal(9, "Oranges"),brewer.pal(9, "Greys"),brewer.pal(9, "Greens"),brewer.pal(9, "Blues"),brewer.pal(11, "BrBG"),
         brewer.pal(11, "RdGy"))
    #print(length(CC))
    #print("length(CC)")
    df0=data.frame(
      GROUP=factor(1:length(CC)),
      color.codes=CC )
    df<-data.frame(head(df0,as.numeric(colourCount)))
    pal = unique(df)
    pal$color.codes=as.character(pal$color.codes)
    pal$GROUP=as.character(pal$GROUP)
    pval = pal$color.codes
    names(pval) = pal$GROUP
    GLOB_PVAL$i<-pval
    ############  
    
    output$TRK_INFO_M04<- renderPlot({    
      
      DD_PAR_01=""
      if(input$ddmethod=="PCA"){
        DD_PAR_01=paste0( "PC",input$PC[[1]]," - PC",input$PC[[2]] )
      }else if (input$ddmethod=="TSNE"){
        DD_PAR_01=paste0("perplexity=",input$PERP)
      }
      
      INFO_M04=data.frame(ddcluster=c( paste0("ddmethod=",input$ddmethod),
                                       paste0("clistermethod=",input$DDC_SELECT),
                                       paste0("clusternum=",colourCount),
                                       DD_PAR_01,
                                       CLU_PAR_01,
                                       CLU_PAR_02,
                                       paste0("sil.score=",mean_silhouette_score)
                                       
      ))
      PI_M04<-tableGrob(INFO_M04)
    #  grid.arrange(PI_M04,nrow=1)
    })
    output$UI_INFO_M04 <- renderUI({
      plotOutput("TRK_INFO_M04", height =1)
    })
    ################ 
    MINX2<-min(tsne_data$V1,tsne_data$V2)
    MAXX2<-max(tsne_data$V1,tsne_data$V2)
    #print("2299>>>>"); #print(head(filterdata$cluster_list));#print("2299<<<")
    PT<-ggplot(tsne_data, aes_string(x="V1", y="V2", color=factor(filterdata$cluster_list))) +
      geom_point(size=1,shape=19,alpha=0.7) +
      guides(colour=guide_legend(override.aes=list(size=6))) +
      # ggtitle(name) +
      theme_bw(base_size=12)+
      coord_fixed(xlim=c(MINX2,MAXX2),ylim=c(MINX2,MAXX2))+
      scale_color_manual(values = GLOB_PVAL$i)+
      theme(axis.text=element_blank(),
            panel.grid = element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.position = input$LGD_R04,
            #legend.direction="horizontal",
            legend.title = element_blank(),
      )  
    
    PT2<-ggplot(tsne_data, aes_string(x="V1", y="V2", color=factor(filterdata$cluster_list))) +
      geom_point(size=1,shape=19,alpha=0.7) +
      guides(colour=guide_legend(override.aes=list(size=6))) +
      # ggtitle(name) +
      theme_bw(base_size=12)+
      scale_color_manual(values = GLOB_PVAL$i)+
      theme(axis.text=element_blank(),
            panel.grid = element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            legend.position = "right",
            #legend.direction="horizontal",
            legend.title = element_blank(),
      )  
    
   
    #coord_fixed()
    GLOB_2D_DFM$i<-sort_data
    GLOB_CLU_PLOT$i<-PT2
    #print(head(sort_data))
    #print("head(sort_data)")
    #print(tail(sort_data))
    #print("tail(sort_data)")
    #print(head(sort_data))
    #print("head(sort_data)")
    if(input$M04_HIT_COL=="color"){
      if(input$HTYPE=="binary")  {
        hitColors<-GLOB_PVAL$i
      }else if (input$HTYPE=="digital"){
        hitColors<-colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100)
      }
    }else if(input$M04_HIT_COL=="mono"){
      hitColors<-colorRampPalette(c(input$MONO_COLOR_NAME))(100)
      
    }
    GLOB_COL$i=hitColors
    if(input$GAP_M04%%2==0){
      GAP_ROW=NULL
    }else{
      GAP_ROW=CLU_CNT$Accu
    }         
    
    #output$DT_M04 = DT::renderDataTable({
    #   DT::datatable(
    #     tsne_data, extensions = 'Buttons', options = list(
    #       dom = 'Bfrtip',
    #buttons = c('copy', 'csv', 'excel', 'pdf', '#print')
    #      buttons = 
    #         list('copy', '#print', list(
    #           extend = 'collection',
    #           buttons = c('csv', 'excel', 'pdf'),
    #           text = 'Download'
    #         ))
    #     )
    #   )
    # })
    #print("clean_sort_data>>>>>>>>>>>>>");#print(head(clean_sort_data));#print("<<<<<<<<<<<<<<<<<clean_sort_data");#print(nrow(clean_sort_data))

    PH<-pheatmap(clean_sort_data,
                 legend = FALSE,
                 cluster_rows=FALSE,
                 cluster_cols=FALSE,
                 border_color = NA,
                 treeheight_row = 0, 
                 treeheight_col = 0,
                 annotation_legend = FALSE,
                 annotation_names_row = FALSE,
                 #  annotation_row=annotation_row,
                 #  annotation_colors =annoColors,
                 color=c(input$BC_M04,hitColors),
                 gaps_row=GAP_ROW,
                 # gaps_row=c(5,10,15,20,25,30,35,40,45,50, 55),
                 show_rownames=FALSE,show_colnames = FALSE,
                 silent = TRUE)
    M04_PARA$i<-sort_data
    M04_PARA$clean<-clean_sort_data
    M04_PARA$colornum<-colourCount
    updateNumericInput(session,"HT_M04",value=200)
  
    #print(str(PH))
    #print("str(PHEATMAP)")
    #KAISHI:R04
    output$TRK_R04<- renderPlot({
      if(input$CHK_R01==FALSE){
        return(NULL)
      }
      if(input$CHK_IPT == FALSE ){
        return(NULL)
      }
      if(input$CHK_CLU04 == FALSE ){
        return(NULL)
      }
  #    updateNumericInput(session,"HT_R01",value=300)
      
      grid.arrange(PH$gtable)
    }) ## END:TRK_R04
    updateNumericInput(session,"HT_R01",value=300)
  #  grid.arrange(PT)  

  #  output$TRK_INFO04<- renderPlot({
    #  updateNumericInput(session,"HT_INFO01",value=600)
      # grid.arrange(tableGrob(GLOB_INFO01$i),tableGrob(GLOB_INFO02$i),GLOB_CLU_PLOT$i,nrow=1)   
   # })
  }) ##EMD:TRK_M04
  
  
  output$UI_R04 <- renderUI({ plotOutput("TRK_R04", height = input$HT_M04+RDM$i,width=paste0(input$WD_M04,"%"))   })
#JIESHU:R04
  output$UI_M04 <- renderUI({ plotOutput("TRK_M04", height = 1,width=paste0(input$WD_R01,"%")) })   
#JIESHU:M04  
  
#  output$UI_INFO04 <- renderUI({ plotOutput("TRK_INFO04", height = input$HT_DM04-50,width=paste0(input$WD_DM04,"%"))   })   
  
  ################################################

  ###########################################

###############################################
#KAISHI:C04LP
  output$TRK_C04LP<- renderPlot({
    
    
    if(input$CHK_R01==FALSE){
      return(NULL)
    }
    if(input$CHK_D04==FALSE){
      return(NULL)
    }
    if(input$CHK_IPT == FALSE ){
      return(NULL)
    }
    
    if(input$CHK_C04LP == FALSE ){
      return(NULL)
    }
    updateCheckboxInput(session,"CHK_CLU04",value= "YES")
    
    
    if(input$M04_HIT_COL=="color"){
      hitColors<- GLOB_PVAL$i
    }else if(input$M04_HIT_COL=="mono"){
      #hitColors<- colorRampPalette(c("black"))(colourCount)
      hitColors<-colorRampPalette(input$MONO_COLOR_NAME)(100)
      #hitColors<- "black"
    }
    WIN01<-GLOB_NEW2D_WIN01$i
    #print("2088>>>")
    if (! is.null(CLU0$i)){
      #CLU0$i=input$COOR_LOC
      
      if(grepl(",", CLU0$i, fixed=TRUE)){
        CLUS=unlist(strsplit(CLU0$i, "[,]"))
        CFWIN01<-WIN01
        for(i in 1:length(CLUS)){
          CLU=CLUS[[i]]
          sig="AND"
          cn=as.integer(CLU)
          CFWIN01<-CFWIN01%>%dplyr::filter(CLUSTERID!=cn)
        }
        
        
      }else{
        CLU=CLU0$i
        sig="ONE"
        cn=as.integer(CLU)
        CFWIN01<-WIN01%>%dplyr::filter(CLUSTERID!=cn)
      }
      WIN01<-CFWIN01
    }##if (! is.null(CLU0$i)){
    
    #CFWIN01<-WIN01%>%dplyr::filter(CLUSTERID!=1)
    #print(head(CFWIN01))
    #print("<<<2088")
    
    RS=RESIZE$xmin
    RE=RESIZE$xmax
    PF<-ggplot(WIN01,aes(group=GEMNUMID,color=as.factor(CLUSTERID),x=freg_start,y=-related_plot_line_num))+
      scale_color_manual(values = hitColors)+
      # ggtitle("L0")+
      
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=input$FSIZE_C04LP,lineend = input$SHAPE_C04LP)+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=input$LSIZE_C04LP,alpha=0.4)+
      theme_bw()+
      theme(legend.position="none",panel.grid = element_blank(),
            panel.border = element_rect(size=0.2),
            #plot.title = element_blank(),
            axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax))
    #  coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))
    if(input$FACET%%2==1){
      PF<-PF+facet_wrap(factor(CLUSTERID)~.,scales="free_y",ncol=1)+theme( strip.background = element_rect(fill="gray85"))
    }
    PLOTC<- PF
    grid.arrange(PLOTC,top="TRK_C04LP")
  })##C04LPNEW
  output$UI_C04LP <- renderUI({ plotOutput("TRK_C04LP", height = input$HT_C04LP,width=paste0(input$WD_C04LP,"%"))   })    
#JIESHU:C04LP 
#############################################################  
#KAISHI:call_domain
  call_domain<-function(WIN01,CLU,hitColors,DJ=FALSE){
    WIN01<-WIN01%>%dplyr::filter(CLUSTERID==CLU)
    if (DJ==FALSE){
          dm_min=input$DOMAIN[[1]]
          dm_max=input$DOMAIN[[2]]
    }else{
          if (CLU==1){
            dm_min=input$DOMAIN01[[1]]
            dm_max=input$DOMAIN01[[2]]
          }else if (CLU==2){
            dm_min=input$DOMAIN02[[1]]
            dm_max=input$DOMAIN02[[2]]                        
          }else if (CLU==3){
            dm_min=input$DOMAIN03[[1]]
            dm_max=input$DOMAIN03[[2]]
          }else if (CLU==4){
            dm_min=input$DOMAIN04[[1]]
            dm_max=input$DOMAIN04[[2]]                       
          }else if (CLU==5){
            dm_min=input$DOMAIN05[[1]]
            dm_max=input$DOMAIN05[[2]]
          }else if (CLU==6){
            dm_min=input$DOMAIN06[[1]]
            dm_max=input$DOMAIN06[[2]]                        
          }else if (CLU==7){
            dm_min=input$DOMAIN07[[1]]
            dm_max=input$DOMAIN07[[2]]
          }else if (CLU==8){
            dm_min=input$DOMAIN08[[1]]
            dm_max=input$DOMAIN08[[2]]
          }else if (CLU==9){
            dm_min=input$DOMAIN09[[1]]
            dm_max=input$DOMAIN09[[2]]
          }else if (CLU==10){
            dm_min=input$DOMAIN10[[1]]
            dm_max=input$DOMAIN10[[2]]   
          }else if (CLU==11){
            dm_min=input$DOMAIN11[[1]]
            dm_max=input$DOMAIN11[[2]]   
          }else if (CLU==12){
            dm_min=input$DOMAIN12[[1]]
            dm_max=input$DOMAIN12[[2]]   
          }else if (CLU==13){
            dm_min=input$DOMAIN13[[1]]
            dm_max=input$DOMAIN13[[2]]   
          }else if (CLU==14){
            dm_min=input$DOMAIN14[[1]]
            dm_max=input$DOMAIN14[[2]]   
          }else if (CLU==15){
            dm_min=input$DOMAIN15[[1]]
            dm_max=input$DOMAIN15[[2]]   
          }else if (CLU==16){
            dm_min=input$DOMAIN16[[1]]
            dm_max=input$DOMAIN16[[2]]   
          }else if (CLU==17){
            dm_min=input$DOMAIN17[[1]]
            dm_max=input$DOMAIN17[[2]]   
          }else if (CLU==18){
            dm_min=input$DOMAIN18[[1]]
            dm_max=input$DOMAIN18[[2]]   
          }else if (CLU==19){
            dm_min=input$DOMAIN19[[1]]
            dm_max=input$DOMAIN19[[2]]   
          }else if (CLU==20){
            dm_min=input$DOMAIN20[[1]]
            dm_max=input$DOMAIN20[[2]]   
          }
    }
    #print(str(WIN01));#print("2667")
    #print(str(CLU));#print("2668")
    RS=RESIZE$xmin
    RE=RESIZE$xmax
    PF<-ggplot(WIN01,aes(group=GEMNUMID,color=as.factor(CLUSTERID),x=freg_start,y=-related_plot_line_num))+
      scale_color_manual(values = hitColors)+
      
      geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                   size=input$FSIZE_DMN,lineend = input$SHAPE_DMN)+
      geom_line(aes(x=freg_start,y=-related_plot_line_num),size=input$LSIZE_DMN,alpha=0.4)+
      theme_bw()+
      theme(legend.position="none",panel.grid = element_blank(),
            panel.border = element_rect(size=0.2),
            #plot.title = element_blank(),
            axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
      scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax))
    #  coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))
    if(input$FACET%%2==1){
      PF<-PF+facet_wrap(factor(CLUSTERID)~.,scales="free_y",ncol=1)+theme( strip.background = element_rect(fill="gray85"))
    }
    PLOTC<- PF
    
    #  if(2357==2441) {  
    #print(head(WIN01$related_plot_line_num));#print("2667")
    MAX_YPOS=max(unlist(WIN01$related_plot_line_num),na.rm = TRUE)
    #NN1<-NNN%>%filter(piece == 12) ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
    #print(nrow(WIN01))
   #print("3196----IN call_domain-------------------------------")
    d=data.frame(x=WIN01$freg_start,y=-WIN01$related_plot_line_num)
    kd <- ks::kde(d, compute.cont=TRUE);#print(kd);#print("kd2770")
      contour_60 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],  z=estimate, levels=cont[input$DMN_CUT])[[1]])
    
  #  contour_60 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],  z=estimate, levels=cont["40%"])[[1]])
    contour_60 <- data.frame(contour_60);#print(contour_60);#print("contour_60")
    PPP2<-ggplot(WIN01, aes(x = freg_start, y = -related_plot_line_num)) +
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      geom_density_2d(aes(color = after_stat(level)))+
      geom_path(aes(x, y), data=contour_60,color="blue",size=5) +
      scale_color_viridis_c()+
      # ggtitle("----------------------------L1")+
      # theme_void()+
      # theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())
      theme_bw()+
      theme(legend.position="none",
            panel.grid = element_blank(), 
            panel.background = element_blank(),
            panel.border =  element_rect(linetype=2,color="red"),
            axis.title= element_blank(), 
            plot.margin =unit(c(t=0, r=0, b=0, l=0), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
            axis.text = element_blank(), 
            axis.ticks =element_blank(),
            
            plot.background = element_blank()
      )+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax))

    PPP2
    BBB2<-ggplot_build(PPP2)
    
    unique(BBB2$data[[1]]$piece)
    NNN2<-BBB2$data[[1]]
    
    #  NN2<-NNN2%>%filter(nlevel>=input$SLD_D04_D[[1]],nlevel<=input$SLD_D04_D[[2]]) ## 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000
    
    # NN2<-NNN2%>%filter(nlevel>=4/8,nlevel<=4.5/8) ## 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000
    NN2<-NNN2%>%filter(nlevel>=dm_min,nlevel<=dm_max)
    BIGNLV=max(NN2$nlevel)
    NN2<-NN2%>%filter(nlevel==BIGNLV)
    #print(BIGNLV);#print("BIGNLV")
    
    if(4 %in% input$CKP_D04){
      PPP2<-ggplot(WIN01, aes(x = freg_start, y = -related_plot_line_num)) +
        scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
        geom_density_2d(aes(color = after_stat(level)))+
        geom_path(aes(x, y), data=NN2,color="red") +
    #    geom_path(aes(x, y), data=contour_60,color="blue",size=5) +
        
        scale_color_viridis_c()+
        # ggtitle("----------------------------L1")+
        # theme_void()+
        # theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())
        theme_bw()+
        theme(legend.position="none",
              panel.grid = element_blank(), 
              panel.background = element_blank(),
              panel.border =  element_rect(linetype=2,color="red"),
              axis.title= element_blank(), 
              plot.margin =unit(c(t=0, r=0, b=0, l=0), "pt"),  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
              axis.text = element_blank(), 
              axis.ticks =element_blank(),
              
              plot.background = element_blank()
        )+
        coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax))
    }
    
    require(dplyr)
    #  MM2<-  NN2 %>% group_by(group) %>% summarise(maxx = max(x),minx=min(x),maxy=max(y),miny=min(y))
    NN2$newgroup <- paste(NN2$group,"",sep="_")
    #print(head(NN2))
    MM2 <- NN2 %>%group_by(newgroup) %>% summarise(maxx = max(x),minx=min(x),maxy=max(y),miny=min(y), .groups = 'drop') 
    
    #print(data.frame(MM2))##### TAD
    #print("data.frame(MM2)")##### TAD
    
    DM_DATA<-data.frame(MM2)
    DM_DATA$cluster<-CLU
    DM_DATA$conter_range<-paste(dm_min,dm_max,sep="-")
    DM_DATA$nlevel=BIGNLV
    DM_DATA$raid<-paste0(RESIZE$chr,":",RESIZE$xmin,"-",RESIZE$xmax)
    
    #print(DM_DATA)##### TAD
    #print("DM_DATA")##### TAD
    
    
    MM2MAXX<-MM2%>%dplyr::select(-minx)
    MM2MINX<-MM2%>%dplyr::select(-maxx)
    colnames(MM2MAXX)<-c("newgroup","point","miny","maxy")
    colnames(MM2MINX)<-c("newgroup","point","miny","maxy")
    #  #print(data.frame(MM2MAXX))##### TAD
    #  #print("data.frame(MM2MAXX)--befor")##### TAD
    # #print(data.frame(MM2MINX))##### TAD
    #  #print("data.frame(MM2MINX)--befor")##### TAD
    
    MM22<-rbind(MM2MINX,MM2MAXX)
    MM22$NR<-1:nrow(MM22)

    MM22<-MM22%>%mutate(lvl=round(maxy+1.2*NR/((maxy-miny)/nrow(MM22))))
    
    
    
    # #print(data.frame(MM22))##### TAD
    #print("data.frame(MM22)--after")##### TAD
    PAD2<-ggplot(NNN2,aes(x,y,color=group,label=newgroup))+
      geom_path(data=NN2,aes(x,y,group=newgroup),size=2,alpha=0,color="black")+
      geom_rect(data=data.frame(MM2),aes(NULL,NULL,xmin=minx,xmax=maxx,ymin=-MAX_YPOS,ymax=0,group=newgroup),alpha=0,size=2,color="cyan")+
      geom_rect(data=data.frame(MM2),aes(NULL,NULL,xmin=minx,xmax=maxx,ymin=-MAX_YPOS,ymax=0,group=newgroup),alpha=0,size=1,color="blue")+
      geom_rect(data=data.frame(MM2),aes(NULL,NULL,xmin=minx,xmax=maxx,ymin=-MAX_YPOS,ymax=0,group=newgroup),alpha=0,size=0.6,color="red")+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      theme_void()+theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-MAX_YPOS,0))
    
    PAD3<-ggplot(NNN2,aes(x,y,color=group,label=newgroup))+
       geom_label(data=data.frame(MM22),aes(x=point,y=lvl*0.7,label=format(round(as.numeric(point), 1), nsmall=0, big.mark=",")), size=6,color="blue",alpha=0.8)+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      theme_void()+theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-MAX_YPOS,0))
    
   
    PAD5<-ggplot(NNN2,aes(x,y))+
      ggtitle(paste("ID:",CLU,"nlevel:",dm_min,"-",dm_max,"=",BIGNLV))+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      theme_void()+theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-MAX_YPOS,0))
    
    #################
    # kd <- ks::kde(WIN01, compute.cont=TRUE)
    #  #print(head(kd,10))
    PP1<-ggplot(WIN01, aes(x = freg_start, y = -related_plot_line_num,group=CLUSTERID)) +
      # geom_bin2d()+  
      #scale_color_manual(values = col)+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      stat_density_2d(aes(size=0.1,alpha=0.3),color="green")+ 
      
      # scale_color_viridis_c()+
      # stat_density_2d(aes(size=0.1,alpha=0.3,geom="polygon"),color="green")+ 
      # ggtitle("-------L3")+
      #geom_density_2d(color="black",size=0.2)+
      theme_void()+theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax))
    PP1
    BB1<-ggplot_build(PP1)
    #print(BB1)
    #print("BB1")
    unique(BB1$data[[1]]$nlevel)
    WWW<-BB1$data[[1]]%>%separate(group, c("group", "S","A"),sep ="-")
    WWW          
    #WW1<-WWW%>%filter(nlevel>=0.4,nlevel<=0.5) ## 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000
    
    
    WW1<-WWW%>%filter(nlevel>=input$SLD_D04_G[[1]],nlevel<=input$SLD_D04_G[[2]]) ## 0.125 0.250 0.375 0.500 0.625 0.750 0.875 1.000
    #print(head(WW1))
    WW1$newgroup <- paste(WW1$group,WW1$piece,sep="_")
    #print(head(WW1))
    UU1 <- WW1 %>%group_by(newgroup)  %>% summarise(maxx = max(x),minx=min(x),maxy=max(y),miny=min(y), .groups = 'drop') 
    #print(head(UU1))
    #head(data.frame(UU1))##### TAD
    PW<-ggplot(WWW,aes(x,y,color=group))+
      scale_color_manual(values = col)+
      # geom_path(data=WW1,aes(x,y,group=newgroup),size=1.2,alpha=0,color="blue")+
      geom_rect(data=data.frame(UU1),aes(NULL,NULL,xmin=minx,xmax=maxx,ymin=miny,ymax=maxy),alpha=0,size=0.6,color="blue")+
      # geom_text(data=data.frame(UU1),aes(x=minx,y=miny,label=group))+
      ggtitle("----------L4")+
      scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))+
      theme_void()+theme(legend.position = "none",axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title=element_blank())+
      coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-MAX_YPOS,0))
    
    
    ## >>>>>>> M
    
    if(1 %in% input$CKP_D04){
      PLOTC<- PLOTC+annotation_custom(
        grob = ggplotGrob(PPP2),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) 
     }
    if(2 %in% input$CKP_D04){
      PLOTC<- PLOTC+annotation_custom(
        grob = ggplotGrob(PAD2),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
      }
    
    if(3 %in% input$CKP_D04){
      PLOTC<- PLOTC+annotation_custom(
        grob = ggplotGrob(PAD3),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    }
    if(5 %in% input$CKP_D04){
      PLOTC<- PLOTC+annotation_custom(
        grob = ggplotGrob(PAD5),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    }
    
    

    
    newlist <- list(PLOTC,DM_DATA)
    return(newlist)
    
    # }#### delete
    
  }###call_domain
#JIESHU:call_domain
  
  ############################################
#KAISHI:DM04
  
    output$TRK_DM04<- renderPlot({
      if(input$CHK_R01==FALSE){
        return(NULL)
      }
      if(input$CHK_DM04==FALSE){
        return(NULL)
      }
      if(input$CHK_IPT == FALSE ){
        return(NULL)
      }
      updateCheckboxInput(session,"CHK_CLU04",value= "YES")
      updateCheckboxInput(session,"CHK_C04LP",value= "YES")
      WIN01<-GLOB_NEW2D_WIN01$i
      #print(str(WIN01));#print("<<<<<2863")
      if(input$M04_HIT_COL=="color"){
        hitColors<- GLOB_PVAL$i
      }else if(input$M04_HIT_COL=="mono"){
        #hitColors<- colorRampPalette(c("black"))(colourCount)
        hitColors<-colorRampPalette(input$MONO_COLOR_NAME)(100)
        #hitColors<- "black"
      }
    #print(unique(WIN01$CLUSTERID));#print("unique(WIN01$CLUSTERID)")
      DM_LIST=list()
      
      TOTAL_DOMAIN<-data.frame(matrix(nrow=0,ncol=5))

      for (x in 1:length(unique(WIN01$CLUSTERID))){
        #print(str(WIN01));#print("<<<<<2875");#print(x)
        DM_LIST[[x]]=call_domain(WIN01,x,hitColors,input$Detail_Adjust)[[1]]
        
        if(x==1){
          DF_DM<-call_domain(WIN01,x,hitColors,input$Detail_Adjust)[[2]]
        }else{
          DF_DM<-rbind(DF_DM,call_domain(WIN01,x,hitColors,input$Detail_Adjust)[[2]])
        }
      }
      #print(DF_DM);#print("DF_DM")
      RAID=unique(DF_DM$raid)
      #print("DF_DM>>>");#print(str(DF_DM));#print("<<<DF_DM")
      DF_MAXX<-data.frame(aggregate(DF_DM$maxx,by=list(DF_DM$cluster,DF_DM$nlevel),max))
      DF_MINX<-data.frame(aggregate(DF_DM$minx,by=list(DF_DM$cluster,DF_DM$nlevel),min))
      
      #print(DF_MAXX);#print("DF_MAXX")
      #print(DF_MINX);#print("DF_MINX")
      
      DM_TABLE<-merge(DF_MINX,DF_MAXX,by=c("Group.1","Group.2"))
      #print(DM_TABLE);#print("DM_TABLE")
      DOMAIN<-DM_TABLE%>%mutate(domain=RAID,cluster=Group.1,nlevel=Group.2,dmleft=floor(x.x),dmright=ceiling(x.y))%>%
        select(-x.x, -x.y,-Group.1,-Group.2)
      #print(DOMAIN);#print("DOMAIN")
      
      TOTAL_DOMAIN=rbind(TOTAL_DOMAIN,DOMAIN)
      #print(TOTAL_DOMAIN);#print("TOTAL_DOMAIN")
      
      output$download_domain <- downloadHandler(
        filename = function() {
          paste("DOMAIN",input$MAINFIN,RESIZE$gene,RESIZE$chr,RESIZE$xmin,RESIZE$xmax,"csv", sep = ".")
        },
        content = function(file) {
          # write.csv(DF, file, row.names = FALSE)
          #write.table(DF, file, row.names = FALSE,quote=FALSE,sep=",")
          write.table(DOMAIN, file, row.names = FALSE,quote=FALSE,sep=",")
        }
      )
      
      updateNumericInput(session,"HT_DM04",value=300)
      
      grid.newpage()
      g<-ggplotGrob(DM_LIST[[1]])
      
      
      
      
      for (q in 2:length(DM_LIST)){
        g2 <- ggplotGrob(DM_LIST[[q]])
        g <- rbind(g,g2)
      }
      
      
      
      grid.draw(g)
    })##output$TRK_DM04
    #print(head(GLOB_2D_WIN01$i))
    #print("head(GLOB_2D_WIN01$i)--2063")
    #print(head(GLOB_2D_DFM$i))
    #print("head(GLOB_2D_DFM$i)")

  
  ## <<<<<<<<<<<<< C
  
  
  
  output$UI_DM04 <- renderUI({ plotOutput("TRK_DM04", height = input$HT_DM04,width=paste0(input$WD_DM04,"%"))   })    
#JIESHU:DM04
  ###################################data_s2 <- sample_n(data, 3)
  ###################################
  
  GLOB_NEW_LIST<-reactiveValues(i=NULL)
  GLOB_CMB_GEMID<-reactiveValues(i=NULL)
  GLOB_CMB_LIST<-reactiveValues(i=NULL)
  #GLOB_A<-reactiveValues(i=NULL)
  PN=reactiveValues(i=NULL)
  GLOB_CLU_AIN2<-reactiveValues(i=NULL)
  GLOB_CLU_WIN01_SLCT<-reactiveValues(i=NULL)
  GLOB_CLU_DFM<-reactiveValues(i=NULL)
  
  
  #KAISHI:VENN
  output$TRK_VENN <- renderPlot({
    if(input$AND01==0) return(NULL)
    if(is.null(input$DATA_SLCT)) return(NULL)
    #print("TRK_VENN:3104")
    #print(input$AND01)
    #  DATATSNE <- TSNEDATA$i
    # pt <- FTSNEPNG$i
    #print(GLOB_A$i);#print("befor TRK_VENN:3100")
    #  AIN2 <- PUB_AIN2$i 
    # AIN3 <- PUB_AIN3$i 

   ###############################################
    #print(str(input$DATA_SLCT))
    CLUSTER_LIST=input$DATA_SLCT
    #############################################
 if(input$LOC_HDLD=="HD"){## START IF HD
    OLD_WIN01= GLOB_NEW_WIN01_F01$i
 }else if(input$LOC_HDLD=="LD"){ ## START IF LD
    OLD_WIN01=GLOB_2D_WIN01$i
 }### END IF LD
    #print(head(OLD_WIN01,4))
    #print("-----")
    NEW_WIN01=NULL
    ######################################################
   #
  if(input$LOC_HDLD=="HD"){## START IF HD
    #print(NEW_DFM_CLU$i)
    OLD_DFM=NEW_DFM_CLU$i
  }else if(input$LOC_HDLD=="LD"){ ## START IF LD
    OLD_DFM=GLOB_2D_DFM$i
  }### END IF LD 
    OLD_DFM$GEMNUMID<-row.names(OLD_DFM)
    #print(head(OLD_DFM,3))
    #print("-------")
    ######################################################3
    
    NEW_DFM=NULL
    for ( u in 1:length(CLUSTER_LIST)){
      NUMCLU=as.integer(CLUSTER_LIST[[u]])
      THIS_WIN01<-OLD_WIN01%>%dplyr::filter(CLUSTERID==NUMCLU)
      if (is.null(NEW_WIN01)){
        
        NEW_WIN01<-THIS_WIN01
        #print(head(NEW_WIN01))
        #print("head(NEW_WIN01) --- 1st")
      }else{
        NEW_WIN01<-rbind(NEW_WIN01,THIS_WIN01)
      }
    }
    for ( u in 1:length(CLUSTER_LIST)){
      NUMCLU=as.integer(CLUSTER_LIST[[u]])
      THIS_DFM<-OLD_DFM%>%dplyr::filter(cluster_list==NUMCLU)
      if (is.null(NEW_DFM)){
        NEW_DFM<-THIS_DFM
        #print(head(NEW_DFM))
        #print("head(NEW_DFM) --- 1st")
      }else{
        NEW_DFM<-rbind(NEW_DFM,THIS_DFM)
      }
      
    }#for
    rownames(NEW_DFM)<-NEW_DFM$GEMNUMID
    NEW_DFM<-NEW_DFM%>%dplyr::select(-GEMNUMID)
    #print(tail(NEW_DFM,2))
    #print("tail(NEW_DFM)--2937--VENN")
    #print(tail(NEW_WIN01,2))
    #print("tail(NEW_WIN01)--2939--VENN")
    #DFM <- FILTERDATA$i
    #print("2758")
    DFM<-NEW_DFM%>%select(-cluster_list)
    #AIN2 <- PUB_AIN2$i 
    AIN2 <-NEW_WIN01%>%select(-related_plot_line_num, -CLUSTERID)
    #print(tail(DFM,2))
    #print("tail(DFM)--2946--VENN")
    GLOB_CLU_DFM$i<-DFM
    GLOB_CLU_AIN2$i<-AIN2
    GLOB_CLU_WIN01_SLCT$i<-NEW_WIN01
    
   if(FLT_EXPR$i =="" ){#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*()) IF:NO_BIN_INPUT
       updateSelectInput(session,"VENN_SLCT",choices= "none",selected="none")
     }else{ ###IF:HV_BIN_INPUT
   #   BINLIST=FLT_EXPR$i
  #    DF_BINLIST=data.frame(unique(unlist(strsplit(BINLIST, ","))))
   #   colnames(DF_BINLIST)<-c("BIN_SELECTED")
   #   ODR_DF_BINLIST<-data.frame(DF_BINLIST[order(as.numeric(DF_BINLIST$BIN_SELECTED)),])
   #   colnames(ODR_DF_BINLIST)<-c("BIN_SELECTED")
  ##    UNISTR_BINLIST<-paste(ODR_DF_BINLIST$BIN_SELECTED,collapse = ",")
   #   #print(head(ODR_DF_BINLIST,2)); #print("ODR_DF_BINLIST:3168")
  #    A=ODR_DF_BINLIST$BIN_SELECTED
      #print((A))
      #print("A--3583")
      #move to 257
       # GLOB_A$i=A
    #  updateTextInput(session,"LOC01",value=UNISTR_BINLIST)
      A=GLOB_A$i
     # DA<-(data.frame(A));#print(str(DA$A[2]));#print("DA^^^^^^")
    #  indx <- sapply(DA, is.factor)
      #DA[indx] <- lapply(DA[indx], function(x) as.numeric(as.character(x)))
      #print(DA);#print("DA================")
      #print(A);#print("A:3204")
      CMB_LIST=list()
      CMB_GEMID=list()
      for (i in 1:length(A)){
        BINZUHE=combn(A,i)
        #print(paste(">>>>>>>>>>>>>> START:i=",i));#print(BINZUHE);#print("BINZUHE:3208");#print(i)
        for (j in 1:ncol(BINZUHE)){
          BINS=data.frame((BINZUHE[,j]))
          colnames(BINS)<-"BINS"
          #print(paste("~~~~~~~~~~~ START:j=",j));#print(BINS);#print("BINS:3212")
          CMB_NAME=(paste(BINS$BINS,collapse = ","));#print(CMB_NAME);#print("<--CMB_NAME")
          CMB_LIST <- append(CMB_LIST, CMB_NAME);#print(CMB_LIST);#print("<--CMB_LIST")
          CMB_GEMID<- append(CMB_GEMID, CMB_NAME);#print(CMB_GEMID);#print("<--CMB_GEMID")
          #print("CMB_LISTVVVVVVVVVVVV")
          LAST=length(CMB_LIST)
          #print(CMB_LIST[[LAST]])
          #print(CMB_GEMID[[LAST]])
          #print("CMB_LIST^^^^^^^^^^^^")
          DFM_GEMID<-data.frame(row.names(DFM))
          colnames(DFM_GEMID)<-"GEMNUMID"
          #print(head(DFM_GEMID))
          for (r in 1:nrow(BINS)){
            #print("***")
            #print(r);#print("***")
            SELECTED_BIN_C<-BINS[r,1][1]
            DB<-(data.frame(BINS));#print(str(DA$A[2]));#print("DA^^^^^^")
            indx <- sapply(DB, is.factor)
            DB[indx] <- lapply(DB[indx], function(x) as.numeric(as.character(x)));#print(DB);#print("DB")
            SELECTED_BIN_N<-as.integer(DB$BINS[r]);#SELECTED_BIN_N<-as.integer(SELECTED_BIN_C)
            #print(SELECTED_BIN_C);#print("SELECTED_BIN_C")
            #print(SELECTED_BIN_N);   #print("SELECTED_BIN_N")
            #print((DFM[1:3,1:5]));#print("3231")
            THIS<-DFM[DFM[,SELECTED_BIN_N]>0,]
            #print(nrow(THIS));#print("THIS 3233")
            THIS_GEMID<-data.frame(row.names(THIS))
            colnames(THIS_GEMID)<-"GEMNUMID"
            
           # #print(DFM_GEMID$GEMNUMID) ; #print("DFM_GEMID$GEMNUMID")
          #  #print(THIS_GEMID$GEMNUMID);#print("THIS_GEMID$GEMNUMID")
           # #print(ncol(THIS))
          #  #print(head(row.names(THIS)))
            #print("3241")
            TEMP<-NULL
            TEMP <-data.frame(intersect(DFM_GEMID$GEMNUMID,THIS_GEMID$GEMNUMID))
            DFM_GEMID<-NULL
            DFM_GEMID<-TEMP
            colnames(DFM_GEMID)<-"GEMNUMID"
            #print("<<<<<<<<< 3249")
            #print(CMB_NAME);#print("CMB_NAME")
            #print(SELECTED_BIN_C);#print("SELECTED_BIN_C")
            #print(head(DFM_GEMID));#print("head(DFM_GEMID)")
            #print(nrow(DFM_GEMID));#print("nrow(DFM_GEMID)")
            
            #print(">>>>>>>>>> 3255")
            
          }#for (r
          CMB_GEMID[[LAST]]<-DFM_GEMID
          #  #print(length(CMB_LIST))
          #   #print(length(CMB_GEMID))
          
          #print("============= END:j")
        }#for (j
        #print("<<<<<<<<<<<<<<<<< END:i")
      }#for (i
      
      NEW_CMB_GEMID<-CMB_GEMID
      #print(str(NEW_CMB_GEMID))
      #print("NEW_CMB_GEMID 3268 VENN")
      if(input$INEXCLUDE=="exclude"){
        for (m in 1:length(CMB_LIST)){
          # M_KEY=unlist(strsplit(CMB_LIST[[m]], ","))
          #  LEN_M_KEY=length(M_KEY)
          #print("mmmmmmmmmmmmm")
          #print(head(CMB_LIST[[m]]))
          #print(LEN_M_KEY)
          #print("nnnnnnnnnnnnn")
          for (n in 1:length(CMB_LIST)){
            #   N_KEY=unlist(strsplit(CMB_LIST[[n]], ","))
            #   LEN_N_KEY=length(N_KEY)             
            if(m != n){
              #print(head(CMB_LIST[[n]]))
              #print(LEN_N_KEY)
              
              #print("YESYESYES")
              M_GEMID_LIST=NEW_CMB_GEMID[[m]]
              N_GEMID_LIST=NEW_CMB_GEMID[[n]]
              INTERSECT_MN<-data.frame(setdiff(M_GEMID_LIST$GEMNUMID,N_GEMID_LIST$GEMNUMID))
              colnames(INTERSECT_MN)<-"GEMNUMID"
              #  #print(head(M_GEMID_LIST))
              #  #print(head(N_GEMID_LIST))
              #  #print(head(INTERSECT_MN))
              #  #print(nrow(M_GEMID_LIST))
              #  #print(nrow(N_GEMID_LIST))
              #  #print(nrow(INTERSECT_MN))
              NEW_CMB_GEMID[[m]]<-INTERSECT_MN
              
              
              #print("-------")
              
              
            }
            
            
            
            
            #else
          }#for(n)
          #print("//////////")
        }#for (m)
      }## exclude
      ALL_GEM_NUM_NOW=0
      for (w in 1:length(NEW_CMB_GEMID)){
        # #print(NEW_CMB_GEMID[[w]]$GEMNUMID)
        GEM_COUNT=length(NEW_CMB_GEMID[[w]]$GEMNUMID)
        # #print(GEM_COUNT)
        ALL_GEM_NUM_NOW= ALL_GEM_NUM_NOW+GEM_COUNT
      }
      #print(head(ALL_GEM_NUM_NOW))
      #print("ALL_GEM_NUM_NOW--3319")
      CUR_CMB_GEMID=NEW_CMB_GEMID
      #print(head(CUR_CMB_GEMID))
      #print("CUR_CMB_GEMID--3322")
      NON_GEMID<-data.frame(row.names(DFM))
      colnames(NON_GEMID)<-"GEMNUMID"
      
      NEW_LIST=list()
      #print(DFM[1:3,1:5])
      #print("head(DFM)-3328")
      GID_DFM<-DFM%>%mutate("GEMNUMID"=row.names(DFM))
      #print(GID_DFM[1:4,1:5]);#print(str(GID_DFM))
      #print("head(GID_DFM)3331")
      
      
      for (k in 1:length(CUR_CMB_GEMID)){
        #print("OUTvvvvvvvvvvvv3335")
        
        #print(k)
        #print(CMB_LIST[[k]])
        #print(head(CMB_GEMID[[k]]))
        #print("OUT^^^^^^3340")
        #print(k)
        
        FLT_DFM<-merge(GID_DFM,CUR_CMB_GEMID[[k]],by="GEMNUMID",drop=TRUE)
        #print(head(FLT_DFM,2))
        rownames(FLT_DFM)<-FLT_DFM$GEMNUMID
        FLT_DFM<-FLT_DFM%>%dplyr::select(-GEMNUMID)
        NEW_LIST[[k]]<-FLT_DFM
        #print(nrow(FLT_DFM))
        #print(head(FLT_DFM,2))
        NON_GEMID<-data.frame(setdiff(NON_GEMID$GEMNUMID,CUR_CMB_GEMID[[k]]$GEMNUMID))
        colnames(NON_GEMID)<-"GEMNUMID"
        #print("OUT^^^^^^^^^^^^^^^^")
        
      }#for:k
      
      NON_DFM<-merge(GID_DFM,NON_GEMID,by="GEMNUMID",drop=TRUE)
      rownames(NON_DFM)<-NON_DFM$GEMNUMID
      NON_DFM<-NON_DFM%>%dplyr::select(-GEMNUMID)
      ALL_GEM_NUM_NOW<-ALL_GEM_NUM_NOW+nrow(NON_DFM)
      
      
      #print("xxxxxxxxxxxx")
      #print(nrow(DFM))
      #print(nrow(NON_DFM))
      #print("zzzzzzzzzzzz")
      #print(head(DFM,2))
      #print("DFM--3100--VENN")
      NEW_LIST[[length(NEW_LIST)+1]] <- NON_DFM
      CMB_GEMID[[length(CMB_GEMID)+1]]<-NON_GEMID
      CMB_LIST[[length(CMB_LIST)+1]]<-"none"
      # #print((NEW_LIST))
      
      
      # par(mfrow=c(2,3))
      
      GLOB_NEW_LIST$i<-NEW_LIST
      GLOB_CMB_GEMID$i<-CMB_GEMID
      GLOB_CMB_LIST$i<-CMB_LIST
      #print(GLOB_NEW_LIST$i)
      #print(GLOB_CMB_GEMID$i)
      #print(GLOB_CMB_LIST$i)
      #print("VENN--FIN--3120")
      updateSelectInput(session,"VENN_SLCT",choices= c("all",CMB_LIST))
      #print(PLOT_LIST)
      #print(FRAG_PLOT)
      #print(CMB_LIST)
      
      
    }#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*()) IF:HV_BIN_INPUT
  })##JIESHU:TRK_VENN
  output$UI_VENN <- renderUI({
    plotOutput("TRK_VENN", height = 1,width="100%")
  })
  #KAISHI:VENN3
  output$TRK_VENN3 <- renderPlot({
    if(is.null(input$DATA_SLCT)) return(NULL)
    if(input$AND01==0) return(NULL)
    if(is.null(FLT_EXPR$i)) return(NULL)
    #print (FLT_EXPR$i)
    #print ("FLT_EXPR$i")
    if(FLT_EXPR$i==""){#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*()) 3400 NO_BIN_SLCT
      DFM<-GLOB_CLU_DFM$i
      WIN01<- GLOB_CLU_WIN01_SLCT$i
      
   #   #print(head(WIN01))
    #  #print("head(WIN01)--TRK_VENN3- ---3136")
    #  #print(head(DFM,3))
    #  #print("head(DFM,3) -- VENN4 NO-FILTER 3330")
      GEM_COUNT=nrow(DFM)
      PH<-pheatmap(DFM,
                   main=input$DATA_SLCT,
                   legend = FALSE,
                   cluster_rows=FALSE,
                   clustering_distance_rows =input$DIST, ##"euclidean",
                   clustering_method = input$CLST,##"complete",
                   cluster_cols=FALSE,
                   border_color = NA,
                   treeheight_row = input$TH_M01, 
                   treeheight_col = 0,
                   annotation_legend = FALSE,
                   annotation_names_row = FALSE,
                   color =colorRampPalette(c("lightcyan","darkgreen"))(2),
                   show_rownames=FALSE,show_colnames = FALSE,
                   silent = TRUE)
      plot(PH[[4]])
      #print(head(WIN01))
      #print("head(WIN01) ---VENN3--NULL 3165")
      WIN01<-WIN01%>%dplyr::select(-related_plot_line_num)
      #    WIN01<-WIN01%>%mutate(related_plot_line_num=old_ypos-min(WIN01$old_ypos))
      #   WIN01<-WIN01%>%dplyr::select(-old_ypos)
      # WIN01$related_plot_line_num=1:nrow(WIN01)
      NEW_ORDER<-data.frame("GEMNUMID"=unique(WIN01$GEMNUMID))
      NEW_ORDER$related_plot_line_num=1:nrow(NEW_ORDER)
      WIN01<- merge(WIN01,NEW_ORDER,by="GEMNUMID",drop=FALSE)
      
      #LH=1
      #FTP="butt"
      #FH=2
      FTP=input$SHAPE_VENN
      LH=input$LSIZE_VENN
      FH=input$FSIZE_VENN
      RS=RESIZE$xmin
      RE=RESIZE$xmax
      
      
      
      
      
      
      PF<-ggplot(WIN01,aes(group=GEMNUMID,color=factor(CLUSTERID),x=freg_start,y=-related_plot_line_num))+
        # PF<-ggplot(WIN01,aes(group=GEMNUMID,x=freg_start,y=-related_plot_line_num))+
        geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                     size=FH,lineend = FTP,)+
        geom_line(aes(x=freg_start,y=-related_plot_line_num),size=LH,alpha=0.4)+
        theme_bw()+
        ggtitle(paste0("n=",GEM_COUNT))+
        theme(legend.position="none",panel.grid = element_blank(),
              panel.border = element_rect(size=0.2),
              #plot.title = element_blank(),
              axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
        scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
        coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))
      
      if(input$M04_HIT_COL=="color"){
        PF<-PF+scale_color_manual(values=GLOB_PVAL$i)
      }else if(input$M04_HIT_COL=="mono"){
        PF<-PF+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(WIN01$CLUSTERID))))
        
      }
      
      
      
      output$TRK_VENN4 <- renderPlot({
        
        updateNumericInput(session,"HT_VENN4",value=100)
        #grid.arrange(PF)
        plot(PH[[4]])
      })
      
      output$TRK_VENN2 <- renderPlot({
        
        updateNumericInput(session,"HT_VENN2",value=300)
        grid.arrange(PF)
        
      })
    }else{#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*()) 3485 HV_BIN_SLCT
      DFM<-GLOB_CLU_DFM$i
      AIN2<-GLOB_CLU_AIN2$i
      NEW_LIST<-GLOB_NEW_LIST$i
      CMB_GEMID<-GLOB_CMB_GEMID$i
      CMB_LIST<- GLOB_CMB_LIST$i
      A<-GLOB_A$i;
      DA<-(data.frame(A));#print(str(DA$A[2]));#print("DA^^^^^^")
      indx <- sapply(DA, is.factor)
      DA[indx] <- lapply(DA[indx], function(x) as.numeric(as.character(x)))
      #print(DA);#print("DA================")
            #print(head(AIN2))
      #print("AIN2--3214--NOTNULL")
      #print(head(NEW_LIST))
      #print("NEW_LIST--3214--NOTNULL")
      for ( k in 1:length(CMB_LIST)){
        if(CMB_LIST[[k]]==input$VENN_SLCT){
          PN$i=k
        }
      }
      
      
      LOCS<-data.frame("freg_start"=NULL,"freg_end"=NULL,"GEMNUMID"=NULL,"YPOS"=NULL)
      for ( a in 1:nrow(DA)){
        CONNUM=as.integer(DA$A[a]);#print(CONNUM);#print("<<<<CONNUM 3511");#print(a);#print("a");#print(length(A));#print("length(A)")
    #  for (a in 1:length(A)){
     #   CONNUM=as.integer(A[a])
                #print("CONNUM 3507--->");#print(CONNUM);#print("<<<<CONNUM 3507")
        #print(colnames(DFM)[CONNUM]);#print("colnames(DFM)[CONNUM]:3515")
        SHA_S0=as.integer(colnames(DFM)[CONNUM]);#print(SHA_S0);#print("SHA_S0 3516 ")
        if(CONNUM==GLOB_BINNUM$i){
          #print(RESIZE$xmax)
          SHA_E0=as.numeric(RESIZE$xmax)
        }else{
          #print(colnames(DFM)[CONNUM+1])
          SHA_E0=as.numeric(colnames(DFM)[CONNUM+1])
        }
        #print("NNNNNNDDDDMMMM")
        LOC1<-data.frame("freg_start"=SHA_S0,"freg_end"=SHA_E0,"GEMNUMID"=a,"YPOS"=1)
        LOCS<-rbind(LOCS,LOC1)
        
      }#for ( a in 1:length(A)){
      
      LOCS<-transform(LOCS,GEMNUMID = as.factor(GEMNUMID));#print(LOCS);#print("LOCS 3528")
      
      
      PLOT_FREGMENT<-function(res,plot_name="NO",bin_list,matrix){
        DF<-data.frame("A","B",stringsAsFactors = FALSE)
        #print(head(matrix,2))
        #print("head(matrix,2)  -- 3536")
        for (i in 1:length(res$tree_row$order)){
          ROW_CLUSTER_ORDER=i
          ROW_CLUSTER_NAME=res$tree_row$labels[res$tree_row$order[i]]
          DF[ROW_CLUSTER_ORDER,] <- list(ROW_CLUSTER_ORDER,ROW_CLUSTER_NAME)
        }
        
        colnames(DF)<-c("related_plot_line_num","GEMNUMID")
        #print(head(AIN2))
        #print(tail(DF))
        #print("DF---3255")
        #print("422")
        MIN<-merge(AIN2,DF,by = "GEMNUMID")
        WIN01=MIN
        # str(WIN01)
        WIN01<-transform(WIN01, related_plot_line_num = as.numeric(related_plot_line_num))
        YBREAK=20 ## howmany scales in yaxis
        TEXTSIZE=16 ## text size in plot
        #LH=as.numeric(input$LSIZE)
        #  FH=as.numeric(input$FSIZE)
        # LH=1
        #FTP="butt"
        #FH=2
        FTP=input$SHAPE_VENN
        LH=input$LSIZE_VENN
        FH=input$FSIZE_VENN
        
        RS=RESIZE$xmin
        RE=RESIZE$xmax
        
        EACH_GEM_LEND<-aggregate(MIN$freg_start,by=list(MIN$gem_id),min)
        colnames(EACH_GEM_LEND)<-c("gem_id","EACH_GEM_LEND")
        #print(head(EACH_GEM_LEND,2))
        #print("head(EACH_GEM_LEND)--3280")
        EACH_GEM_REND<-aggregate(MIN$freg_end,by=list(MIN$gem_id),max)
        colnames(EACH_GEM_REND)<-c("gem_id","EACH_GEM_REND")
        #print(head(EACH_GEM_REND,2))
        #print("head(EACH_GEM_REND)")
        MGD_EACH_GEM_ENDS<-merge(EACH_GEM_LEND,EACH_GEM_REND,by="gem_id")
        MGD_EACH_GEM_LEN<-MGD_EACH_GEM_ENDS%>%mutate(EACH_GEM_COV=EACH_GEM_REND-EACH_GEM_LEND)
        #print(head(MGD_EACH_GEM_LEN))
        #print("head(MGD_EACH_GEM_LEN)--3288")
        SORT_MGD_EACH_GEM_LEN<-MGD_EACH_GEM_LEN[order(MGD_EACH_GEM_LEN$EACH_GEM_COV,decreasing = FALSE),]
        SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(SORT_MGD_EACH_GEM_LEN)
        #print(head(SORT_MGD_EACH_GEM_LEN))
        #print("head(SORT_MGD_EACH_GEM_LEN)--3292")
        WINE<-merge(MIN,SORT_MGD_EACH_GEM_LEN)
        if (!str_detect(bin_list, ",") &bin_list!="none" ){     ##3583 KAISHI:1-BIN IF.BIN.NUM.SE1
          #print(bin_list)
          CONNUM=as.integer(bin_list)
          #print(CONNUM)
          #print(colnames(DFM)[CONNUM])
          SHA_S=as.integer(colnames(DFM)[CONNUM])
          if(CONNUM==GLOB_BINNUM$i){
            #print(RESIZE$xmax)
            SHA_E=as.integer(RESIZE$xmax)
          }else{
            #print(colnames(DFM)[CONNUM+1])
            SHA_E=as.integer(colnames(DFM)[CONNUM+1])
          }
          #print("NNNNNNDDDDMMMM")
          #print(head(SORT_MGD_EACH_GEM_LEN))
          #print(paste("LOC:",SHA_S,SHA_E))
          for (w in 1:nrow(SORT_MGD_EACH_GEM_LEN)){
            LEFT_END=SORT_MGD_EACH_GEM_LEN[w,]$EACH_GEM_LEND
            RIGHT_END=SORT_MGD_EACH_GEM_LEN[w,]$EACH_GEM_REND
            L0=LEFT_END-SHA_S
            L1=LEFT_END-SHA_E
            R0=RIGHT_END-SHA_S
            R1=RIGHT_END-SHA_E
            #print("**********")
            #print(paste(L0,L1,R0,R1))
            # #print(SORT_MGD_EACH_GEM_LEN[w,])
            if(L0<0 & L1<0 & R0>=0 & R1<=0){
              NEW_COV=0-((SHA_S-RESIZE$xmin)-(SHA_S-LEFT_END))
              SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN%>%mutate(inside="left")
              #print("LLLLLLLL")
            }else if(L0>=0 & L1<=0 & R0>0 & R1>0){
              NEW_COV=RIGHT_END-SHA_E
              #print("RRRRRRRR")
              SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN%>%mutate(inside="right")
            }else if(L0>=0 & L1<=0 & R0>=0 & R1<=0){
              NEW_COV=0
              #print("0000000")
              SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN%>%mutate(inside="inside")
            }else if(L0<0 & L1<0 & R0>0 & R1>0){
              NEW_COV=ifelse((RIGHT_END-SHA_E)>(SHA_S-LEFT_END),RIGHT_END-SHA_E,0-((SHA_S-RESIZE$xmin)-(SHA_S-LEFT_END)))
              #print("---------")
              SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN%>%mutate(inside="cross")
            }else{
              
              NEW_COV="ERROR"
              #print("ERROR")
            }
            SORT_MGD_EACH_GEM_LEN[w,]$EACH_GEM_COV<-NEW_COV
          }
          SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN[order(SORT_MGD_EACH_GEM_LEN$EACH_GEM_COV,decreasing = FALSE),]
          SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(SORT_MGD_EACH_GEM_LEN)
          WINE<-merge(MIN,SORT_MGD_EACH_GEM_LEN)
          
          NUM_SHORT=0
          SORT_MGD_EACH_GEM_LEN<-SORT_MGD_EACH_GEM_LEN%>%mutate(inside="long")
          for (b in 1:nrow(SORT_MGD_EACH_GEM_LEN)){
            if( (SORT_MGD_EACH_GEM_LEN[b,]$EACH_GEM_LEND>=SHA_S)&(SORT_MGD_EACH_GEM_LEN[b,]$EACH_GEM_REND<=SHA_E)){
              SORT_MGD_EACH_GEM_LEN[b,]$inside="short"
              NUM_SHORT=NUM_SHORT+1
            }
          }
          
          #print(NUM_SHORT)
          #print("NUM_SHORT")
          #print(head(SORT_MGD_EACH_GEM_LEN,2))
          #print("head(SORT_MGD_EACH_GEM_LEN)--3360")
          if (NUM_SHORT>0){   #KAISHI:NUM_SHORTB0
            SHORT_SORT_MGD_EACH_GEM_LEN0<-dplyr::filter(SORT_MGD_EACH_GEM_LEN,inside=="short")
            
            SHORT_SORT_MGD_EACH_GEM_LEN<-SHORT_SORT_MGD_EACH_GEM_LEN0[order(SHORT_SORT_MGD_EACH_GEM_LEN0$EACH_GEM_COV,decreasing = FALSE),]
            SHORT_SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(SHORT_SORT_MGD_EACH_GEM_LEN)
            SHORT_WINE<-merge(MIN,SHORT_SORT_MGD_EACH_GEM_LEN)
            LONG_SORT_MGD_EACH_GEM_LEN0<-dplyr::filter(SORT_MGD_EACH_GEM_LEN,inside=="long")
            LONG_SORT_MGD_EACH_GEM_LEN<-LONG_SORT_MGD_EACH_GEM_LEN0[order(LONG_SORT_MGD_EACH_GEM_LEN0$EACH_GEM_COV,decreasing = FALSE),]
            LONG_SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(LONG_SORT_MGD_EACH_GEM_LEN)
            LONG_WINE<-merge(MIN,LONG_SORT_MGD_EACH_GEM_LEN)
            # SORT_MGD_EACH_GEM_LEN$YPOS=1:nrow(SORT_MGD_EACH_GEM_LEN)
            
            # SHORT_WINE<-transform(SHORT_WINE, related_plot_line_num = as.numeric(related_plot_line_num))
            # LONG_WINE<-transform(LONG_WINE, related_plot_line_num = as.numeric(related_plot_line_num))
            #print(head(SHORT_WINE,2))
            #print("head(SHORT_WINE,2)")
            #print(head(LONG_WINE,2))
            #print("head(LONG_WINE,2)")
            #pf1<-ggplot(LONG_WINE,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID,x=freg_start,y=-YPOS))+
            CLUSTER_NAME<-dplyr::select(GLOB_CLU_WIN01_SLCT$i,GEMNUMID,CLUSTERID)            
            LONG_WINE<-merge(LONG_WINE,CLUSTER_NAME,by="GEMNUMID",drop=TRUE)
            pf1<-ggplot(LONG_WINE,aes(group=GEMNUMID,x=freg_start,y=-YPOS,color=factor(CLUSTERID)))+
              geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-max(LONG_WINE$YPOS)-5,ymax=5),
                        color="skyblue",fill="skyblue",size=0,alpha=0.6)+
              geom_segment(aes(x=freg_start,xend=freg_end,y=-YPOS,yend=-YPOS),
                           size=FH,lineend = FTP)+
              geom_line(aes(x=freg_start,y=-YPOS),size=LH,alpha=0.4)+
              theme_bw()+
              ggtitle(paste0(plot_name," outside n= ",nrow(LONG_SORT_MGD_EACH_GEM_LEN)))+
              #  labs(x="Weather    stations", y="Accumulated Rainfall [mm]", title="Rainfall",   subtitle="plot_name")+
              theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                    panel.border = element_rect(size=0.2),
                    #plot.title = element_blank(),
                    axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
              # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
              scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
              coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(LONG_WINE$YPOS))-5,5)) 
            if(input$M04_HIT_COL=="color"){
              pf1<-pf1+scale_color_manual(values=GLOB_PVAL$i)
            }else if(input$M04_HIT_COL=="mono"){
              pf1<-pf1+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(LONG_WINE$CLUSTERID))))  
            }
      #      CLUSTER_NAME<-dplyr::select(GLOB_CLU_WIN01_SLCT$i,GEMNUMID,CLUSTERID)
            SHORT_WINE<-merge(SHORT_WINE,CLUSTER_NAME,by="GEMNUMID",drop=TRUE)
            pf2<-ggplot(SHORT_WINE, aes(group=GEMNUMID,x=freg_start,y=-YPOS,color=factor(CLUSTERID)))+
              #    pf2<-ggplot(SHORT_WINE,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID,x=freg_start,y=-YPOS))+
              
              geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-max(SHORT_WINE$YPOS)-5,ymax=5),
                        color="skyblue",fill="skyblue",size=0,alpha=0.6)+
              geom_segment(aes(x=freg_start,xend=freg_end,y=-YPOS,yend=-YPOS),
                           size=FH,lineend = FTP)+
              geom_line(aes(x=freg_start,y=-YPOS),size=LH,alpha=0.4)+
              theme_bw()+
              ggtitle(paste0(plot_name," inside n= ",nrow(SHORT_SORT_MGD_EACH_GEM_LEN)))+
              #  labs(x="Weather    stations", y="Accumulated Rainfall [mm]", title="Rainfall",   subtitle="plot_name")+
              theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                    panel.border = element_rect(size=0.2),
                    #plot.title = element_blank(),
                    axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
              # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
              scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
              coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(SHORT_WINE$YPOS))-5,5)) 
            
            if(input$M04_HIT_COL=="color"){
              pf2<-pf2+scale_color_manual(values=GLOB_PVAL$i)
            }else if(input$M04_HIT_COL=="mono"){
              pf2<-pf2+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(SHORT_WINE$CLUSTERID))))
            }
            
            if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)<10){
              PLOT_LONG=100
            }else if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)<50){
              PLOT_LONG=200
            }else if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)<100){
              PLOT_LONG=300
            }else if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)<200){
              PLOT_LONG=450
            }else if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)<300){
              PLOT_LONG=600
            }else if(nrow(LONG_SORT_MGD_EACH_GEM_LEN)>1000){
              PLOT_LONG=1000
            }else{
              PLOT_LONG=nrow(LONG_SORT_MGD_EACH_GEM_LEN)
            }
            if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)<10){
              PLOT_SHORT=100
            }else if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)<50){
              PLOT_SHORT=200
            }else if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)<100){
              PLOT_SHORT=300
            }else if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)<200){
              PLOT_SHORT=450
            }else if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)<300){
              PLOT_SHORT=600
            }else if(nrow(SHORT_SORT_MGD_EACH_GEM_LEN)>1000){
              PLOT_SHORT=1000
            }else{
              PLOT_SHORT=nrow(SHORT_SORT_MGD_EACH_GEM_LEN)
            }
            PF<-ggarrange(pf1,pf2,ncol = 1,heights=c(PLOT_LONG,PLOT_SHORT))
          }else{ # #JIESHU:NUM_SHORTB0  
            ##KAISHI:NUM_SHORTSE0
            #print(head(WINE))
            #print("head(WINE)-----3560---")
            # PF<-ggplot(WINE,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID,x=freg_start,y=-YPOS))+
            CLUSTER_NAME<-dplyr::select(GLOB_CLU_WIN01_SLCT$i,GEMNUMID,CLUSTERID)
            WINE<-merge(WINE,CLUSTER_NAME,by="GEMNUMID",drop=TRUE)
            #print(head(WINE))
            #print("WINE)-----3560---")
            PF<-ggplot(WINE,aes(group=GEMNUMID,x=freg_start,y=-YPOS,color=factor(CLUSTERID)))+
              geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-max(WINE$YPOS)-5,ymax=5),
                        color="skyblue",fill="skyblue",size=0.2,alpha=0.6)+
              geom_segment(aes(x=freg_start,xend=freg_end,y=-YPOS,yend=-YPOS),
                           size=FH,lineend = FTP)+
              geom_line(aes(x=freg_start,y=-YPOS),size=LH,alpha=0.4)+
              theme_bw()+
              ggtitle(plot_name)+
              #  labs(x="Weather    stations", y="Accumulated Rainfall [mm]", title="Rainfall",   subtitle="plot_name")+
              theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                    panel.border = element_rect(size=0.2),
                    #plot.title = element_blank(),
                    axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
              # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
              scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
              coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WINE$YPOS))-5,5)) 
            
            
            if(input$M04_HIT_COL=="color"){
              PF<-PF+scale_color_manual(values=GLOB_PVAL$i)
            }else if(input$M04_HIT_COL=="mono"){
              PF<-PF+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(WINE$CLUSTERID))))  
            }
            # grid.arrange(PF)
            
            
            
            
            
            
          }##JIESHU:NUM_SHORTSE0
        }else{#3789 ##JIESHU:1-BIN ##KAISHI:MULTI-BINS IF.BIN.NUM.SE1  IF.NUM_SHOT.B1
          CLUSTER_NAME<-dplyr::select(GLOB_CLU_WIN01_SLCT$i,GEMNUMID,CLUSTERID)
          WINE<-merge(WINE,CLUSTER_NAME,by="GEMNUMID",drop=TRUE)
          PF<-ggplot(WINE,aes(group=GEMNUMID,x=freg_start,y=-YPOS,color=factor(CLUSTERID)))+
            # PF<-ggplot(WINE,aes(group=GEMNUMID,fill=GEMNUMID,color=GEMNUMID,x=freg_start,y=-YPOS))+
            geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-max(WINE$YPOS)-5,ymax=5),
                      color="skyblue",fill="skyblue",size=0.2,alpha=0.6)+
            geom_segment(aes(x=freg_start,xend=freg_end,y=-YPOS,yend=-YPOS),
                         size=FH,lineend = FTP)+
            geom_line(aes(x=freg_start,y=-YPOS),size=LH,alpha=0.4)+
            theme_bw()+
            ggtitle(plot_name)+
            #  labs(x="Weather    stations", y="Accumulated Rainfall [mm]", title="Rainfall",   subtitle="plot_name")+
            theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(size=0.2),
                  #plot.title = element_blank(),
                  axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
            # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
            scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
            coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WINE$YPOS))-5,5)) 
          # grid.arrange(PF)
          if(input$M04_HIT_COL=="color"){
            PF<-PF+scale_color_manual(values=GLOB_PVAL$i)
          }else if(input$M04_HIT_COL=="mono"){
            PF<-PF+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(WINE$CLUSTERID))))  
          }
        }##JIESHU:MULTI-BINS
        plot(PF)
      }#PLOT_FREGMENT
      
      PLOT_FREGMENT_1<-function(MAT,plot_name="NO"){
        MAT$IDS<-row.names(MAT)
        related_plot_line_num=1
        GEMNUMID=as.integer(MAT$IDS[[1]])
        #print(MAT$IDS[[1]])
        #print("#print(MAT)")
        DF<-data.frame(related_plot_line_num,GEMNUMID,stringsAsFactors = FALSE)
        
        colnames(DF)<-c("related_plot_line_num","GEMNUMID")
        #print(DF)
        
        #print("422")
        MIN<-merge(AIN2,DF,by = "GEMNUMID")
        WIN01=MIN
        #print(WIN01)
        WIN01<-transform(WIN01, related_plot_line_num = as.numeric(related_plot_line_num))
        YBREAK=20 ## howmany scales in yaxis
        TEXTSIZE=16 ## text size in plot
        #LH=as.numeric(input$LSIZE)
        #  FH=as.numeric(input$FSIZE)
        #FTP="butt"
        #FH=2
        FTP=input$SHAPE_VENN
        LH=input$LSIZE_VENN
        FH=input$FSIZE_VENN
        RS=RESIZE$xmin
        RE=RESIZE$xmax
        CLUSTER_NAME<-dplyr::select(GLOB_CLU_WIN01_SLCT$i,GEMNUMID,CLUSTERID)
        WIN01<-merge(WIN01,CLUSTER_NAME,by="GEMNUMID",drop=TRUE)
        PF<-ggplot(WIN01,aes(group=GEMNUMID,x=freg_start,y=-related_plot_line_num,color=factor(CLUSTERID)))+
          geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-7,ymax=5),
                    color="skyblue",fill="skyblue",size=0.2,alpha=0.6)+
          geom_segment(aes(x=freg_start,xend=freg_end,y=-related_plot_line_num,yend=-related_plot_line_num),
                       size=FH,lineend = FTP)+
          geom_line(aes(x=freg_start,y=-related_plot_line_num),size=LH,alpha=0.4)+
          theme_bw()+
          ggtitle(plot_name)+
          #  labs(x="Weather    stations", y="Accumulated Rainfall [mm]", title="Rainfall",   subtitle="plot_name")+
          theme(legend.position="none",panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                panel.border = element_rect(size=0.2),
                #plot.title = element_blank(),
                axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
          # scale_x_continuous(limits=c(RS,RE),labels=scales::comma,breaks=c(RS,seq(RS,RE,round((RE-RS)/10+RS)),RE),expand = c(0, 0))
          scale_x_continuous(breaks=seq(RESIZE$xmin,RESIZE$xmax,(RESIZE$xmax-RESIZE$xmin)/XBRKNUM),labels=scales::comma)+
          coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-max(as.integer(WIN01$related_plot_line_num))-5,5))  
        if(input$M04_HIT_COL=="color"){
          PF<-PF+scale_color_manual(values=GLOB_PVAL$i)
        }else if(input$M04_HIT_COL=="mono"){
          PF<-PF+scale_color_manual(values=rep(input$MONO_COLOR_NAME,length(unique(WIN01$CLUSTERID))))  
        }
        plot(PF)
      }##PLOT_FREGMENT_1
      
      PLOT_FREGMENT_0<-function(plot_name="NO"){
        PF<-ggplot(data.frame())+
          geom_rect(data=LOCS,aes(xmin=freg_start,xmax=freg_end,ymin=-5,ymax=5),
                    color="skyblue",fill="skyblue",size=0.2,alpha=0.6)+
          ggtitle(plot_name)+
          theme_bw()+
          theme(legend.position="none",panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(), panel.border = element_rect(size=0.2),
                axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())+
          coord_cartesian(expand = FALSE,xlim=c(RESIZE$xmin,RESIZE$xmax),ylim=c(-5,5))  
        plot(PF)
      }#PLOT_FREGMENT_0
      
      
      PLOT_LIST=list()
      FRAG_PLOT=list()
      
      PLOT_HT=list()
      for (x in 1:length(NEW_LIST)){
        #print(CMB_LIST[[x]])
        #print(CMB_GEMID[[x]])
        #print(NEW_LIST[[x]])
        #print(nrow(NEW_LIST[[x]]))
        #print("3560--------------")
        if(nrow(NEW_LIST[[x]])>=2){
          
          
          if(nrow(NEW_LIST[[x]])<100){
            PLOT_HT[[x]]=300
          }else if(nrow(NEW_LIST[[x]])<200){
            PLOT_HT[[x]]=450
          }else if(nrow(NEW_LIST[[x]])<300){
            PLOT_HT[[x]]=600
          }else if(nrow(NEW_LIST[[x]])>1000){
            PLOT_HT[[x]]=1000
          }else{
            PLOT_HT[[x]]=nrow(NEW_LIST[[x]])
          }
          SUB_NAME=paste(CMB_LIST[[x]],nrow(NEW_LIST[[x]]),sep=" nrow = ")
          #print(head(NEW_LIST[[x]],3))
          #print("head(NEW_LIST[[x]],3) -- VENN4 IN-FILTER 3828")
          PLOT_LIST[[x]]<- pheatmap(NEW_LIST[[x]],
                                    main=SUB_NAME,
                                    legend = FALSE,
                                    cluster_rows=TRUE,
                                    clustering_distance_rows =input$DIST, ##"euclidean",
                                    clustering_method = input$CLST,##"complete",
                                    cluster_cols=FALSE,
                                    border_color = NA,
                                    treeheight_row = input$TH_M01, 
                                    treeheight_col = 0,
                                    annotation_legend = FALSE,
                                    annotation_names_row = FALSE,
                                    color=c(input$BC_M04,GLOB_COL$i),
                                    #  color =colorRampPalette(c("lightcyan","darkgreen"))(2),
                                    show_rownames=FALSE,show_colnames = FALSE,
                                    silent = TRUE)
          FRAG_PLOT[[x]]<-PLOT_FREGMENT(PLOT_LIST[[x]],SUB_NAME,CMB_LIST[[x]],NEW_LIST[[x]])
        }else if(nrow(NEW_LIST[[x]])==1){
          PLOT_HT[[x]]=150
          SUB_NAME=paste(CMB_LIST[[x]],nrow(NEW_LIST[[x]]),sep=" nrow = ")
          PLOT_LIST[[x]]=pheatmap(rbind(NEW_LIST[[x]],NEW_LIST[[x]],NEW_LIST[[x]]),
                                  main=paste(CMB_LIST[[x]],nrow(NEW_LIST[[x]]),sep=" nrow = "),
                                  legend = FALSE,
                                  cluster_rows=FALSE,
                                  clustering_distance_rows =input$DIST, ##"euclidean",
                                  clustering_method = input$CLST,##"complete",
                                  cluster_cols=FALSE,
                                  border_color = NA,
                                  treeheight_row = input$TH_M01, 
                                  treeheight_col = 0,
                                  annotation_legend = FALSE,
                                  annotation_names_row = FALSE,
                                  # color =colorRampPalette(c("lightcyan","darkgreen"))(2),
                                  color=c(input$BC_M04,GLOB_COL$i),
                                  
                                  show_rownames=FALSE,show_colnames = FALSE,
                                  silent = TRUE)
          FRAG_PLOT[[x]]<-PLOT_FREGMENT_1(NEW_LIST[[x]],SUB_NAME)
        }else{ ##nrow(NEW_LIST[[x]])==0
          PLOT_HT[[x]]=150
          SUB_NAME=paste(CMB_LIST[[x]],nrow(NEW_LIST[[x]]),sep=" nrow = ")
          PLOT_LIST[[x]]=  pheatmap(data.frame(c(0,1)),
                                    main=paste(CMB_LIST[[x]],"0",sep=" nrow = "),
                                    legend = FALSE,
                                    cluster_rows=FALSE,
                                    #clustering_distance_rows =input$DIST, ##"euclidean",
                                    #clustering_method = input$CLST,##"complete",
                                    cluster_cols=FALSE,
                                    border_color = NA,
                                    treeheight_row = input$TH_M01, 
                                    treeheight_col = 0,
                                    annotation_legend = FALSE,
                                    annotation_names_row = FALSE,
                                    #color =colorRampPalette(c("lightcyan","lightcyan"))(2),
                                    color=c(input$BC_M04,GLOB_COL$i),
                                    
                                    show_rownames=FALSE,show_colnames = FALSE,
                                    silent = TRUE)
          FRAG_PLOT[[x]]<-PLOT_FREGMENT_0(SUB_NAME)
        }####nrow(NEW_LIST[[x]])==0
      }##for (x in 1:length(NEW_LIST)){
      
      # plot(PLOT_LIST[[length(PLOT_LIST)]][[4]])
      
      #________________________
      
      
      
      output$TRK_VENN4 <- renderPlot({
        if(input$AND01==0) return(NULL)
        
        
        if(input$VENN_SLCT!="all"){
          updateNumericInput(session,"HT_VENN4",value=250)
          plot(PLOT_LIST[[PN$i]][[4]])
        }else{
          HT1PLOT=150
          if(length(NEW_LIST)==2){
            updateNumericInput(session,"HT_VENN",value=HT1PLOT*length(NEW_LIST))
            grid.arrange(PLOT_LIST[[1]][[4]],PLOT_LIST[[2]][[4]],
                         ncol=1)
          }else if (length(NEW_LIST)==4){
            updateNumericInput(session,"HT_VENN",value=HT1PLOT*length(NEW_LIST))
            grid.arrange(PLOT_LIST[[1]][[4]],PLOT_LIST[[2]][[4]],PLOT_LIST[[3]][[4]],PLOT_LIST[[4]][[4]],
                         ncol=1)
          }else if (length(NEW_LIST)==8){
            updateNumericInput(session,"HT_VENN",value=HT1PLOT*length(NEW_LIST))
            grid.arrange(PLOT_LIST[[1]][[4]],PLOT_LIST[[2]][[4]],PLOT_LIST[[3]][[4]],PLOT_LIST[[4]][[4]],
                         PLOT_LIST[[5]][[4]],PLOT_LIST[[6]][[4]],PLOT_LIST[[7]][[4]],PLOT_LIST[[8]][[4]],
                         ncol=1)
          }else if (length(NEW_LIST)==16){
            updateNumericInput(session,"HT_VENN",value=HT1PLOT*length(NEW_LIST))
            grid.arrange(PLOT_LIST[[1]][[4]],PLOT_LIST[[2]][[4]],PLOT_LIST[[3]][[4]],PLOT_LIST[[4]][[4]],
                         PLOT_LIST[[5]][[4]],PLOT_LIST[[6]][[4]],PLOT_LIST[[7]][[4]],PLOT_LIST[[8]][[4]],
                         PLOT_LIST[[9]][[4]],PLOT_LIST[[10]][[4]],PLOT_LIST[[11]][[4]],PLOT_LIST[[12]][[4]],
                         PLOT_LIST[[13]][[4]],PLOT_LIST[[14]][[4]],PLOT_LIST[[15]][[4]],PLOT_LIST[[16]][[4]],
                         ncol=1)
          }else if (length(NEW_LIST)==32){
            updateNumericInput(session,"HT_VENN",value=HT1PLOT*length(NEW_LIST))
            grid.arrange(PLOT_LIST[[1]][[4]],PLOT_LIST[[2]][[4]],PLOT_LIST[[3]][[4]],PLOT_LIST[[4]][[4]],
                         PLOT_LIST[[5]][[4]],PLOT_LIST[[6]][[4]],PLOT_LIST[[7]][[4]],PLOT_LIST[[8]][[4]],
                         PLOT_LIST[[9]][[4]],PLOT_LIST[[10]][[4]],PLOT_LIST[[11]][[4]],PLOT_LIST[[12]][[4]],
                         PLOT_LIST[[13]][[4]],PLOT_LIST[[14]][[4]],PLOT_LIST[[15]][[4]],PLOT_LIST[[16]][[4]],
                         PLOT_LIST[[17]][[4]],PLOT_LIST[[18]][[4]],PLOT_LIST[[19]][[4]],PLOT_LIST[[20]][[4]],
                         PLOT_LIST[[21]][[4]],PLOT_LIST[[22]][[4]],PLOT_LIST[[23]][[4]],PLOT_LIST[[24]][[4]],
                         PLOT_LIST[[25]][[4]],PLOT_LIST[[26]][[4]],PLOT_LIST[[27]][[4]],PLOT_LIST[[28]][[4]],
                         PLOT_LIST[[29]][[4]],PLOT_LIST[[30]][[4]],PLOT_LIST[[31]][[4]],PLOT_LIST[[32]][[4]],
                         ncol=1)
          }
        }
      })##JIESHI:TRK_VENN4
      
      
      
      #print(" ######!!!!!!!!!!!!!!!!!!!!!!!")
      output$TRK_VENN2 <- renderPlot({
        #print("VENN2:4002")
        if(input$AND01==0) return(NULL)
        if(input$VENN_SLCT!="all"){
          updateNumericInput(session,"HT_VENN2",value=PLOT_HT[[PN$i]])
          grid.arrange(FRAG_PLOT[[PN$i]])
          
        } else{
          grid.newpage()
          g<-ggplotGrob(FRAG_PLOT[[1]])
          for (q in 2:length(FRAG_PLOT)){
            g2 <- ggplotGrob(FRAG_PLOT[[q]])
            g <- rbind(g,g2)
          }
          grid.draw(g)
          updateNumericInput(session,"HT_VENN2",value=1200)
        }
      })##JIESHU:TRK_VENN2
      
      ############  
      output$TRK_INFO_VENN<- renderPlot({    
        INFO_VENN=data.frame(locfilter=c( paste0("binselect=",input$LOC01),
                                          paste0("in/exclude=",input$INEXCLUDE)
        ))
        PI_VENN<-tableGrob(INFO_VENN)
        grid.arrange(PI_VENN,nrow=1)
      })
     # output$UI_INFO_VENN <- renderUI({
      #  plotOutput("TRK_INFO_VENN", height =0)
     # })##TRK_INFO_VENN JIESHU:TRK_INFO_VENN 
      ################
      ############  
      output$TRK_INFO_VENN2<- renderPlot({   
        x=paste( unlist(input$DATA_SLCT), collapse=' ')
        #  #print(writeLines(strwrap(x, width = 5)))
        INFO_VENN2=data.frame(clusterselect=c(strwrap(x, width = 20)))
        
        PI_VENN2<-tableGrob(INFO_VENN2)
        
        
        
        
        grid.arrange(PI_VENN2,nrow=1)
      })
     # output$UI_INFO_VENN2 <- renderUI({
    #    plotOutput("TRK_INFO_VENN2", height =0)
    #  })#JIESHU:TRK_INFO_VENN2
      ################
    }#####!@#$!@#$%^&*()#$%^&*()*&^%$#$%^&*()(*&^%$%^&*())      HV_BIN_SLCT 4074
    
  })##JIESHU:VENN3

  output$UI_VENN2 <- renderUI({
    plotOutput("TRK_VENN2", height = input$HT_VENN2+RDM1$i,width=paste0(input$WD_VENN2,"%"))
  })
  output$UI_VENN3 <- renderUI({
    plotOutput("TRK_VENN3", height = 1,width="100%")
  })
  output$UI_VENN4 <- renderUI({
    plotOutput("TRK_VENN4", height = input$HT_VENN4+RDM1$i,width=paste0(input$WD_VENN4,"%"))
  })
  ###################################data_s2 <- sample_n(data, 3)
  

  ###################################
  ###################################data_s2 <- sample_n(data, 3)
  ###################################data_s2 <- sample_n(data, 3)
  #print(session_info())
  ###################################data_s2 <- sample_n(data, 3)
})#shinyServer(function(input, output) {
