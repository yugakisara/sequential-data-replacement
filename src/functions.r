
#### get_pm_par ####
get_pm_par <- function(MSYsummary = true_MSY_lowRec_constSel, adjust = FALSE, SB = FALSE, Slex = NULL){
  
  if(adjust == TRUE){
    F <-  MSYsummary$all.stat[grep("^F(?!ref2Fcurrent$)", names(MSYsummary$all.stat), perl = TRUE)]
    M <-  MSYsummary[["input"]][["tmb_data"]][["M_mat"]][1:length(F)]
    SBmsy <- B_MSY <- sum(MSYsummary$all.stat[grep("^SSB-mean-", names(MSYsummary$all.stat))][1,] * exp(-(F[1,] + M)), na.rm=TRUE)
    MSY <- MSYsummary$summary$Catch[1]
    SB0 <- K <- sum(MSYsummary$all.stat[grep("^SSB-mean-", names(MSYsummary$all.stat))][2,]*exp(-(F[2,] + M)), na.rm=TRUE)
  } else {
    if(SB == TRUE){
      SAA <- (MSYsummary$all.stat[grep("^SSB-mean-", names(MSYsummary$all.stat))][1,])/ (MSYsummary$all.stat[grep("^TB-mean-", names(MSYsummary$all.stat))][1,])
      SBmsy <- B_MSY <- sum(SAA*(MSYsummary$all.stat[grep("^TB-mean-", names(MSYsummary$all.stat))][1,]) , na.rm=TRUE)
      MSY <- sum((MSYsummary$all.stat[grep("^TC-mean-", names(MSYsummary$all.stat))][1,]))
      SB0 <- K <- sum(SAA*(MSYsummary$all.stat[grep("^TB-mean-", names(MSYsummary$all.stat))][2,]), na.rm=TRUE)
    } else {
      SBmsy <- B_MSY <- sum(Slex*(MSYsummary$all.stat[grep("^TB-mean-", names(MSYsummary$all.stat))][1,]), na.rm=TRUE )
      MSY <- sum((MSYsummary$all.stat[grep("^TC-mean-", names(MSYsummary$all.stat))][1,]))
      SB0 <- K <- sum(Slex*(MSYsummary$all.stat[grep("^TB-mean-", names(MSYsummary$all.stat))][2,]), na.rm=TRUE)
    }
    
  } 
  calc_n <- function(n, K, Bmsy){
    Bmsy2 <- K * (n^(1/(1-n)))
    (log(Bmsy)-log(Bmsy2))^2
  }
  
  aa <- optimise(calc_n, c(0,10), Bmsy=B_MSY, K=K)
  n <- aa$minimum # nはB_MSYとKから導出
  r <- MSY/K * n ^ (n/(n-1)) # rはMSY, K, nから計算
  
  # Yield curve function
  PT_yield <- function(r, K, n, Bt, Ft){
    r/(n-1) * Bt * (1-(Bt/K)^(n-1)) - Ft*Bt
  }
  
  seq_B <- seq(from=0, to=K, length=100)
  yield <- PT_yield(r, K, n, seq_B, Ft=0)
  
  return(tibble(n=n, r=r, Bmsy=B_MSY,MSY = MSY, K=K, yield_pm=yield, B=seq_B))
}



#### make_F_plus ####
make_F_plus <- function(faa, naa, age_rows) {
  stopifnot(all(age_rows %in% seq_len(nrow(faa))))
  
  F_plus <- sapply(seq_len(ncol(faa)), function(t) {
    F <- faa[age_rows, t]
    N <- naa[age_rows, t]
    
    ok <- !is.na(F) & !is.na(N) & N > 0
    if (sum(ok) == 0) return(NA_real_)
    
    sum(F[ok] * N[ok]) / sum(N[ok])
  })
  
  F_plus
}

#### sim_org ####
sim_org <- function(Control_data = FALSE, #Index : CPUE -> SSB
                Control_Rec  = FALSE, #Rec : sd -> 0.2, rho -> 0
                Control_Selx = FALSE, #Slex : org -> saa
                Control_Selx_SB = FALSE,
                Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                Control_Rec_sd = FALSE,
                Control_Rec_rho = FALSE,
                nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                Prior = FALSE,
                prior_sd = 0.5, 
                #Selx_add = 0, 
                nyr = NULL, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                vpa_org = vpa_org0,
                SR_org = SR_org0,
                Fdata = Fdata,
                Index_data = Index_data,
                simulation_label = "No setting",
                fin_year = NULL,
                rep_n = 100,
                fra = FALSE,# データがエクセルvpaかfrasyrか 
                species_name = species_name,
                Prior_K = FALSE,
                rec_selex = FALSE) 
{

  nsim <- rep_n
  if(fra == T){
    if(vpa_org[["Pope"]] == TRUE){Pope = TRUE}else{Pope = FALSE}
  }else
  {
    if(vpa_org$input$Pope == TRUE){Pope = TRUE}else{Pope = FALSE}
  }
  
  if(species_name %in% c("Japanese spanish mackerel_SIS","Walleye pollock_P")){
    if(species_name == "Walleye pollock_P"){
      plus_ages <- 8:10
    }else{
      plus_ages <- 4:5
    }
    new_plus <- plus_ages[1]
    
    faa <- vpa_org$faa
    naa <- vpa_org$naa
    ssb <- vpa_org$ssb
    baa <- vpa_org$baa
    wcaa <- vpa_org$wcaa
    
    caa <- vpa_org$input$dat$caa
    maa <- vpa_org$input$dat$maa
    waa <- vpa_org$input$dat$waa
    M <- vpa_org$input$dat$M
    maa.tune <- vpa_org$input$dat$maa.tune
    waa.catch <- vpa_org$input$dat$waa.catch
    
    #faa再計算
    collapse_faa <- function(faa, naa, plus_ages) {
      rows_plus <- which(rownames(faa) %in% as.character(plus_ages))
      rows_keep <- setdiff(seq_len(nrow(faa)), rows_plus)
      F_plus <- make_F_plus(faa, naa, rows_plus)
      faa_new <- rbind(
        faa[rows_keep, , drop = FALSE],
        `plus` = F_plus)
      faa_new
    }
    
    faa_new <- collapse_faa(faa, naa, plus_ages = plus_ages)
    rownames(faa_new)[nrow(faa_new)] <- new_plus
    
    #saa再計算
    col_max_i <- apply(faa_new, 2, max)
    saa_new <- sweep(faa_new, 2, col_max_i, FUN = "/")
    
    #waa再計算
    collapse_waa <- function(waa, caa, plus_ages) {
      rows_plus <- which(rownames(waa) %in% as.character(plus_ages))
      rows_keep <- setdiff(seq_len(nrow(waa)), rows_plus)
      W_plus <- apply(waa[rows_plus, , drop = FALSE] *caa[rows_plus, , drop = FALSE],2,sum, na.rm = TRUE) / 
        apply(caa[rows_plus, , drop = FALSE],2,sum,na.rm = TRUE)
      waa_new <- rbind(
        waa[rows_keep, , drop = FALSE],
        plus = W_plus)
      rownames(waa_new)[nrow(waa_new)] <- as.character(min(plus_ages))
      waa_new
    }
    
    waa_new <- collapse_waa(waa, caa, plus_ages = plus_ages)
    waa.catch_new <- waa_new
    
    #caa,
    collapse_plus <- function(val, plus_ages) {
      rows_plus <- which(rownames(val) %in% as.character(plus_ages))
      rows_keep <- setdiff(seq_len(nrow(val)), rows_plus)
      C_plus <- apply(
        val[rows_plus, , drop = FALSE],
        2,
        sum,
        na.rm = TRUE
      )
      new <- rbind(
        val[rows_keep, , drop = FALSE],
        plus = C_plus
      )
      rownames(new)[nrow(new)] <- as.character(min(plus_ages))
      new
    }
    
    
    caa_new <- collapse_plus(caa, plus_ages = plus_ages)
    naa_new <- collapse_plus(naa, plus_ages = plus_ages)
    ssb_new <- collapse_plus(ssb, plus_ages = plus_ages)
    baa_new <- collapse_plus(baa, plus_ages = plus_ages)
    
    wcaa_new <- caa_new * waa_new
    
    maa_new <- maa[0:(new_plus+1),]
    M_new <- M[0:(new_plus+1),]
    maa.tune_new <- maa.tune[0:(new_plus+1),]
    
    vpa_org$faa <- faa_new
    vpa_org$naa <- naa_new
    vpa_org$ssb <- ssb_new
    vpa_org$baa <- baa_new
    vpa_org$wcaa <- wcaa_new
    #vpa_org$wcaa <- saa_new
    vpa_org$input$dat$caa <- caa_new
    vpa_org$input$dat$maa <- maa_new
    vpa_org$input$dat$waa <- waa_new
    vpa_org$input$dat$M <- M_new
    vpa_org$input$dat$maa.tune <- maa.tune_new
    vpa_org$input$dat$waa.catch <- waa.catch_new
    
    vpa_org[["term.f"]] <- faa_new[,length(faa_new)]
    vpa_org[["Fc.at.age"]] <- NULL
    
    
  }else{}
  years <- colnames(vpa_org$naa) %>% as.numeric() #データの年数
  fin_year <- years[length(years)]
  years <- years[1]:fin_year
  
  maa <- vpa_org$input$dat$maa[,1] 
  
  
  if(Control_Rec == TRUE|Control_Selx == TRUE){vpa_org$naa[1,-1] <- 0}
  
  if(Control_Rec == TRUE){
    if(Control_Rec_sd == TRUE){SR_org$pars$sd <- 0.09}else{}
    if(Control_Rec_rho == TRUE){SR_org$pars$rho <- 0}else{}
  }else{}
  
  
  #(S0,S1,S0_p)  
  if(Control_Rec == FALSE&Control_contrast == FALSE){
    future_initial_year_name <- max(years) ## 何年からsimulationするか
    start_F_year_name <- max(years)+1 ## 将来予測でF全体にmultiplierを乗じる場合、multiplierを乗じる最初の
    start_biopar_year_name <- max(years)+1 	#生物パラメータを将来の生物パラメータとして設定された値に置き換える年の最初の年
    start_random_rec_year_name = max(years)+1 # 何年から加入をランダムにするか
    start_ABC_year_name <- max(years)+1  # betaでFをコントロールする年  
  }else{}
  
  
  # Fは変えないで加入をコントロールする(S2,S3,S4,S5,S6)  
  if(Control_Rec == TRUE&Control_contrast == FALSE){
    future_initial_year_name <- min(years) ## 何年からsimulationするか
    start_F_year_name <- max(years)+1 ## 将来予測でF全体にmultiplierを乗じる場合、multiplierを乗じる最初の
    start_biopar_year_name <- max(years)+1	#生物パラメータを将来の生物パラメータとして設定された値に置き換える年の最初の年
    start_random_rec_year_name = min(years)+1 # 何年から加入をランダムにするか
    start_ABC_year_name <-  max(years)+1# betaでFをコントロールする年
  }else{}  
  
  
  #選択率を変えるので、全て変える(S5,S6,S7)
  if(Control_Selx == TRUE){
    future_initial_year_name <- min(years) ## 何年からsimulationするか
    start_F_year_name <- min(years) ## 将来予測でF全体にmultiplierを乗じる場合、multiplierを乗じる最初の
    start_biopar_year_name <- min(years)	#生物パラメータを将来の生物パラメータとして設定された値に置き換える年の最初の年
    start_random_rec_year_name = min(years)+1 # 何年から加入をランダムにするか
    start_ABC_year_name <-  min(years)# betaでFをコントロールする年
  }else{}
  
  
  
  saa_data_i <- vpa_org[["faa"]]
  # 列ごとの最大値（NAを無視）
  col_max_i <- apply(saa_data_i, 2, max, na.rm = TRUE)
  # 各列を最大値で割る
  saa_data_i <- sweep(saa_data_i, 2, col_max_i, FUN = "/")
  # 行平均を計算（NAを無視）
  if(rec_selex == FALSE){　　#今は全てTになっており、選択率を変えない全てのシナリオの MSYを計算する選択率は過去5年平均
    future_F <- rowMeans(saa_data_i, na.rm = TRUE)
  }else{future_F <- rowMeans(saa_data_i[,(length(saa_data_i)-4):length(saa_data_i)], na.rm = TRUE)
}

  # NA の場合、直前の値で置き換える
  if (is.na(future_F[length(future_F)])) {
    future_F[length(future_F)] <- future_F[length(future_F) - 1]
  }else{}


  #選択率をコントロールする場合の過去の各年齢の漁獲圧は、
  #選択率が成熟率比orEBと同じ場合のFmsyを求め、我が国報告書のF/FmsyにかけることでFを設定する。
  
      if(Control_Selx == TRUE){
        if(Control_Selx_SB == TRUE){
          if(species_name %in% c("Round herring_TC", "Japanese anchovy_SIS","Japanese anchovy_TC")){
            Selx_add <- 0.01
          }else{Selx_add <- 0}
          preFmsy <- maa + Selx_add #Fmsyを計算するための選択率
         # preFmsy <- ifelse(maa == 0, maa + Selx_add, maa) #Fmsyを計算するための選択率
        }else{
          saa_data_i <- vpa_org[["faa"]]
          # 列ごとの最大値（NAを無視）
          col_max_i <- apply(saa_data_i, 2, max, na.rm = TRUE)
          # 各列を最大値で割る
          saa_data_i <- sweep(saa_data_i, 2, col_max_i, FUN = "/")
          # 行平均を計算（NAを無視）
          if(rec_selex == FALSE){
            preFmsy <- rowMeans(saa_data_i, na.rm = TRUE)
          }else{  preFmsy <- rowMeans(saa_data_i[,(length(saa_data_i)-4):length(saa_data_i)], na.rm = TRUE)
          # NA の場合、直前の値で置き換える
          if (is.na(preFmsy[length(preFmsy)])) {
            preFmsy[length(preFmsy)] <- preFmsy[length(preFmsy) - 1]
          }
          
          }
          
          
          
        }
    data_future <-  make_future_data(vpa_org, nsim = nsim, nyear =nyear_make_future_data,　#100
                                     future_initial_year_name =future_initial_year_name, 
                                     start_F_year_name = start_F_year_name,
                                     start_biopar_year_name = start_biopar_year_name, 
                                     start_random_rec_year_name =start_random_rec_year_name, 
                                     waa_year=years, waa_catch_year=years, 
                                     maa_year=years, M_year=years, 
                                     futureF=preFmsy, 
                                     start_ABC_year_name = start_ABC_year_name, 
                                     res_SR = SR_org, 
                                     seed_number = 123, scale_ssb = 1, scale_R = 1, resid_type = "lognormal", 
                                     bias_correction = TRUE)
    
    true_MSY <- est_MSYRP(data_future, candidate_PGY=-1)
    Fmsy <- unlist(lapply(0:100, function(i) true_MSY[["summary"]][[paste0("F", i)]][1]))　#選択率を一定にした時のFmsy
    future_F <- Fmsy
  }else{future_F <- future_F}　#それ以外のFmsyを計算するときの選択率
  
  

  data_future <-  make_future_data(vpa_org, nsim = nsim, nyear = nyear_make_future_data, #100
                                   future_initial_year_name =future_initial_year_name, #何年から？
                                   start_F_year_name = start_F_year_name,
                                   start_biopar_year_name = start_biopar_year_name, 
                                   start_random_rec_year_name = start_random_rec_year_name, #
                                   waa_year=years, waa_catch_year=years, 
                                   maa_year=years, M_year=years, 
                                   futureF=future_F,                     
                                   start_ABC_year_name = start_ABC_year_name,
                                   res_SR = SR_org, # 再生産関係 _orgって書いているが、シナリオによっては上書きされてorgじゃない
                                   seed_number = 123, scale_ssb = 1, scale_R = 1, resid_type = "lognormal", 
                                   bias_correction = TRUE)
  
  # 真のr,n,kを求める
  true_MSY <- est_MSYRP(data_future, candidate_PGY=-1)　#ここで、MSYを計算する
  Fratio_new <- get.SPR(vpa_org, target.SPR=true_MSY[["summary"]][["perSPR"]][1]*100)$ysdata$"F/Ftarget"
  
  # ここは結局EBです。
  future_F / max(future_F)
  tmp_sm_par <-  get_pm_par(true_MSY, SB = FALSE, Slex = future_F / max(future_F)) #SB = FALSE → 事前情報はEBで計算される

  (r <- tmp_sm_par$r[1])#;tmp_EB$r[1]
  (n <- tmp_sm_par$n[1])#;tmp_EB$n[1]
  (K <- tmp_sm_par$K[1])#;tmp_EB$K[1]
  (Bmsy <-  tmp_sm_par$Bmsy[1])#;tmp_EB$Bmsy[1]
  (msy <- tmp_sm_par$MSY[1])#;tmp_EB$MSY[1] #msyは一緒
  (true_MSY[["summary"]][["perSPR"]][1]*100 ) #targetSPR
  
  

  
  ## 将来の漁獲圧、本当は関数の外で設定したい(S7)
  if(Control_Selx == TRUE&Control_contrast == TRUE){
    data_future <- redo_future(data_future,  lst(nyear= length(Fdata$F_Fmsy)+nyr), only_data = T)
    fres<- redo_future(data_future,
                       list(HCR_beta_year=tibble(year=c(years,(tail(years,1)+1):(tail(years,1)+nyr)),
                                                beta=c(Fdata$F_Fmsy, 
                                                       exp(seq(log(Fdata$F_Fmsy[length(Fdata$F_Fmsy)]),log(0.5), length.out =10)),
                                                       rep( 0.5, 5),
                                                       exp(seq(log(0.5), log(2), length.out = 15)),
                                                       rep( 2, 5),
                                                       exp(seq(log (2), log(0.5), length.out = 15))
                                                ))),
                       SPRtarget = true_MSY[["summary"]][["perSPR"]][1]*100)　# + 60年
 
  }else{}
  
  
  #betaにF/Fmsyを設定してRe将来予測(S5,S6)
  if(Control_Selx == TRUE&Control_contrast == FALSE){
    data_future <- redo_future(data_future, lst(nyear=length(Fdata$F_Fmsy)),only_data = T)
    fres <- redo_future(data_future, list(HCR_beta_year=tibble(year=years, beta=Fdata$F_Fmsy)),
                        SPRtarget = true_MSY[["summary"]][["perSPR"]][1]*100)
  }else{}
  
  ## S2,S3,S4
  if(Control_Selx == FALSE&Control_Rec == TRUE){
    fres <- redo_future(data_future, lst(nyear=length(years)),
                        SPRtarget = true_MSY[["summary"]][["perSPR"]][1]*100)
  }else{}
  
  ## S0, S1  
  if(Control_Rec == FALSE&Control_Selx == FALSE){
    fres <- redo_future(data_future, lst(nyear=1),
                        SPRtarget = true_MSY[["summary"]][["perSPR"]][1]*100)
  }else{}
  
  
  
  plot_futures(future.list = list(fres))　#mac用にアレンジした関数
  plot <- fres #後で外でplotできるように
  
  
  
  # rbind(tmp_EB|>mutate(Biomass = "EB"), tmp_sm_par|>mutate(Biomass = "SB")) |>
  #   ggplot()+
  #   geom_point(aes (x = B, y = yield_pm ,color = Biomass))
  # 
  
  n_beta_year <- if(Control_contrast == TRUE){length(years)+nyr}else{length(years)}
  fin_year <- if(Control_contrast == TRUE){fin_year+nyr}else{fin_year}
  
  fit_spict <- list()
  fit_spict_pdHess <- list()
  SB_vpa　<- EB_vpa <- TB_vpa <- FFmsy_vpa<- list()
  
  #  i = 1
  set.seed(1234)
  for(i in 1:rep_n){
    message("試行 ", i, " / ", rep_n, " 開始"," / " , simulation_label," / " ,species_name)
    
    res_i <- tryCatch({
      
      # ----------------
      #Catch
      #Catch <- colSums(fres[["wcaa"]][,,i])[-length(as.numeric(names(fres$SR_mat[,i,"ssb"])))][-(1:length(age_class))] #シミュレーション作る時一年できるので一年消す。
      Catch <- colSums(fres[["wcaa"]][,,i])[names(colSums(fres[["wcaa"]][,,i])) <= fin_year]######[-(1:length(age_class))]
      
      if(Control_data== FALSE){
        Index <- subset(Index_data$Index, as.numeric(Index_data$year) <= fin_year)
      }else{
        colSums((fres[["naa"]][,,i] * fres[["waa"]][,,i] * fres[["maa"]][,,1])[, as.numeric(colnames(fres[["naa"]][,,i])) <= fin_year])#[,-(1:length(age_class))]  )   #[-length(fres[["naa"]][1,,i])][-(1:length(age_class))]
        fres[["summary"]][["SSB"]]
        
        
        saa_const <- sweep(fres[["faa"]][,,i],2,apply(fres[["faa"]][,,i], 2, max, na.rm = TRUE), "/")
        
        Index <- colSums(
          (fres[["naa"]][,,i] * fres[["waa"]][,,i] * saa_const)[,
        　 as.numeric(colnames(fres[["naa"]][,,i])) <= fin_year
          ],
          na.rm = TRUE  # 列和を計算するときにNAを無視
        ) 
        #  if(Control_contrast == FALSE){      # tibble化して年と値をセット
        #saa_const <- sweep(fres[["faa"]][,,i], 2, apply(fres[["faa"]][,,i], 2, max), "/")
        #Index <- colSums(fres[["naa"]][,,i] *fres[["waa"]][,,i] * saa_const)[,as.numeric(colnames(fres[["naa"]][,,i] *fres[["waa"]][,,i] * saa_const)) <= fin_year][,-(1:length(age_class))]  
        #Index <- colSums(fres[["naa"]][,,i] *fres[["waa"]][,,i] *fres[["maa"]][,,1])[names(colSums(fres[["naa"]][,,i]*fres[["waa"]][,,i]*fres[["maa"]][,,1])) <= fin_year][-(1:length(age_class))]
        
        index_years <- as.numeric(names(Index))
        Index_df <- tibble(
          year = index_years,
          Index = as.numeric(Index))
        Index <- Index_df$Index * exp(rnorm(length(Index_df$Index),mean=0, sd=0.05)) # abundance index with observation error
      }
      
      if(Control_data == TRUE){
        # create data for spict
        data_raw <- bind_rows(
          tibble(Year=as.numeric(names(Catch)),
                 Stock=1,
                 Label="Catch",
                 Fleet="All",CV=NA,
                 Value=Catch),
          tibble(Year=Index_df$year,#as.numeric(names(fres$SR_mat[,i,"ssb"]))[-length(as.numeric(names(fres$SR_mat[,i,"ssb"])))],#[-(1:length(age_class))],
                 Stock=1,
                 Label="Index",
                 Fleet="cpue1",CV=NA,
                 Value=Index))#[-length(Index)]))
      }else{
        data_raw <- bind_rows(
          tibble(Year=as.numeric(names(Catch)),
                 Stock=1,
                 Label="Catch",
                 Fleet="All",CV=NA,
                 Value=Catch),
          tibble(Year=Index_data$year[1]:fin_year,
                 Stock=1,
                 Label="Index",
                 Fleet="cpue1",CV=NA,
                 Value=Index))
      }
      
      
      dat_spict <- get_spict_data(data_raw)
      dat_spict$dteuler <- 1
      inp_spict <- check.inp(dat_spict)
      
      # spictのデフォルト設定ではプロセス誤差=観測誤差、漁獲量の誤差=Fのランダムウォーク誤差を仮定しているのでその設定をオフにしておく
      # その結果、どうなるんだ？
      inp_spict$priors$logalpha[3] <- 0
      inp_spict$priors$logbeta[3] <- 0
      inp_spict$priors$logn[3] <- 0
      
      
   
      
      
      # 漁獲量の誤差はない
      # inp_spict$priors$logsdc <- c(-9, 0.01, 1)
      
      
      if(Prior == TRUE){
        inp_spict$priors$logr[1] <- log(r)
        inp_spict$priors$logr[2] <- prior_sd
        inp_spict$priors$logr[3] <- 1


        inp_spict$priors$logn[1] <- log(n)
        inp_spict$priors$logn[2] <- prior_sd
        inp_spict$priors$logn[3] <- 1
      }else{     
      #inp_spict$stabilise <- 0
      }

      
      if(Prior_K == TRUE){
        inp_spict$priors$logK[1] <- log(K)
        inp_spict$priors$logK[2] <- prior_sd
        inp_spict$priors$logK[3] <- 1
      }else{}
      
      #inp_spict$dteuler <- 1/4  
      #res_spict4_2[[i]] <-try(fit.spict(inp_spict))
      #  inp_spict$dteuler <- 1/8
      #  res_spict8[[i]] <- fit.spict(inp_spict)  

      fit_spict[[i]] <-try(fit.spict(inp_spict))
      

      fit_spict_pdHess[[i]] <- sdreport(fit_spict[[i]]$obj)


      #資源量[[i]]を計算
      SB_vpa[[i]] <- (fres[["naa"]][,,i] * fres[["waa"]][,,i] * fres[["maa"]][,,1])[, as.numeric(colnames(fres[["naa"]][,,i])) <= fin_year]#[,-(1:length(age_class))]     #[-length(fres[["naa"]][1,,i])][-(1:length(age_class))]
      saa_const <- sweep(fres[["faa"]][,,i],2,apply(fres[["faa"]][,,i], 2, max, na.rm = TRUE), "/")
      EB_vpa[[i]] <- (fres[["naa"]][,,i] *fres[["waa"]][,,i] * saa_const)[,as.numeric(colnames(fres[["naa"]][,,i] *fres[["waa"]][,,i] * saa_const)) <= fin_year]#[,-(1:length(age_class))]         #[-length(fres[["naa"]][1,,i])][-(1:length(age_class))]
      TB_vpa[[i]] <-(fres[["naa"]][,,i] *fres[["waa"]][,,i])[,as.numeric(colnames(fres[["naa"]][,,i] *fres[["waa"]][,,i])) <= fin_year]#[,-(1:length(age_class))]                     #[-length(fres[["naa"]][1,,i])][-(1:length(age_class))]
      
      
      ffdata<- data.frame(year = fres$summary$year,
                          FFmsy_vpa = fres$summary$Fratio,
                          id = i)
      
      ffdata <- ffdata[1:(nrow(ffdata)-1),]
      
      FFmsy_vpa[[i]]  <- ffdata 
      
      list(
        fit_spict   = fit_spict[[i]],
        SB_vpa      = SB_vpa[[i]],
        EB_vpa      = EB_vpa[[i]],
        TB_vpa      = TB_vpa[[i]],
        FFmsy_vpa   = FFmsy_vpa[[i]]
      )
      
    }, error = function(e){
      message("Error at rep ", i, ": ", e$message)
      return(NULL)  # エラーが出たら NULL を返す
    })
    
    # res_i が NULL ならスキップ
    if(is.null(res_i)) next
    
    # 成功した場合は代入
    fit_spict[[i]]  <- res_i$fit_spict
    SB_vpa[[i]]     <- res_i$SB_vpa
    EB_vpa[[i]]     <- res_i$EB_vpa
    TB_vpa[[i]]     <- res_i$TB_vpa
    FFmsy_vpa[[i]]  <- res_i$FFmsy_vpa
  }
  
  
  
  
  ### 以降は100回分の計算結果を用いた計算のはず
  
  # VPAの方の値を
  SB_vpa_sums <- lapply(SB_vpa, colSums)
  EB_vpa_sums <- lapply(EB_vpa, colSums)
  TB_vpa_sums <- lapply(TB_vpa, colSums)
  
  res_SB_vpa <- as.data.frame(do.call(rbind, SB_vpa_sums)) |>
    mutate(id = row_number()) %>%  # シミュレーション番号を追加
    pivot_longer(cols = -id, names_to = "year", values_to = "SB_vpa")
  res_EB_vpa <- as.data.frame(do.call(rbind, EB_vpa_sums)) |>
    mutate(id = row_number()) %>%  # シミュレーション番号を追加
    pivot_longer(cols = -id, names_to = "year", values_to = "EB_vpa")
  res_TB_vpa <- as.data.frame(do.call(rbind, TB_vpa_sums)) |>
    mutate(id = row_number()) %>%  # シミュレーション番号を追加
    pivot_longer(cols = -id, names_to = "year", values_to = "TB_vpa")
  
  
  res_FFmsy_vpa <-bind_rows(FFmsy_vpa)
  
  #ABC = Fmsy*Biomass
  Fmsy_age <- true_MSY[["Fvector"]][1,]
  
  SB_rec <- lapply(SB_vpa, function(x) x[, ncol(x)-1])
  SB_rec_df <- as.data.frame(do.call(cbind, SB_rec))
  colnames(SB_rec_df) <- paste0("", seq_along(SB_vpa))
  ABC_SB_vpa <- colSums(SB_rec_df*t(Fmsy_age))
  
  EB_rec <- lapply(EB_vpa, function(x) x[, ncol(x)-1])
  EB_rec_df <- as.data.frame(do.call(cbind, EB_rec))
  colnames(EB_rec_df) <- paste0("", seq_along(EB_vpa))
  ABC_EB_vpa <- colSums(EB_rec_df*t(Fmsy_age))
  
  TB_rec <- lapply(TB_vpa, function(x) x[, ncol(x)-1])
  TB_rec_df <- as.data.frame(do.call(cbind, TB_rec))
  colnames(TB_rec_df) <- paste0("", seq_along(TB_vpa))
  ABC_TB_vpa <- colSums(TB_rec_df*t(Fmsy_age))
  
  ABC_vpa <- data_frame(id = 1:rep_n,
                        ABC_SB_vpa = ABC_SB_vpa,
                        ABC_EB_vpa = ABC_EB_vpa,
                        ABC_TB_vpa = ABC_TB_vpa, 
                        simulation_label = simulation_label)
  
  
  res_vpa <- left_join(res_SB_vpa, res_EB_vpa)|> left_join(res_TB_vpa) |>
    mutate(r_vpa = r, 
           K_vpa = K, 
           n_vpa = n, 
           SBmsy_vpa =  true_MSY[["summary"]][["SSB"]][1], #Bmsy
           msy_vpa = msy
    )|>
    mutate(BBmsy_vpa = SB_vpa/SBmsy_vpa)|>
    mutate(year = as.numeric(year))|>
    left_join(res_FFmsy_vpa)|>
    mutate(simulation_label = simulation_label)#|>na.omit()
  
  res_spict <- remake_res(fit_spict,dt = "1", simulation_label = simulation_label)
  
  return(list(res_vpa = res_vpa, ABC_vpa = ABC_vpa, res_spict=res_spict, plot =plot , fit_spict_pdHess = fit_spict_pdHess))
  
} #end func



       
#### SR function 関係####
# derive a, b parameters in SR function from h and R0
get.ab.ri <- function(h,R0,biopars){
  SPR0 <- get.SPR0(biopars$M,biopars$maa,biopars$waa)
  S0 <- R0*SPR0
  b <- (log(h)-log(0.2))/0.8/S0
  a <- R0/S0/exp(-b*S0)    
  return(listN2(SPR0,R0,h,S0,a,b))
}

get.ab.bh <- function(h,R0,biopars){
  SPR0 <- get.SPR0(biopars$M,biopars$maa,biopars$waa)
  S0 <- R0*SPR0    
  beta <- (5*h-1)/(4*h*R0)
  alpha <- SPR0*(1-h)/(4*h)
  a <- 1/alpha
  b <- beta/alpha
  return(listN2(SPR0,R0,h,S0,a,b))
}

get.ab.hs <- function(h,R0,biopars){
  SPR0 <- get.SPR0(biopars$M,biopars$maa,biopars$waa)
  S0 <- R0*SPR0
  b <- S0 * (1-h)
  a <- R0/b
  return(listN2(SPR0,R0,h,S0,a,b))
}

get.SPR0 <- function(M,maa,waa,output="simple"){
  nage <- length(M)
  S <- exp(-M)
  N <- numeric()
  N[1] <- 1
  for(i in 2:(nage-1)) N[i] <- N[i-1]*S[i-1]
  N[nage] <- N[nage-1] * S[nage]/(1-S[nage])
  SPR0 <- sum(N * maa * waa)
  if(output=="simple") return(SPR0) else return(listN2(N,SPR0))
}

listN2 <- function(...){
  dots <- list(...)
  inferred <- sapply(substitute(list(...)), function(x) deparse(x)[1])[-1]
  if(is.null(names(inferred))){
    names(dots) <- inferred
  } else {
    names(dots)[names(inferred) == ""] <- inferred[names(inferred) == ""]
  }
  dots
}




remake_res<- function(data,dt, simulation_label =simulation_label) {
  map_dfr(data, function(x) {
    tryCatch(
      get_spict_res(x),
      error = function(e) {
        # エラーが発生した場合は空のデータフレームを返す
        tibble()
      }
    )
  }, .id = "id") %>%
    mutate(dt = dt, simulation = simulation_label)
}



getFFmsy <- function (dres, target.SPR = 30, Fmax = 10, max.age = Inf) {
  dres$ysdata <- matrix(0, ncol(dres$faa), 5)
  dimnames(dres$ysdata) <- list(colnames(dres$faa), c("perSPR", 
                                                      "YPR", "SPR", "SPR0", "F/Ftarget"))
  for (i in 1:ncol(dres$faa)) {
    dres$Fc.at.age <- dres[["faa"]][,,1][, i]
    
    if (!all(dres$Fc.at.age == 0, na.rm = T)) {
      byear <- colnames(dres$faa)[i]
      a <- ref.F(res = NULL,
                 Fcurrent = dres[["faa"]][,,1][, i],
                 waa = dres[["waa"]][,,1][, i], 
                 maa = dres[["maa"]][,,1][,i], 
                 M = dres[["input"]][["tmb_data"]][["M_mat"]][,,1][, i], 
                 waa.catch = dres[["wcaa"]][,,1][,i],
                 pSPR = target.SPR,
                 Pope = TRUE, plot = FALSE)
      dres$ysdata[i, 1:2] <- (as.numeric(rev(a$ypr.spr[which(a$ypr.spr$Frange2Fcurrent == 
                                                               1)[1], 2:3])))
      dres$ysdata[i, 3] <- a$spr0 * dres$ysdata[i, 1]/100
      dres$ysdata[i, 4] <- a$spr0
      dres$ysdata[i, 5] <- 1/a$summary[3, grep("SPR", 
                                               colnames(a$summary))][1]
    }
    else {
      break
    }
  }
  dres$ysdata <- as.data.frame(dres$ysdata)
  dres$target.SPR <- target.SPR
  return(dres)
}


