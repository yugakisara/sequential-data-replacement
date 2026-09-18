##Input ----

library(tidyverse)
library(spict)
library(frasyr)
library(frapmr)
set.seed(202609)

# Example
res_vpa <-res_vpa_example2
vpa_org0 <- res_vpa # output of frasyr::vpa()
# 年齢情報
age <- as.numeric(rownames(vpa_org0$naa))
n_age <- length(age)
final_year <- as.numeric(names(vpa_org0[["naa"]])[length(names(vpa_org0[["naa"]]))])

# 関数
source("./src/functions.r")

# Index data
# Index_data <-data.frame(
#   year = 2010:2017,
#   Index =  exp(seq(log(3), log(1), length.out = 8))
# )



Fdata <- data.frame(
  year = 1988:2017,
  F_Fmsy = c(seq(0.2, 2.0, by = 0.1), seq(2.0, 1.0, by = -0.1)) # 真のF/Fmsyを当てる必要がある
)



# 実行する魚種
species_name <- "fish_name"
# シミュレーションの名前
sim_name <- "ave_five"

# 再生産関係の情報（ダミーで入れている。真の結果を入れる必要がある）
SR <- "RI" # available("RI,"BH", HS")
parm_a  <- 0.05
parm_b <-  2.5e-05
parm_sd <- 0.3
parm_rho <- 0
    
# 再生産関係
a <- parm_a ; b <- parm_b; sd_real <- parm_sd; rho <- parm_rho
SR_org0 <- list()
SR_org0$input$SR <- SR
SR_org0$pars <- list(a=a, b=b, sd=sd_real, rho=rho)
    
# 前情報
n_Index <- length(Index_data$Index)
age_class <- age
    
    
##simulation ----
    
    
    rep_n <- 5
    fuc_save <- TRUE
    
    

    if(n_Index > 0){
      sim0  <- sim_org ( Control_data = FALSE, #Index; S0
                         
                         Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                         Control_Rec_sd = FALSE, #S0,S1
                         Control_Rec_rho = FALSE, #S0,S1
                         
                         Control_Selx = FALSE, #Slex : org -> mean.saa # S0,S1,S4
                         Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                         
                         Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                         
                         nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                         
                         Prior = FALSE,
                         prior_sd = 0.5, 
                         Prior_K = FALSE, # priorにKを使うか
                         
                         nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                         vpa_org = vpa_org0,
                         SR_org = SR_org0,
                         Fdata = Fdata,
                         Index_data = Index_data,
                         simulation_label = "S0_rec",
                         fin_year = final_year,
                         rep_n = rep_n,
                         species_name = species_name,
                         fra = TRUE, #データがエクセルvpaかrvaa
                         
                         rec_selex = TRUE # 過去5年平均
      )
      

      sim0_p  <- sim_org ( Control_data = FALSE, #Index; S0
                           
                           Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                           Control_Rec_sd = FALSE, #S0,S1
                           Control_Rec_rho = FALSE, #S0,S1
                           
                           Control_Selx = FALSE, #Slex : org -> mean.saa # S0,S1,S4
                           Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                           
                           Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                           
                           nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                           
                           Prior = TRUE,
                           prior_sd = 0.5, 
                           Prior_K = FALSE, # priorにKを使うか
                           
                           nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                           vpa_org = vpa_org0,
                           SR_org = SR_org0,
                           Fdata = Fdata,
                           Index_data = Index_data,
                           simulation_label = "S0_p_rec",
                           fin_year = final_year,
                           rep_n = rep_n,
                           species_name = species_name,
                           fra = TRUE, #データがエクセルvpaかrvaa
                           
                           rec_selex = TRUE # 過去5年平均 
      )
      
      sim0_p_K  <- sim_org ( Control_data = FALSE, #Index; S0
                             Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                             Control_Rec_sd = FALSE, #S0,S1
                             Control_Rec_rho = FALSE, #S0,S1
                             Control_Selx = FALSE, #Slex : org -> mean.saa # S0,S1,S4
                             Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                             Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                             nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                             Prior = TRUE,
                             prior_sd = 0.5, 
                             Prior_K = TRUE, # priorにKを使うか
                             nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                             vpa_org = vpa_org0,
                             SR_org = SR_org0,
                             Fdata = Fdata,
                             Index_data = Index_data,
                             simulation_label = "S0_p_K_rec",
                             fin_year = final_year,
                             rep_n = rep_n,
                             species_name = species_name,
                             fra = TRUE, #データがエクセルvpaかrvaa
                             rec_selex = TRUE # 過去5年平均 
      )
      
    }else{
      cat("no Index")
    }
    

    sim1  <- sim_org (Control_data = TRUE, #Index; S0
                      
                      Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                      Control_Rec_sd = FALSE, #S0,S1
                      Control_Rec_rho = FALSE, #S0,S1
                      
                      Control_Selx = FALSE, #Slex : org -> mean.saa # S0,S1,S4
                      Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                      
                      Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                      
                      nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                      
                      Prior = FALSE,
                      prior_sd = 0.5, 
                      Prior_K = FALSE, # priorにKを使うか
                      
                      nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                      vpa_org = vpa_org0,
                      SR_org = SR_org0,
                      Fdata = Fdata,
                      Index_data = Index_data,
                      simulation_label = "S1_rec",
                      fin_year = final_year,
                      rep_n = rep_n,
                      species_name = species_name,
                      fra = TRUE, #データがエクセルvpaかrvaa
                      rec_selex = TRUE )
    
 
    

    sim2  <- sim_org (Control_data = TRUE, #Index; S0
                      
                      Control_Rec  = TRUE, #Rec : sd -> , rho -> 0 #S0,S1
                      Control_Rec_sd = TRUE, #S0,S1
                      Control_Rec_rho = TRUE, #S0,S1
                      
                      Control_Selx = FALSE, #Slex : org -> mean.saa # S0,S1,S4
                      Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                      
                      Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                      
                      nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                      
                      Prior = FALSE,
                      prior_sd = 0.5, 
                      Prior_K = FALSE, # priorにKを使うか
                      
                      nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                      vpa_org = vpa_org0,
                      SR_org = SR_org0,
                      Fdata = Fdata,
                      Index_data = Index_data,
                      simulation_label = "S2_rec",
                      fin_year = final_year,
                      rep_n = rep_n,
                      species_name = species_name,
                      fra = TRUE, #データがエクセルvpaかrvaa
                      
                      rec_selex = TRUE)
    

    #Slex : org -> saa
    sim3  <- sim_org (Control_data = TRUE, #Index; S0
                      
                      Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                      Control_Rec_sd = FALSE, #S0,S1
                      Control_Rec_rho = FALSE, #S0,S1
                      
                      Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                      Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                      
                      Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                      
                      nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                      
                      Prior = FALSE,
                      prior_sd = 0.5, 
                      Prior_K = FALSE, # priorにKを使うか
                      
                      nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                      vpa_org = vpa_org0,
                      SR_org = SR_org0,
                      Fdata = Fdata,
                      Index_data = Index_data,
                      simulation_label = "S3_rec",
                      fin_year = final_year,
                      rep_n = rep_n,
                      species_name = species_name,
                      fra = TRUE, #データがエクセルvpaかrvaa
                      
                      rec_selex = TRUE )
    
    #Slex : org -> saa
    sim3_SB  <- sim_org (Control_data = TRUE, #Index; S0
                         Control_Rec  = FALSE, #Rec : sd -> , rho -> 0 #S0,S1
                         Control_Rec_sd = FALSE, #S0,S1
                         Control_Rec_rho = FALSE, #S0,S1
                         
                         Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                         Control_Selx_SB = TRUE, #SelexがTのち成熟率ベースのものはT
                         
                         Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                         
                         nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                         
                         Prior = FALSE,
                         prior_sd = 0.5, 
                         Prior_K = FALSE, # priorにKを使うか
                         
                         nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                         vpa_org = vpa_org0,
                         SR_org = SR_org0,
                         Fdata = Fdata,
                         Index_data = Index_data,
                         simulation_label = "S3_SB",
                         fin_year = final_year,
                         rep_n = rep_n,
                         species_name = species_name,
                         fra = TRUE, #データがエクセルvpaかrvaa
                         
                         rec_selex = TRUE )
    
    

    #Slex : org -> saa
    #Rec : sd -> 0.25, rho -> 0
    sim4 <- sim_org (Control_data = TRUE, #Index; S0
                     
                     Control_Rec  = TRUE, #Rec : sd -> , rho -> 0 #S0,S1
                     Control_Rec_sd = TRUE, #S0,S1
                     Control_Rec_rho = TRUE, #S0,S1
                     
                     Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                     Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                     
                     Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                     
                     nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                     
                     Prior = FALSE,
                     prior_sd = 0.5, 
                     Prior_K = FALSE, # priorにKを使うか
                     
                     nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                     vpa_org = vpa_org0,
                     SR_org = SR_org0,
                     Fdata = Fdata,
                     Index_data = Index_data,
                     simulation_label = "S4_rec",
                     fin_year = final_year,
                     rep_n = rep_n,
                     species_name = species_name,
                     fra = TRUE, #データがエクセルvpaかrvaa
                     
                     rec_selex = TRUE )
    
    sim4_SB <- sim_org (Control_data = TRUE, #Index; S0
                        
                        Control_Rec  = TRUE, #Rec : sd -> , rho -> 0 #S0,S1
                        Control_Rec_sd = TRUE, #S0,S1
                        Control_Rec_rho = TRUE, #S0,S1
                        
                        Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                        Control_Selx_SB = TRUE, #SelexがTのち成熟率ベースのものはT
                        
                        Control_contrast = FALSE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                        
                        nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                        
                        Prior = FALSE,
                        prior_sd = 0.5, 
                        Prior_K = FALSE, # priorにKを使うか
                        
                        nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                        vpa_org = vpa_org0,
                        SR_org = SR_org0,
                        Fdata = Fdata,
                        Index_data = Index_data,
                        simulation_label = "S4_SB",
                        fin_year = final_year,
                        rep_n = rep_n,
                        species_name = species_name,
                        fra = TRUE, #データがエクセルvpaかrvaa
                        
                        rec_selex = TRUE)
    
    #Slex : org -> saa
    #Rec : sd -> 0.25, rho -> 0
    #Add time
    sim5 <- sim_org (Control_data = TRUE, #Index; S0
                     
                     Control_Rec  = TRUE, #Rec : sd -> , rho -> 0 #S0,S1
                     Control_Rec_sd = TRUE, #S0,S1
                     Control_Rec_rho = TRUE, #S0,S1
                     
                     Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                     Control_Selx_SB = FALSE, #SelexがTのち成熟率ベースのものはT
                     
                     Control_contrast = TRUE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                     
                     nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                     
                     Prior = FALSE,
                     prior_sd = 0.5, 
                     Prior_K = FALSE, # priorにKを使うか
                     
                     nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                     vpa_org = vpa_org0,
                     SR_org = SR_org0,
                     Fdata = Fdata,
                     Index_data = Index_data,
                     simulation_label = "S5_rec",
                     fin_year = final_year,
                     rep_n = rep_n,
                     species_name = species_name,
                     fra = TRUE, #データがエクセルvpaかrvaa
                     
                     rec_selex = TRUE)
    
    
    sim5_SB <- sim_org (Control_data = TRUE, #Index; S0
                        
                        Control_Rec  = TRUE, #Rec : sd -> , rho -> 0 #S0,S1
                        Control_Rec_sd = TRUE, #S0,S1
                        Control_Rec_rho = TRUE, #S0,S1
                        
                        Control_Selx = TRUE, #Slex : org -> mean.saa # S0,S1,S4
                        Control_Selx_SB = TRUE, #SelexがTのち成熟率ベースのものはT
                        
                        Control_contrast = TRUE, # 年数増やすシナリオ、 Control系が全てTRUEの時のみ使用可能
                        
                        nyear_make_future_data =  max(round(Generation.Time(vpa_org0)*20),100),
                        
                        Prior = FALSE,
                        prior_sd = 0.5, 
                        Prior_K = FALSE, # priorにKを使うか
                        
                        nyr = 50, # 伸ばす年数。60年以外の設定は関数の中をいじらないといけない。
                        vpa_org = vpa_org0,
                        SR_org = SR_org0,
                        Fdata = Fdata,
                        Index_data = Index_data,
                        simulation_label = "S5_SB",
                        fin_year = final_year,
                        rep_n = rep_n,
                        species_name = species_name,
                        fra = TRUE, #データがエクセルvpaかrvaa
                        
                        rec_selex = TRUE)
    
    
    
    
    
    if(n_Index > 0){
      plot <- plot_futures(future.list = list(sim0[[4]], 
                                                  sim1[[4]], 
                                                  sim2[[4]], 
                                                  sim3[[4]],
                                                  sim4[[4]]))
      list_sim <- list(sim0, 
                       sim0_p, sim0_p_K,
                       sim1, 
                       sim2, 
                       sim3, sim4, sim5,
                       sim3_SB, sim4_SB, sim5_SB)
    }else{
      plot <- plot_futures(future.list = list(sim1[[4]], 
                                                  sim2[[4]], 
                                                  sim3[[4]], 
                                                  sim4[[4]]))
      list_sim <- list(#sim0, 
        sim1, 
        sim2, 
        sim3, sim4, sim5,
        sim3_SB, sim4_SB, sim5_SB
     )  
    }
    
    
    res_time_series_vpa_list <- list()
    res_all_spict_list <- list()
    res_time_series_list <- list()
    res_others_list <- list()
    df_conv_pd_list <- list()
    
    # 図示のために整形 ----
    
    for (i in seq_along(list_sim)) {
      
      res_sim <- list_sim[[i]]
      
      df_pd <- data.frame(
        id = as.character(seq_along(res_sim[[5]])),
        pdHess = sapply(res_sim[[5]], function(x) x$pdHess)
      )
      
      df_conv <- res_sim[[3]] |> 
        filter(stat == "convergence") |>
        mutate(id = id)|>
        select(id, est, simulation, stat)
      
  
      df_conv_pd <- df_conv |> 
        left_join(df_pd, by = "id")
      
      conv0 <- df_conv_pd|>filter(est == 0 & pdHess == TRUE)
      
      
      res_spict <- res_sim[[3]] |> #na.omit()|>　#na.omitつけると収束してものに加えて、何かしらの推定値がNaNだったものも消える。
        semi_join(conv0, by = c("id","simulation")) |># 収束したものだけ
        mutate(id = as.integer(id))
      
      res_time_series_vpa <- res_sim[[1]] |> na.omit() |> select(id, year,SB_vpa, EB_vpa, TB_vpa, BBmsy_vpa, FFmsy_vpa, simulation_label)|>mutate(simulation = simulation_label)
      res_others_vpa <- res_sim[[1]] |> na.omit() |> select(id, year, r_vpa, K_vpa, n_vpa, SBmsy_vpa, msy_vpa, simulation_label) |>
        filter(year == max(year)) |> select(-year) |> left_join(res_sim[[2]])|>mutate(simulation = simulation_label)
      
      # 時系列結果
      res_B <- res_spict |> 
        filter(stat=="B" & obs_pred=="obs") |> 
        mutate(B_spict = est, B_spict_ll = ll, B_spict_ul = ul) |> 
        select(id, B_spict,B_spict_ll,B_spict_ul, year, simulation)
      
      res_F <- res_spict |> 
        filter(stat == "F" & obs_pred == "obs") |> 
        mutate(F_spict = est, F_spict_ll = ll, F_spict_ul = ul) |> 
        select(id, F_spict, F_spict_ll, F_spict_ul, year, simulation)
      
      res_BBmsy <- res_spict |> 
        filter(stat == "BBmsy" & obs_pred == "obs") |> 
        mutate(BBmsy_spict = est, BBmsy_spict_ll = ll, BBmsy_spict_ul = ul) |> 
        select(id, BBmsy_spict, BBmsy_spict_ll, BBmsy_spict_ul, year, simulation)
      
      res_FFmsy <- res_spict |> 
        filter(stat == "FFmsy" & obs_pred == "obs") |> 
        mutate(FFmsy_spict = est, FFmsy_spict_ll = ll, FFmsy_spict_ul = ul) |> 
        select(id, FFmsy_spict, FFmsy_spict_ll, FFmsy_spict_ul, year, simulation)
      
      res_time_series <- res_B |> left_join(res_F) |> left_join(res_BBmsy) |> left_join(res_FFmsy)|> left_join(res_time_series_vpa)
      
      # 時系列関係ない結果
      res_msy <- res_spict |> 
        filter(stat == "m") |> 
        mutate(msy_spict = est, msy_spict_ll = ll, msy_spict_ul = ul) |> 
        select(id, msy_spict, msy_spict_ll, msy_spict_ul, simulation)
      
      res_r <- res_spict |> 
        filter(stat == "r") |> 
        mutate(r_spict = est, r_spict_ll = ll, r_spict_ul = ul) |> 
        select(id, r_spict, r_spict_ll, r_spict_ul, simulation)
      
      res_K <- res_spict |> 
        filter(stat == "K") |> 
        mutate(K_spict = est, K_spict_ll = ll, K_spict_ul = ul) |> 
        select(id, K_spict, K_spict_ll, K_spict_ul, simulation)
      
      res_n <- res_spict |> 
        filter(stat == "n") |> 
        mutate(n_spict = est, n_spict_ll = ll, n_spict_ul = ul) |> 
        select(id, n_spict, n_spict_ll, n_spict_ul, simulation)
      
      res_sdi <- res_spict |> 
        filter(stat == "sdi") |> 
        mutate(sdi_spict = est, sdi_spict_ll = ll, sdi_spict_ul = ul) |> 
        select(id, sdi_spict, sdi_spict_ll, sdi_spict_ul, simulation)
      
      res_sdb <- res_spict |> 
        filter(stat == "sdb") |> 
        mutate(sdb_spict = est, sdb_spict_ll = ll, sdb_spict_ul = ul) |> 
        select(id, sdb_spict, sdb_spict_ll, sdb_spict_ul, simulation)
      
      res_Bmsy <- res_B |>
        left_join(res_BBmsy, by = c("year", "id"), suffix = c("_B", "_BBmsy")) |>
        filter(year == max(year)) |>
        mutate(Bmsy_spict = B_spict / BBmsy_spict, simulation = simulation_B) |> select(id, Bmsy_spict, simulation)
      
      calc_ABC <- res_F |>
        left_join(res_FFmsy, by = c("year", "id"), suffix = c("_F", "_FFmsy")) |>
        mutate(Fmsy_spict = F_spict / FFmsy_spict, simulation = simulation_F) |> select(id, Fmsy_spict, year, simulation)
      
      res_ABC <- res_B |> left_join(calc_ABC, by = c("year", "id"), suffix = c("_B", "_Fmsy")) |>
        filter(year == max(year)) |> mutate(ABC_spict = B_spict * Fmsy_spict, simulation = simulation_B) |> select(id, ABC_spict, simulation)
      
      res_others <- res_msy |> left_join(res_r) |> left_join(res_K) |> left_join(res_n) |> left_join(res_Bmsy) |> left_join(res_ABC) |> left_join(res_others_vpa)|> left_join(res_sdi) |> left_join(res_sdb)
      
      res_time_series <- res_time_series |> mutate(conv = length(unique(res_time_series$id)) / rep_n * 100)
      res_others <- res_others |> mutate(conv = length(unique(res_others$id)) / rep_n *100)
      
      res_time_series_vpa_list[[i]] <- res_time_series_vpa
      res_all_spict_list[[i]] <- res_spict
      res_time_series_list[[i]] <- res_time_series
      res_others_list[[i]] <- res_others
      df_conv_pd_list[[i]] <- df_conv_pd
      
    }
    
    #結果をまとめる
    res_time_series_vpa <- bind_rows(res_time_series_vpa_list)|>
      mutate(name = species_name)
    res_spict <- bind_rows(res_all_spict_list)|>
      mutate(name = species_name)
    res_time_series <- bind_rows(res_time_series_list)|>
      mutate(name = species_name)
    res_others <- bind_rows(res_others_list)|>
      mutate(name = species_name)
    
    res_conv_pd <- bind_rows(df_conv_pd_list)|>
      mutate(name = species_name)
    
    
    