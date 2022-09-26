
# From \\nwctantalus.nmfs.local\jwallace\h_jwallace\WCGBTS_JV_3_P VAST runs used in the paper.

options(width = 140)

for (iii in 1:13) {
   
   SpFolders <- JRWToolBox::scanIn("
                    SP                       Folder
                          
                   ARTH       2020-06-07_ARTH_DV_WCGBTS_LM22_v5_5_0_Rho=3_AS_P_nx=600
				   DBRK       2020-06-07_DBRK_DV_WCGBTS_LM15_v5_5_0_Rho=3_AS_P_nx=600
				   DOVR       2020-06-07_DOVR_DV_WCGBTS_LM17_v5_5_0_Rho=3_AS_P_nx=600
				   EGLS       2020-06-07_EGLS_DV_WCGBTS_LM16_v5_5_0_Rho=3_AS_P_nx=600
				   GREN       2020-06-07_GREN_DV_WCGBTS_LM3_v5_5_0_Rho=3_AS_P_nx=600
				   LCOD       2020-06-07_LCOD_DV_WCGBTS_LM25_v5_5_0_Rho=3_AS_P_nx=600
				   LSPN       2020-06-07_LSPN_DV_WCGBTS_LM7_v5_5_0_Rho=3_AS_P_nx=600
				   PDAB       2020-06-07_PDAB_DV_WCGBTS_LM13_v5_5_0_Rho=3_AS_P_nx=600
				   PTRL       2020-06-07_PTRL_DV_WCGBTS_LM21_v5_5_0_Rho=3_AS_P_nx=600
				   PWHT       2020-06-07_PWHT_DV_WCGBTS_LM26_v5_5_0_Rho=3_AS_P_nx=600
				   SABL       2020-06-07_SABL_DV_WCGBTS_LM29_v5_5_0_Rho=3_AS_P_nx=600
				   SNOS       2020-06-07_SNOS_DV_WCGBTS_LM10_v5_5_0_Rho=3_AS_P_nx=600
				   SSPN       2020-06-08_SSPN_DV_WCGBTS_LM8_v5_5_0_Rho=3_AS_P_nx=600
    ") 

   cat("\n", SpFolders[iii, 2], "\n")
   base::load(paste0(SpFolders[iii, 2], '/Image_', SpFolders[iii, 1], '_P.RData'))
   assign(paste0(SpFolders[iii, 1], '_SP.Results.Dpth'), SP.Results.Dpth)
   cat("\n", paste0(SpFolders[iii, 1], '_SP.Results.Dpth'), "\n")
   save(list = paste0(SpFolders[iii, 1], '_SP.Results.Dpth'), file = paste0('WCGBTS_JV_', SpFolders[iii, 1], '_SP.Results.Dpth 13 May 2022.RData'))
   rm(list = base::ls(all= TRUE))  
}



 SpFolders <- JRWToolBox::scanIn("
                    SP                       Folder
                          
                   ARTH       2020-06-07_ARTH_DV_WCGBTS_LM22_v5_5_0_Rho=3_AS_P_nx=600
				   DBRK       2020-06-07_DBRK_DV_WCGBTS_LM15_v5_5_0_Rho=3_AS_P_nx=600
				   DOVR       2020-06-07_DOVR_DV_WCGBTS_LM17_v5_5_0_Rho=3_AS_P_nx=600
				   EGLS       2020-06-07_EGLS_DV_WCGBTS_LM16_v5_5_0_Rho=3_AS_P_nx=600
				   GREN       2020-06-07_GREN_DV_WCGBTS_LM3_v5_5_0_Rho=3_AS_P_nx=600
				   LCOD       2020-06-07_LCOD_DV_WCGBTS_LM25_v5_5_0_Rho=3_AS_P_nx=600
				   LSPN       2020-06-07_LSPN_DV_WCGBTS_LM7_v5_5_0_Rho=3_AS_P_nx=600
				   PDAB       2020-06-07_PDAB_DV_WCGBTS_LM13_v5_5_0_Rho=3_AS_P_nx=600
				   PTRL       2020-06-07_PTRL_DV_WCGBTS_LM21_v5_5_0_Rho=3_AS_P_nx=600
				   PWHT       2020-06-07_PWHT_DV_WCGBTS_LM26_v5_5_0_Rho=3_AS_P_nx=600
				   SABL       2020-06-07_SABL_DV_WCGBTS_LM29_v5_5_0_Rho=3_AS_P_nx=600
				   SNOS       2020-06-07_SNOS_DV_WCGBTS_LM10_v5_5_0_Rho=3_AS_P_nx=600
				   SSPN       2020-06-08_SSPN_DV_WCGBTS_LM8_v5_5_0_Rho=3_AS_P_nx=600
    ") 
 

for(iii in 1:13) {

  load(paste0('WCGBTS_JV_', SpFolders[iii, 1], '_SP.Results.Dpth 13 May 2022.RData'))
}


rm(iii, SpFolders)

save(list = base::ls(), file = 'WCGBTS_JV_All_SP.Results.Dpth 13 May 2022.RData')






