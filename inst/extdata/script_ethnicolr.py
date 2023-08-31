import os 
import pandas as pd 
import ethnicolr

def race_eth(tab, methods = ["CEL", "FLF", "FLL"]):
    
    # CEL: CENSUS DATA, prediction on LAST NAME ------------------------------------------
    if "CEL" in methods:
        df1 = ethnicolr.pred_census_ln(df=tab, namecol="last_name")
        cols = {'api': 'pasian', 'black': 'pblack', 'hispanic': 'phispa', 'white': 'pwhite'}
        df1.rename(columns=cols, inplace=True)
        df1["method"]="CEL-1-0-0-0-0-N"
    else:
        df1=pd.DataFrame()
    
    # FLF: FLORIDA DATA, prediction on FULL NAME -----------------------------------------
    if "FLF" in methods:
        df2 = ethnicolr.pred_fl_reg_name(df=tab, lname_col="last_name", fname_col="first_name")
        cols={'asian': 'pasian', 'nh_black': 'pblack', 'hispanic': 'phispa', 'nh_white': 'pwhite'}
        df2.rename(columns=cols, inplace=True)
        df2["method"]="FLF-1-1-0-0-0-N"
    else:
        df2=pd.DataFrame()
    
    # FLL: FLORIDA DATA, prediction on LAST NAME -----------------------------------------
    if "FLL" in methods:
        df3 = ethnicolr.pred_fl_reg_ln(df=tab, namecol="last_name")
        cols={'asian': 'pasian', 'nh_black': 'pblack', 'hispanic': 'phispa', 'nh_white': 'pwhite'}
        df3.rename(columns=cols, inplace=True)
        df3["method"]="FLL-1-1-0-0-0-N"
    else:
        df3=pd.DataFrame()
  
    # combine dataframes to single dataframe ----------------------------------------------------
    df_out=pd.concat([df1, df2, df3], ignore_index=True)
    df_out["pother"]=0
    
    # select and reorder columns ----------------------------------------------------------------
    cols_old=list(tab.columns.values)
    cols_old.remove("__name")
    cols_new=["method", "pasian", "pblack", "phispa", "pwhite", "pother", "race"]
    df_out=df_out[cols_old+cols_new]
    
    # recode race values ------------------------------------------------------------------------
    race_old=["hispanic", "nh_white", "nh_black", "api"]
    race_new=["hispa", "white", "black", "asian"]
    df_out["race"]=df_out["race"].replace(race_old, race_new)
    
    # return output -----------------------------------------------------------------------------
    return df_out
