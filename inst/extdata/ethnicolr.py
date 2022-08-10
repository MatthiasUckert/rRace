import os 
import pandas as pd 
import ethnicolr

def race_eth(tab, methods = ["ce-l", "fl-f", "fl-l"]):
    
    # ce-l: census data, prediction on last name ------------------------------------------------
    if "ce-l" in methods:
        df1 = ethnicolr.pred_census_ln(df=tab, namecol="last_name")
        cols = {'api': 'pasian', 'black': 'pblack', 'hispanic': 'phispa', 'white': 'pwhite'}
        df1.rename(columns=cols, inplace=True)
        df1["method"]="ece-1-0-0-0-0-N"
    else:
        df1=pd.DataFrame()
    
    # fl-f: florida data, prediction on full name -----------------------------------------------
    if "fl-f" in methods:
        df2 = ethnicolr.pred_fl_reg_name(df=tab, lname_col="last_name", fname_col="first_name")
        cols={'asian': 'pasian', 'nh_black': 'pblack', 'hispanic': 'phispa', 'nh_white': 'pwhite'}
        df2.rename(columns=cols, inplace=True)
        df2["method"]="efl-1-1-0-0-0-N"
    else:
        df2=pd.DataFrame()
    
    # fl-l: florida data, prediction on last name -----------------------------------------------
    if "fl-l" in methods:
        df3 = ethnicolr.pred_fl_reg_ln(df=tab, namecol="last_name")
        cols={'asian': 'pasian', 'nh_black': 'pblack', 'hispanic': 'phispa', 'nh_white': 'pwhite'}
        df3.rename(columns=cols, inplace=True)
        df3["method"]="efl-1-1-0-0-0-N"
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
    race_old=["hispanic", "nh_white", "nh_black"]
    race_new=["hispa", "white", "black"]
    df_out["race"]=df_out["race"].replace(race_old, race_new)
    
    # return output -----------------------------------------------------------------------------
    return df_out
