# the stable signatures are snapshot-guarded / snapshots formals() so a breaking contract change is caught in review

    Code
      names(formals(met_history))
    Output
      [1] "site"       "resolution" "variables"  "from"       "to"        
      [6] "as_of"     
    Code
      names(formals(met_record))
    Output
      [1] "site"      "variables" "from"      "to"        "as_of"    
    Code
      names(formals(met_forecast_archive))
    Output
      [1] "site"       "source"     "issue_from" "issue_to"   "valid_from"
      [6] "valid_to"   "members"   
    Code
      names(formals(met_verification))
    Output
      [1] "site"   "source" "..."   

