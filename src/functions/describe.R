describe <- function(data_table, numeric_vars= "", categoric_vars= "", text_vars= "", date_vars= "", id_vars= "", n_top_cat= 5, n_top_id= 3) {
  
  # VERSION: 14-10-2021
  
  dt <- copy(data_table)
  nrows <- nrow(dt)
  summary <- data.table()
  
  for(var in colnames(dt)){
    
    # numeric vars ####
    # Describe numeric variable (var):
    if(var %in% numeric_vars){
      dt[, (var) := as.numeric(get(var))]
      num_dt <- data.table(Variable= colnames(dt[, ..var]), 
                           Type= 'Numeric',
                           Total= nrow(dt[, ..var]),
                           Missing= colSums(is.na(dt[, ..var])), 
                           Perc_Missing= round(colSums(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                           Unique= sapply(X = dt[, ..var], FUN = uniqueN), 
                           Perc_Uniques= round(sapply(X = dt[, ..var], FUN = uniqueN)/nrows*100, digits = 1), 
                           Levels= '',
                           N_or_median= sapply(X = dt[, ..var], FUN = median, na.rm= T), 
                           Perc_or_IQR= sapply(X = dt[, ..var], FUN = function(x) paste0(round(quantile(x, prob = 0.25, na.rm = T),2),
                                                                                         " - ", 
                                                                                         round(quantile(x, prob = 0.75, na.rm = T),2))),
                           Min_Max= sapply(X= dt[, ..var], FUN = function(x) paste0(min(x, na.rm = T),' - ', max(x, na.rm = T)))  )
      
      # append to summary. Need: All columns must be coerced to character.
      num_dt[, (colnames(num_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(num_dt)]
      summary <- rbind(summary, num_dt, fill = TRUE)
    }
    
    # categoric vars ####
    # Describe categoric variables.
    if(var %in% categoric_vars) {
      dt[, (var) := as.factor(get(var))]
      n_levels <- nlevels(dt[, get(var)])
      
      if(all(is.na(dt[, ..var]))) {
        cat_dt <- data.table(Variable= var,
                             Type= 'Categoric',
                             Total= nrow(dt[, ..var]),
                             Missing= sum(is.na(dt[, ..var])), 
                             Perc_Missing= round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                             Unique= 1, 
                             Perc_Uniques= 100,
                             Levels= dt[, .N, by= var][order(-N)][1:n_levels, get(var)],
                             N_or_median= dt[, .N, by= var][order(-N)][1:n_levels, N],
                             Perc_or_IQR= round(dt[, .N, by= var][order(-N)][1:n_levels, N]/nrows*100, digits = 1))
        cat_dt[, (colnames(cat_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(cat_dt)]
        summary <- rbind(summary, cat_dt, fill= TRUE)
        
        
      } else {
        if (n_levels <= n_top_cat){
          cat_dt <- data.table(Variable= c(var, rep(x = NA, times= n_levels-1)),
                               Type= c('Categoric', rep(x = NA, times= n_levels-1)),
                               Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_levels-1)),
                               Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_levels-1)), 
                               Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)), 
                               Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_levels-1)), 
                               Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)),
                               Levels= dt[, .N, by= var][order(-N)][1:n_levels, get(var)],
                               N_or_median= dt[, .N, by= var][order(-N)][1:n_levels, N],
                               Perc_or_IQR= round(dt[, .N, by= var][order(-N)][1:n_levels, N]/nrows*100, digits = 1))
        } else {
          cat_dt <- data.table(Variable= c(var, rep(x = NA, times= n_top_cat)),
                               Type= c('Categoric', rep(x = NA, times= n_top_cat)),
                               Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_top_cat)),
                               Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_top_cat)), 
                               Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_top_cat)), 
                               Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_top_cat)), 
                               Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_top_cat)),
                               Levels= c(as.character(dt[, .N, by= var][order(-N)][1:n_top_cat, get(var)]), "The rest"),
                               N_or_median= c(dt[, .N, by= var][order(-N)][1:n_top_cat, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_cat, N])),
                               Perc_or_IQR= round(c(dt[, .N, by= var][order(-N)][1:n_top_cat, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_cat, N]))/nrows*100, digits = 1))
        }
        cat_dt[, (colnames(cat_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(cat_dt)]
        summary <- rbind(summary, cat_dt, fill= TRUE)
      }
    }
    
    # date vars ####
    # Describe Date variables.
    if(var %in% date_vars) {
      if(all(is.na(dt[, ..var]))) {
        date_dt <- data.table(Variable= colnames(dt[, ..var]), 
                              Type= 'Date',
                              Total= nrow(dt[, ..var]),
                              Missing= colSums(is.na(dt[, ..var])), 
                              Perc_Missing= round(colSums(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                              Unique= sapply(X = dt[, ..var], FUN = uniqueN), 
                              Perc_Uniques= round(sapply(X = dt[, ..var], FUN = uniqueN)/nrows*100, digits = 1), 
                              Levels= '',
                              N_or_median= NA, 
                              Perc_or_IQR= NA)
        date_dt[, (colnames(date_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(date_dt)]
        summary <- rbind(summary, date_dt, fill= TRUE)
        
      } else {
      tryCatch(
        expr = {
          dt[, (var) := as.Date(get(var), format = '%Y-%m-%d', origin= '1970-01-01')]
          date_dt <- data.table(Variable= colnames(dt[, ..var]), 
                                Type= 'Date',
                                Total= nrow(dt[, ..var]),
                                Missing= colSums(is.na(dt[, ..var])), 
                                Perc_Missing= round(colSums(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                                Unique= sapply(X = dt[, ..var], FUN = uniqueN), 
                                Perc_Uniques= round(sapply(X = dt[, ..var], FUN = uniqueN)/nrows*100, digits = 1), 
                                Levels= '',
                                N_or_median= as.Date(sapply(X = dt[, ..var], FUN = function(x) median(x, na.rm= T)), origin= '1970-01-01'), 
                                Perc_or_IQR= sapply(X = dt[, ..var], FUN = function(x) paste0(as.Date(quantile(as.numeric(x), prob = 0.25, na.rm = T), origin= '1970-01-01'),
                                                                                              " - ",
                                                                                              as.Date(quantile(as.numeric(x), prob = 0.75, na.rm = T), origin= '1970-01-01'))),
                                Min_Max= sapply(X= dt[, ..var], FUN = function(x) paste0(min(x, na.rm = T),' - ', max(x, na.rm = T))) )
          date_dt[, (colnames(date_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(date_dt)]
          summary <- rbind(summary, date_dt, fill= TRUE)
        },
        error = function(e){ 
          message(c('Error a:  ', var, ' probad con forzar el tipo de dato a Date con formato "%Y-%m-%d" previamente.'))
          print(e)
          date_dt <- data.table(Variable= colnames(dt[, ..var]), 
                                Type= 'Date',
                                Total= nrow(dt[, ..var]),
                                Missing= colSums(is.na(dt[, ..var])), 
                                Perc_Missing= round(colSums(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                                Unique= sapply(X = dt[, ..var], FUN = uniqueN), 
                                Perc_Uniques= round(sapply(X = dt[, ..var], FUN = uniqueN)/nrows*100, digits = 1), 
                                Levels= '',
                                N_or_median= NA, 
                                Perc_or_IQR= NA,
                                Min_Max= NA )
          date_dt[, (colnames(date_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(date_dt)]
          summary <- rbind(summary, date_dt, fill= TRUE)
        },
        warning = function(w){
          message('Warning a:   ', var, ' probad con forzar el tipo de dato a Date con formato "%Y-%m-%d" previamente.')
          print(w)
          date_dt <- data.table(Variable= colnames(dt[, ..var]), 
                                Type= 'Date',
                                Total= nrow(dt[, ..var]),
                                Missing= colSums(is.na(dt[, ..var])), 
                                Perc_Missing= round(colSums(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                                Unique= sapply(X = dt[, ..var], FUN = uniqueN), 
                                Perc_Uniques= round(sapply(X = dt[, ..var], FUN = uniqueN)/nrows*100, digits = 1), 
                                Levels= '',
                                N_or_median= NA, 
                                Perc_or_IQR= NA,
                                Min_Max= NA )
          date_dt[, (colnames(date_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(date_dt)]
          summary <- rbind(summary, date_dt, fill= TRUE)
        })
      }
    }
    
    # text vars ####
    # Describe Text variables.
    if(var %in% text_vars) {
      dt[get(var) == '', (var) := NA]
      dt[, (var) := as.factor(get(var))]
      n_levels <- nlevels(dt[, get(var)])
      if (n_levels <= n_top_cat){
        text_dt <- data.table(Variable= c(var, rep(x = NA, times= n_levels-1)),
                              Type= c('Text', rep(x = NA, times= n_levels-1)),
                              Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_levels-1)),
                              Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_levels-1)), 
                              Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)), 
                              Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_levels-1)), 
                              Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)),
                              Levels= dt[, .N, by= var][order(-N)][1:n_levels, get(var)],
                              N_or_median= dt[, .N, by= var][order(-N)][1:n_levels, N],
                              Perc_or_IQR= round(dt[, .N, by= var][order(-N)][1:n_levels, N]/nrows*100, digits = 1))
      } else {
        text_dt <- data.table(Variable= c(var, rep(x = NA, times= n_top_cat)),
                              Type= c('Text', rep(x = NA, times= n_top_cat)),
                              Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_top_cat)),
                              Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_top_cat)), 
                              Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_top_cat)), 
                              Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_top_cat)), 
                              Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_top_cat)),
                              Levels= c(as.character(dt[, .N, by= var][order(-N)][1:n_top_cat, get(var)]), "The rest"),
                              N_or_median= c(dt[, .N, by= var][order(-N)][1:n_top_cat, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_cat, N])),
                              Perc_or_IQR= round(c(dt[, .N, by= var][order(-N)][1:n_top_cat, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_cat, N]))/nrows*100, digits = 1))
      }
      text_dt[, (colnames(text_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(text_dt)]
      summary <- rbind(summary, text_dt, fill= TRUE)
    }
    
    # id vars ####
    # Describe ID variables.
    # identical as categorical vars but different n_levels.
    if(var %in% id_vars) {
      dt[, (var) := as.factor(get(var))]
      n_levels <- nlevels(dt[, get(var)])
      if(all(is.na(dt[, ..var]))) {
        id_dt <- data.table(Variable= var,
                             Type= 'id',
                             Total= nrow(dt[, ..var]),
                             Missing= sum(is.na(dt[, ..var])), 
                             Perc_Missing= round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), 
                             Unique= 1, 
                             Perc_Uniques= 100,
                             Levels= dt[, .N, by= var][order(-N)][1:n_levels, get(var)],
                             N_or_median= dt[, .N, by= var][order(-N)][1:n_levels, N],
                             Perc_or_IQR= round(dt[, .N, by= var][order(-N)][1:n_levels, N]/nrows*100, digits = 1))
        id_dt[, (colnames(id_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(id_dt)]
        summary <- rbind(summary, id_dt, fill= TRUE)
        
        
      } else {
      if (n_levels <= n_top_id){
        id_dt <- data.table(Variable= c(var, rep(x = NA, times= n_levels-1)),
                             Type= c('id', rep(x = NA, times= n_levels-1)),
                             Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_levels-1)),
                             Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_levels-1)), 
                             Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)), 
                             Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_levels-1)), 
                             Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_levels-1)),
                             Levels= dt[, .N, by= var][order(-N)][1:n_levels, get(var)],
                             N_or_median= dt[, .N, by= var][order(-N)][1:n_levels, N],
                             Perc_or_IQR= round(dt[, .N, by= var][order(-N)][1:n_levels, N]/nrows*100, digits = 1))
      } else {
        id_dt <- data.table(Variable= c(var, rep(x = NA, times= n_top_id)),
                             Type= c('id', rep(x = NA, times= n_top_id)),
                             Total= c(nrow(dt[, ..var]), rep(x = NA, times= n_top_id)),
                             Missing= c(sum(is.na(dt[, ..var])), rep(x = NA, times= n_top_id)), 
                             Perc_Missing= c(round(sum(is.na(dt[, ..var]))/nrows*100, digits = 1), rep(x = NA, times= n_top_id)), 
                             Unique= c(dt[!is.na(get(var)), uniqueN(get(var))], rep(x = NA, times= n_top_id)), 
                             Perc_Uniques= c(round(dt[!is.na(get(var)), uniqueN(get(var))]/nrows*100, digits = 1), rep(x = NA, times= n_top_id)),
                             Levels= c(as.character(dt[, .N, by= var][order(-N)][1:n_top_id, get(var)]), "The rest"),
                             N_or_median= c(dt[, .N, by= var][order(-N)][1:n_top_id, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_id, N])),
                             Perc_or_IQR= round(c(dt[, .N, by= var][order(-N)][1:n_top_id, N], nrows-sum(dt[, .N, by= var][order(-N)][1:n_top_id, N]))/nrows*100, digits = 1))
      }
      id_dt[, (colnames(id_dt)) := lapply(X = .SD, as.character), .SDcols= colnames(id_dt)]
      summary <- rbind(summary, id_dt, fill= TRUE)
      }
    }
  }
  summary[is.na(Levels), Levels := 'NAs' ]
  return(summary)
}

