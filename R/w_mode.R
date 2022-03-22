`w_mode` <- function(x, wt = NULL) {
    tbl <- w_table(x, wt = wt)
    wm <- which.max(tbl)
    
    fmode <- names(tbl[wm])
    if (admisc::possibleNumeric(fmode)) {
        fmode <- admisc::asNumeric(fmode)
    }

    if (length(tbl[tbl == max(tbl)]) > 1) {
        message("Multiple modes detected, only the first is returned.")
    }
    
    return(fmode)
}
