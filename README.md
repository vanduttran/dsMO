dsMO: non-disclosive federated Multi-Omics analysis

The dsMO package suit, providing a number of non-disclosive federated Multi-Omics analyses,
is comprised of three packages:

1. dsMOprimal, to install on remote servers with data contributing to the virtual cohort

2. dsMOdual, to install on a remote server different from the previous ones  

3. dsMO, to install on the analyst side

- devtools::install_github('vanduttran/dsMO')

- login data
    logins <- read.table('logindata_BEAt.txt', header=T)
    logindata <- logins[, -c(grep('user|password', colnames(logins)))]

- function for data preprocessing: create raw data matrices 
    dataProc <- function(conns, symbol) {
        require(dsSwissKnifeClient)
        lapply(conns, function(conn) {
            DSI::datashield.assign(conn, 'tmp', paste0(conn@name, '.cnsim'), 
                                   variables=c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS'), async=T)
        })
        dssSubset(symbol='tmp', what='tmp', row.filter='complete.cases(tmp)', datasources=conns)
        dssSubset(symbol='tmp', what='tmp', row.filter='!is.na(LAB_TSC) & as.numeric(as.character(LAB_TSC)) < 4', datasources=conns)
        
        rns <- datashield.aggregate(conns, as.symbol('rowNames(tmp)'), async=T)
        lapply(names(conns), function(opn) {
            datashield.assign(conns[opn], symbol, 
                              as.call(list(as.symbol("setRowNames"),
                                           as.symbol("tmp"),
                                           dsSwissKnifeClient:::.encode.arg(rns[[opn]]))),
                              async=T)
        })
    }

