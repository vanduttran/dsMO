dsMO: non-disclosive federated Multi-Omics analysis

The dsMO package suit, providing a number of non-disclosive federated Multi-Omics analyses, is comprised of three packages:

1. dsMOprimal, to install on remote servers with data contributing to the virtual cohort

2. dsMOdual, to install on a remote server different from the previous ones  

3. dsMO, to install on the analyst side


** INSTALL

- devtools::install_github('vanduttran/dsMO')


** EXAMPLE

- login data
    logindata <- read.table('logindata_BEAt.txt', header=T)
  logindata contains login credentials, which can be used to execute as it is.
  Otherwise, to prompt for login credentials:
    logindata <- logindata[, -c(grep('user|password', colnames(logindata)))]

- function for data preprocessing: create raw data matrices 
  -- One data block: for federated PCA
    dataProc.SingleOmics <- function(conns, symbol) {
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

  -- Two data blocks: for federated RCCA, ComDim, SNF
    dataProc.BiOmics <- function(conns, symbol) {
        require(dsSwissKnifeClient)
        lapply(conns, function(conn) {
            DSI::datashield.assign(conn, 'tmp', paste0(conn@name, '.cnsim'), 
                                   variables=c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS'), async=T)
        })
        dssSubset(symbol='tmp', what='tmp', row.filter='complete.cases(tmp)', datasources=conns)
        dssSubset(symbol='tmp', what='tmp', row.filter='!is.na(LAB_TSC) & as.numeric(as.character(LAB_TSC)) < 4', datasources=conns)
        rns <- datashield.aggregate(conns, as.symbol('rowNames(tmp)'), async=T)
        lapply(names(conns), function(opn) {
            datashield.assign(conns[opn], 'tmp', 
                              as.call(list(as.symbol("setRowNames"),
                                           as.symbol("tmp"),
                                           dsdsSwissKnifeClient:::.encode.arg(rns[[opn]]))),
                              async=T)
        })
        dssSubset(symbol=symbol[1], what='tmp', col.filter="c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL')", datasources=conns)
        dssSubset(symbol=symbol[2], what='tmp', col.filter="c('LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS')", datasources=conns)
    }

- run exec on different analyses
    res.pca <- exec('federatePCA', loginFD=logindata[1,], logins=logindata[1:2,], func=dataProc.SingleOmics, symbol='rawDataX')

    res.rcca <- exec('federateRCCA', loginFD=logindata[1,], logins=logindata[1:2,], func=dataProc, symbol=c('rawDataX', 'rawDataY'),
        0.001, 0.001, TRUE, 
        dsMO:::.encode.arg(list(nfold = 2, grid1 = seq(0.001, 1, length = 1), grid2 = seq(0.001, 1, length = 1))))
    
    res.comdim <- exec('federateComDim', loginFD=logindata[3,], logins=logindata[1:2,], func=dataProc.BiOmics, symbol=c('rawDataX', 'rawDataY'),
        2, 'none', 'uniform')

    res.snf <- exec('federateSNF', loginFD=logindata[3,], logins=logindata[1:2,], func=dataProc.BiOmics, symbol=c('rawDataX', 'rawDataY'))

