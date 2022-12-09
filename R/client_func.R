#' @title Encode function  argument
#' @description Serialize to JSON, then encode base64,
#'  then replace '+', '/' and '=' in the result in order to play nicely with the opal entry.
#'  Used to encode non-scalar function arguments prior to sending to the opal server.
#'  There's a corresponding function in the server package called .decode.arg.
#'  See \code{dsSwissKnifeClient:::.encode.arg}.
#' @param some.object the object to be encoded
#' @return encoded text with offending characters replaced by strings
#' @keywords internal
.encode.arg <- function(some.object, serialize.it = TRUE){
    if(serialize.it){
        encoded <- paste0(RCurl::base64Encode(jsonlite::serializeJSON(some.object)), 'serialized')
    } else {
        encoded <- RCurl::base64Encode(jsonlite::toJSON(some.object, null = 'null'))
    }
    # go fishing for '+', '/' and '=', opal rejects them :
    my.dictionary <- c('\\/' = '-slash-', '\\+' = '-plus-', '\\=' = '-equals-')
    sapply(names(my.dictionary), function(x){
        encoded[1] <<- gsub(x, my.dictionary[x], encoded[1])
    })
    return(paste0(encoded[1],'base64'))
}


#' @title Wrapper of datashield.login
#' @description This function ensures datashield.login to all the servers without error of simultaneously connecting to the same server
#' @param logins A data frame of login information. See \code{datashield.login}
#' @return Object(s) of class DSConnection
#' @import DSI
#' @keywords internal
.login <- function(logins) {
    opals <- list()
    prev.url <- ''
    for (i in login$server) {
        this.url <- logins[logins$server == i,'url']
        if(this.url == prev.url){
            cat(paste0('Waiting a bit to avoid spooking the server at ', unname(this.url), "\n"))
            Sys.sleep(1)
        }
        prev.url <- this.url
        cat(paste0('Connection to ', unname(logins[logins$server == i,'server']),"\n"))
        
        tryCatch({
            opals[i] <- datashield.login(logins[logins$server == i, , drop = FALSE])
        }, error = function(e){
            datashield.logout(opals)
            stop(e)
        })
    }
    return (opals)
}


#' @title Wrapper call function for non-disclosive federated analysis
#' @description This wrapper function is used to call all the federated analysis functions provided in the dsMO package suite.
#' @param name A character string naming the function to be called, among federatePCA, federateRCCA, federateComDim, federateSNF, federateUMAP, federateHdbscan.
#' @param loginFD Login information of the federated server, where the function \code{name} will be executed. 
#' For functions using X'X, such as federatePCA, federateRCCA, this should be a server with dsMOprimal installed. 
#' For functions using XX', such as federateComDim, federateSNF, federateUMAP, federateHdbscan, this should be a server with dsMOdual installed.
#' @param logins Login information of data repositories, where dsMOprimal is installed.
#' @param func Definition of a function for preparation of raw data matrices.
#' @param symbol The symbol provided when calling the function \code{func} for data preparation.
#' @param ... Other arguments of the function \code{name}, preferably in the same order.
#' @import dsSwissKnifeClient keyring
#' @examples 
#' data(logindata)
#' data(procFunc)
#' \donttest{
#' res.pca <- exec('federatePCA', loginFD=logindata[1,], logins=logindata[1:2,], func=procFunc$SingleOmics, symbol='rawDataX')
#' res.rcca <- exec('federateRCCA', loginFD=logindata[1,], logins=logindata[1:2,], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'),
#'       0.001, 0.001, TRUE, 
#'       dsMO:::.encode.arg(list(nfold = 2, grid1 = seq(0.001, 1, length = 1), grid2 = seq(0.001, 1, length = 1))))
#' res.comdim <- exec('federateComDim', loginFD=logindata[3,], logins=logindata[1:2,], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'),
#'       2, 'none', 'uniform')
#' res.snf <- exec('federateSNF', loginFD=logindata[3,], logins=logindata[1:2,], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' res.umap <- exec('federateUMAP', loginFD=logindata[3,], logins=logindata[1:2,], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' res.hdbscan <- exec('federateHdbscan', loginFD=logindata[3,], logins=logindata[1:2,], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' }
#' @export
exec <- function(name, loginFD, logins, func, symbol, ...) {
    ## check arguments
    name <- match.arg(name, choices=c('federatePCA', 'federateRCCA', 'federateComDim', 'federateSNF', 'federateUMAP', 'federateHdbscan'))
    if (name %in% c('federateComDim', 'federateSNF', 'federateUMAP', 'federateHdbscan') && loginFD$url %in% logins$url) {
        stop(paste0("For ", name, ": loginFD server should not be one of logins servers"))
    }
    if (name %in% c('federatePCA', 'federateRCCA') && !(loginFD$url %in% logins$url)) {
        stop(paste0("For ", name, ": loginFD server should be one of logins servers"))
    }
    if (!is.data.frame(loginFD) || nrow(loginFD)!=1) stop("loginFD should be a 1-row data frame.")
    
    ## username-password prompt
    kr_service <- "logindb"
    # for loginFD
    if ((! 'user' %in% colnames(loginFD)) || is.null(loginFD$user)) {
        readline('Please provide login credentials!')
        options('user.FD' = readline('Username for federated server (loginFD): '))
        loginFD$userserver <- loginFD$user <- getOption('user.FD')
        
        keyring::key_set(service = kr_service,
                         username = loginFD$user)
        loginFD$passwordserver <- loginFD$password <- keyring::key_get(service = kr_service,
                                                                       username = loginFD$user)
    }
    # for logins
    if ((! 'user' %in% colnames(logins)) || is.null(logins$user)) {
        for (i in 1:nrow(logins)) {
            options('user' = readline(paste0('Username for data server ', i, ' (logins[', i, ',]): ')))
            logins$userserver[i] <- logins$user[i] <- getOption('user')
            keyring::key_set(service = kr_service, 
                             username = logins$user[i])
            logins$passwordserver[i] <- logins$password[i] <- keyring::key_get(service = kr_service,
                                                                               username = logins$user[i])
        }
    }
    #TODO: delete keyring
    ## execute name at loginFD server with data from logins servers
    cally <- list(as.symbol(name),
                  .encode.arg(loginFD),
                  .encode.arg(logins),
                  .encode.arg(func, serialize.it=T),
                  .encode.arg(symbol))
    cally <- c(cally, list(...)) # pass customized options of the function 'name'
    opalFD <- .login(loginFD) #datashield.login(loginFD)
    tryCatch({
        res <- datashield.aggregate(opalFD,
                                    as.call(cally),
                                    async=T)
    }, error=function(e) print(paste0("Function exec failed: ", e, " --- ", datashield.errors())), finally=datashield.logout(opalFD))
    return (res)
}


#' @title Coloring schema for plotting
#' @description This functions mapped data values from a variable in the virtual cohort to color codes
#' @param logins Login information of data repositories, where dsMOprimal is installed.
#' @param func Definition of a function for preparation of raw data matrices.
#' @param symbol The symbol provided when calling the function \code{func} for data preparation.
#' @param variable The variable name used to map to color codes.
#' @param continuous_scale A logical value indicating whether the coloring mapping is continuous. Default: TRUE.
#' @param nbreaks An integer indicating the number of intervals into which x is to be cut, less than 1/10 of number of samples, when x is the coloring scale is continuous.
#' @param colors A vector of colors to interpolate, must be a valid argument to col2rgb(). Default: \code{c('orange', 'blue')}.
#' @param ... arguments to pass to \code{colorRampPalette}
#' @return A vector of The color codes for all samples
#' @import dsSwissKnifeClient
#' @export
#' @examples 
#' data(logindata)
#' data(procFunc)
#' \donttest{
#' resColors <- coloring(logins=logindata[1:2,], func=procFunc$SingleOmics, symbol='rawDataX', what='rawDataX_discrete$PM_BMI_CONTINUOUS', continuous_scale=TRUE, nbreaks=4)
#' resColors <- coloring(logins=logindata[1:2,], func=procFunc$SingleOmics, symbol='rawDataX', what='rawDataX_discrete$PM_BMI_CATEGORICAL', continuous_scale=FALSE, levels=NULL)
#' }
coloring <- function(logins, func, symbol, what, continuous_scale = TRUE, nbreaks = 2, colors = c('orange', 'blue'), ...) {
    error <- 0.01
    if (!grepl("$", what)) stop("what should be a variable of a data frame in form of 'data.frame$variable'")
    opals <- .login(logins) #datashield.login(logins)
    tryCatch({
        func(opals, symbol)
        if (continuous_scale) {
            ranges <- datashield.aggregate(opals, as.symbol(paste0("dsRange(", what, ")")))
            cally <- list(x=what,
                          range.min=Reduce(min, ranges)*(1-error), # down min by error
                          range.max=Reduce(max, ranges)*(1+error), # up max by error
                          nbreaks=nbreaks,
                          colors=paste0("'", .encode.arg(colors), "'"))
            cally <- c(cally, list(...))
            callys <- paste0("mapColor(", paste(paste(names(cally), cally, sep="="), collapse=", "), ")")
        } else {
            datashield.assign(opals, "what_factor", as.symbol(paste0("dsFactor(", what, ")")))
            globalLevels <- Reduce(union, datashield.aggregate(opals, as.symbol(paste0("dsLevels(what_factor)"))))
            cally <- list(x="what_factor",
                          levels=paste0("'", .encode.arg(globalLevels), "'"),
                          colors=paste0("'", .encode.arg(colors), "'"))
            cally <- c(cally, list(...))
            callys <- paste0("mapColor(", paste(paste(names(cally), cally, sep="="), collapse=", "), ")")
        }
        res <- datashield.aggregate(opals, as.symbol(callys), async=T)
        resNames <- datashield.aggregate(opals, as.symbol(paste0("rowNames(", symbol, ")")))
    }, error=function(e) print(paste0("Function coloring failed: ", e, " --- ", datashield.errors())), finally=datashield.logout(opals))
    return (setNames(unlist(res), unlist(resNames)))
}
