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


#' @title Wrapper call function for federated analysis
#' @description ...
#' @param name A character string naming the function to be called, among federatePCA, federateRCCA, federateComDim, federateSNF.
#' @param loginFD Login information of the federated server, where the function \code{name} will be executed. 
#' For functions using X'X, such as federatePCA, federateRCCA, this should be a server with dsMOprimal installed. 
#' For functions using XX', such as federateComDim, federateSNF, this should be a server with dsMOdual installed.
#' @param logins Login information of data repositories, where dsMOprimal is installed.
#' @param func Definition of a function for preparation of raw data matrices.
#' @param symbol The symbol provided when calling the function \code{func} for data preparation.
#' @import dsSwissKnifeClient
#' @examples 
#' data(logindata)
#' data(procFunc)
#' \donttest{
#' exec('federatePCA', loginFD=logindata[,1], logins=logindata[,1:2], func=procFunc$SingleOmics, symbol='rawDataX')
#' exec('federateRCCA', loginFD=logindata[,1], logins=logindata[,1:2], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' exec('federateComDim', loginFD=logindata[,3], logins=logindata[,1:2], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' exec('federateSNF', loginFD=logindata[,3], logins=logindata[,1:2], func=procFunc$BiOmics, symbol=c('rawDataX', 'rawDataY'))
#' }
#' @export
exec <- function(name, loginFD, logins, func, symbol, ...) {
    ## check arguments
    name <- match.arg(name, choices=c('federatePCA', 'federateRCCA', 'federateComDim', 'federateSNF'))
    if (name %in% c('federateComDim', 'federateSNF') && loginFD$url %in% logins$url) {
        stop(paste0("For ", name, ": loginFD server should not be one of logins servers"))
    }
    ## execute name at loginFD server with data from logins servers
    cally <- list(as.symbol(name),
                  .encode.arg(loginFD),
                  .encode.arg(logins),
                  .encode.arg(func, serialize.it=T),
                  .encode.arg(symbol))
    if (!name %in% c('federatePCA')) cally <- c(cally, list(...)) # pass customized options of the function 'name'
    res <- datashield.aggregate(datashield.login(loginFD),
                                as.call(cally),
                                async=T)
    return (res)
}
