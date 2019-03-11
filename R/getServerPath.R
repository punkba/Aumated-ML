getServerPath <-function(sessionId,workDir){
    path <- split_path(workDir)
    path[2] <- sessionId
    path[3] <- "ocpu-store"
    path <- paste0("/", paste0(rev(path[2:(length(path)-1)]), collapse="/"))

    return (path)
}
