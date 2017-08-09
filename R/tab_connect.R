#' @title Connect to API
#' @description Connect to your Tableau Server's REST API.
#' @param server_name The name or IP address of your Tableau Server.
#' @param api_version The API version of your Tableau REST API.
#' @param user Your Tableau Server user name.
#' @param pass Your Tableau Server password.
#' @param ssl Logical. Does your Tableau Server instance use SSL? The default is TRUE.
#' @param site A specific site for a Tableau Server running multiple sites. The default is an empty string.
#' If the Tablau Server is only running one site, the empty string will return your default site.
#' @param ... Additional arguments.
#' @import jsonlite
#' @import httr
#' @examples
#' \dontrun{
#' tab_connect(server_name = "my.server.com", api_version = 2.0, user = myuser, pass = mypass, ssl = F)
#' }
#'
#' @export
#'
tab_connect <- function (server_name=NULL, api_version=NULL, user=NULL, pass=NULL, ssl=T, site="", ...){
    # TODOs
    # write method for connecting to Tableau Public.
    # write ifelse to check args.

    #if(is.null(server_name | api_version | user | pass)) {
    #    warning("Missing argument. Please make sure server_name, api_version, user, and pass arguments are valid.")
    #}

    if(isTRUE(ssl)) {
        prefix = "https://"
    } else { prefix = "http://" }

    # Make URL for the signin request.
    signin_url <- paste0(prefix, server_name, "/", "api/", api_version, "/", "auth/signin")

    auth_creds <- jsonlite::toJSON(list(credentials = list(name=user, password=pass,
                                                           site=list(contentUrl=site))), auto_unbox = T)

    auth_req <- POST(signin_url, body = auth_creds,
                     config = add_headers(c(accept="application/json", `content-type`="application/json")),
                     encode = "json") %>% stop_for_status(auth_req)

    auth_pload <- content(auth_req, "text") %>% fromJSON()
    token <- auth_pload$credentials$token
    site_id <- auth_pload$credentials$site$id

    print("Sign in successful.")
    print(paste0("token = ", token))
    print(paste0("site_id = ", site_id))

}
