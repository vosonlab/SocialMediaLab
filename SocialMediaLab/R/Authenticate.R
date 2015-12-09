## The AuthenticateWithTwitterAPI is not functional because it rely on Side Effect. It is a twitteR design problem
## AuthenticateWithFacebookAPI can be fixed to make it functional.

## TODO: Maybe need to unified the variable names, currently there are:
### facebook: appID, appSecret, extended_permissions, useCachedToken
### twitter: api_key, api_secret, access_token, access_token_secret, createToken <- inconsistent?
### youtube: apiKeyYoutube <- inconsistent?
### instagram: appID, appSecret, useCachedToken

## Maybe make it consistent with only camel, as the rest of the package uses camel, not underscore. But hedleyverse packages usually use underscores:
## Therefore, unified variable names:
## appID, appSecret, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken, extendedPermissions, createToken

Authenticate <- function(datasource, ...) {
    authenticator <- switch(datasource,
                            facebook = AuthenticateWithFacebookAPI,
                            youtube = youtubeAuthenticator,
                            twitter = AuthenticateWithTwitterAPI,
                            instagram = AuthenticateWithInstagram,
                            stop("Unknown datasource")
                            )
    auth <- authenticator(...)
    ## we can add more, such as youtube, twitter and instagram
    credential <- list(datasource = datasource, auth = auth)
    class(credential) <- append(class(credential), "credential")
    return(credential)
}

### *Authenicator functions should not be exported. It is just a bunch of helper functions to bridge the AuthenticateWith* functions with Authenticate(), but with datasource as the first argument and always return an auth object

youtubeAuthenticator <- function(apiKey) {
    return(AuthenticateWithYoutubeAPI(apiKey))
}
