## The AuthenticateWithTwitterAPI is not functional because it rely on Side Effect. It is a twitteR design problem
## AuthenticateWithFacebookAPI can be fixed to make it functional.

## TODO: Maybe need to unified the variable names, currently there are:
### facebook: appID, appSecret, extended_permissions, useCachedToken
### twitter: api_key, api_secret, access_token, access_token_secret, createToken <- inconsistent?
### youtube: apiKeyYoutube <- inconsistent?
### instagram: appID, appSecret, useCachedToken

## Maybe make it consistent with only camel, as the rest of the package uses camel, not underscore. But hadleyverse packages usually use underscores:
## Therefore, unified variable names:
## appID, appSecret, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken, extendedPermissions, createToken

Authenticate <- function(datasource, ...) {
    authenticator <- switch(tolower(datasource),
                            facebook = facebookAuthenticator,
                            youtube = youtubeAuthenticator,
                            twitter = twitterAuthenticator,
                            instagram = instagramAuthenticator,
                            stop("Unknown datasource")
                            )
    auth <- authenticator(...)
    credential <- list(datasource = tolower(datasource), auth = auth)
    class(credential) <- append(class(credential), "credential")
    return(credential)
}

### for the side effect of saving the credential into a file
### useful to cache the Credential to a file and then re-use it in the future session
### i.e. Authenticate %>% SaveCredential %>% Collect
### and then, LoadCredential %>% Collect

SaveCredential <- function(credential, filename = "credential.RDS") {
    if (credential$datasource == "twitter") {
        warning("Credential created for Twitter will not be saved.")
    } else {
        saveRDS(credential, filename)
    }
    return(credential)
}

LoadCredential <- function(filename = "credential.RDS") {
    credential <- readRDS(filename)
    return(credential)
}

### *Authenicator functions should not be exported. It is just a bunch of helper functions to bridge the AuthenticateWith* functions with Authenticate(), but with datasource as the first argument and always return an auth object

### as a convention, function starts with lower case shouldn't be exported.

youtubeAuthenticator <- function(apiKey) {
    return(AuthenticateWithYoutubeAPI(apiKey))
}

### Currently, this Authenticator will return nothing, only for its side effect
### SAD!!!!!!!!!!!!!!!!!!
### i.e. cannot use SaveCredential and LoadCredential!

twitterAuthenticator <- function(apiKey, apiSecret, accessToken, accessTokenSecret, createToken) {
    AuthenticateWithTwitterAPI(api_key = apiKey, api_secret = apiSecret, access_token = accessToken, access_token_secret = accessTokenSecret, createToken = createToken) # ah, only for its side effect, really bad design decision, twitteR!
    return(NULL)
}

facebookAuthenticator <- function(appID, appSecret, extendedPermissions = FALSE) {
    return(AuthenticateWithFacebookAPI(appID, appSecret, extended_permissions = extendedPermissions, useCachedToken = FALSE))
}

instagramAuthenticator <- function(appID, appSecret) {
    return(AuthenticateWithInstagramAPI(appID, appSecret))
}

