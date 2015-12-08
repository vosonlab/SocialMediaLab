## The AuthenticateWithTwitterAPI is not functional because it rely on Side Effect. It is a twitteR design problem
## AuthenticateWithFacebookAPI can be fixed to make it functional.

## TODO: Maybe need to unified the variable names, currently there are:
### facebook: appID, appSecret, extended_permissions, useCachedToken
### twitter: api_key, api_secret, access_token, access_token_secret, createToken <- inconsistent?
### youtube: apiKeyYoutube <- inconsistent?
### instagram: appID, appSecret, useCachedToken

## Maybe make it consistent with only camel, as the rest of the package uses camel, not underscore. But hedleyverse packages usually use underscores:
## Therefore:
## appID, appSecret, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken, extendedPermissions, createToken

Authenticate <- function(datasource, ...) {
    auth <- switch(datasource,
                   facebook = AuthenticateWithFacebookAPI(...),
                   youtube = AuthenticateWithYoutubeAPI(...),
                   twitter = AuthenticateWithTwitterAPI(...),
                   instagram = AuthenticateWithInstagram(...)
    )
    ## we can add more, such as youtube, twitter and instagram
    credential <- list(datasource = datasource, auth = auth)
    class(credential) <- append(class(credential), "credential")
    return(credential)
}
