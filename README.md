## Servant-Options

This library provides a middleware that returns `HTTP OPTIONS` responses for
valid application routes defined by your (Proxy :: Proxy api).
It is especially useful when trying to write an API that can be used
in a cross-origin capacity, as browsers will send "pre-flight" checks
by requesting OPTIONS for routes that are about to be called.

Before:

![](//github.com/sordina/servant-options/blob/master/images/before.png?raw=true)

After:

![](//github.com/sordina/servant-options/blob/master/images/after.png?raw=true)

Usage:

    module MyApp where

    import App
    import Servant
    import Network.Wai.Middleware.Cors
    import Network.Wai.Middleware.Servant.Options

    app :: Application
    app = logStdoutDev
        $ cors (const $ Just policy)
        $ provideOptions apiProxy
        $ serve apiProxy apiServer
      where
      policy = simpleCorsResourcePolicy
               { corsRequestHeaders = [ "content-type" ] }

## See Also

* <https://github.com/haskell-servant/servant/issues/278>
