--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "musicians/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/musician.html" defaultContext

    create ["musicians.html"] $ do
        route idRoute
        compile $ do
            musicians <- loadAll "musicians/*"
            let musicianCtx =
                    constField "title" "Musikere" `mappend`
                    listField "musicians" defaultContext (return musicians) `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/musicians.html" musicianCtx
                >>= loadAndApplyTemplate "templates/default.html" musicianCtx
                >>= relativizeUrls

    match (fromList ["index.html", "booking.html"]) $ do
        route idRoute
        compile $ getResourceBody >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
