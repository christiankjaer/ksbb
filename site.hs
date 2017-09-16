--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*.css" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/about.html" defaultContext >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    match "musicians/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>=
        loadAndApplyTemplate "templates/musician.html" defaultContext
    match "events/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler >>= loadAndApplyTemplate "templates/event.html" eventCtx
    create ["events.html"] $ do
      route idRoute
      compile $ do
        events <- chronological =<< loadAll "events/*"
        let eventCtx =
              constField "title" "Koncerter" `mappend`
              listField "events" eventCtx (return events) `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/events.html" eventCtx >>=
          loadAndApplyTemplate "templates/default.html" eventCtx >>=
          relativizeUrls
    create ["musicians.html"] $ do
      route idRoute
      compile $ do
        musicians <- loadAll "musicians/*"
        let musicianCtx =
              constField "title" "Musikere" `mappend`
              listField "musicians" defaultContext (return musicians) `mappend`
              defaultContext
        makeItem "" >>=
          loadAndApplyTemplate "templates/musicians.html" musicianCtx >>=
          loadAndApplyTemplate "templates/default.html" musicianCtx >>=
          relativizeUrls
    match (fromList ["index.html", "booking.html"]) $ do
      route idRoute
      compile $
        getResourceBody >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
eventCtx :: Context String
eventCtx = dateField "date" "%F" `mappend` defaultContext
