--------------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options as Pandoc.Options


--------------------------------------------------------------------------------
-- Pandoc
--------------------------------------------------------------------------------


pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
    { Pandoc.Options.writerHtml5 = True
    , Pandoc.Options.writerHtmlQTags = True
    --, Pandoc.Options.writerNumberSections = True
    --, Pandoc.Options.writerNumberOffset = [1]
    , Pandoc.Options.writerSectionDivs = True
    }


tocWriterOptions :: Pandoc.Options.WriterOptions
tocWriterOptions = pandocWriterOptions
    { writerTableOfContents = True
    , writerTemplate =
        "$if(toc)$<div id=\"toc\"><h2>Contents</h2>\n$toc$</div>\n$endif$$body$"
    , writerStandalone = True
    }


--------------------------------------------------------------------------------
-- Site building
--------------------------------------------------------------------------------


main :: IO ()
main = hakyll $ do


    -- put all the images in /images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler


    -- copy site icon to `favicon.ico`
    match "images/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler


    -- route the fonts
    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- route my extra files
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- javascript
    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler


    -- route the css
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- robots
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    -- compile the scss and put it in _site/css/
    match "scss/*" $ do
        route $ gsubRoute "scss/" (const "css/") `composeRoutes`
            setExtension "css"
        compile $ getResourceString
            >>= withItemBody
                ( unixFilter "sass"
                    [ "-s"
                    , "--scss"
                    , "--style"
                    , "compressed"]
                )
            >>= return . fmap compressCss


    -- make top-level pages
    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes`
            setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["404.html"] $ do
        route idRoute
        compile $ do
            let notFoundCtx =
                    constField "title" "404 you're lost" `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/default.html" notFoundCtx


    match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home"                `mappend`
                    modificationTimeField "modified" "%B %e, %Y" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls


    match "templates/*" $ compile templateCompiler
