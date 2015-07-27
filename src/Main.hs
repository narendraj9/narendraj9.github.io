--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified Data.Set as S
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

	match "js/*" $ do
		route idRoute
		compile copyFileCompiler

	-- files : resume.pdf, etc.
	match "data/*" $ do
		route idRoute
		compile copyFileCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWithMath
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWithMath
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- music, books, movies
    match "miscellany/*" $ do
      route $ setExtension "html"
      compile $ pandocCompilerWithMath
            >>= loadAndApplyTemplate "templates/miscellany.html" miscCtx
            >>= loadAndApplyTemplate "templates/default.html" miscCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = 
    tagsField "tags" tags `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

miscCtx :: Context String
miscCtx =
	dateField "date" "%B %e, %Y" `mappend`
	defaultContext

-- configuration for automatically deploying _site to narendraj9.github.io
config :: Configuration
config = defaultConfiguration
         { providerDirectory = "./web/"
         }

-- pandoc compiler with support for latex mathematics equations
pandocCompilerWithMath =
  let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                        Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr S.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions { writerExtensions = newExtensions
                                                 , writerHTMLMathMethod = MathJax ""
                                                 }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
