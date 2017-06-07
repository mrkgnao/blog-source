{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Text.Mustache       ((~>))
import qualified Text.Mustache       as MT
import qualified Text.Mustache.Types as MT

import           Protolede
import           SitePipe            hiding ((&), (</>))

(.=:) :: KeyValue kv => Text -> Text -> kv
a .=: b = a .= b

-- | Creates an index.html page from the template.
createIndex :: [Value] -> [Value] -> [Value] -> SiteM ()
createIndex posts drafts tags = writeTemplate "templates/front-page.mustache" [indexContext]
  where
    indexContext =
      object ["posts" .= posts, "drafts" .= drafts, "tags" .= tags, "url" .=: "/index.html"]

-- | Creates an RSS feed.
createRssFeed :: [Value] -> SiteM ()
createRssFeed posts = writeTemplate "templates/rss.xml" [rssContext]
  where
    rssContext =
      object
        [ "posts" .= posts
        , "domain" .=: "http://mrkgnao.github.io"
        , "url" .=: "/rss.xml"
        ]

-- | Creates an index.html page from the template.
createStaticPages :: SiteM ()
createStaticPages = do
  writeTemplate "templates/about.html" [object ["url" .=: "/about/index.html"]]
  writeTemplate "templates/colophon.html" [object ["url" .=: "/colophon/index.html"]]

-- | Render the posts from the templates.
createPosts :: [Value] -> SiteM ()
createPosts = writeTemplate "templates/post.html"

-- | Render individual tag pages.
createTags :: [Value] -> SiteM ()
createTags = writeTemplate "templates/tag.html"

-- | Copy over static assets.
copyAssets :: SiteM ()
copyAssets =
  copyFiles
    [ "css"
    , "js"
    , "images"
    ]

siteDef :: SiteM ()
siteDef = do
  postsRaw <- resourceLoaderPretty markdownReader ["posts/**/*.md"]
  drafts <- resourceLoaderPretty markdownReader ["drafts/**/*.md"]

  let posts = postsRaw <&> addReadingTime
      tags = getTags (stringify makeTagUrl) (posts <> drafts)

  createIndex posts drafts tags

  createPosts posts
  createPosts drafts

  createTags tags
  createRssFeed posts

  createStaticPages
  copyAssets

addReadingTime :: Value -> Value
addReadingTime post =
     post
  & _Object
  .  at "readingTime"
  .~ Just (String readingTime)

  where
    content = post ^. key "content" . _String
    readingTime = tshow (round (length (T.words content) / 60)) <> " min"

-- | List of functions to be available in Mustache templates.
templateFuncs :: MT.Value
templateFuncs = MT.object
  [ "tagUrl" ~> MT.overText makeTagUrl
  ]

makeTagUrl :: Text -> Text
makeTagUrl tagName
   =  relative "tags"
  </> tagName </> "index.html"

makeTagUrlPretty :: Text -> Text
makeTagUrlPretty tagName
   =  relative "tags" </> tagName

main :: IO ()
main = siteWithGlobals templateFuncs siteDef
