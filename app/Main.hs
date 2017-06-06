{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Text.Mustache       ((~>))
import qualified Text.Mustache       as MT
import qualified Text.Mustache.Types as MT

import           Protolede
import           SitePipe            hiding ((&))

(.=:) :: KeyValue kv => Text -> Text -> kv
a .=: b = a .= b

addReadingTime :: Value -> Value
addReadingTime post = post & _Object . at "readingTime" .~ Just (String readingTime)
    where
      content :: Text
      content = post ^. key "content" . _String

      readingTime :: Text
      readingTime = tshow (round $ length (T.words content) / 60) <> " min"

-- | Creates an index.html page from the template.
createIndex :: [Value] -> [Value] -> SiteM ()
createIndex posts tags = writeTemplate "templates/index.html" [indexContext]
  where
    indexContext =
      object ["posts" .= posts, "tags" .= tags, "url" .=: "/index.html"]

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

-- | Render the posts from the templates.
createPosts :: [Value] -> SiteM ()
createPosts = writeTemplate "templates/post.html"

-- | Render individual tag pages.
createTags :: [Value] -> SiteM ()
createTags = writeTemplate "templates/tags.html"


siteDef :: SiteM ()
siteDef = do
  postsRaw <- resourceLoader markdownReader ["posts/*.md"]

  let posts = postsRaw <&> addReadingTime
      tags = getTags (stringify makeTagUrl) posts

  createIndex posts tags
  createPosts posts
  createTags tags
  createRssFeed posts

  staticAssets

main :: IO ()
main = siteWithGlobals templateFuncs siteDef

-- | List of functions to be available in Mustache templates.
templateFuncs :: MT.Value
templateFuncs = MT.object
  [ "tagUrl" ~> MT.overText makeTagUrl
  ]

makeTagUrl :: Text -> Text
makeTagUrl tagName = "/tags/" <> tagName <> ".html"

-- | Copy over static assets.
staticAssets :: SiteM ()
staticAssets = copyFiles
    [ "css/*.css"
    , "js"
    , "images"
    ]
