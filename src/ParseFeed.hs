module ParseFeed (transformXMLToItems) where

import qualified Data.XML.Types as XT
import qualified Data.Text as T
import qualified Item
import Data.Maybe (maybeToList)

transformXMLToItems :: XT.Document -> [Item.Item]
transformXMLToItems doc = let
    root = XT.documentRoot doc
    channel = XT.elementChildren root >>= XT.isNamed (rss "channel")
    items = channel >>= XT.elementChildren >>= XT.isNamed (rss "item") >>= transformToItem
  in items

rss :: String -> XT.Name
rss localName = XT.Name {
    XT.nameLocalName = T.pack localName,
    XT.nameNamespace = Nothing,
    XT.namePrefix = Nothing
  }

transformToItem :: XT.Element -> [Item.Item]
transformToItem i = do
  title <- XT.elementChildren i >>= XT.isNamed (rss "title") >>= XT.elementText
  description <- XT.elementChildren i >>= XT.isNamed (rss "description") >>= XT.elementText
  enclosure <- XT.elementChildren i >>= XT.isNamed (rss "enclosure") >>= transformToEnclosure
  [Item.Item { Item.title = title, Item.description = description, Item.enclosure = enclosure }]

transformToEnclosure :: XT.Element -> [Item.Enclosure]
transformToEnclosure e = do
  url <- maybeToList $ XT.attributeText (rss "url") e
  l <- maybeToList $ XT.attributeText (rss "length") e >>= parseInteger
  mediaType <- maybeToList $ XT.attributeText (rss "type") e
  [Item.Enclosure { Item.url = url, Item.length = l, Item.mediaType = mediaType }]

parseInteger :: T.Text -> Maybe Integer
parseInteger t = case reads (T.unpack t) of
  [(i, "")] -> Just i
  _ -> Nothing

