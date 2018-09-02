module PDDLParser.Parse.Case
  ( titleToDash
  )
where

import           Data.Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

titleToDash :: Text -> Text
titleToDash x = Text.intercalate "-" (map Text.toLower (titleWords x))

titleWords :: Text -> [Text]
titleWords w = case Text.uncons w of
  Nothing -> []
  Just (c1, _) | not (isUpper c1) ->
    error $ "Does not begin upper case: " ++ show w
  Just (_, cs) -> case Text.span isUpper cs of
    ("", rest) ->
      let (restWord, otherChars) = Text.break isUpper rest
      in  Text.take (Text.length restWord + 1) w : titleWords otherChars
    (restCaps, "") -> [Text.take (Text.length restCaps + 1) w]
    (restCaps, _) ->
      let (before, after) = Text.splitAt (Text.length restCaps) w
      in  before : titleWords after
