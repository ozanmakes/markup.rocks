module Formats where

import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Text.Pandoc
import           Text.Pandoc.Error (PandocError)

sourceFormats :: Map String String
sourceFormats =
  Map.fromList
    [("dbk","DocBook")
    ,("html","HTML")
    ,("latex","LaTeX")
    ,("md","Markdown")
    ,("mw","MediaWiki")
    ,("opml","OPML")
    ,("org","Org")
    ,("rst","reStructuredText")
    ,("t2t","txt2tags")
    ,("textile","Textile")
    ,("twiki","TWiki")]

extToSource :: String -> String
extToSource ext =
  case ext of
    "markdown" -> "md"
    "xml" -> "docbook"
    otherwise -> ext

stringToReader :: String -> ReaderOptions -> String -> Either PandocError Pandoc
stringToReader s =
  case s of
    "dbk" -> readDocBook
    "html" -> readHtml
    "latex" -> readLaTeX
    "mw" -> readMediaWiki
    "opml" -> readOPML
    "org" -> readOrg
    "rst" -> readRST
    "t2t" -> readTxt2TagsNoMacros
    "textile" -> readTextile
    "twiki" -> readTWiki
    otherwise -> readMarkdown

resultFormats :: Map String String
resultFormats =
  Map.fromList
    [("1preview","Preview")
    ,("ascii","AsciiDoc")
    ,("dbk","Docbook")
    ,("dw","DokuWiki")
    ,("html","HTML")
    ,("icml","ICML")
    ,("latex","LaTeX")
    ,("man","Man")
    ,("md","Markdown")
    ,("mw","MediaWiki")
    ,("odt","OpenDocument")
    ,("opml","OPML")
    ,("org","Org")
    ,("plain","Plaintext")
    ,("rst","reStructuredText")
    ,("texinfo","Texinfo")
    ,("textile","Textile")]

supportedExtensions :: [String]
supportedExtensions =
  [".dbk"
  ,".html"
  ,".markdown"
  ,".md"
  ,".mw"
  ,".opml"
  ,".org"
  ,".rst"
  ,".t2t"
  ,".tex"
  ,".textile"
  ,".twiki"
  ,".txt"
  ,".xml"]
