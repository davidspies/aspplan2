module MCP.Rules
  ( docRules
  )
where

import           Clingo.Symbol                  ( PureSymbol )
import qualified Text.PrettyPrint.Leijen.Text  as PP

docRules :: [PureSymbol] -> PP.SimpleDoc
docRules xs = PP.renderPretty 1.0 100
  $ PP.vcat ([ PP.pretty s <> "." | s <- xs ] ++ [PP.line])
