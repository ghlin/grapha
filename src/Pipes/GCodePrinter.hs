module Pipes.GCodePrinter
  ( printGCode
  ) where

import           Text.Printf                    ( printf )
import           Lang.GCode

printGCode :: [GInstr] -> [String]
printGCode = mconcat . fmap pp'

singleS :: String -> String -> String
singleS = printf "%-13s%s"

singleI :: String -> Int -> String
singleI = printf "%-13s%d"

dual :: String -> String -> Int -> String
dual = printf "%-13s%-20s%d"

pp' :: GInstr -> [String]
pp' p@GGlobalStart{} = ["", pp p]
pp' p                = [pp p]

pp :: GInstr -> String
pp (GPack        n a) = dual    "pack"        n a
pp (GGlobalStart n a) = dual    "globalstart" n a
pp (GPush       r)    = singleI "push"        r
pp (GPick       i)    = singleI "pick"        i
pp (GTest       n)    = singleS "test"        n
pp (GPushPrimI  i)    = singleI "pushi"       i
pp (GPushGlobal n)    = singleS "pushglobal"  n
pp (GUpdate     i)    = singleI "update"      i
pp (GPop        i)    = singleI "pop"         i
pp (GSlide      i)    = singleI "slide"       i
pp (GAlloc      n)    = singleI "alloc"       n
pp (GLabel      i)    = singleI "label"       i
pp (GJump       i)    = singleI "jump"        i
pp (GJumpFalse  i)    = singleI "jfalse"      i
pp (GBuiltin    n)    = singleS "builtin"     n
pp (GEntry      e)    = singleS "entry"       e
pp (GComment    s)    = "; " <> s
pp GGlobalEnd         = "globalend"
pp GUnwind            = "unwind"
pp GMkApp             = "mkapp"
