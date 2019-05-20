module Lang.GCode where

import           Misc

type Ref = Int

data GInstr
  = GPush        Ref
  | GPushPrimI   Int
  -- TODO GPushPrimS, GPushPrimD, GPushPrimC...
  | GPushGlobal  Name
  | GBuiltin     Name
  | GPack        Name Int
  | GPick             Int
  | GTest        Name
  | GMkApp
  | GUpdate      Ref
  | GPop         Int
  | GSlide       Int
  | GAlloc       Int
  | GLabel       Int
  | GJump        Int
  | GJumpFalse   Int
  | GGlobalStart Name Int
  | GGlobalEnd
  | GUnwind
  | GComment     String
  | GEntry       Name
  deriving (Show, Eq)

