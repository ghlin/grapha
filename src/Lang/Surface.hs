module Lang.Surface where

import           Data.Char                      ( isUpper )
import           Lang.Literal
import           Lang.Type
import           Misc                           ( Name )

-- {{{ 用于表示(Grapha语言中)数据结构

-- | Product类型定义
data ProductDef
  = ProductDef Name [Type]
  deriving (Show, Eq)

-- | ADT定义
data DataTypeDef
  = DataTypeDef Name [Name] [ProductDef]
  deriving (Show, Eq)
-- }}}

-- {{{ 表达式

-- | Expression
data Expression
  -- 基本元素
  = ELit             Literal                           -- ^ 字面量
  | EVar             Name                              -- ^ 符号名
  | ELam             [Pattern] Expression              -- ^ lambda
  | EApp             Expression Expression             -- ^ 函数应用
  | EIf              Expression Expression Expression  -- ^ if
  | ECase            Expression [CaseAlternative]      -- ^ 模式匹配
  | ELet             [LetBinding] Expression           -- ^ let 绑定
  -- 以下仅在CST中出现,后续可以被转换为上面几种更简单的形式(多为语法糖)
  | EUnary           Name Expression                   -- ^ 一元前缀运算
  | EBinary          Name Expression Expression        -- ^ 二元中缀运算
  | EListLiteral     [Expression]                      -- ^ 列表字面量
  | ETupleLiteral    [Expression]                      -- ^ 元组字面量
  | EQuoted          Expression                        -- ^ 同EIndir
  | EIndir           Expression                        -- ^ 仅在恢复结合顺序时使用
  deriving (Show, Eq)

-- | Let绑定
data LetBinding
  = LetBinding LetBindingForm Expression -- ^ 绑定
  deriving (Show, Eq)

-- | Let绑定的形式
data LetBindingForm
  = PatternBinding    Pattern            -- ^ 绑定某个pattern
  | CombinatorBinding Name    [Pattern]  -- ^ 组合子定义
  deriving (Show, Eq)

-- | case分支
data CaseAlternative
  = CaseAlternative Pattern Expression
  deriving (Show, Eq)

-- }}}

-- {{{ 模式

-- | 表示一个Pattern
data Pattern
  = PVar      Name              -- ^ 绑定符号(名字)
  | PLit      Literal           -- ^ 字面量
  | PCon      Name    [Pattern] -- ^ 复合的
  | PWildcard                   -- ^ 通配符,Haskell中的`_`
  deriving (Show, Eq)

-- }}}

-- {{{ Toplevel定义(函数/运算符)

-- | Combinator(函数)
data CombinatorDef
  = CombinatorDef Name [Pattern] Expression
  deriving (Show, Eq)

-- | (二元)中缀运算符的优先级和结合性声明
-- 优先级越高表示结合越紧密
-- 若某个运算符无此声明,则默认为左结合和优先级5
data InfixDef
  = InfixDef Name Int InfixAssoc
  deriving (Show, Eq)

-- | 结合性定义
data InfixAssoc
  = AssocLeft   -- ^ 左结合
  | AssocRight  -- ^ 右结合
  | AssocNone   -- ^ 不结合
  deriving (Show, Eq)

-- | Toplevel definition
data ToplevelDef
  = ToplevelCombinatorDef   CombinatorDef   -- ^ 组合子/中缀运算符定义
  | ToplevelInfixDef        InfixDef        -- ^ 中缀运算符结合性/优先级定义
  | ToplevelDataTypeDef     DataTypeDef     -- ^ ADT定义
  deriving (Show, Eq)

-- }}}

-- {{{

-- | program由许多Toplevel定义组成
data Program
  = Program
    { combinatorDefs   :: [CombinatorDef]
    , dataTypeDefs     :: [DataTypeDef]
    , infixDefs        :: [InfixDef]
    }
  deriving (Show)

-- }}}

-- {{{ helpers

propagateE :: (Expression -> Expression) -> Expression -> Expression
propagateE f (ELet bs e)        = f $ ELet (mapE (propagateE f) <$> bs) $ propagateE f $ f e
propagateE f (ECase e alts)     = f $ ECase (propagateE f $ f e) $ mapE (propagateE f) <$> alts
propagateE f (EApp l r)         = f $ EApp (propagateE f $ f l) (propagateE f $ f r)
propagateE f (EIf c t e)        = f $ EIf (propagateE f $ f c) (propagateE f $ f t) (propagateE f $ f e)
propagateE f (ELam ps e)        = f $ ELam ps $ propagateE f $ f e
propagateE f (EUnary n e)       = f $ EUnary n $ propagateE f $ f e
propagateE f (EBinary n l r)    = f $ EBinary n (propagateE f $ f l) (propagateE f $ f r)
propagateE f (EListLiteral es)  = f $ EListLiteral $ propagateE f <$> (f <$> es)
propagateE f (ETupleLiteral es) = f $ ETupleLiteral $ propagateE f <$> (f <$> es)
propagateE f (EQuoted e)        = f $ EQuoted $ propagateE f $ f e
propagateE f (EIndir e)         = f $ EIndir $ propagateE f $ f e
propagateE f e                  = f e

class HasExpression f where
  mapE :: (Expression -> Expression) -> f -> f

instance HasExpression LetBinding where
  mapE f (LetBinding fm e) = LetBinding fm $ f e

instance HasExpression CaseAlternative where
  mapE f (CaseAlternative p e) = CaseAlternative p $ f e

instance HasExpression CombinatorDef where
  mapE f (CombinatorDef n p e) = CombinatorDef n p $ f e

instance HasExpression ToplevelDef where
  mapE f (ToplevelCombinatorDef x) = ToplevelCombinatorDef $ mapE f x
  mapE _ x                         = x

instance HasExpression Program where
  mapE f prog = let cds = mapE f <$> combinatorDefs prog
                 in prog { combinatorDefs = cds }

fn :: Type -> Type -> Type
fn t1 t2 = TApp (TApp (TCon "->") t1) t2

isConstr :: Name -> Bool
isConstr "[]" = True
isConstr n    = isUpper (head n) || (head n == ':') || (head n == '(')

builtinTyConstrs :: [Name]
builtinTyConstrs = ["Int", "String", "Double", "Char"]

-- }}}
