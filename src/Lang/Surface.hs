module Lang.Surface where

import           Misc                           ( Name )

-- {{{ 用于表示(Grapha语言中)类型

-- | Type
data Type
  = TVar Name        -- ^ 类型变量
  | TCon Name        -- ^ 类型构造器(名)
  | TApp Type Type   -- ^ 应用
  deriving (Show, Eq)

-- | 经qualified某物(Type/Scheme), 比如 (Num a, Show b) => SomeThing a -> b
data Qual t  = Qual [Pred] t
  deriving (Show, Eq)

-- | Prediction, 如 Num a, Show b
data Pred = Pred Name Type
  deriving (Show, Eq)

-- }}}

-- {{{ 用于表示(Grapha语言中)数据结构

-- | Product类型定义
data ProductDef
  = ProductDef Name [Type]
  deriving (Show, Eq)

-- | ADT定义
data DataTypeDef
  = DataTypeDef Name [Name] [ProductDef]
  deriving (Show, Eq)

-- | Alias定义
data AliasDef
  = AliasDef Name [Name] Type
  deriving (Show, Eq)
-- }}}

-- {{{ interface(类型类)和instance定义

-- | interface定义
data InterfaceDef
  = InterfaceDef Name [Pred] Name [CombinatorAnnot]
  deriving (Show, Eq)

-- | instance定义
data InstanceDef
  = InstanceDef  Name [Pred] Type [CombinatorDef]
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
  | EDo              [DoStmt]                          -- ^ do-notation
  | EUnary           Name Expression                   -- ^ 一元前缀运算
  | EBinary          Name Expression Expression        -- ^ 二元中缀运算
  | EListLiteral     [Expression]                      -- ^ 列表字面量
  | ETupleLiteral    [Expression]                      -- ^ 元组字面量
  | EQuoted          Expression                        -- ^ 同EIndir
  | EIndir           Expression                        -- ^ 仅在恢复结合顺序时使用
  deriving (Show, Eq)

-- | Let绑定
data LetBinding
  = LetBinding        LetBindingForm Expression -- ^ 绑定
  | BindingAnnotation Annotation                -- ^ 类型注解
  deriving (Show, Eq)

-- | 对某个名字的注解
data Annotation
  = Annotation Name (Qual Type)
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

-- | do-记法中的绑定
data DoStmt
  = DoBind          (Maybe Pattern) Expression -- ^ pattern <- expr
  | DoLetBinding    LetBindingForm  Expression -- ^ let ... = expr
  deriving (Show, Eq)

-- }}}

-- {{{ 模式

-- | 表示一个Pattern
data Pattern
  = PVar      Name           -- ^ 绑定符号(名字)
  | PLit      Literal        -- ^ 字面量
  | PCon      Name [Pattern] -- ^ 复合的
  | PWildcard                -- ^ 通配符,Haskell中的`_`
  deriving (Show, Eq)

-- }}}

-- {{{ 字面量

-- | Literal
data Literal
  = LInteger   Int
  | LDouble    Double -- TODO
  | LString    String
  | LChar      Char
  deriving (Show, Eq)

-- }}}

-- {{{ Toplevel定义(函数/运算符)

-- | Combinator(函数)
data CombinatorDef
  = CombinatorDef Name [Pattern] Expression
  deriving (Show, Eq)

-- | Combinator的类型注解
data CombinatorAnnot
  = CombinatorAnnot Name (Qual Type)
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
  | ToplevelCombinatorAnnot CombinatorAnnot -- ^ 组合子/中缀运算符类型注解
  | ToplevelInfixDef        InfixDef        -- ^ 中缀运算符结合性/优先级定义
  | ToplevelDataTypeDef     DataTypeDef     -- ^ ADT定义
  | ToplevelAliasDef        AliasDef        -- ^ Alias定义
  | ToplevelInterfaceDef    InterfaceDef    -- ^ 接口(类型类)定义
  | ToplevelInstanceDef     InstanceDef     -- ^ 实例定义
  deriving (Show, Eq)

-- }}}

-- {{{

-- | program由许多Toplevel定义组成
data Program
  = Program
    { interfaceDefs    :: [InterfaceDef]
    , instanceDefs     :: [InstanceDef]
    , combinatorDefs   :: [CombinatorDef]
    , combinatorAnnots :: [CombinatorAnnot]
    , dataTypeDefs     :: [DataTypeDef]
    , aliasDefs        :: [AliasDef]
    , infixDefs        :: [InfixDef]
    }
  deriving (Show)

-- }}}

-- {{{ helpers

class HasExpression f where
  mapE :: (Expression -> Expression) -> f -> f

instance HasExpression LetBinding where
  mapE f (LetBinding fm e) = LetBinding fm $ f e
  mapE _ x                 = x

instance HasExpression DoStmt where
  mapE f (DoBind p e)       = DoBind p $ f e
  mapE f (DoLetBinding p e) = DoLetBinding p $ f e

instance HasExpression CaseAlternative where
  mapE f (CaseAlternative p e) = CaseAlternative p $ f e

instance HasExpression CombinatorDef where
  mapE f (CombinatorDef n p e) = CombinatorDef n p $ f e

instance HasExpression InstanceDef where
  mapE f (InstanceDef n p t c) = InstanceDef n p t $ mapE f <$> c

instance HasExpression ToplevelDef where
  mapE f (ToplevelCombinatorDef x) = ToplevelCombinatorDef $ mapE f x
  mapE f (ToplevelInstanceDef   x) = ToplevelInstanceDef   $ mapE f x
  mapE _ x                         = x

instance HasExpression Program where
  mapE f prog = let ids = mapE f <$> instanceDefs prog
                    cds = mapE f <$> combinatorDefs prog
                 in prog { instanceDefs = ids, combinatorDefs = cds }


-- }}}
