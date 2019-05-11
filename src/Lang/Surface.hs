module Lang.Surface where

import           Misc                           ( Name )

-- {{{ 用于表示(Grapha语言中)类型

-- | Type
data TypeX a
  = TVar a Name                -- ^ 类型变量
  | TCon a Name                -- ^ 类型构造器(名)
  | TApp a (TypeX a) (TypeX a) -- ^ 应用
  deriving (Show, Eq)

-- | Type scheme (前束的Type)
data SchemeX a
  = Forall [(Name, a)] (QualX (TypeX a) a)
  deriving (Show, Eq)

-- | 经qualified某物(Type/Scheme), 比如 (Num a, Show b) => SomeThing a -> b
data QualX t a = Qual [PredX a] t
  deriving (Show, Eq)

-- | Prediction, 如 Num a, Show b
data PredX a = Pred Name (TypeX a)
  deriving (Show, Eq)

-- }}}

-- {{{ 用于表示(Grapha语言中)数据结构

-- | Product类型定义
data ProductDefX a
  = ProductDef Name [TypeX a]        -- ^ 普通的Product
  | RecordDef Name [(Name, TypeX a)] -- ^ Record风格的定义
  deriving (Show, Eq)

-- | ADT定义
data DataTypeDefX a
  = DataTypeDef Name [(Name, a)] [ProductDefX a]
  deriving (Show, Eq)

-- | Alias定义
data AliasDefX a
  = AliasDef Name [(Name, a)] (TypeX a)
  deriving (Show, Eq)
-- }}}

-- {{{ interface(类型类)和instance定义

-- | interface定义
data InterfaceDefX a
  = InterfaceDef Name [PredX a] (Name, a) [CombinatorAnnotX a]
  deriving (Show, Eq)

-- | instance定义
data InstanceDefX a
  = InstanceDef  Name [PredX a] (TypeX a) [CombinatorDefX a]
  deriving (Show, Eq)

-- }}}

-- {{{ 表达式

-- | Expression, 其中b是可能的类型注解
data ExpressionX' b a
  -- 基本元素
  = ELit             b Literal                                         -- ^ 字面量
  | EVar             b Name                                            -- ^ 符号名
  | ELam             b [Pattern] (ExpressionX a)                       -- ^ lambda
  | EApp             b (ExpressionX a) (ExpressionX a)                 -- ^ 函数应用
  | ECase            b (ExpressionX a) [CaseAlternativeX a]            -- ^ 模式匹配
  | EIf              b (ExpressionX a) (ExpressionX a) (ExpressionX a) -- ^ if
  | ELet                               [LetBindingX a] (ExpressionX a) -- ^ let 绑定
  -- 以下仅在CST中出现,后续可以被转换为上面几种更简单的形式(多为语法糖)
  | EDo              b [DoStmtX a]                                     -- ^ do-notation
  | EUnary           b Name (ExpressionX a)                            -- ^ 一元前缀运算
  | EBinary          b Name (ExpressionX a) (ExpressionX a)            -- ^ 二元中缀运算
  | EListLiteral     b [ExpressionX a]                                 -- ^ 列表字面量
  | ETupleLiteral    b [ExpressionX a]                                 -- ^ 元组字面量
  | EIndir             (ExpressionX a)                                 -- ^ 仅在恢复结合顺序时使用
  | EQuoted          b (ExpressionX a)                                 -- ^ 同EIndir
  -- TODO: list comprehension
  -- | EMultiIf         b [(ExpressionX a, ExpressionX a)]                -- ^ multi-way if
  -- | ELamCase         b [CaseAlternativeX a]                            -- ^ lambda-case
  deriving (Show, Eq)

type ExpressionX a = ExpressionX' (MaybeAnnotatedX a) a

-- | 注解
type MaybeAnnotatedX a = Maybe (SchemeX a)

-- | Let绑定
data LetBindingX a
  = LetBinding        (LetBindingFormX a) (ExpressionX a) -- ^ 绑定
  | BindingAnnotation (AnnotationX a)                     -- ^ 类型注解
  deriving (Show, Eq)

-- | 对某个名字的注解
data AnnotationX a
  = Annotation Name (SchemeX a)
  deriving (Show, Eq)

-- | Let绑定的形式
data LetBindingFormX a
  = PatternBinding    Pattern         -- ^ 绑定某个pattern
  | CombinatorBinding Name [Pattern]  -- ^ 组合子定义
  deriving (Show, Eq)

-- | case分支
data CaseAlternativeX a
  = CaseAlternative Pattern (ExpressionX a)
  deriving (Show, Eq)

-- | do-记法中的绑定
data DoStmtX a
  = DoBind          (Maybe Pattern)     (ExpressionX a) -- ^ pattern <- expr
  | DoLetBinding    (LetBindingFormX a) (ExpressionX a) -- ^ let ... = expr
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
data CombinatorDefX a
  = CombinatorDef Name [Pattern] (ExpressionX a)
  deriving (Show, Eq)

-- | Combinator的类型注解
data CombinatorAnnotX a
  = CombinatorAnnot Name (SchemeX a)
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
data ToplevelDefX a
  = ToplevelCombinatorDef   (CombinatorDefX a)   -- ^ 组合子/中缀运算符定义
  | ToplevelCombinatorAnnot (CombinatorAnnotX a) -- ^ 组合子/中缀运算符类型注解
  | ToplevelInfixDef        InfixDef             -- ^ 中缀运算符结合性/优先级定义
  | ToplevelDataTypeDef     (DataTypeDefX a)     -- ^ ADT定义
  | ToplevelAliasDef        (AliasDefX a)        -- ^ Alias定义
  | ToplevelInterfaceDef    (InterfaceDefX a)    -- ^ 接口(类型类)定义
  | ToplevelInstanceDef     (InstanceDefX a)     -- ^ 实例定义
  deriving (Show, Eq)

-- }}}

-- {{{

-- | program由许多Toplevel定义组成
data ProgramX a
  = Program
    { interfaceDefs    :: [InterfaceDefX    a]
    , instanceDefs     :: [InstanceDefX     a]
    , combinatorDefs   :: [CombinatorDefX   a]
    , combinatorAnnots :: [CombinatorAnnotX a]
    , dataTypeDefs     :: [DataTypeDefX     a]
    , aliasDefs        :: [AliasDefX        a]
    , infixDefs        :: [InfixDef]
    }
  deriving (Show)

-- }}}

-- {{{

-- | Kind
data Kind
  = KStar            -- *
  | KArr  Kind  Kind -- ... -> ...
  deriving (Show, Eq)

-- }}}

-- {{{ 为会用到的X部分取简短的名字

type Type             = TypeX            ()
type Scheme           = SchemeX          ()
type Qual t           = QualX t          ()
type Pred             = PredX            ()
type ProductDef       = ProductDefX      ()
type DataTypeDef      = DataTypeDefX     ()
type AliasDef         = AliasDefX        ()
type InterfaceDef     = InterfaceDefX    ()
type InstanceDef      = InstanceDefX     ()
type Expression       = ExpressionX      ()
type Annotation       = AnnotationX      ()
type MaybeAnnotated   = MaybeAnnotatedX  ()
type LetBinding       = LetBindingX      ()
type LetBindingForm   = LetBindingFormX  ()
type CaseAlternative  = CaseAlternativeX ()
type DoStmt           = DoStmtX          ()
type ToplevelDef      = ToplevelDefX     ()
type CombinatorDef    = CombinatorDefX   ()
type CombinatorAnnot  = CombinatorAnnotX ()
type Program          = ProgramX         ()

type TypeK            = TypeX            Kind
type SchemeK          = SchemeX          Kind
type QualK t          = QualX t          Kind
type PredK            = PredX            Kind
type ProductDefK      = ProductDefX      Kind
type DataTypeDefK     = DataTypeDefX     Kind
type AliasDefK        = AliasDefX        Kind
type InterfaceDefK    = InterfaceDefX    Kind
type InstanceDefK     = InstanceDefX     Kind
type ExpressionK      = ExpressionX      Kind
type AnnotationK      = AnnotationX      Kind
type MaybeAnnotatedK  = MaybeAnnotatedX  Kind
type LetBindingK      = LetBindingX      Kind
type LetBindingFormK  = LetBindingFormX  Kind
type CaseAlternativeK = CaseAlternativeX Kind
type DoStmtK          = DoStmtX          Kind
type CombinatorDefK   = CombinatorDefX   Kind
type CombinatorAnnotK = CombinatorAnnotX Kind
type ToplevelDefK     = ToplevelDefX     Kind
type ProgramK         = ProgramX         Kind

-- }}}

-- {{{ helpers

mapAnnot :: (MaybeAnnotatedX a -> MaybeAnnotatedX a) -> ExpressionX a -> ExpressionX a
mapAnnot f o = case o of
  ELit         ann l       -> ELit         (f ann) l
  EVar         ann v       -> EVar         (f ann) v
  ELam         ann p b     -> ELam         (f ann) p b
  EApp         ann a b     -> EApp         (f ann) a b
  ECase        ann c a     -> ECase        (f ann) c a
  EIf          ann c t e   -> EIf          (f ann) c t e
  EDo          ann s       -> EDo          (f ann) s
  EUnary       ann n u     -> EUnary       (f ann) n u
  EBinary      ann n l r   -> EBinary      (f ann) n l r
  EListLiteral ann l       -> EListLiteral (f ann) l
  EQuoted      ann e       -> EQuoted      (f ann) e
  ELet             b e     -> ELet                 b $ mapAnnot f e
  _                        -> o

-- | 更新注解
putAnnot :: SchemeX a -> ExpressionX a -> ExpressionX a
putAnnot = mapAnnot . const . Just

class HasExpression f where
  mapE :: (ExpressionX a -> ExpressionX a) -> f a -> f a

instance HasExpression LetBindingX where
  mapE f (LetBinding fm e) = LetBinding fm $ f e
  mapE _ x                 = x

instance HasExpression DoStmtX where
  mapE f (DoBind p e)       = DoBind p $ f e
  mapE f (DoLetBinding p e) = DoLetBinding p $ f e

instance HasExpression CaseAlternativeX where
  mapE f (CaseAlternative p e) = CaseAlternative p $ f e

instance HasExpression CombinatorDefX where
  mapE f (CombinatorDef n p e) = CombinatorDef n p $ f e

instance HasExpression InstanceDefX where
  mapE f (InstanceDef n p t c) = InstanceDef n p t $ mapE f <$> c

instance HasExpression ToplevelDefX where
  mapE f (ToplevelCombinatorDef x) = ToplevelCombinatorDef $ mapE f x
  mapE f (ToplevelInstanceDef   x) = ToplevelInstanceDef   $ mapE f x
  mapE _ x                         = x

instance HasExpression ProgramX where
  mapE f prog = let ids = mapE f <$> instanceDefs prog
                    cds = mapE f <$> combinatorDefs prog
                 in prog { instanceDefs = ids, combinatorDefs = cds }


-- }}}
