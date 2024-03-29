module Pipes.Grouper
  ( regroup
  ) where

import           Data.List                      ( (\\) )
import           Lang.Surface
import           Lang.Surface.Subst
import           Misc
import           Pipe

{--- | 为Let和CombinatorDefs重新分组, 使得每一组仅依赖组内和/或之前组的定义
 ---
 --- 例如:
 ---
 ---
 ---    let (a, b, c) = ... f ...
 ---        f ...     = ... g ...
 ---        g ...     = ...
 ---        m ..      = ... m ... n ...
 ---        n ..      = ... m ... n ...
 ---     in ...
 ---
 ---              ||
 ---              \/
 ---
 ---    let g ...      = ...               in
 ---    let f ...      = ... g ...         in
 ---    let (a, b, c)  = ... f ...         in
 ---    let rec m ..   = ... m ... n ...
 ---            n ..   = ... m ... n ...
 ---     in ...
 ---
 ---}
regroup :: Pipe ErrorMessage Program [[CombinatorDef]]
regroup prog = let cds = combinatorDefs prog
                in Right $ fmap (mapE regroupE) <$> regroupCombinators cds

-- | 为Let分组
regroupE :: Expression -> Expression
regroupE = regroupE'

-- | 为toplevel分组
regroupCombinators :: [CombinatorDef] -> [[CombinatorDef]]
regroupCombinators defs =
  let mkN d@(CombinatorDef name pats body) = mk (fvs body \\ mconcat (fvsP <$> pats)) [name] d
   in filter (not . null) $ fmap snd <$> re (mkN <$> defs)

type N a = (([Name], [Name]), a)
-- ((<deps>, <provides>), <payload>)

re' :: [Name] -> [N a] -> ([N a], [N a])
re' = split . satisfied
  where satisfied syms ((deps, _), _) = all (`elem` syms) deps

re ::  [N a] -> [[N a]]
re nodes = r initialDeps [] nodes
  where allDeps         = mconcat $ fst . fst <$> nodes
        allProvides     = mconcat $ snd . fst <$> nodes
        initialDeps     = allProvides \\ allDeps
        r syms outs ins = case re' syms ins of
                            ([], cyc)  -> cyc:outs -- cyc内存在循环依赖或者依赖缺失
                                                   -- TODO 考虑:
                                                   --   m: (["m", "n"], "m")
                                                   --   n: (["m", "n"], "n")
                                                   --   q: (["m", "n"], "q")
                                                   -- 实际结果应当为     (m, n)    (q)
                                                   -- 但现在会粗略地返回 (m, n, q)
                            (ok, pend) -> let introduced = mconcat $ snd . fst <$> ok
                                           in r (syms <> introduced) ((singleton <$> ok) <> outs) pend

mk :: [Name] -> [Name] -> a -> N a
mk deps provides payload = ((deps \\ provides, provides), payload)

regroupLetBinding :: [LetBinding] -> Expression -> Expression
regroupLetBinding bs body =
  let mkN b@(LetBinding (PatternBinding p) e)       = ((fvs e, fvsP p), b)
      mkN b@(LetBinding (CombinatorBinding n ps) e) = mk (fvs e \\ mconcat (fvsP <$> ps)) [n] b
      groups                                        = fmap snd <$> re (mkN <$> bs)
   in foldr ELet body groups

regroupE' :: Expression -> Expression
regroupE' (ELet  bs e   ) = regroupLetBinding bs e
regroupE' (ECase e  alts) = ECase (regroupE' e) (regroupA <$> alts)
regroupE' (EApp  l  r   ) = EApp (regroupE' l) (regroupE' r)
regroupE' (EIf c t e    ) = EIf (regroupE' c) (regroupE' t) (regroupE' e)
regroupE' (ELam ps e    ) = ELam ps $ regroupE' e
regroupE' e               = e

regroupA :: CaseAlternative -> CaseAlternative
regroupA (CaseAlternative p x) = CaseAlternative p $ regroupE' x
