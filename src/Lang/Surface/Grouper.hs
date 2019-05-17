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
module Lang.Surface.Grouper
where

import           Debug.Trace
import           Data.List                      ( union
                                                , intersect
                                                , (\\)
                                                )
import           Misc
import           Lang.Surface
import           Lang.Surface.Subst

-- | 为Let分组
regroupE :: Expression -> Expression
regroupE = propagateE regroupE'

-- | 为toplevel分组
regroupCombinators :: [CombinatorDef] -> [[CombinatorDef]]
regroupCombinators defs =
  let mkN d@(CombinatorDef name pats body) = mk (fvs body \\ mconcat (fvsP <$> pats)) [name] d
   in fmap snd <$> re (mkN <$> defs)

type N a = (([Name], [Name]), a)
-- ((<deps>, <provides>), <payload>)

re' :: [Name] -> [N a] -> ([N a], [N a])
re' = split . satisfied
  where satisfied syms ((deps, _), _) = all (`elem` syms) deps

re ::  [N a] -> [[N a]]
re nodes = r initialDeps [] nodes
  where allDeps         = mconcat $ fst . fst <$> nodes
        allProvides     = mconcat $ snd . fst <$> nodes
        initialDeps     = allDeps \\ allProvides
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
regroupE' (ELet bs e) = regroupLetBinding bs e
regroupE' e           = e

