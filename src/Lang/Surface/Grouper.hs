{--- | 排序Let和CombinatorDefs, 使得每一组仅依赖组内和之前组的定义
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

type N a = (([Name], [Name]), a)
-- ((<deps>, <provides>), <payload>)

re' :: [Name] -> [N a] -> ([N a], [N a])
re' = split . satisfied
  where satisfied syms ((deps, _), _) = all (`elem` syms) deps

re :: [Name] -> [N a] -> [[N a]]
re syms = r syms []
  where r syms outs ins = case re' syms ins of
                            ([], cyc)  -> cyc:outs
                            (ok, pend) -> let introduced = mconcat $ (snd . fst) <$> ok
                                           in r (syms <> introduced) ((singleton <$> ok) <> outs) pend

mk :: [Name] -> [Name] -> a -> N a
mk deps provides payload = ((deps \\ provides, provides), payload)

regroupLetBinding :: [LetBinding] -> Expression -> Expression
regroupLetBinding bs body =
  let mkN b@(LetBinding (PatternBinding p) e)       = ((fvs e, fvsP p), b)
      mkN b@(LetBinding (CombinatorBinding n ps) e) = mk (fvs e \\ mconcat (fvsP <$> ps)) ([n]) b
      nodes                                         = mkN <$> bs
      depSyms                                       = mconcat $ fst . fst <$> nodes
      provideSyms                                   = mconcat $ snd . fst <$> nodes
      initialSyms                                   = depSyms \\ provideSyms
      groups                                        = fmap snd <$> re initialSyms nodes
   in foldr ELet body groups

regroupE' :: Expression -> Expression
regroupE' (ELet bs e)                              = regroupLetBinding bs e
regroupE' e                                        = e

regroupE :: Expression -> Expression
regroupE = propagateE regroupE'

