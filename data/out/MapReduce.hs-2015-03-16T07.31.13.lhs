\begin{code}

{-
Map-Reduce
==========

<div class="hidden">
-}
module MapReduce (mapReduce) where
{-@ LIQUID "--prune-unsorted" @-}

import qualified Data.Map as M
import List
import Prelude hiding (map, reduce, concat, foldr, foldr1)
expand   :: (a -> List (k, v)) -> List a -> List (k, v)
group    :: (Ord k) => List (k, v) -> M.Map k (List v)
collapse :: (v -> v -> v) -> M.Map k (List v) -> M.Map k v
{-
</div>

The following is a super simplified implementation of
[Map-Reduce](http://en.wikipedia.org/wiki/MapReduce)
using the [Lists](List.lhs) and `Data.Map`.

-}

--Non-empty lists
{-@ type ListNE a = {v:List a | size v > 0} @-}

{-@ mapReduce :: (a -> List (k, v))
                     -> (v -> v -> v)
                     -> ListNE a
                     -> M.Map k v @-}
mapReduce :: (Ord k) => (a -> List (k, v))
                     -> (v -> v -> v)
                     -> List a
                     -> M.Map k v

mapReduce fm fr xs = kvm
  where
    kvs   = expand      fm xs     -- step 1
    kvsm  = group       kvs       -- step 2
    kvm   = collapse fr kvsm      -- step 3
{-

**The Problem** If you solved `foldr1` then you should
get a single type error below, in the call to `foldr1`
in `collapse`. Fix the error by **modifying the
refinement type specifications** only (do not modify any code).

**Note:** This problem requires you to have solved the
`foldr1` problem from [List.lhs](List.lhs); otherwise
no points.

Next, we briefly describe and show each step of the
`mapReduce` function.

Step 1: Map Elements into Key-Value Lists
-----------------------------------------

-}
{-@ expand  :: (a -> ListNE (k, v)) -> ListNE a -> ListNE (k, v) @-}
expand f xs = concat (specialMap f xs)
--expand f xs = concat (map f xs)

{-@ specialMap :: (a -> ListNE b) -> ListNE a -> ListNE b @-}
specialMap f xs = map f xs
{-

Step 2: Group By Key
--------------------

-}
{-@ group :: (Ord k) => ListNE (k, v) -> M.Map k (ListNE v) @-}
group     = foldr addKV  M.empty

addKV (k,v) m = M.insert k vs' m
  where
    vs'       = add v (M.findWithDefault empty k m)
{-

Step 3: Reduce Each Key to Single Value
---------------------------------------

-}

{-@ collapse  :: (v -> v -> v) -> M.Map k (ListNE v) -> M.Map k v @-}
collapse f = M.map (foldr1 f)

toList :: M.Map k v -> List (k, v)
toList = M.foldrWithKey (\k v acc -> add (k, v) acc) empty

\end{code}

