
import System.FilePath
import System.Directory
import Control.Monad (forM, forM_)
import Data.List (sortBy, isSuffixOf, isPrefixOf, isSubsequenceOf, isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Config = MkConfig
  {
    lhsPath :: FilePath
  , outDir  :: FilePath
  , extName :: FilePath
  , logFile :: FilePath
  }

cfg0 :: Config
cfg0 = MkConfig {
  lhsPath = "data/unsafe/",
  outDir  = "data/unsafe_dedup/",
  extName = ".lhs",
  logFile = "data/done.txt"
}

main :: IO ()
main = do
  putStrLn "Hello, sowld!"
  fd       <- dedupFiles cfg0
  -- takeFile <- mkTaker    cfg0
  let fd' = [(file, tx file contents) | (file, contents) <- fd {-, takeFile file -} ]
  writeFileData cfg0 fd'

type FileData = [(FilePath, String)]

_mkTaker :: Config -> IO (FilePath -> Bool)
_mkTaker cfg = do
  ls <- lines <$> readFile (logFile cfg)
  let files = S.fromList (fileName <$> ls)
  putStrLn ("files: " ++ show files)
  return (\f -> not (takeFileName f `S.member` files))

fileName :: FilePath -> String
fileName = head . splitOn ".log"

--------------------------------------------------------------------------------------------------------------------------------
newtype Transform = MkT (FilePath -> String -> String)
--------------------------------------------------------------------------------------------------------------------------------

tx :: FilePath -> String -> String
tx = runTx $ foldr1 (\(MkT f1) (MkT f2) -> MkT (\f s -> f1 f (f2 f s))) transforms

runTx :: Transform -> FilePath -> String -> String
runTx (MkT f) = f

mkTx :: (FilePath -> [String] -> [String]) -> Transform
mkTx f = MkT (\file -> unlines . f file . lines)

transforms :: [Transform]
transforms =
  [ indentBetween "{-@ measure" "@-}"
  , insertAfter "{-@ LIQUID \"--no-termination" pruneUnsorted
  , insertAfter "module MapReduce"              pruneUnsorted
  , wrapCode
  ]
  where
    pruneUnsorted = "{-@ LIQUID \"--prune-unsorted\" @-}"

_myLines :: [String]
_myLines =
  [ "{-@ measure foo :: List -> Int"
  , "    foo (A) = 10"
  , "    foo (B) = 12"
  , "  @-}"
  , "bar"
  , "{-@ measure cludim :: Cluster -> Int"
  , "    cludim (n, p)  = size p"
  , "  @-}"
  , "{-@ mergeCluster :: n:Nat -> ClusterN n -> ClusterN n -> ClusterN n @-}"
  , "mergeCluster n (n1, p1) (n2, p2) = (n1 + n2, zipWith (+) p1 p2)"
  ]

-- >>> tx (unlines _myLines)
-- "{-@ measure foo :: List -> Int\n      foo (A) = 10\n      foo (B) = 12\n  @-}\nbar\n{-@ measure cludim :: Cluster -> Int\n      cludim (n, p)  = size p\n  @-}\n{-@ mergeCluster :: n:Nat -> ClusterN n -> ClusterN n -> ClusterN n @-}\nmergeCluster n (n1, p1) (n2, p2) = (n1 + n2, zipWith (+) p1 p2)\n"

wrapCode :: Transform
wrapCode = mkTx (\file ls -> if isHsFile file then wrap ls else ls)
  where
    wrap str   = ["\\begin{code}\n"] ++ str ++ ["\n\\end{code}\n"]
    isHsFile f = ".hs-" `isInfixOf` f
--------------------------------------------------------------------------------------------------------------------------------
-- | Transform : `indentAfter prefix k` indents `k` lines after any line matching `prefix` by 2 spaces
--------------------------------------------------------------------------------------------------------------------------------
_indentAfter :: String -> Int -> Transform
_indentAfter prefix k = mkTx (const go)
  where
    go :: [String] -> [String]
    go (l:ls)
      | prefix `isPrefixOf` l = let (ls', rest) = splitAt k ls
                                in l : (("  " ++) <$> ls') ++ go rest
      | otherwise             = l : go ls
    go []                     = []

-- | Transform : `indentBetween start end` indents all lines AFTER any line matching `start` and BEFORE matching `end`
indentBetween :: String -> String -> Transform
indentBetween start end = mkTx (const go)
  where
    go :: [String] -> [String]
    go (l:ls)
      | start `isPrefixOf` l = l : goI ls
      | otherwise            = l : go ls
    go []                    = []

    goI (l:ls)
      | end `isSubsequenceOf` l   = l : go ls
      | otherwise            = (pad ++ l) : goI ls
    goI []                   = []
    pad                      = replicate 8 ' '

--------------------------------------------------------------------------------------------------------------------------------
-- | `insertAfter prefix str` inserts a line with `str` right after a line matching `prefix`
--------------------------------------------------------------------------------------------------------------------------------
insertAfter :: String -> String -> Transform
insertAfter prefix str = mkTx (const go)
  where
    go :: [String] -> [String]
    go (l:ls)
      | prefix `isPrefixOf` l = l : str : go ls
      | otherwise             = l : go ls
    go []                     = []




--------------------------------------------------------------------------------------------------------------------------------
-- | Dedup Files
--------------------------------------------------------------------------------------------------------------------------------

dedupFiles :: Config -> IO FileData
dedupFiles cfg = do
  fd <- readFileData cfg
  return (dedupFileData fd)


readFileData :: Config -> IO FileData
readFileData cfg = do
  files <- getDirectoryContents (lhsPath cfg)
  let files' = filter (\file -> extName cfg `isSuffixOf` file) files
  forM files' $ \file -> do
    let file' = lhsPath cfg </> file
    putStrLn ("reading: " ++ file')
    contents <- readFile file'
    return (file, contents)

dedupFileData :: FileData -> FileData
dedupFileData = dedup . sortBy (\(a,_) (b,_) -> compare a b)

dedup :: (Eq v) => [(k, v)] -> [(k, v)]
dedup ((k,v):rest@((_,v'):_))
  | v == v'   =          dedup rest
  | otherwise = (k, v) : dedup rest
dedup kvs     = kvs

writeFileData :: Config -> FileData -> IO ()
writeFileData cfg fds = do
  forM_ fds $ \(file, contents) -> do
    let outFile = outDir cfg </> file
    putStrLn ("writing: " ++ outFile)
    writeFile (outDir cfg </> file) contents
