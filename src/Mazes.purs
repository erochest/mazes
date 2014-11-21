module Mazes where


import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Tuple
import Debug.Trace


type NodeNo = Number

data Node a = Node
            { number :: NodeNo
            , label  :: a
            , edges  :: [Edge]
            }

data Edge = E
          { from :: NodeNo
          , to   :: NodeNo
          }

type Graph a = M.Map NodeNo (Node a)

data Context a = C [Edge] (Node a) [Edge]

data Decomp a = Decomp (Context a) (Graph a)

data Hole = Hole

neighbors :: forall a. Context a -> Graph a -> [Node a]
neighbors (C incoming _ outgoing) g =
    mapMaybe (lookup g) $ outgoing <> incoming
    where
        lookup g (E e) = M.lookup e.to g

neighborEdges :: forall a. Context a -> Graph a -> [Edge]
neighborEdges (C incoming _ outgoing) g =
    filter (isJust <<< lookup g) $ outgoing <> incoming
    where
        lookup g (E e) = M.lookup e.to g

match :: forall a. Number -> Graph a -> Tuple (Maybe (Context a)) (Graph a)
match number g =
    case M.lookup number g of
         Just n@(Node node) -> Tuple (Just $ context n node.edges)
                                     (M.delete number g)
         Nothing          -> Tuple Nothing g
    where
        context n edges = C (filter (\(E e) -> e.to == number) edges)
                            n
                            (filter (\(E e) -> e.from == number) edges)

liftMaybe :: forall a. Tuple (Maybe (Context a)) (Graph a) -> Maybe (Decomp a)
liftMaybe (Tuple (Just c) g) = Just $ Decomp c g
liftMaybe (Tuple Nothing _)  = Nothing

matchAny :: forall a. Graph a -> Maybe (Decomp a)
matchAny g | M.isEmpty g = Nothing
matchAny g = case M.keys g of
                  (n:_) -> liftMaybe $ match n g
                  []    -> Nothing

insertNode :: forall a. Node a -> Graph a -> Graph a
insertNode n@(Node n') g = M.insert n'.number n g

insertContext :: forall a. Context a -> Graph a -> Graph a
insertContext (C _ n _) g = insertNode n g

ghead :: forall a. Graph a -> Maybe (Node a)
ghead g = getNode <$> matchAny g
      where getNode (Decomp (C _ n _) _) = n

gmap :: forall a b. (a -> b) -> Graph a -> Graph b
gmap _ g | M.isEmpty g = M.empty
gmap f g = case matchAny g of
                Just (Decomp (C i (Node n) o) g') ->
                    insertContext (C i (Node $ n { label = f n.label }) o)
                                  (gmap f g')
                Nothing -> M.empty

dfs :: forall a. Node a -> Graph a -> [Node a]
dfs start g = go [start] g
    where
        go :: forall a. [Node a] -> Graph a -> [Node a]
        go [] _ = []
        go _ g | M.isEmpty g = []
        go ((n@(Node n')):ns) g =
            case match n'.number g of
                 Tuple (Just c) g' -> n : (go ((neighbors c g') <> ns) g')
                 Tuple Nothing g'  -> go ns g'
        go (_:ns) g = go ns g

edfs :: forall a. Node a -> Graph a -> Eff (random :: Random) [Edge]
edfs start@(Node s) g =
    case match s.number g of
         Tuple (Just c) g' -> go (neighborEdges c g') g'
         Tuple Nothing g'  -> return []
    where
        go [] _ = return []
        go _ g | M.isEmpty g = return []
        go ((e@(E e')) : es) g =
            case match e'.to g of
                 Tuple (Just c) g' -> do
                     edges <- shuffle $ neighborEdges c g'
                     (:) e <$> (go (edges <> es) g')
                 Tuple Nothing g'  -> go es g'
        go (_:es) g = go es g

shuffle :: forall a. [a] -> Eff (random :: Random) [a]
shuffle a = go (length a) a
    where
        go 0 a = return a
        go n a = do
            i <- (*) n <$> random
            let n' = n - 1
            let x = fromJust $ a !! n'
            let y = fromJust $ a !! i
            go n' $ updateAt n' y $ updateAt n x a


main = do
    print "Hello, world!"
    random >>= print <<< ((++) "Random!! ") <<< show
