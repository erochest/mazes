# Module Documentation

## Module Mazes

### Types

    data Context a where
      C :: [Edge] -> Node a -> [Edge] -> Context a

    data Decomp a where
      Decomp :: Context a -> Graph a -> Decomp a

    data Edge where
      E :: { to :: NodeNo, from :: NodeNo } -> Edge

    type Graph a = M.Map NodeNo (Node a)

    data Hole where
      Hole :: Hole

    data Node a where
      Node :: { edges :: [Edge], label :: a, number :: NodeNo } -> Node a

    type NodeNo = Number


### Values

    dfs :: forall a. Node a -> Graph a -> [Node a]

    edfs :: forall a. Node a -> Graph a -> Eff (random :: Random) [Edge]

    ghead :: forall a. Graph a -> Maybe (Node a)

    gmap :: forall a b. (a -> b) -> Graph a -> Graph b

    insertContext :: forall a. Context a -> Graph a -> Graph a

    insertNode :: forall a. Node a -> Graph a -> Graph a

    liftMaybe :: forall a. Tuple (Maybe (Context a)) (Graph a) -> Maybe (Decomp a)

    match :: forall a. Number -> Graph a -> Tuple (Maybe (Context a)) (Graph a)

    matchAny :: forall a. Graph a -> Maybe (Decomp a)

    neighborEdges :: forall a. Context a -> Graph a -> [Edge]

    neighbors :: forall a. Context a -> Graph a -> [Node a]



