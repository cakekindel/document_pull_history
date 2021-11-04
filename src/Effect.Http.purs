module Effect.Http where

import Milkis as M
import Milkis.Impl.Node (nodeFetch)

fetch :: M.Fetch
fetch = M.fetch nodeFetch
