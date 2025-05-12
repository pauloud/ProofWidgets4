import Std.Data.HashSet.Basic
structure DiGraph where
  verticesNumber : Nat := 0
  verticesContent : Array String := Array.emptyWithCapacity 8
  arks : Std.HashSet (Nat × Nat) := Std.HashSet.emptyWithCapacity 16

def addNode (G:DiGraph) (content : String) :=
  let vC := G.verticesContent ++ Array.singleton content
  let vN := G.verticesNumber + 1
  {G with verticesNumber := vN, verticesContent := vC}

def addEdge (G:DiGraph) (v1 v2 : Nat) := {G with arks := G.arks.insert (v1,v2)}
