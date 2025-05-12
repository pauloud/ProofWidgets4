import ProofWidgets.Data.Svg
import ProofWidgets.StageM2.Graph
import ProofWidgets.StageM2.Util
open ProofWidgets Svg
def drawNode {f:Frame} (id : String) (content : String) (pos:Point f) : List (Element f) :=
  let text : Element f := Svg.text pos content (Size.px 20) |>.setFillString "var(--vscode-editor-foreground)"
  let ellipse := Svg.ellipse pos (Size.px (content.length * 20)) (Size.px 60) |>.setFillString "#3498db"
  [ellipse,text]

def drawGraph {f:Frame} (G:DiGraph) (contents : List String) : Svg f :=
  let range := List.range G.verticesNumber
  let list : List (Element f):= do
    let n:Nat <- range
    let id:String := "vertex_" ++ n.repr
    let content := G.verticesContent[n]!
    let elem <- drawNode id content (Point.px (n*30) 0)
    return elem
  {elements:=list.toArray}
