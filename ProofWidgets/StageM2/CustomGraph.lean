import ProofWidgets.Component.Basic
import ProofWidgets.Data.Html
import ProofWidgets.Component.HtmlDisplay

namespace ProofWidgets.CustomGraph
open Lean Server Jsx ProofWidgets RequestM

/-- A themed `<circle>` SVG element, with optional extra attributes. -/
def mkCircle (attrs : Array (String × Json) := #[]) : Html :=
  <circle
    r={5}
    fill="var(--vscode-editor-background)"
    stroke="var(--vscode-editor-foreground)"
    strokeWidth={.num 1.5}
    {...attrs}
  />

/-- A shape containing the vertex label.
Used to position incident edge endpoints.
The shape is assumed to be centred on the vertex position. -/
-- TODO: use `getBoundingClientRect` to dynamically compute size
inductive BoundingShape where
  /-- A circle of fixed radius. -/
  | circle (radius : Float) : BoundingShape
  /-- A rectangle of fixed dimensions. -/
  | rect (width height : Float) : BoundingShape
  deriving Inhabited, FromJson, ToJson

structure Vertex where
  /-- Identifier for this vertex. Must be unique. -/
  id : String
  /-- The label is drawn at the vertex position.
  This must be an SVG element.
  Use `<foreignObject>` to draw non-SVG elements. -/
  label : Html := mkCircle
  boundingShape : BoundingShape := .circle 5
  /-- Details are shown below the graph display
  after the vertex label has been clicked.
  See also `Props.showDetails`. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure Edge where
  /-- Source vertex. Must match the `id` of one of the vertices. -/
  source : String
  /-- Target vertex. Must match the `id` of one of the vertices. -/
  target : String
  /-- Extra attributes to set on the SVG `<line>` element representing this edge.
  See also `Props.defaultEdgeAttrs`. -/
  attrs : Array (String × Json) := #[]
  /-- If present, the label is shown over the edge midpoint.
  This must be an SVG element.
  Use `<foreignObject>` to draw non-SVG elements. -/
  label? : Option Html := none
  /-- Details are shown below the graph display
  after the edge has been clicked.
  See also `Props.showDetails`. -/
  details? : Option Html := none
  deriving Inhabited, RpcEncodable

structure ForceCenterParams where
  x? : Option Float := none
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceCollideParams where
  radius? : Option Float := none
  strength? : Option Float := none
  iterations? : Option Nat := none
  deriving Inhabited, FromJson, ToJson

structure ForceLinkParams where
  distance? : Option Float := none
  strength? : Option Float := none
  iterations? : Option Nat := none
  deriving Inhabited, FromJson, ToJson

structure ForceManyBodyParams where
  strength? : Option Float := none
  theta? : Option Float := none
  distanceMin? : Option Float := none
  distanceMax? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceXParams where
  x? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceYParams where
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

structure ForceRadialParams where
  radius : Float
  x? : Option Float := none
  y? : Option Float := none
  strength? : Option Float := none
  deriving Inhabited, FromJson, ToJson

/-- Settings for the simulation of forces on vertices.
See https://d3js.org/d3-force. -/
inductive ForceParams where
  | center : ForceCenterParams → ForceParams
  | collide : ForceCollideParams → ForceParams
  | link : ForceLinkParams → ForceParams
  | manyBody : ForceManyBodyParams → ForceParams
  | x : ForceXParams → ForceParams
  | y : ForceYParams → ForceParams
  | radial : ForceRadialParams → ForceParams
  deriving Inhabited, FromJson, ToJson

structure Props where
  vertices : Array Vertex
  /-- At most one edge may exist between any two vertices.
  Self-loops are allowed,
  but (TODO) are currently not rendered well. -/
  edges : Array Edge
  /-- Attributes to set by default on `<line>` elements representing edges. -/
  defaultEdgeAttrs : Array (String × Json) := #[
    ("fill", "var(--vscode-editor-foreground)"),
    ("stroke", "var(--vscode-editor-foreground)"),
    ("strokeWidth", 2),
    ("markerEnd", "url(#arrow)")
  ]
  /-- Which forces to apply to the vertices.
  Most force parameters are optional, using default values if not specified. -/
  forces : Array ForceParams := #[ .link {}, .manyBody {}, .x {}, .y {} ]
  /-- Whether to show a details box below the graph. -/
  showDetails : Bool := false
  deriving Inhabited, RpcEncodable



/-- Display a graph with an interactive force simulation. -/









def stable (nodes : List (String × String)) : CustomGraph.Props := runST (fun _ => do
  let mut nodesWithInfos : Array CustomGraph.Vertex := #[]
  let mut maxRadius := 10
  for node in nodes.toArray do
      let content := node.snd
      let rx := content.length * 3
      maxRadius := Nat.max maxRadius rx
      let newNode : CustomGraph.Vertex := {
        id := node.fst
        label :=
          <g>
            <ellipse
              fill="var(--vscode-editor-foreground)"
              stroke="var(--vscode-editorHoverWidget-border)"
              rx={(rx*2 : Nat)}
              ry="10"
            />
            <text x={s!"-{rx}"} y="5" className="font-code">{.text content}</text>
          </g>
        boundingShape := .rect (rx*4).toFloat 20
      }
      nodesWithInfos := nodesWithInfos.push newNode
  /-return <CustomGraph
      vertices={nodesWithInfos}
      edges={[].foldl (init := #[]) fun acc (a,b) => acc.push {source := a, target := b}}
      forces={#[
        .link { distance? := Float.ofNat (maxRadius * 2) },
        .collide { radius? := Float.ofNat maxRadius },
        .x { strength? := some 0.05 },
        .y { strength? := some 0.05 }
      ]}
      showDetails={true}
    />-/
    return {
      vertices :=nodesWithInfos
      edges :=[].foldl (init := #[]) fun acc (a,b) => acc.push {source := a, target := b}
      forces:=#[
        .link { distance? := Float.ofNat (maxRadius * 2) },
        .collide { radius? := Float.ofNat maxRadius },
        .x { strength? := some 0.05 },
        .y { strength? := some 0.05 }
      ]
      showDetails:=true
    }

)
abbrev foo := stable [("toto","toto") , ("titi","titi")]


@[server_rpc_method]
def sendMessage (msg : String) : RequestM (RequestTask CustomGraph.Props) :=
  return (ServerTask.pure (Except.pure (stable [(msg,msg)])))


@[widget_module]
def CustomGraph : Component CustomGraph.Props where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "customGraph.js"



#html Html.ofComponent CustomGraph foo Array.empty

end CustomGraph
