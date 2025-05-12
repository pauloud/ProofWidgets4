import ProofWidgets.Component.InteractiveSvg
import ProofWidgets.Component.HtmlDisplay

open Lean
open ProofWidgets Svg Jsx

abbrev State := Nat

def init := 0

def frame : Frame :=
{ xmin := -1
  ymin := -1
  xSize := 2
  width := 400
  height := 400 }

def center : Point frame := Point.abs 0.0 0.0

def text (n:State) : Element frame :=
(Svg.text center (Nat.repr n) (Size.px 20)).setFill {r:=255,g:=255,b:=255} |>.setId "textlabel" |>.setData "textlabel"

def update (time_ms Δt_ms : Float) (action : Action)
         (mouseStart mouseEnd : Option (Svg.Point frame))
         (selectedId : Option String) (getSelectedData : (α : Type) → [FromJson α] → Option α)
         (n: State) : State := match action.kind with
          |ActionKind.mouseup => n+1
          | _ => n
def render (n:State) : Svg frame := {elements := Array.singleton (text n)}

def isvg : InteractiveSvg State :=
{init := init,frame := frame,update := update, render := fun _ _ _ => render}

def initResult : UpdateResult State := {
  html := <div>Init!!!</div>,
  state := { state := isvg.init
             time := 0
             selected := none
             mousePos := none
             idToData := isvg.render 0 none none isvg.init |>.idToDataList}
}
open Server RequestM in
@[server_rpc_method]
def updateSvg (params : UpdateParams State) : RequestM (RequestTask (UpdateResult State)) := isvg.serverRpcMethod params

@[widget_module]
def SvgWidget : Component (UpdateResult State) where
  javascript := include_str ".." / ".." / ".lake" / "build" / "js" / "interactiveSvg.js"
#html <SvgWidget html={initResult.html} state={initResult.state}/>
