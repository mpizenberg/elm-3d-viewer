module OrbitingCamera exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle
import Axis3d
import Browser
import Browser.Events
import Color
import Html
import Html.Events.Extra.Wheel as Wheel
import Http
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Obj.Decode exposing (ObjCoordinates)
import OrbitCamera
import OrbitViewer exposing (OrbitViewer)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import TriangularMesh exposing (TriangularMesh)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


loadMesh : Cmd Msg
loadMesh =
    Http.get
        { url = "data/suzanne.obj.txt"
        , expect = Obj.Decode.expectObj GotMesh Length.meters Obj.Decode.triangles
        }


type Msg
    = GotMesh (Result Http.Error (TriangularMesh (Point3d Meters ObjCoordinates)))
    | MouseDown ( Quantity Float Pixels, Quantity Float Pixels )
    | MouseMove ( Quantity Float Pixels, Quantity Float Pixels )
    | MouseUp
    | KeyDown ModifierKey
    | KeyUp ModifierKey
    | ZoomIn ( Quantity Float Pixels, Quantity Float Pixels )
    | ZoomOut ( Quantity Float Pixels, Quantity Float Pixels )


type ModifierKey
    = ControlKey


type OrbitControlState
    = OrbitIdle
    | OrbitControlling ( Quantity Float Pixels, Quantity Float Pixels )


type alias Model =
    { mesh : Mesh ObjCoordinates { normals : () }
    , orbitViewer : OrbitViewer Meters ObjCoordinates
    , orbitControlState : OrbitControlState
    , ctrlKeyDown : Bool
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mesh = Mesh.facets []
      , ctrlKeyDown = False
      , orbitControlState = OrbitIdle
      , orbitViewer =
            { size = ( Pixels.pixels 800, Pixels.pixels 400 )
            , camera = OrbitCamera.init Point3d.origin (Length.meters 5)
            }
      }
    , loadMesh
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.orbitControlState ) of
        -- Create a triangular mesh from the loaded OBJ file
        ( GotMesh (Ok mesh), _ ) ->
            ( { model | mesh = Mesh.indexedFacets mesh }, Cmd.none )

        ( GotMesh (Err err), _ ) ->
            let
                _ =
                    Debug.log "OBJ decoder error:" err
            in
            ( model, Cmd.none )

        -- Record the fact that modifier keys, such as CTRL, are pressed down
        ( KeyDown key, _ ) ->
            case key of
                ControlKey ->
                    ( { model | ctrlKeyDown = True }, Cmd.none )

        ( KeyUp key, _ ) ->
            case key of
                ControlKey ->
                    ( { model | ctrlKeyDown = False }, Cmd.none )

        -- Start orbiting when a mouse button is pressed
        ( MouseDown pos, _ ) ->
            ( { model | orbitControlState = OrbitControlling pos }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        ( MouseUp, _ ) ->
            ( { model | orbitControlState = OrbitIdle }, Cmd.none )

        -- Orbit camera on mouse move
        ( MouseMove ( x2, y2 ), OrbitControlling ( x1, y1 ) ) ->
            let
                dx =
                    Quantity.difference x2 x1

                dy =
                    Quantity.difference y2 y1

                -- How fast we want to orbit the camera (orbiting the camera
                -- by 1 degree per pixel of drag is a decent default to start with)
                rotationRate =
                    Angle.degrees 1 |> Quantity.per Pixels.pixel

                updatedViewer =
                    if model.ctrlKeyDown then
                        OrbitViewer.pan ( dx, dy ) model.orbitViewer

                    else
                        OrbitViewer.orbit rotationRate ( dx, dy ) model.orbitViewer
            in
            ( { model
                | orbitViewer = updatedViewer
                , orbitControlState = OrbitControlling ( x2, y2 )
              }
            , Cmd.none
            )

        ( MouseMove _, _ ) ->
            ( model, Cmd.none )

        -- Zooming in
        ( ZoomIn pos, _ ) ->
            -- ( { model | orbitViewer = OrbitViewer.zoomIn model.orbitViewer }, Cmd.none )
            ( { model | orbitViewer = OrbitViewer.zoomToward pos model.orbitViewer }, Cmd.none )

        -- Zooming out
        ( ZoomOut pos, _ ) ->
            -- ( { model | orbitViewer = OrbitViewer.zoomOut model.orbitViewer }, Cmd.none )
            ( { model | orbitViewer = OrbitViewer.zoomAwayFrom pos model.orbitViewer }, Cmd.none )


chooseZoom : Wheel.Event -> Msg
chooseZoom wheelEvent =
    let
        mousePosition =
            wheelEvent.mouseEvent.clientPos
                |> Tuple.mapBoth Pixels.pixels Pixels.pixels
    in
    if wheelEvent.deltaY > 0 then
        ZoomOut mousePosition

    else
        ZoomIn mousePosition


{-| Use movementX and movementY for simplicity (don't need to store initial
mouse position in the model) - not supported in Internet Explorer though
-}
decodeMousePos : Decoder ( Quantity Float Pixels, Quantity Float Pixels )
decodeMousePos =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" (Decode.map Pixels.float Decode.float))
        (Decode.field "clientY" (Decode.map Pixels.float Decode.float))


keyDecoder : Decoder ModifierKey
keyDecoder =
    Decode.andThen toModifierKey (Decode.field "key" Decode.string)


toModifierKey : String -> Decoder ModifierKey
toModifierKey string =
    case string of
        "Control" ->
            Decode.succeed ControlKey

        _ ->
            Decode.fail ("Unsupported key: " ++ string)


subscriptions : Model -> Sub Msg
subscriptions model =
    -- If we're currently orbiting, listen for mouse moves and mouse button
    -- up events (to stop orbiting); in a real app we'd probably also want
    -- to listen for page visibility changes to stop orbiting if the user
    -- switches to a different tab or something
    let
        upAndDownListeners =
            [ Browser.Events.onMouseDown (Decode.map MouseDown decodeMousePos)
            , Browser.Events.onMouseUp (Decode.succeed MouseUp)
            , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
            , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
            ]

        mouseMoveListener =
            Browser.Events.onMouseMove (Decode.map MouseMove decodeMousePos)
    in
    if model.orbitControlState /= OrbitIdle then
        Sub.batch (mouseMoveListener :: upAndDownListeners)

    else
        Sub.batch upAndDownListeners


view : Model -> Browser.Document Msg
view model =
    { title = "OrbitingCamera"
    , body =
        [ Scene3d.cloudy
            { camera = OrbitCamera.toCamera3d model.orbitViewer.camera
            , upDirection = SketchPlane3d.normalDirection model.orbitViewer.camera.groundPlane
            , clipDepth = Length.meters 0.01
            , dimensions = Tuple.mapBoth Quantity.round Quantity.round model.orbitViewer.size
            , background = Scene3d.backgroundColor Color.lightGrey
            , entities =
                -- Draw the loaded OBJ mesh
                [ Scene3d.mesh (Material.matte Color.orange) model.mesh
                    |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
                    |> Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
                ]
            }
            |> List.singleton
            |> Html.div [ Wheel.onWheel chooseZoom ]
        , Html.div [] [ Html.text "Click and drag to rotate" ]
        , Html.div [] [ Html.text "Hold [CTRL] to pan instead of rotate" ]
        , Html.div [] [ Html.text "Use the mouse wheel to zoom in and out" ]
        ]
    }
