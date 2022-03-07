module OrbitingCameraAndLight exposing (main)

{-| This example shows how you can allow orbiting of a scene by listening for
mouse events and moving the camera accordingly.
-}

import Angle
import Axis3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d exposing (Direction3d)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Frame3d
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import Http
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Obj.Decode exposing (ObjCoordinates)
import OrbitCamera
import OrbitViewer exposing (OrbitViewer)
import Phosphor
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SketchPlane3d
import Sphere3d
import Task
import TriangularMesh exposing (TriangularMesh)
import Vector2d
import Vector3d
import Viewpoint3d


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
    | WindowResize Float Float
    | SelectController Controller
    | MouseDown ( Quantity Float Pixels, Quantity Float Pixels )
    | MouseMove ( Quantity Float Pixels, Quantity Float Pixels )
    | MouseUp
    | KeyDown ModifierKey
    | KeyUp ModifierKey
    | LightDown (Maybe ( Quantity Float Pixels, Quantity Float Pixels ))
    | LightMoves ( Quantity Float Pixels, Quantity Float Pixels )
    | ZoomIn ( Quantity Float Pixels, Quantity Float Pixels )
    | ZoomOut ( Quantity Float Pixels, Quantity Float Pixels )


type ModifierKey
    = ControlKey


type PointerState
    = PointerIdle
    | PointerMoving ( Quantity Float Pixels, Quantity Float Pixels )


type Controller
    = RotationControl
    | PanControl
    | ZoomControl


type alias Model =
    { mesh : Mesh ObjCoordinates { normals : () }
    , orbitViewer : OrbitViewer Meters ObjCoordinates
    , pointerState : PointerState
    , controller : Controller
    , lightDirection : Direction3d ObjCoordinates
    , lightSphere : Scene3d.Entity ObjCoordinates
    , lightOrbitViewer : OrbitViewer Meters ObjCoordinates
    , lightPointerDown : Maybe ( Quantity Float Pixels, Quantity Float Pixels )
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mesh = Mesh.facets []
      , pointerState = PointerIdle
      , controller = RotationControl
      , orbitViewer =
            { size = ( Pixels.pixels 800, Pixels.pixels 400 )
            , camera = OrbitCamera.init Point3d.origin (Length.meters 5)
            }
      , lightPointerDown = Nothing
      , lightDirection = Direction3d.negativeZ
      , lightSphere =
            Scene3d.sphere
                (Material.texturedMatte (Material.constant Color.white))
                (Sphere3d.atOrigin (Length.meters 0.35))
      , lightOrbitViewer =
            { size = ( Pixels.pixels 200, Pixels.pixels 200 )
            , camera = OrbitCamera.init Point3d.origin (Length.meters 2)
            }
      }
    , Cmd.batch
        [ loadMesh
        , Browser.Dom.getViewport
            |> Task.perform (\vp -> WindowResize vp.viewport.width vp.viewport.height)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.pointerState, model.lightPointerDown ) of
        -- Create a triangular mesh from the loaded OBJ file
        ( GotMesh (Ok mesh), _, _ ) ->
            ( { model | mesh = Mesh.indexedFacets mesh }, Cmd.none )

        ( GotMesh (Err _), _, _ ) ->
            ( model, Cmd.none )

        -- Update the viewer when the window is resized
        ( WindowResize width height, _, _ ) ->
            ( { model
                | orbitViewer =
                    OrbitViewer.resize ( Pixels.float width, Pixels.float height ) model.orbitViewer
              }
            , Cmd.none
            )

        -- Select the controller to use when the pointer moves
        ( SelectController controller, _, _ ) ->
            ( { model | controller = controller }, Cmd.none )

        -- Record the fact that modifier keys, such as CTRL, are pressed down
        ( KeyDown key, _, _ ) ->
            case ( key, model.controller ) of
                ( ControlKey, RotationControl ) ->
                    ( { model | controller = PanControl }, Cmd.none )

                ( ControlKey, PanControl ) ->
                    ( { model | controller = RotationControl }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( KeyUp key, _, _ ) ->
            case ( key, model.controller ) of
                ( ControlKey, RotationControl ) ->
                    ( { model | controller = PanControl }, Cmd.none )

                ( ControlKey, PanControl ) ->
                    ( { model | controller = RotationControl }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- Start orbiting when a mouse button is pressed
        ( MouseDown pos, _, _ ) ->
            ( { model | pointerState = PointerMoving pos }, Cmd.none )

        -- Stop orbiting when a mouse button is released
        ( MouseUp, _, _ ) ->
            ( { model | pointerState = PointerIdle }, Cmd.none )

        -- Orbit camera on mouse move
        ( MouseMove ( x2, y2 ), PointerMoving ( x1, y1 ), _ ) ->
            let
                dx =
                    Quantity.difference x2 x1

                dy =
                    Quantity.difference y2 y1

                -- How fast we want to orbit the camera (orbiting the camera
                -- by 1 degree per pixel of drag is a decent default to start with)
                rotationRate =
                    Angle.degrees 1 |> Quantity.per Pixels.pixel

                zoomRate =
                    Quantity.float 0.01 |> Quantity.per Pixels.pixel

                updatedViewer =
                    case model.controller of
                        PanControl ->
                            OrbitViewer.pan ( dx, dy ) model.orbitViewer

                        RotationControl ->
                            OrbitViewer.orbit rotationRate ( dx, dy ) model.orbitViewer

                        ZoomControl ->
                            OrbitViewer.zoom zoomRate dx model.orbitViewer
            in
            ( { model
                | orbitViewer = updatedViewer
                , pointerState = PointerMoving ( x2, y2 )

                -- Match the orienation of the main camera for the light camera
                , lightOrbitViewer = OrbitViewer.setOrientation updatedViewer.camera model.lightOrbitViewer
              }
            , Cmd.none
            )

        ( MouseMove _, PointerIdle, _ ) ->
            ( model, Cmd.none )

        ( LightMoves _, _, Nothing ) ->
            ( model, Cmd.none )

        ( LightMoves ( x2, y2 ), _, Just ( x1, y1 ) ) ->
            let
                dx =
                    Quantity.difference x2 x1

                dy =
                    Quantity.difference y2 y1

                -- View frame moved to the origin to only account for rotations
                viewFrameOrigin =
                    OrbitCamera.toCamera3d model.lightOrbitViewer.camera
                        |> Camera3d.viewpoint
                        |> Viewpoint3d.viewPlane
                        |> SketchPlane3d.toFrame
                        |> Frame3d.moveTo Point3d.origin

                -- Rotation vector in the viewing plane
                rotationVectorLocal =
                    Vector3d.xyz dx (Quantity.negate dy) Quantity.zero
                        |> Vector3d.rotateAround Axis3d.z (Angle.degrees 90)

                -- Rotation axis in the local coordinates (with origin shifted to 0,0)
                rotationAxisLocal =
                    Axis3d.through
                        Point3d.origin
                        (Vector3d.direction rotationVectorLocal
                            |> Maybe.withDefault Direction3d.y
                        )

                -- Rotation axis in world coordinates
                rotationAxisGlobal =
                    Axis3d.placeIn viewFrameOrigin rotationAxisLocal

                -- Rotation magnitude
                rotationMagnitude =
                    Vector2d.length (Vector2d.xy dx dy)
                        |> Quantity.at (Angle.degrees 1 |> Quantity.per Pixels.pixel)

                -- Rotated light direction
                updatedLightDirection =
                    Direction3d.rotateAround rotationAxisGlobal rotationMagnitude model.lightDirection
            in
            ( { model | lightDirection = updatedLightDirection, lightPointerDown = Just ( x2, y2 ) }, Cmd.none )

        -- Record the current start/stop interaction with the light direction
        ( LightDown lightPointerIsDown, _, _ ) ->
            ( { model | lightPointerDown = lightPointerIsDown }, Cmd.none )

        -- Zooming in
        ( ZoomIn pos, _, _ ) ->
            -- ( { model | orbitViewer = OrbitViewer.zoomIn model.orbitViewer }, Cmd.none )
            ( { model | orbitViewer = OrbitViewer.zoomToward pos model.orbitViewer }, Cmd.none )

        -- Zooming out
        ( ZoomOut pos, _, _ ) ->
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
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        , Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "OrbitingCamera"
    , body =
        [ Element.layout
            [ Element.inFront (viewControls model)
            , Element.inFront (viewLight model)
            ]
            (viewElmUi model)
        ]
    }


viewControls : Model -> Element Msg
viewControls model =
    let
        controlButton selected msg icon =
            let
                shadow =
                    if selected then
                        Element.Border.glow (Element.rgb 0 0 0) 4

                    else
                        Element.Border.glow (Element.rgb 0 0 0) 0
            in
            icon Phosphor.Regular
                |> Phosphor.withSize 48
                |> Phosphor.withSizeUnit "px"
                |> Phosphor.toHtml []
                |> Element.html
                |> Element.el [ Element.padding 8 ]
                |> Element.el [ shadow, Element.Border.rounded 8, Element.Events.onClick msg ]

        ( width, height ) =
            model.orbitViewer.size
    in
    if width |> Quantity.greaterThan height then
        Element.column
            [ Element.Background.color (Element.rgba 0.3 0.3 0.3 0.85)
            , Element.centerY
            , Element.moveRight 32
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Border.rounded 8
            ]
            [ controlButton (model.controller == RotationControl) (SelectController RotationControl) Phosphor.planet
            , controlButton (model.controller == PanControl) (SelectController PanControl) Phosphor.handGrabbing
            , controlButton (model.controller == ZoomControl) (SelectController ZoomControl) Phosphor.magnifyingGlassPlus
            ]

    else
        Element.row
            [ Element.Background.color (Element.rgba 0.3 0.3 0.3 0.85)
            , Element.centerX
            , Element.moveDown 32
            , Element.Font.color (Element.rgb 1 1 1)
            , Element.Border.rounded 8
            ]
            [ controlButton (model.controller == RotationControl) (SelectController RotationControl) Phosphor.planet
            , controlButton (model.controller == PanControl) (SelectController PanControl) Phosphor.handGrabbing
            , controlButton (model.controller == ZoomControl) (SelectController ZoomControl) Phosphor.magnifyingGlassPlus
            ]


viewElmUi : Model -> Element Msg
viewElmUi model =
    let
        commonListeners =
            [ Wheel.onWheel chooseZoom
            , Html.Events.preventDefaultOn "pointerdown" (Decode.map (\e -> ( MouseDown e, True )) decodeMousePos)
            , Html.Events.preventDefaultOn "pointerup" (Decode.succeed ( MouseUp, True ))
            , Html.Attributes.style "touch-action" "none"
            ]

        mouseMoveListener =
            Html.Events.preventDefaultOn "pointermove" (Decode.map (\e -> ( MouseMove e, True )) decodeMousePos)

        allListeners =
            if model.pointerState == PointerIdle then
                commonListeners

            else
                mouseMoveListener :: commonListeners

        scene3d =
            Scene3d.sunny
                { camera = OrbitCamera.toCamera3d model.orbitViewer.camera
                , shadows = False
                , sunlightDirection = model.lightDirection
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
                |> Html.div allListeners
                |> Element.html
    in
    scene3d


viewLight : Model -> Element Msg
viewLight model =
    let
        commonListeners =
            [ Html.Events.preventDefaultOn "pointerdown" (Decode.map (\e -> ( LightDown (Just e), True )) decodeMousePos)
            , Html.Events.preventDefaultOn "pointerup" (Decode.succeed ( LightDown Nothing, True ))
            , Html.Attributes.style "touch-action" "none"
            ]

        mouseMoveListener =
            Html.Events.preventDefaultOn "pointermove" (Decode.map (\e -> ( LightMoves e, True )) decodeMousePos)

        allListeners =
            case model.lightPointerDown of
                Nothing ->
                    commonListeners

                Just _ ->
                    mouseMoveListener :: commonListeners
    in
    Scene3d.sunny
        { camera = OrbitCamera.toCamera3d model.lightOrbitViewer.camera
        , shadows = False
        , sunlightDirection = model.lightDirection
        , upDirection = SketchPlane3d.normalDirection model.lightOrbitViewer.camera.groundPlane
        , clipDepth = Length.meters 0.01
        , dimensions = Tuple.mapBoth Quantity.round Quantity.round model.lightOrbitViewer.size
        , background = Scene3d.transparentBackground

        -- Draw the 1m diameter sphere to show the light
        , entities = [ model.lightSphere ]
        }
        |> List.singleton
        |> Html.div allListeners
        |> Element.html
        |> Element.el [ Element.alignRight, Element.alignBottom ]
