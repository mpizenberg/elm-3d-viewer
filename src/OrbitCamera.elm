module OrbitCamera exposing (..)

{-| Helper module to handle a camera able to navigate a scene and to orbit around its focus point.
-}

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d


{-| A camera able to navigate a scene and to orbit around its focal point.

The orientation of the camera is defined by its azimuth and elevation angles,
with regard to a ground plane and its perpendicular up direction.

Instead of always controlling all the camera parameters,
a simple alternative is just to initialize it with default parameters
for the ground plane, azimuth, and elevation,
and then just use the provided helpers to navigate the scene
in response to user mouse events.

-}
type alias OrbitCamera units coordinates =
    { focalPoint : Point3d units coordinates
    , groundPlane : SketchPlane3d units coordinates {}
    , azimuth : Angle
    , elevation : Angle
    , distance : Quantity Float units
    , projection : Projection units
    }


{-| The type of 3D projection, currently only perspective projection is supported.
-}
type Projection units
    = Perspective Angle



-- Constructors


{-| Initialize a 3D camera focusing on a given point at some distance of the point,
with default parameters for the camera orientation.
-}
init : Point3d units coordinates -> Quantity Float units -> OrbitCamera units coordinates
init focalPoint distance =
    { focalPoint = focalPoint
    , groundPlane = SketchPlane3d.xy
    , azimuth = Angle.degrees 30
    , elevation = Angle.degrees 30
    , distance = distance
    , projection = Perspective (Angle.degrees 30)
    }



-- ROTATIONS #########################################################


{-| Rotate sideways around the orbit center.
-}
rotateAround : Angle -> OrbitCamera units coordinates -> OrbitCamera units coordinates
rotateAround angle camera =
    { focalPoint = camera.focalPoint
    , groundPlane = camera.groundPlane
    , azimuth = Quantity.fractionalModBy (Angle.degrees 360) (Quantity.plus angle camera.azimuth)
    , elevation = camera.elevation
    , distance = camera.distance
    , projection = camera.projection
    }


{-| Rotate up or down around by changing the camera elevation.
-}
elevate : Angle -> OrbitCamera units coordinates -> OrbitCamera units coordinates
elevate angle camera =
    { focalPoint = camera.focalPoint
    , groundPlane = camera.groundPlane
    , azimuth = camera.azimuth
    , elevation = Quantity.fractionalModBy (Angle.degrees 360) (Quantity.plus angle camera.elevation)
    , distance = camera.distance
    , projection = camera.projection
    }



-- TRANSLATIONS ######################################################


{-| Move the camera to a new focus point.
-}
focusAt : Point3d units coordinates -> OrbitCamera units coordinates -> OrbitCamera units coordinates
focusAt focalPoint camera =
    { focalPoint = focalPoint
    , groundPlane = camera.groundPlane
    , azimuth = camera.azimuth
    , elevation = camera.elevation
    , distance = camera.distance
    , projection = camera.projection
    }


{-| Move the camera by the given displacement vector.
-}
translateBy : Vector3d units coordinates -> OrbitCamera units coordinates -> OrbitCamera units coordinates
translateBy vector camera =
    { focalPoint = Point3d.translateBy vector camera.focalPoint
    , groundPlane = camera.groundPlane
    , azimuth = camera.azimuth
    , elevation = camera.elevation
    , distance = camera.distance
    , projection = camera.projection
    }


{-| Move the camera in the current viewing plane (orthogonal to viewing direction).
-}
viewPlaneTranslateBy : Vector2d units coordinates -> OrbitCamera units coordinates -> OrbitCamera units coordinates
viewPlaneTranslateBy v2d camera =
    translateBy (viewPlaneTranslation v2d camera) camera


viewPlaneTranslation : Vector2d units coordinates -> OrbitCamera units coordinates -> Vector3d units coordinates
viewPlaneTranslation v2d camera =
    let
        viewPoint =
            Viewpoint3d.orbit
                { focalPoint = camera.focalPoint
                , groundPlane = camera.groundPlane
                , azimuth = camera.azimuth
                , elevation = camera.elevation
                , distance = camera.distance
                }
    in
    Vector3d.on (Viewpoint3d.viewPlane viewPoint) v2d



-- SCALING ###########################################################


{-| Zoom in, by getting closer to the focal point.
-}
zoomIn : OrbitCamera units coordinates -> OrbitCamera units coordinates
zoomIn camera =
    zoomBy zoomInCoef camera


{-| Zoom out, by getting away from the focal point.
-}
zoomOut : OrbitCamera units coordinates -> OrbitCamera units coordinates
zoomOut camera =
    zoomBy zoomOutCoef camera


{-| Zoom by changing the distance from the focal point.
-}
zoomBy : Float -> OrbitCamera units coordinates -> OrbitCamera units coordinates
zoomBy zoomCoef camera =
    { focalPoint = camera.focalPoint
    , groundPlane = camera.groundPlane
    , azimuth = camera.azimuth
    , elevation = camera.elevation
    , distance = Quantity.multiplyBy zoomCoef camera.distance
    , projection = camera.projection
    }


zoomInCoef : Float
zoomInCoef =
    2.0 / 3.0


zoomOutCoef : Float
zoomOutCoef =
    1.0 / zoomInCoef


{-| Same as `zoomIn` except the camera is also slightly translated
so that the `fixPoint` stays at the same location in the frame after zooming in.
-}
zoomToward :
    { fixPoint : ( Quantity Float Pixels, Quantity Float Pixels ), canvasHeight : Quantity Float Pixels }
    -> OrbitCamera units coordinates
    -> OrbitCamera units coordinates
zoomToward fixPointConfig camera =
    zoomWithFixPoint zoomInCoef fixPointConfig camera


{-| Same as `zoomOut` except the camera is also slightly translated
so that the `fixPoint` stays at the same location in the frame after zooming out.
-}
zoomAwayFrom :
    { fixPoint : ( Quantity Float Pixels, Quantity Float Pixels ), canvasHeight : Quantity Float Pixels }
    -> OrbitCamera units coordinates
    -> OrbitCamera units coordinates
zoomAwayFrom fixPointConfig camera =
    zoomWithFixPoint zoomOutCoef fixPointConfig camera


{-| Zoom in or out, with the fix point provided that should stay at the same location in the canvas.

Let's try to write down the formulas to be able to zoom while keeping
the point under the mouse cursor fixed.

Let x\_p be the x pixel coordinate of that cursor, with (0,0) being the screen center.
We want to zoom, and keep the thing at x\_p at a fix position.
Let f be the focal length of the camera.

Let x1 be the real world coordinates (in the frame of the camera)
of the thing we observe at a distance d1 of the camera corresponding to x\_p.
Let x2 be the real world coordinates (in the frame of the camera)
of the same observed thing at a distance d2, after moving the camera to zoom in or out.

If x\_p is expected to stay constant after zooming, we have the following relations (Thales):

```haskell
(1)    x1 / d1 = x2 / d2 = x_p / f
```

Now considering that zooming simply consists in scaling the orbit distance by a given coefficent alpha,
we also have the relation:

```haskell
(2)   d2 = alpha * d1
      x2 = alpha * x1
```

After zooming, to keep x\_p at the same position,
we need to apply a translation delta\_x = x2 - x1 to the camera.
According to (2), that translation to apply is:

```haskell
(3)   delta_x = x1 * (alpha - 1)
```

To be able to know the value of x1, we can first compute the value of f.
Let fov be the vertical field of view of the camera.
We can express it in relation to the canvas height and the focal length f as follows:

```haskell
(4)   tan( fov / 2 ) = (canvasHeight / 2) / f
```

And thus, we can express f as follows:

```haskell
(5)   f = canvasHeight / (2 * tan( fov / 2 ))
```

Now according to (1) and (5), we can express x1 as follows:

```haskell
(6)   x1 = d1 * x_p * (2 * tan( fov / 2 )) / canvasHeight
```

And finally, by replacing x1 by the expression in (6), equation (3) gives us
that the tranlation to apply delta\_x to keep x\_p at a fix point is the following:

```haskell
(7)   delta_x = (alpha - 1) * d1 * x_p * (2 * tan( fov / 2 )) / canvasHeight
```

-}
zoomWithFixPoint :
    Float
    -> { fixPoint : ( Quantity Float Pixels, Quantity Float Pixels ), canvasHeight : Quantity Float Pixels }
    -> OrbitCamera units coordinates
    -> OrbitCamera units coordinates
zoomWithFixPoint zoomCoef { fixPoint, canvasHeight } camera =
    let
        (Perspective fov) =
            camera.projection

        scaleCoef =
            Quantity.multiplyBy ((zoomCoef - 1) * 2 * tan (Angle.inRadians fov / 2)) camera.distance

        ( x_p, y_p ) =
            fixPoint

        delta_x =
            Quantity.multiplyBy (Quantity.ratio x_p canvasHeight) scaleCoef

        delta_y =
            Quantity.multiplyBy (Quantity.ratio y_p canvasHeight) scaleCoef
    in
    zoomBy zoomCoef camera
        |> viewPlaneTranslateBy (Vector2d.xy delta_x delta_y)



-- TO SCENE3D ########################################################


toCamera3d : OrbitCamera units coordinates -> Camera3d units coordinates
toCamera3d { focalPoint, groundPlane, azimuth, elevation, distance, projection } =
    let
        (Perspective fov) =
            projection
    in
    Camera3d.perspective
        { verticalFieldOfView = fov
        , viewpoint =
            Viewpoint3d.orbit
                { focalPoint = focalPoint
                , groundPlane = groundPlane
                , azimuth = azimuth
                , elevation = elevation
                , distance = distance
                }
        }
