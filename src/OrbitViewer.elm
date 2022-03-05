module OrbitViewer exposing (..)

{-| A 3D viewer with helper functions for navigation control.
-}

import Angle exposing (Angle, Radians)
import OrbitCamera exposing (OrbitCamera)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Vector2d


{-| A viewer has:

  - a size, provided by its context
  - a camera, containing the properties of the 3D projection

Its size corresponds to the render size of the canvas,
and has to be kept up to date if the viewer dimensions change
due to responsive layout.

The camera properties contain the full projection parameters,
whether it is a perspective or orthographic projection.
Other types of projections could be added in the future if required.

-}
type alias OrbitViewer units coordinates =
    { size : ( Quantity Float Pixels, Quantity Float Pixels )
    , camera : OrbitCamera units coordinates
    }



-- Constructors


{-| Initialize a 3D viewer of a given render size with the provided camera.
-}
withSizeAndCamera : ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitCamera units coordinates -> OrbitViewer units coordinates
withSizeAndCamera size camera =
    { size = size
    , camera = camera
    }


{-| Update the viewer size while keeping its camera parameters.
-}
resize : ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitViewer units coordinates -> OrbitViewer units coordinates
resize newSize { camera } =
    { size = newSize
    , camera = camera
    }



-- ROTATIONS #########################################################


orbit :
    Quantity Float (Rate Radians Pixels)
    -> ( Quantity Float Pixels, Quantity Float Pixels )
    -> OrbitViewer units coordinates
    -> OrbitViewer units coordinates
orbit angularSpeed ( dx, dy ) { size, camera } =
    { size = size
    , camera =
        OrbitCamera.rotateAround (Quantity.negate dx |> Quantity.at angularSpeed) camera
            |> OrbitCamera.elevate (dy |> Quantity.at angularSpeed)
    }



-- TRANSLATIONS ######################################################


{-| Move within the viewer plane (orthogonal to viewing direction).

Convenient to handle a pan gesture (touch hold and move).
The pan movement must use the same coordinate scale as the canvas size.
So a scaling might be needed if the canvas pixel coordinates are not
at the same scale than the CSS pixel coordinates given by the pointer event.

-}
pan : ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitViewer units coordinates -> OrbitViewer units coordinates
pan ( px, py ) { size, camera } =
    let
        ( _, height ) =
            size

        (OrbitCamera.Perspective fov) =
            camera.projection

        scaleCoef =
            Quantity.multiplyBy (2 * tan (Angle.inRadians fov / 2)) camera.distance

        dx =
            Quantity.multiplyBy (Quantity.ratio (Quantity.negate px) height) scaleCoef

        dy =
            Quantity.multiplyBy (Quantity.ratio py height) scaleCoef
    in
    { size = size
    , camera = OrbitCamera.viewPlaneTranslateBy (Vector2d.xy dx dy) camera
    }



-- SCALING ###########################################################


{-| Zoom in, keeping the center point fix.
-}
zoomIn : OrbitViewer units coordinates -> OrbitViewer units coordinates
zoomIn { size, camera } =
    { size = size
    , camera = OrbitCamera.zoomIn camera
    }


{-| Zoom out, keeping the center point fix.
-}
zoomOut : OrbitViewer units coordinates -> OrbitViewer units coordinates
zoomOut { size, camera } =
    { size = size
    , camera = OrbitCamera.zoomOut camera
    }


{-| Advanced zoom usage.

This function is useful to zoom on a specific point in the viewer.

-}
zoomWithFixPoint : Float -> ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitViewer units coordinates -> OrbitViewer units coordinates
zoomWithFixPoint zoomCoef fixPoint { size, camera } =
    { size = size
    , camera = OrbitCamera.zoomWithFixPoint zoomCoef { fixPoint = fixPoint, canvasHeight = Tuple.second size } camera
    }


{-| Zoom in, keeping a given fix point.

Very convenient to keep the point under the mouse fix for example.
This is equivalent to `zoomIn` if the fix point is the center point.

-}
zoomToward : ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitViewer units coordinates -> OrbitViewer units coordinates
zoomToward fixPoint viewer =
    zoomWithFixPoint OrbitCamera.zoomInCoef fixPoint viewer


{-| Zoom out, keeping a given fix point.

Very convenient to keep the point under the mouse fix for example.
This is equivalent to `zoomOut` if the fix point is the center point.

-}
zoomAwayFrom : ( Quantity Float Pixels, Quantity Float Pixels ) -> OrbitViewer units coordinates -> OrbitViewer units coordinates
zoomAwayFrom fixPoint viewer =
    zoomWithFixPoint OrbitCamera.zoomOutCoef fixPoint viewer
