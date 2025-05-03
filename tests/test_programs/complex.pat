// plt-project/tests/test_programs/complex_sast_test.pat
package main

// Import (even if semantically ignored for now, good test)
import "math" // Hypothetical import

// --- Type Definitions ---
type Point struct {
    x: f32
    y: f32
    label: string
}

type Vector = Point // Type alias

// --- Global Variables ---
const scale_factor : f32 = 2.5
const global_offset : Vector = Vector{x: 0.1, y: -0.1, label: "offset"}

// --- Struct Method ---
// Method to calculate squared distance from origin
func (p Point) distSq() f32 {
    return p.x * p.x + p.y * p.y
}

// --- Functions ---
func add_points(p1: Point, p2: Vector) Point {
    // Creates a new point by adding coordinates
    result : Point
    result.x = p1.x + p2.x
    result.y = p1.y + p2.y
    result.label = p1.label + "+" + p2.label // String concatenation
    return result
}

func scale_and_label(p Point, factor f32, new_label string) (Point, bool) {
    // Scales a point and updates its label, returns modified point and success
    scaled_p : Point = p // Copy struct
    scaled_p.x = p.x * factor
    scaled_p.y = p.y * factor
    scaled_p.label = new_label
    return scaled_p, true // Return multiple values
}

// --- Main Function ---
func main() {
    // Local variables & struct literals
    origin : Point = Point{x: 0.0, y: 0.0, label: "origin"}
    pt1 : Point = Point{x: 1.0, y: 2.0, label: "pt1"}
    pt2 := Point{x: 3.0, y: -1.0, label: "pt2"} // Inferred type

    // Function call & assignment
    sum_pt := add_points(pt1, global_offset) // Use global var

    // Method call
    dist_sq := pt1.distSq()

    // Multiple return assignment (need syntax for this, assuming tuple-like or multiple vars)
    // Let's simulate by calling and using one result for now, as multi-assign isn't in AST yet
    // scaled_pt : Point
    // success : bool
    // scaled_pt, success = scale_and_label(pt2, scale_factor, "scaled_pt2")
    temp_scaled, _ := scale_and_label(pt2, scale_factor, "scaled_pt2")
    sum_pt = temp_scaled // Reassign

    // Control Flow (If/Else)
    message : string
    if dist_sq > 10.0 {
        message = "Far from origin"
    } else {
        message = "Close to origin"
    }

    // Array and Slice
    points_arr : [3]Point = [
        origin,
        pt1,
        Point{x: 5.0, y: 5.0, label:"arr_pt"}
    ]
    points_slice : []Point = make([]Point, 2, 5) // Make slice

    // Index access and assignment
    points_slice[0] = points_arr[1] // Assign pt1
    points_slice[1] = pt2

    // Slice expression
    sub_slice : []Point = points_slice[0:1] // Create sub-slice

    // Null assignment (to slice)
    sub_slice = null

    // For loop with cast and break/continue
    loop_counter : i32 = 0
    float_sum : f32 = 0.0
    for i := 0; i < 10; i++ {
        loop_counter = i // Assign loop var
        if i == 2 {
            continue // Skip iteration 2
        }
        float_sum += (f32)i * 0.5 // Cast i32 to f32 for arithmetic
        if float_sum > 3.0 {
            break // Exit loop early
        }
    }

    // Block scope
    {
        const inner_const : bool = true
        if inner_const && loop_counter > 0 {
           // Do something
        }
        // inner_const goes out of scope here
    }

    // Example of checking if slice is null (hypothetical)
    // if sub_slice == null {
    //    message = "Slice is null now"
    // }

    // Final expression (result often ignored in main, but checked)
    loop_counter + 1
}
