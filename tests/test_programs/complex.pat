package main

// Import
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
func (p: Point) distSq() f32 {
    return p.x * p.x + p.y * p.y
}

// --- Functions ---
func add_points(p1: Point, p2: Vector) Point {
    // Creates a new point by adding coordinates
    result : Point
    result.x = p1.x + p2.x
    result.y = p1.y + p2.y
    result.label = "Combined"   // p1.label + "+" + p2.label // String concatenation
    return result
}

func scale_and_label(p: Point, factor: f32, new_label: string) (Point, bool) {
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

    printf("Distance squared for pt1: %.2f\n", dist_sq)

    // Multiple return assignment? ( multi-assign isn't in AST yet)
    // scaled_pt : Point
    // success : bool
    // scaled_pt, success = scale_and_label(pt2, scale_factor, "scaled_pt2")
    temp_scaled := scale_and_label(pt2, scale_factor, "scaled_pt2")
    sum_pt = temp_scaled[0] // Reassign

    // Print pt1
    printf("pt1: x=%.2f, y=%.2f, label=%s\n", pt1.x, pt1.y, pt1.label)

    // Print boolean (success)
    printf("Success: %d\n", temp_scaled[1])

    // Print sum_pt
    printf("sum_pt: x=%.2f, y=%.2f, label=%s\n", sum_pt.x, sum_pt.y, sum_pt.label)

    // Control Flow (If/Else)
    message : string; // STILL NEED A SEMICOLON HERE (FIX)
    if dist_sq > 10.0 {
        message = "Far from origin"
    } else {
        message = "Close to origin"
    }
    printf("Message: %s\n", message);

    // Array and Slice
    points_arr : [3]Point = [3]Point{
        origin,
        pt1,
        Point{x: 5.0, y: 5.0, label:"arr_pt"}}; // SEMICOLON WRONGLY ADDED HERE IF LAST BRACE ON NEW LINE

    points_another_arr: [2]Point;
    points_another_arr[0] = points_arr[1];
    points_another_arr[1] = pt2;

    // For loop with cast and break/continue
    loop_counter : i32 = 0
    float_sum : f32 = 0.0
    for i := 0; i < 10; ++i {
        loop_counter = i // Assign loop var
        if i == 2 {
            continue // Skip iteration 2
        }
        float_sum += (f32)i * 0.5 // Cast i32 to f32 for arithmetic
        if float_sum > 3.0 {
            break // Exit loop early
        }
    }
    printf("After loop: loop_counter = %d, float_sum = %.2f\n", loop_counter, float_sum)

    // Block scope
    {
        const inner_const : bool = true
        if inner_const && loop_counter > 0 {
           // Do something
        }
        // inner_const goes out of scope here
    }

    // Final expression
    loop_counter + 1
}
