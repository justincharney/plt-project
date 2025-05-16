// Single line comment.
package main

/* Multi-line comment.
* All variables below (except in loop and function body) are in global scope. The
* rest are in local scope. Every statement of code should end in a semicolon.
*/

/* Declaring and initializing variables. */
var1 : i16; // Declared vars. must specify type
const var2 : f32 = (-1.50f); // Constant var init. Negatives must be wrapped in ().
var3 := "string type"; // Walrus operator infers type from right hand side.
assert(len(var3) == 11); // len(.) gets length of string.

/* Type alias and casting. Casting only works b/t primitives. See LRM. */
type PAT = i32; // Type alias for signed 32-bit int.
x : PAT = (100 * 10) + 1;
y := (u8)x; // Casting. Signed 32-bit int --> unsigned 8-bit int.
assert(y == 232); // Due to truncation.
assert(x != true && bool(x) == true); // Only bool types can be true or false.

/* Structs. Structs can have any of the modifiers shown below. See LRM. */
type MyStruct struct{
    private a bool = false // Can initialize value.
    final b i32
    late c f32
    mut d string
};
MyStruct z = {a:true, b:123, c:4.56, d:"false"};

/* Arrays. */
var4 : [5]u64; // Unsigned 64-bit int array of size 5.
assert(var4[0] == var1 == 0); // 0-based indexing. Uninit. vars. are equal to 0.
assert(cap(var4) == len(var4) == 5); // cap(.) and len(.) get size of array.

/* Loops and conditionals. */
for i := 0; i < cap(var4); i++ {
    if i != 5 { // Parentheses are optional for conditionals.
        var4[i] = 2 << 1;
        continue;
    }
    else { break; }
}
j : i8;
while j < cap(var4) { printf("%d ", var4[j++]); } // C-styled printf.