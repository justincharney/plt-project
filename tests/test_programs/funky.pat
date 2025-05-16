package main

// Goody struct type.
type goody struct {
    name: string
    price: i32
};

// The funky menu is global and does not change.
const menu = [4]goody{
    goody{name:"funky burger", price:14},
    goody{name:"funky nuggets", price:9},
    goody{name:"funky milkshake", price:8},
    goody{name:"funky fries", price:7}
}

// Money in wallet.
wallet := 10;

// Main function - entry point of the program
func main() {
    printf("Funky program starting...\n");
    exit(0);
}
