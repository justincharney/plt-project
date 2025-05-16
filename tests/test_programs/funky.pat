package main

// Goody struct type.
type goody struct {
    name: string
    price: i32
};

// The funky menu is global and does not change.
const menu = [4]goody{
    goody{name:"You bought a funky burger\n", price:14},
    goody{name:"You bought funky nuggets\n", price:9},
    goody{name:"You bought a funky milkshake\n", price:8},
    goody{name:"You bought funky fries\n", price:7}
}

// Money in wallet.
wallet := 10;

// Cart for goodies.
cart := [2]string{"", ""}; // Initialize with empty strings
cart_index := 0;

// going to buy all i can from funky menu
func buy(money : i32 ) (string, i32) {
    if money < 7 {
        return "failure...", 1;
    }

    for i := 0; i < len(menu); ++i {
        if !(money - menu[i].price < 0) && (cart_index < cap(cart)) {
            money -= menu[i].price;
            cart[cart_index] = menu[i].name;
            ++cart_index;
            printf(menu[i].name);
        }
    }
    return "so funky, you really go it like that", 0;
}

// Main function - entry point of the program
func main() {
    printf("Funky program starting...\n");

    // Call buy
    buy_result_tuple := buy(wallet);

    // Destructure tuple
    val_message : string = buy_result_tuple[0];
    err_value : i32 = buy_result_tuple[1];

    if (err_value != 0){
        printf("An error occurred during buy!\n");
        exit(1);
    } else {
        printf("Buy attempt result: %s\n", val_message);
    }

    printf("Items in cart after buy:\n");
    index_print := 0;
    for ; index_print < cart_index; ++index_print {
        printf("- %s\n", cart[index_print]);
    }

    printf("Global wallet  (unchanged by this 'buy' call, as 'buy' uses a local copy): %d\n", wallet);
    printf("Global cart index: %d\n", cart_index);

    exit(0);
}
