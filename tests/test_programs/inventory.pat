package main

// Define a simple item in our inventory
type Item struct {
    name: string
    quantity: i32
    price_per_item: i32
}

// Calculate the total value of a list of items
func calculate_total_value(items: [3]Item) i32 {
    total_value : i32 = 0;
    i := 0;
    for ; i < 3; ++i {
        item_value := items[i].quantity * items[i].price_per_item;
        total_value += item_value;
        printf("Item: %s, Quantity: %d, Price: %d, Value: %d\n",
                    items[i].name, items[i].quantity, items[i].price_per_item, item_value);
    }

    return total_value;
}

func main() {
    printf("Welcome to the P.A.T Inventory Calculator!\n");

    // Initialize our Inventory
    inventory := [3]Item{
        Item{name: "Apples", quantity: 10, price_per_item: 2},
        Item{name: "Bananas", quantity: 5, price_per_item: 3},
        Item{name: "Cherries", quantity: 20, price_per_item: 1}
    };

    grand_total := calculate_total_value(inventory);
    printf("---------------------------------------\n");
    printf("Grand Total Inventory Value: %d\n", grand_total);

    if grand_total > 50 {
        printf("That's a well-stocked inventory!\n");
    } else {
        printf("Time to restock!\n");
    }

    exit(0);
}
