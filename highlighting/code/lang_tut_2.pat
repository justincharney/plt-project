package main

type User struct {
    final name string
    final age  i32
}

// Struct method.
func (u User) IsEligible() bool {
    return u.age >= 18;
}

// Function example.
func verifyUser(u User) (string, error) {
    if !u.IsEligible() {
        msg := "User is underage";
        error error_type = error(msg); // Error is special type of casting.
        return "Access denied for " + u.name, error_type;
    }
    return "Access granted to " + u.name, null; // Null is a special data type.
}

// Variadic function example.
func verifyAll(users ...User) {
    for (i := 0; i < len(users); i++) {
        msg, err := verifyUser(users[i]);
        if err != null {
            printf("%s (error: %s)\n", msg, err);
            exit(1); // Exit program with code 1.
        }
        printf("%s\n", msg);
    }
}

// An array of size 3 of User structs.
mainUsers := [
    User{"Alice", 22},
    User{"Bob", 17},
    User{"Carol", 30}
];
verifyAll(mainUsers);

// Will not reach here if exit is called.
printf("All users passed verification.\n");
