package main

func add(a: i32, b: i32) i32 {
    return a + b
}

func main() {
    c := add(5, 3)
    print_int(c);

    f:f64 = (f64)3.4
    print_float(f);

    printf("Hello\n");
    print_fancy("Hello World",230,27,true,true);
    print_fancy("Hello World",201,32,true,false);
    print_fancy("Hello World",124,157,false,true);
    print_fancy("Hello World",253,129,false,false);
}
