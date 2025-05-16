package main

func reverse(n: i32) i32 {
    res := 0
    while n > 0 {
        res = res * 10 + n % 10
        n /= 10
    }
    return res
}

func main() {
    num := 12345
    new_num : i32 = reverse(num)
    print_int(new_num)

    float_a : f64 = (f64)1.2
    float_b := (f64)3.4
    print_float(float_a + float_b)
}