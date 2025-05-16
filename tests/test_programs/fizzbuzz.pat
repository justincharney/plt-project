package main


func main() {
  i : i32;
  for i=0; i < 20 ; i+=1{
    if(i%3==0 && i%5==0){
      print_fancy("Fizzbuzz",4,9,true,true);
      printf("\n");
    }else if(i%5==0){
      print_fancy("Fizz",40,0,true,false);
      printf("\n");
    }else if(i%3==0){
      print_fancy("Buzz",200,0,false,true);
      printf("\n");
    }else{
      print_int(i);
    }
  }

}
