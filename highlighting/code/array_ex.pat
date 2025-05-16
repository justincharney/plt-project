package main

const scale : i32 = 2

type Point struct {
    x: i32
    y: i32
}

func (p: Point) calcDist() i32{
  return p.x * p.x + p.y * p.y
}

func (p: Point) withScaleEnough (min:i32) (i32, bool) {
  newX := p.x * scale 
  newY := p.y * scale 
  mean := (newX + newY) / 2

  if mean > min {
    return mean ,true
  } else {
    return mean, false
  }
}

func main() {
  p1 := Point{x:10,y:10}
  p2 := Point{x:20,y:20}
  p3 := Point{x:27,y:15}
  my_arr := [3]Point{p1,p2,p3}

  centroidX : i32 = 0
  centroidY : i32 = 0

  for i:=0; i < 3 ; i+=1{
    tmpPt := my_arr[i]
    print_fancy("Center for Point: ",21,0,false,true)
    print_int(tmpPt.calcDist())

    tmp_scale := tmpPt.withScaleEnough(30)
    newMean := tmp_scale[0]
    isEnough := tmp_scale[1]

    if isEnough {
      printf("is enough\nmean: ")
      print_int(newMean)
    } else {
      printf("not enough\nmean: ")
      print_int(newMean)
    }

    centroidX += tmpPt.x
    centroidY += tmpPt.y
  }
  centroidX /= 3
  centroidY /= 3

  print_int(centroidX)
  print_int(centroidY)
}