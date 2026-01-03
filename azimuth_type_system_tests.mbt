// Azimuth 类型系统测试
// 专注于测试MoonBit的类型系统和类型转换

// 测试1: 基本类型检查
test "基本类型检查测试" {
  // Int类型
  let int_value : Int = 42
  assert_eq(type_of_int(int_value), "Int")
  
  // Bool类型
  let bool_value : Bool = true
  assert_eq(type_of_bool(bool_value), "Bool")
  
  // String类型
  let string_value : String = "hello"
  assert_eq(type_of_string(string_value), "String")
  
  // Array类型
  let array_value : Array[Int] = [1, 2, 3]
  assert_eq(type_of_array(array_value), "Array")
}

fn type_of_int(_ : Int) -> String { "Int" }
fn type_of_bool(_ : Bool) -> String { "Bool" }
fn type_of_string(_ : String) -> String { "String" }
fn type_of_array[T](_ : Array[T]) -> String { "Array" }

// 测试2: 类型转换
test "类型转换测试" {
  // Int到String的转换
  assert_eq(int_to_string(42), "42")
  assert_eq(int_to_string(0), "0")
  assert_eq(int_to_string(-123), "-123")
  
  // String到Int的转换
  assert_eq(string_to_int("42"), Some(42))
  assert_eq(string_to_int("0"), Some(0))
  assert_eq(string_to_int("-123"), Some(-123))
  assert_eq(string_to_int("abc"), None)
  assert_eq(string_to_int(""), None)
  
  // Bool到String的转换
  assert_eq(bool_to_string(true), "true")
  assert_eq(bool_to_string(false), "false")
  
  // String到Bool的转换
  assert_eq(string_to_bool("true"), Some(true))
  assert_eq(string_to_bool("false"), Some(false))
  assert_eq(string_to_bool("True"), Some(true))
  assert_eq(string_to_bool("False"), Some(false))
  assert_eq(string_to_bool("abc"), None)
}

fn int_to_string(n : Int) -> String {
  if (n == 0) {
    return "0"
  }
  
  let is_negative = n < 0
  let abs_n = if (is_negative) { -n } else { n }
  let result = ""
  
  let temp = abs_n
  while (temp > 0) {
    let digit = temp % 10
    result = (digit + '0').to_string() + result
    temp = temp / 10
  }
  
  if (is_negative) {
    "-" + result
  } else {
    result
  }
}

fn string_to_int(s : String) -> Option[Int] {
  if (s.length() == 0) {
    return None
  }
  
  let is_negative = s[0] == '-'
  let start = if (is_negative) { 1 } else { 0 }
  let result = 0
  
  for i = start; i < s.length(); i = i + 1 {
    let char = s[i]
    if (char < '0' or char > '9') {
      return None
    }
    result = result * 10 + (char - '0')
  }
  
  if (is_negative) {
    Some(-result)
  } else {
    Some(result)
  }
}

fn bool_to_string(b : Bool) -> String {
  if (b) { "true" } else { "false" }
}

fn string_to_bool(s : String) -> Option[Bool] {
  match s.to_lower() {
    "true" => Some(true)
    "false" => Some(false)
    _ => None
  }
}

// 测试3: 自定义类型
test "自定义类型测试" {
  let point = Point(3, 4)
  assert_eq(point_x(point), 3)
  assert_eq(point_y(point), 4)
  
  let moved_point = move_point(point, 1, 2)
  assert_eq(point_x(moved_point), 4)
  assert_eq(point_y(moved_point), 6)
  
  let distance = point_distance(point, Point(0, 0))
  assert_eq(distance, 5)  // 3-4-5 triangle
}

type Point(Int, Int)

fn point_x(p : Point) -> Int {
  match p {
    Point(x, _) => x
  }
}

fn point_y(p : Point) -> Int {
  match p {
    Point(_, y) => y
  }
}

fn move_point(p : Point, dx : Int, dy : Int) -> Point {
  match p {
    Point(x, y) => Point(x + dx, y + dy)
  }
}

fn point_distance(p1 : Point, p2 : Point) -> Int {
  match p1 {
    Point(x1, y1) => {
      match p2 {
        Point(x2, y2) => {
          let dx = x1 - x2
          let dy = y1 - y2
          int_sqrt(dx * dx + dy * dy)
        }
      }
    }
  }
}

fn int_sqrt(n : Int) -> Int {
  if (n < 0) {
    0  // 错误情况，返回0
  } else if (n == 0 or n == 1) {
    n
  } else {
    let result = 1
    while (result * result <= n) {
      result = result + 1
    }
    result - 1
  }
}

// 测试4: 枚举类型
test "枚举类型测试" {
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let rgb = Color::RGB(128, 64, 192)
  
  assert_eq(color_to_name(red), "Red")
  assert_eq(color_to_name(green), "Green")
  assert_eq(color_to_name(blue), "Blue")
  assert_eq(color_to_name(rgb), "Custom")
  
  assert_eq(is_primary_color(red), true)
  assert_eq(is_primary_color(green), true)
  assert_eq(is_primary_color(blue), true)
  assert_eq(is_primary_color(rgb), false)
}

type Color {
  Red
  Green
  Blue
  RGB(Int, Int, Int)
}

fn color_to_name(color : Color) -> String {
  match color {
    Color::Red => "Red"
    Color::Green => "Green"
    Color::Blue => "Blue"
    Color::RGB(_, _, _) => "Custom"
  }
}

fn is_primary_color(color : Color) -> Bool {
  match color {
    Color::Red | Color::Green | Color::Blue => true
    _ => false
  }
}

// 测试5: 泛型类型
test "泛型类型测试" {
  let int_box = Box(42)
  assert_eq(box_get(int_box), 42)
  
  let string_box = Box("hello")
  assert_eq(box_get(string_box), "hello")
  
  let bool_box = Box(true)
  assert_eq(box_get(bool_box), true)
  
  let int_pair = Pair(1, 2)
  assert_eq(pair_first(int_pair), 1)
  assert_eq(pair_second(int_pair), 2)
  
  let string_pair = Pair("hello", "world")
  assert_eq(pair_first(string_pair), "hello")
  assert_eq(pair_second(string_pair), "world")
}

type Box[T](T)

fn box_get[T](b : Box[T]) -> T {
  match b {
    Box(value) => value
  }
}

type Pair[T, U](T, U)

fn pair_first[T, U](p : Pair[T, U]) -> T {
  match p {
    Pair(first, _) => first
  }
}

fn pair_second[T, U](p : Pair[T, U]) -> U {
  match p {
    Pair(_, second) => second
  }
}

// 测试6: 递归类型
test "递归类型测试" {
  let empty_list = Nil
  assert_eq(list_length(empty_list), 0)
  
  let single_list = Cons(1, Nil)
  assert_eq(list_length(single_list), 1)
  
  let multi_list = Cons(1, Cons(2, Cons(3, Nil)))
  assert_eq(list_length(multi_list), 3)
  
  let sum = list_sum(multi_list)
  assert_eq(sum, 6)
}

type List[T] {
  Nil
  Cons(T, List[T])
}

fn list_length[T](lst : List[T]) -> Int {
  match lst {
    Nil => 0
    Cons(_, tail) => 1 + list_length(tail)
  }
}

fn list_sum(lst : List[Int]) -> Int {
  match lst {
    Nil => 0
    Cons(head, tail) => head + list_sum(tail)
  }
}

// 测试7: 类型约束
test "类型约束测试" {
  let int_result = add_generic(5, 3)
  assert_eq(int_result, 8)
  
  let string_result = concatenate_generic("hello", "world")
  assert_eq(string_result, "helloworld")
  
  let bool_result = and_generic(true, false)
  assert_eq(bool_result, false)
}

fn add_generic(a : Int, b : Int) -> Int {
  a + b
}

fn concatenate_generic(a : String, b : String) -> String {
  a + b
}

fn and_generic(a : Bool, b : Bool) -> Bool {
  a and b
}

// 测试8: 类型检查和验证
test "类型检查和验证测试" {
  assert_eq(is_positive(5), true)
  assert_eq(is_positive(0), false)
  assert_eq(is_positive(-5), false)
  
  assert_eq(is_even(4), true)
  assert_eq(is_even(5), false)
  assert_eq(is_even(0), true)
  assert_eq(is_even(-4), true)
  
  assert_eq(is_empty_string(""), true)
  assert_eq(is_empty_string("hello"), false)
  
  assert_eq(is_empty_array([]), true)
  assert_eq(is_empty_array([1, 2, 3]), false)
  
  assert_eq(is_some(Some(42)), true)
  assert_eq(is_some(None), false)
  
  assert_eq(is_ok(Ok(42)), true)
  assert_eq(is_ok(Err("error")), false)
}

fn is_positive(n : Int) -> Bool {
  n > 0
}

fn is_even(n : Int) -> Bool {
  n % 2 == 0
}

fn is_empty_string(s : String) -> Bool {
  s.length() == 0
}

fn is_empty_array[T](arr : Array[T]) -> Bool {
  arr.length() == 0
}

fn is_some[T](opt : Option[T]) -> Bool {
  match opt {
    Some(_) => true
    None => false
  }
}

type Result[T, E] {
  Ok(T)
  Err(E)
}

fn is_ok[T, E](res : Result[T, E]) -> Bool {
  match res {
    Ok(_) => true
    Err(_) => false
  }
}