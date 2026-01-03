// Azimuth 模式匹配测试
// 专注于测试MoonBit的模式匹配功能

// 测试1: 基本模式匹配
test "基本模式匹配测试" {
  assert_eq(match_number(0), "zero")
  assert_eq(match_number(1), "one")
  assert_eq(match_number(2), "small")
  assert_eq(match_number(5), "small")
  assert_eq(match_number(10), "medium")
  assert_eq(match_number(50), "large")
  assert_eq(match_number(100), "very large")
}

fn match_number(n : Int) -> String {
  match n {
    0 => "zero"
    1 => "one"
    2 | 3 | 4 | 5 => "small"
    6 | 7 | 8 | 9 | 10 => "medium"
    11 ... 50 => "large"
    _ => "very large"
  }
}

// 测试2: Option类型模式匹配
test "Option类型模式匹配测试" {
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(option_to_string(some_value), "Some(42)")
  assert_eq(option_to_string(none_value), "None")
  
  assert_eq(option_double(some_value), Some(84))
  assert_eq(option_double(none_value), None)
  
  assert_eq(option_default(some_value, 0), 42)
  assert_eq(option_default(none_value, 0), 0)
}

fn option_to_string(opt : Option[Int]) -> String {
  match opt {
    Some(value) => "Some(" + value.to_string() + ")"
    None => "None"
  }
}

fn option_double(opt : Option[Int]) -> Option[Int] {
  match opt {
    Some(value) => Some(value * 2)
    None => None
  }
}

fn option_default(opt : Option[Int], default : Int) -> Int {
  match opt {
    Some(value) => value
    None => default
  }
}

// 测试3: Result类型模式匹配
test "Result类型模式匹配测试" {
  let ok_value = Ok(42)
  let err_value = Err("Error message")
  
  assert_eq(result_to_string(ok_value), "Ok(42)")
  assert_eq(result_to_string(err_value), "Err(Error message)")
  
  assert_eq(result_double(ok_value), Ok(84))
  assert_eq(result_double(err_value), Err("Error message"))
  
  assert_eq(result_or_default(ok_value, 0), 42)
  assert_eq(result_or_default(err_value, 0), 0)
}

type Result[T, E] {
  Ok(T)
  Err(E)
}

fn result_to_string(res : Result[Int, String]) -> String {
  match res {
    Ok(value) => "Ok(" + value.to_string() + ")"
    Err(error) => "Err(" + error + ")"
  }
}

fn result_double(res : Result[Int, String]) -> Result[Int, String] {
  match res {
    Ok(value) => Ok(value * 2)
    Err(error) => Err(error)
  }
}

fn result_or_default(res : Result[Int, String], default : Int) -> Int {
  match res {
    Ok(value) => value
    Err(_) => default
  }
}

// 测试4: 自定义枚举类型模式匹配
test "自定义枚举类型模式匹配测试" {
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let rgb = Color::RGB(128, 64, 192)
  let hex = Color::Hex("#FF00FF")
  
  assert_eq(color_to_string(red), "Red")
  assert_eq(color_to_string(green), "Green")
  assert_eq(color_to_string(blue), "Blue")
  assert_eq(color_to_string(rgb), "RGB(128, 64, 192)")
  assert_eq(color_to_string(hex), "Hex(#FF00FF)")
  
  assert_eq(is_primary_color(red), true)
  assert_eq(is_primary_color(green), true)
  assert_eq(is_primary_color(blue), true)
  assert_eq(is_primary_color(rgb), false)
  assert_eq(is_primary_color(hex), false)
}

type Color {
  Red
  Green
  Blue
  RGB(Int, Int, Int)
  Hex(String)
}

fn color_to_string(color : Color) -> String {
  match color {
    Color::Red => "Red"
    Color::Green => "Green"
    Color::Blue => "Blue"
    Color::RGB(r, g, b) => "RGB(" + r.to_string() + ", " + g.to_string() + ", " + b.to_string() + ")"
    Color::Hex(value) => "Hex(" + value + ")"
  }
}

fn is_primary_color(color : Color) -> Bool {
  match color {
    Color::Red | Color::Green | Color::Blue => true
    _ => false
  }
}

// 测试5: 链表模式匹配
test "链表模式匹配测试" {
  let empty_list = Nil
  let single_element = Cons(1, Nil)
  let two_elements = Cons(1, Cons(2, Nil))
  let many_elements = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  
  assert_eq(list_to_string(empty_list), "[]")
  assert_eq(list_to_string(single_element), "[1]")
  assert_eq(list_to_string(two_elements), "[1, 2]")
  assert_eq(list_to_string(many_elements), "[1, 2, 3, 4]")
  
  assert_eq(list_length(empty_list), 0)
  assert_eq(list_length(single_element), 1)
  assert_eq(list_length(two_elements), 2)
  assert_eq(list_length(many_elements), 4)
  
  assert_eq(list_sum(empty_list), 0)
  assert_eq(list_sum(single_element), 1)
  assert_eq(list_sum(two_elements), 3)
  assert_eq(list_sum(many_elements), 10)
}

type List[T] {
  Nil
  Cons(T, List[T])
}

fn list_to_string(lst : List[Int]) -> String {
  match lst {
    Nil => "[]"
    Cons(head, Nil) => "[" + head.to_string() + "]"
    Cons(head, tail) => "[" + head.to_string() + ", " + list_tail_to_string(tail) + "]"
  }
}

fn list_tail_to_string(lst : List[Int]) -> String {
  match lst {
    Nil => ""
    Cons(head, Nil) => head.to_string()
    Cons(head, tail) => head.to_string() + ", " + list_tail_to_string(tail)
  }
}

fn list_length(lst : List[Int]) -> Int {
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

// 测试6: 树结构模式匹配
test "树结构模式匹配测试" {
  let leaf = Tree::Leaf(5)
  let node = Tree::Node(
    Tree::Leaf(3),
    10,
    Tree::Leaf(15)
  )
  let complex_tree = Tree::Node(
    Tree::Node(
      Tree::Leaf(1),
      2,
      Tree::Leaf(3)
    ),
    4,
    Tree::Node(
      Tree::Leaf(5),
      6,
      Tree::Leaf(7)
    )
  )
  
  assert_eq(tree_sum(leaf), 5)
  assert_eq(tree_sum(node), 28)  // 3 + 10 + 15
  assert_eq(tree_sum(complex_tree), 28)  // 1+2+3+4+5+6+7 = 28
  
  assert_eq(tree_height(leaf), 1)
  assert_eq(tree_height(node), 2)
  assert_eq(tree_height(complex_tree), 3)
  
  assert_eq(is_balanced(leaf), true)
  assert_eq(is_balanced(node), true)
  assert_eq(is_balanced(complex_tree), true)
}

type Tree {
  Leaf(Int)
  Node(Tree, Int, Tree)
}

fn tree_sum(tree : Tree) -> Int {
  match tree {
    Tree::Leaf(value) => value
    Tree::Node(left, value, right) => tree_sum(left) + value + tree_sum(right)
  }
}

fn tree_height(tree : Tree) -> Int {
  match tree {
    Tree::Leaf(_) => 1
    Tree::Node(left, _, right) => 1 + max(tree_height(left), tree_height(right))
  }
}

fn max(a : Int, b : Int) -> Int {
  if (a > b) { a } else { b }
}

fn is_balanced(tree : Tree) -> Bool {
  match tree {
    Tree::Leaf(_) => true
    Tree::Node(left, _, right) => {
      let left_height = tree_height(left)
      let right_height = tree_height(right)
      let height_diff = abs(left_height - right_height)
      height_diff <= 1 and is_balanced(left) and is_balanced(right)
    }
  }
}

fn abs(x : Int) -> Int {
  if (x < 0) { -x } else { x }
}

// 测试7: 复杂模式匹配
test "复杂模式匹配测试" {
  let point1 = Point2D(0, 0)
  let point2 = Point2D(5, 0)
  let point3 = Point2D(0, 5)
  let point4 = Point2D(3, 4)
  let point5 = Point2D(-3, -4)
  
  assert_eq(classify_point(point1), "origin")
  assert_eq(classify_point(point2), "on x-axis")
  assert_eq(classify_point(point3), "on y-axis")
  assert_eq(classify_point(point4), "first quadrant")
  assert_eq(classify_point(point5), "third quadrant")
  
  assert_eq(distance_from_origin(point1), 0)
  assert_eq(distance_from_origin(point2), 5)
  assert_eq(distance_from_origin(point3), 5)
  assert_eq(distance_from_origin(point4), 5)  // 3-4-5 triangle
}

type Point2D(Int, Int)

fn classify_point(point : Point2D) -> String {
  match point {
    Point2D(0, 0) => "origin"
    Point2D(x, 0) if x > 0 => "positive x-axis"
    Point2D(x, 0) if x < 0 => "negative x-axis"
    Point2D(0, y) if y > 0 => "positive y-axis"
    Point2D(0, y) if y < 0 => "negative y-axis"
    Point2D(x, y) if x > 0 and y > 0 => "first quadrant"
    Point2D(x, y) if x < 0 and y > 0 => "second quadrant"
    Point2D(x, y) if x < 0 and y < 0 => "third quadrant"
    Point2D(x, y) if x > 0 and y < 0 => "fourth quadrant"
    Point2D(_, 0) => "on x-axis"
    Point2D(0, _) => "on y-axis"
    _ => "somewhere else"
  }
}

fn distance_from_origin(point : Point2D) -> Int {
  match point {
    Point2D(x, y) => int_sqrt(x * x + y * y)
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