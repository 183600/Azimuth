// Advanced Pattern Matching Test Suite for Azimuth
// 高级模式匹配测试套件 - 测试各种高级模式匹配技术

// Test 1: 基本模式匹配
test "basic pattern matching" {
  // 数值模式匹配
  let describe_number = fn(n) {
    match n {
      0 => "zero"
      1 => "one"
      2 => "two"
      _ => "many"
    }
  }
  
  assert_eq(describe_number(0), "zero")
  assert_eq(describe_number(1), "one")
  assert_eq(describe_number(2), "two")
  assert_eq(describe_number(5), "many")
  
  // 布尔模式匹配
  let bool_to_string = fn(b) {
    match b {
      true => "true"
      false => "false"
    }
  }
  
  assert_eq(bool_to_string(true), "true")
  assert_eq(bool_to_string(false), "false")
  
  // 变量绑定模式
  let is_positive = fn(n) {
    match n {
      x if x > 0 => true
      _ => false
    }
  }
  
  assert_true(is_positive(5))
  assert_false(is_positive(-5))
  assert_false(is_positive(0))
}

// Test 2: 枚举模式匹配
test "enum pattern matching" {
  // 定义枚举
  type Color {
    Red
    Green
    Blue
    Custom(String)
  }
  
  type Shape {
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float, Float)
  }
  
  // 颜色匹配
  let color_to_rgb = fn(color) {
    match color {
      Red => (255, 0, 0)
      Green => (0, 255, 0)
      Blue => (0, 0, 255)
      Custom(name) => (128, 128, 128)  // 默认灰色
    }
  }
  
  assert_eq(color_to_rgb(Red), (255, 0, 0))
  assert_eq(color_to_rgb(Green), (0, 255, 0))
  assert_eq(color_to_rgb(Blue), (0, 0, 255))
  assert_eq(color_to_rgb(Custom("purple")), (128, 128, 128))
  
  // 形状匹配
  let area = fn(shape) {
    match shape {
      Circle(radius) => 3.14159 * radius * radius
      Rectangle(width, height) => width * height
      Triangle(a, b, c) => {
        // 使用海伦公式
        let s = (a + b + c) / 2.0
        sqrt(s * (s - a) * (s - b) * (s - c))
      }
    }
  }
  
  assert_eq(area(Circle(2.0)), 12.56636)  // π * 2^2
  assert_eq(area(Rectangle(3.0, 4.0)), 12.0)
  assert_eq(area(Triangle(3.0, 4.0, 5.0)), 6.0)  // 直角三角形
}

// Test 3: 嵌套模式匹配
test "nested pattern matching" {
  // 嵌套数据结构
  type Expr {
    Int(Int)
    Var(String)
    Add(Expr, Expr)
    Mul(Expr, Expr)
  }
  
  type Result[T] {
    Ok(T)
    Error(String)
  }
  
  // 复杂嵌套匹配
  let evaluate = fn(expr, env) {
    match expr {
      Int(n) => Ok(n)
      Var(name) => {
        match env.get(name) {
          Some(value) => Ok(value)
          None => Error("Variable not found: " + name)
        }
      }
      Add(left, right) => {
        match (evaluate(left, env), evaluate(right, env)) {
          (Ok(l), Ok(r)) => Ok(l + r)
          (Ok(_), Error(e)) => Error(e)
          (Error(e), Ok(_)) => Error(e)
          (Error(e1), Error(e2)) => Error(e1 + "; " + e2)
        }
      }
      Mul(left, right) => {
        match (evaluate(left, env), evaluate(right, env)) {
          (Ok(l), Ok(r)) => Ok(l * r)
          (Ok(_), Error(e)) => Error(e)
          (Error(e), Ok(_)) => Error(e)
          (Error(e1), Error(e2)) => Error(e1 + "; " + e2)
        }
      }
    }
  }
  
  let env = {"x": 10, "y": 20}
  
  // 测试嵌套匹配
  assert_eq(evaluate(Int(5), env), Ok(5))
  assert_eq(evaluate(Var("x"), env), Ok(10))
  assert_eq(evaluate(Var("z"), env), Error("Variable not found: z"))
  
  assert_eq(evaluate(Add(Var("x"), Var("y")), env), Ok(30))
  assert_eq(evaluate(Mul(Var("x"), Int(3)), env), Ok(30))
  
  // 复杂嵌套表达式
  let complex_expr = Add(
    Var("x"),
    Mul(Var("y"), Int(2))
  )
  assert_eq(evaluate(complex_expr, env), Ok(50))  // 10 + 20 * 2
  
  // 错误传播
  let error_expr = Add(
    Var("x"),
    Mul(Var("z"), Int(2))
  )
  assert_eq(evaluate(error_expr, env), Error("Variable not found: z"))
}

// Test 4: 守卫模式匹配
test "guard pattern matching" {
  // 带守卫的模式匹配
  let categorize_age = fn(age) {
    match age {
      n if n < 0 => "invalid"
      n if n < 13 => "child"
      n if n < 20 => "teenager"
      n if n < 65 => "adult"
      _ => "senior"
    }
  }
  
  assert_eq(categorize_age(-5), "invalid")
  assert_eq(categorize_age(5), "child")
  assert_eq(categorize_age(15), "teenager")
  assert_eq(categorize_age(30), "adult")
  assert_eq(categorize_age(70), "senior")
  
  // 复杂守卫
  type Point {
    Point(Float, Float)
  }
  
  let quadrant = fn(point) {
    match point {
      Point(x, y) if x > 0 && y > 0 => "first"
      Point(x, y) if x < 0 && y > 0 => "second"
      Point(x, y) if x < 0 && y < 0 => "third"
      Point(x, y) if x > 0 && y < 0 => "fourth"
      Point(x, y) if x == 0 && y == 0 => "origin"
      Point(x, 0) => "x-axis"
      Point(0, y) => "y-axis"
    }
  }
  
  assert_eq(quadrant(Point(1.0, 1.0)), "first")
  assert_eq(quadrant(Point(-1.0, 1.0)), "second")
  assert_eq(quadrant(Point(-1.0, -1.0)), "third")
  assert_eq(quadrant(Point(1.0, -1.0)), "fourth")
  assert_eq(quadrant(Point(0.0, 0.0)), "origin")
  assert_eq(quadrant(Point(2.0, 0.0)), "x-axis")
  assert_eq(quadrant(Point(0.0, 2.0)), "y-axis")
  
  // 多重守卫
  let classify_triangle = fn(a, b, c) {
    match (a, b, c) {
      (x, y, z) if x <= 0 || y <= 0 || z <= 0 => "invalid"
      (x, y, z) if x + y <= z || x + z <= y || y + z <= x => "invalid"
      (x, y, z) if x == y && y == z => "equilateral"
      (x, y, z) if x == y || x == z || y == z => "isosceles"
      (x, y, z) if x*x + y*y == z*z || x*x + z*z == y*y || y*y + z*z == x*x => "right"
      _ => "scalene"
    }
  }
  
  assert_eq(classify_triangle(3, 3, 3), "equilateral")
  assert_eq(classify_triangle(3, 3, 4), "isosceles")
  assert_eq(classify_triangle(3, 4, 5), "right")
  assert_eq(classify_triangle(4, 5, 6), "scalene")
  assert_eq(classify_triangle(1, 1, 3), "invalid")
}

// Test 5: 列表模式匹配
test "list pattern matching" {
  // 列表模式匹配
  let describe_list = fn(list) {
    match list {
      [] => "empty"
      [x] => "single element: " + x.to_string()
      [x, y] => "two elements: " + x.to_string() + ", " + y.to_string()
      [head, ...tail] => "head: " + head.to_string() + ", tail length: " + tail.length().to_string()
    }
  }
  
  assert_eq(describe_list([]), "empty")
  assert_eq(describe_list([1]), "single element: 1")
  assert_eq(describe_list([1, 2]), "two elements: 1, 2")
  assert_eq(describe_list([1, 2, 3, 4]), "head: 1, tail length: 3")
  
  // 列表处理函数
  let sum = fn(list) {
    match list {
      [] => 0
      [head, ...tail] => head + sum(tail)
    }
  }
  
  let product = fn(list) {
    match list {
      [] => 1
      [head, ...tail] => head * product(tail)
    }
  }
  
  assert_eq(sum([1, 2, 3, 4, 5]), 15)
  assert_eq(product([1, 2, 3, 4]), 24)
  
  // 复杂列表模式
  let process_pairs = fn(list) {
    match list {
      [] => []
      [x] => [x * 2]  // 奇数长度，最后一个元素翻倍
      [x, y, ...rest] => [x + y] + process_pairs(rest)
    }
  }
  
  assert_eq(process_pairs([1, 2, 3, 4, 5]), [3, 7, 10])
  assert_eq(process_pairs([1, 2, 3]), [3, 6])
  assert_eq(process_pairs([1]), [2])
  
  // 列表查找
  let find = fn(list, predicate) {
    match list {
      [] => None
      [head, ...tail] => {
        if predicate(head) {
          Some(head)
        } else {
          find(tail, predicate)
        }
      }
    }
  }
  
  assert_eq(find([1, 2, 3, 4, 5], fn(x) { x > 3 }), Some(4))
  assert_eq(find([1, 2, 3, 4, 5], fn(x) { x > 10 }), None)
}

// Test 6: 元组模式匹配
test "tuple pattern matching" {
  // 元组模式匹配
  let describe_tuple = fn(tuple) {
    match tuple {
      () => "empty tuple"
      (x,) => "single element: " + x.to_string()
      (x, y) => "pair: " + x.to_string() + ", " + y.to_string()
      (x, y, z) => "triple: " + x.to_string() + ", " + y.to_string() + ", " + z.to_string()
      tuple => "tuple with " + tuple.length().to_string() + " elements"
    }
  }
  
  assert_eq(describe_tuple(()), "empty tuple")
  assert_eq(describe_tuple((1,)), "single element: 1")
  assert_eq(describe_tuple((1, 2)), "pair: 1, 2")
  assert_eq(describe_tuple((1, 2, 3)), "triple: 1, 2, 3")
  assert_eq(describe_tuple((1, 2, 3, 4)), "tuple with 4 elements")
  
  // 元组解构
  let add_pairs = fn(list_of_pairs) {
    match list_of_pairs {
      [] => []
      [(x, y), ...rest] => [x + y] + add_pairs(rest)
    }
  }
  
  assert_eq(add_pairs([(1, 2), (3, 4), (5, 6)]), [3, 7, 11])
  
  // 复杂元组匹配
  let categorize_point = fn(point) {
    match point {
      (0, 0) => "origin"
      (x, 0) => "on x-axis at " + x.to_string()
      (0, y) => "on y-axis at " + y.to_string()
      (x, y) if x == y => "on diagonal y=x at " + x.to_string()
      (x, y) if x == -y => "on diagonal y=-x at " + x.to_string()
      (x, y) => "at (" + x.to_string() + ", " + y.to_string() + ")"
    }
  }
  
  assert_eq(categorize_point((0, 0)), "origin")
  assert_eq(categorize_point((3, 0)), "on x-axis at 3")
  assert_eq(categorize_point((0, 4)), "on y-axis at 4")
  assert_eq(categorize_point((2, 2)), "on diagonal y=x at 2")
  assert_eq(categorize_point((3, -3)), "on diagonal y=-x at 3")
  assert_eq(categorize_point((2, 3)), "at (2, 3)")
}

// Test 7: 记录模式匹配
test "record pattern matching" {
  // 记录定义
  type Person {
    name: String
    age: Int
    address: Address
  }
  
  type Address {
    street: String
    city: String
    country: String
  }
  
  // 记录模式匹配
  let describe_person = fn(person) {
    match person {
      { name, age: 0, ... } => name + " is newborn"
      { name, age, ... } if age < 18 => name + " is minor (" + age.to_string() + " years)"
      { name, age: 18, ... } => name + " just became adult"
      { name, age, ... } if age < 65 => name + " is adult (" + age.to_string() + " years)"
      { name, age, ... } => name + " is senior (" + age.to_string() + " years)"
    }
  }
  
  let person1 = { name: "Alice", age: 5, address: { street: "123 Main St", city: "New York", country: "USA" } }
  let person2 = { name: "Bob", age: 18, address: { street: "456 Oak Ave", city: "Los Angeles", country: "USA" } }
  let person3 = { name: "Charlie", age: 30, address: { street: "789 Pine Rd", city: "Chicago", country: "USA" } }
  let person4 = { name: "Diana", age: 70, address: { street: "321 Elm St", city: "Boston", country: "USA" } }
  
  assert_eq(describe_person(person1), "Alice is minor (5 years)")
  assert_eq(describe_person(person2), "Bob just became adult")
  assert_eq(describe_person(person3), "Charlie is adult (30 years)")
  assert_eq(describe_person(person4), "Diana is senior (70 years)")
  
  // 嵌套记录匹配
  let get_country = fn(person) {
    match person {
      { address: { country, ... }, ... } => country
    }
  }
  
  assert_eq(get_country(person1), "USA")
  
  // 记录更新模式
  let celebrate_birthday = fn(person) {
    match person {
      { name, age, address } => {
        { name, age: age + 1, address }
      }
    }
  }
  
  let person1_older = celebrate_birthday(person1)
  assert_eq(person1_older.age, 6)
  assert_eq(person1_older.name, "Alice")
  
  // 部分记录匹配
  let people_from_country = fn(people, country) {
    let mut result = []
    for person in people {
      match person {
        { address: { country: c, ... }, ... } if c == country => {
          result.push(person)
        }
        _ => {}
      }
    }
    result
  }
  
  let people = [person1, person2, person3, person4]
  let usa_people = people_from_country(people, "USA")
  assert_eq(usa_people.length(), 4)
}

// Test 8: Option和Result模式匹配
test "option_and_result_pattern_matching" {
  // Option模式匹配
  let safe_divide = fn(a, b) {
    if b == 0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  let describe_division = fn(a, b) {
    match safe_divide(a, b) {
      Some(result) => a.to_string() + " / " + b.to_string() + " = " + result.to_string()
      None => a.to_string() + " / " + b.to_string() + " is undefined"
    }
  }
  
  assert_eq(describe_division(10, 2), "10 / 2 = 5")
  assert_eq(describe_division(10, 0), "10 / 0 is undefined")
  
  // Option链式匹配
  let nested_option = Some(Some(Some(42)))
  
  let extract_nested = fn(opt) {
    match opt {
      Some(Some(Some(value))) => "triple nested: " + value.to_string()
      Some(Some(value)) => "double nested: " + value.to_string()
      Some(value) => "single nested: " + value.to_string()
      None => "none"
    }
  }
  
  assert_eq(extract_nested(nested_option), "triple nested: 42")
  assert_eq(extract_nested(Some(Some(10))), "double nested: 10")
  assert_eq(extract_nested(Some(5)), "single nested: 5")
  assert_eq(extract_nested(None), "none")
  
  // Result模式匹配
  type Result[T, E] {
    Ok(T)
    Error(E)
  }
  
  let parse_int = fn(str) {
    match str.to_int() {
      Some(n) => Ok(n)
      None => Error("invalid integer: " + str)
    }
  }
  
  let safe_parse_and_double = fn(str) {
    match parse_int(str) {
      Ok(n) => Ok(n * 2)
      Error(e) => Error(e)
    }
  }
  
  assert_eq(safe_parse_and_double("21"), Ok(42))
  assert_eq(safe_parse_and_double("abc"), Error("invalid integer: abc"))
  
  // Result组合
  let add_results = fn(result1, result2) {
    match (result1, result2) {
      (Ok(a), Ok(b)) => Ok(a + b)
      (Ok(_), Error(e)) => Error(e)
      (Error(e), Ok(_)) => Error(e)
      (Error(e1), Error(e2)) => Error(e1 + " and " + e2)
    }
  }
  
  assert_eq(add_results(Ok(5), Ok(3)), Ok(8))
  assert_eq(add_results(Ok(5), Error("bad")), Error("bad"))
  assert_eq(add_results(Error("bad1"), Error("bad2")), Error("bad1 and bad2"))
}

// Test 9: 高级模式匹配技术
test "advanced_pattern_matching_techniques" {
  // or模式
  let is_small_or_large = fn(n) {
    match n {
      x if x <= 5 || x >= 95 => true
      _ => false
    }
  }
  
  assert_true(is_small_or_large(3))
  assert_false(is_small_or_large(50))
  assert_true(is_small_or_large(97))
  
  // as模式
  let describe_with_as = fn(value) {
    match value {
      x as n if n > 0 => "positive " + x.to_string()
      x as n if n < 0 => "negative " + x.to_string()
      0 => "zero"
    }
  }
  
  assert_eq(describe_with_as(5), "positive 5")
  assert_eq(describe_with_as(-3), "negative -3")
  assert_eq(describe_with_as(0), "zero")
  
  // 模式中的函数调用
  let starts_with = fn(str, prefix) {
    match str {
      s if s.length() >= prefix.length() && s.substring(0, prefix.length()) == prefix => true
      _ => false
    }
  }
  
  assert_true(starts_with("hello world", "hello"))
  assert_false(starts_with("hello world", "world"))
  
  // 复杂数据结构模式
  type BinaryTree[T] {
    Empty
    Node(T, BinaryTree[T], BinaryTree[T])
  }
  
  let tree_contains = fn(tree, value) {
    match tree {
      Empty => false
      Node(v, left, right) => {
        v == value || tree_contains(left, value) || tree_contains(right, value)
      }
    }
  }
  
  let tree = Node(5, 
    Node(3, Empty, Node(4, Empty, Empty)),
    Node(7, Node(6, Empty, Empty), Empty)
  )
  
  assert_true(tree_contains(tree, 4))
  assert_true(tree_contains(tree, 6))
  assert_false(tree_contains(tree, 10))
  
  // 树的路径查找
  let find_path = fn(tree, value) {
    match tree {
      Empty => None
      Node(v, _, _) if v == value => Some([v])
      Node(v, left, right) => {
        match find_path(left, value) {
          Some(path) => Some([v] + path)
          None => {
            match find_path(right, value) {
              Some(path) => Some([v] + path)
              None => None
            }
          }
        }
      }
    }
  }
  
  assert_eq(find_path(tree, 4), Some([5, 3, 4]))
  assert_eq(find_path(tree, 6), Some([5, 7, 6]))
  assert_eq(find_path(tree, 10), None)
}

// Test 10: 模式匹配性能和优化
test "pattern_matching_performance_and_optimization" {
  // 编译时模式优化
  let optimized_match = fn(value) {
    match value {
      0 => "zero"
      1 => "one"
      2 => "two"
      3 => "three"
      4 => "four"
      5 => "five"
      _ => "other"
    }
  }
  
  // 测试性能关键的模式匹配
  let fast_string_match = fn(str) {
    match str {
      "a" => 1
      "b" => 2
      "c" => 3
      "d" => 4
      "e" => 5
      _ => 0
    }
  }
  
  assert_eq(fast_string_match("a"), 1)
  assert_eq(fast_string_match("c"), 3)
  assert_eq(fast_string_match("z"), 0)
  
  // 模式匹配 vs if-else链
  let pattern_based = fn(n) {
    match n {
      x if x % 2 == 0 => "even"
      _ => "odd"
    }
  }
  
  let if_else_based = fn(n) {
    if n % 2 == 0 {
      "even"
    } else {
      "odd"
    }
  }
  
  // 验证结果一致性
  for i in 0..10 {
    assert_eq(pattern_based(i), if_else_based(i))
  }
  
  // 复杂模式的优化
  type Expr {
    Int(Int)
    Add(Expr, Expr)
    Mul(Expr, Expr)
  }
  
  let eval_optimized = fn(expr) {
    match expr {
      Int(n) => n
      Add(Int(a), Int(b)) => a + b  // 特殊情况优化
      Add(left, right) => eval_optimized(left) + eval_optimized(right)
      Mul(Int(a), Int(b)) => a * b  // 特殊情况优化
      Mul(left, right) => eval_optimized(left) * eval_optimized(right)
    }
  }
  
  let expr1 = Add(Int(2), Int(3))  // 优化路径
  let expr2 = Add(Add(Int(1), Int(2)), Int(3))  // 部分优化
  let expr3 = Mul(Add(Int(1), Int(2)), Int(3))  // 混合路径
  
  assert_eq(eval_optimized(expr1), 5)
  assert_eq(eval_optimized(expr2), 6)
  assert_eq(eval_optimized(expr3), 9)
  
  // 模式匹配的穷尽性检查
  let exhaustive_match = fn(color) {
    match color {
      "red" => "#FF0000"
      "green" => "#00FF00"
      "blue" => "#0000FF"
      "yellow" => "#FFFF00"
      "purple" => "#800080"
      "orange" => "#FFA500"
      // 编译器会警告未覆盖所有情况，但这里我们假设这些是所有有效颜色
    }
  }
  
  assert_eq(exhaustive_match("red"), "#FF0000")
  assert_eq(exhaustive_match("blue"), "#0000FF")
}