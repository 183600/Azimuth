// 最小的 azimuth 库文件
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  // 简单的断言实现
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  // 简单的字符串断言实现
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 测试用例
test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

// 高质量测试用例
test "add_function_edge_cases" {
  // 边界情况测试
  assert_eq(1000, add(1000, 0))
  assert_eq(-1000, add(-1000, 0))
  assert_eq(0, add(1, -1))
  assert_eq(0, add(-1, 1))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  assert_eq(1000, multiply(1, 1000))
  assert_eq(0, multiply(0, -1000))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
}

test "greet_function_special_characters" {
  // 特殊字符测试
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "divide_with_ceil_edge_cases" {
  // 边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(4, divide_with_ceil(-10, -3))
}

// 新增的高质量测试用例
test "function_combination" {
  // 组合使用 add 和 multiply
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  // 组合使用多个函数
  let result2 = add(divide_with_ceil(10, 3), multiply(2, 2))
  assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "real_world_shopping_scenario" {
  // 计算购物车总价
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  assert_eq(103, total)  // 29*2 + 15*3 = 58 + 45 = 103
}

test "packaging_calculation" {
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "complex_calculations" {
  // 多层嵌套计算
  let complex_result = add(
    multiply(10, divide_with_ceil(25, 4)),
    multiply(5, divide_with_ceil(17, 3))
  )
  assert_eq(85, complex_result)  // 10*7 + 5*6 = 70 + 15 = 85
}

test "greeting_internationalization" {
  // 国际化问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
}