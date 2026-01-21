// 标准 MoonBit 测试文件 - 测试 azimuth 库的核心功能
// 使用 MoonBit 标准测试语法和项目中的验证方式

// 辅助函数：验证相等性
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    @builtin.abort("Assertion failed: " + lhs.to_string() + " != " + rhs.to_string())
  }
}

// 辅助函数：验证字符串相等性
fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    @builtin.abort("String assertion failed: " + lhs + " != " + rhs)
  }
}

// 测试的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
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

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_edge_cases" {
  // 测试加法边界情况
  assert_eq(1000, add(1000, 0))
  assert_eq(0, add(1, -1))
  assert_eq(0, add(-1, 1))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  assert_eq(1000, multiply(1, 1000))
  assert_eq(0, multiply(0, -1000))
  assert_eq(1, multiply(-1, -1))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_function_edge_cases" {
  // 测试问候函数边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}

test "divide_with_ceil_positive" {
  // 测试正数向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "divide_with_ceil_negative" {
  // 测试负数向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "practical_packaging_scenario" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}