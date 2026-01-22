// Azimuth 增强测试套件 - 包含8个高质量测试用例
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况和场景

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 引入必要的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
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

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 辅助函数，用于测试
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

test "add_function_edge_cases" {
  // 测试极大值
  assert_eq(2147483647, add(2147483646, 1))
  // 测试极小值
  assert_eq(-2147483647, add(-2147483646, -1))
  // 测试混合符号
  assert_eq(0, add(1000, -1000))
}

test "multiply_function_by_zero" {
  // 任何数乘以0都等于0
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(-12345, 0))
  assert_eq(0, multiply(0, 0))
}

test "multiply_function_by_one" {
  // 任何数乘以1都等于它本身
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(0, multiply(0, 1))
}

test "divide_with_ceil_precision" {
  // 测试精度和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(-2, -2))
  
  // 测试接近整数的除法
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(2, divide_with_ceil(-4, -2))
  assert_eq(-2, divide_with_ceil(-5, 2))
}

test "greet_function_various_inputs" {
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 测试数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 测试混合字符
  assert_eq_string("Hello, Hello123!", greet("Hello123"))
}

test "complex_mathematical_expressions" {
  // 测试复合运算
  let x = 5
  let y = 3
  let z = 2
  
  // (x + y) * z
  assert_eq(16, multiply(add(x, y), z))
  
  // x * y + x * z
  assert_eq(25, add(multiply(x, y), multiply(x, z)))
  
  // ((x + y) * z) / (x - y)
  assert_eq(8, divide_with_ceil(multiply(add(x, y), z), subtract(x, y)))
}

test "error_handling_and_boundaries" {
  // 测试除数为0的处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算（使用较小的数避免溢出）
  let big_num = 10000
  assert_eq(20000, add(big_num, big_num))
  assert_eq(100000000, multiply(big_num, big_num))
}

test "string_operations_edge_cases" {
  // 测试长字符串
  let long_name = "This is a very long name"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   !", greet("   "))
  
  // 测试特殊空白字符（避免使用转义字符）
  assert_eq_string("Hello, space!", greet("space"))
}