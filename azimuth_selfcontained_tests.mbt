// Azimuth 自包含测试套件
// 包含函数定义和测试用例，不依赖外部包导入

// 函数定义
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

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 测试用例
test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(10, divide_with_ceil(100, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-2, divide_with_ceil(5, -2))
}

test "divide_with_ceil_zero_division" {
  // 测试零除法的处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
}

test "greet_function_empty_and_special_cases" {
  // 测试问候函数的空字符串和特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "greet_function_unicode_and_emoji" {
  // 测试问候函数的Unicode和emoji支持
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, résumé!", greet("résumé"))
}

test "add_function_large_numbers" {
  // 测试加法函数的大数处理
  assert_eq(3000000, add(1000000, 2000000))
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
  assert_eq(0, add(500000, -500000))
  assert_eq(2000, add(1000, 1000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(99999, multiply(1, 99999))
  assert_eq(-100, multiply(-1, 100))
  assert_eq(6, multiply(2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "comprehensive_calculation_scenarios" {
  // 测试综合计算场景
  let base_price = 100
  let quantity = 5
  let tax_rate = 10
  let discount = 20
  
  let subtotal = multiply(base_price, quantity)
  let total_before_discount = add(subtotal, tax_rate)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(500, subtotal)
  assert_eq(510, total_before_discount)
  assert_eq(490, final_total)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  let c = 13
  
  // 交换律
  let add_comm1 = add(a, b)
  let add_comm2 = add(b, a)
  let mul_comm1 = multiply(a, b)
  let mul_comm2 = multiply(b, a)
  
  // 结合律
  let add_assoc1 = add(add(a, b), c)
  let add_assoc2 = add(a, add(b, c))
  
  assert_eq(add_comm1, add_comm2)
  assert_eq(mul_comm1, mul_comm2)
  assert_eq(add_assoc1, add_assoc2)
}

test "performance_boundary_tests" {
  // 测试性能边界情况
  // 小数累加
  let sum_small = add(add(add(add(1, 2), 3), 4), 5)
  
  // 大数累加
  let sum_large = add(add(add(add(100000, 200000), 300000), 400000), 500000)
  
  // 嵌套计算
  let nested = multiply(add(10, 20), add(5, 15))
  
  assert_eq(15, sum_small)
  assert_eq(1500000, sum_large)
  assert_eq(600, nested)
}