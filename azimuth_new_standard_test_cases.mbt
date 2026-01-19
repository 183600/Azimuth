// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法

// 定义测试所需的函数
// 基本函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if (condition) {
    ()
  } else {
    @builtin.abort("Assertion failed")
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if (condition) {
    @builtin.abort("Assertion failed")
  } else {
    ()
  }
}

// 测试辅助函数
pub fn absolute_value(x : Int) -> Int {
  if x < 0 { -x } else { x }
}

pub fn is_even(n : Int) -> Bool {
  n % 2 == 0
}

pub fn is_odd(n : Int) -> Bool {
  n % 2 != 0
}

pub fn max_of_two(a : Int, b : Int) -> Int {
  if a > b { a } else { b }
}

pub fn min_of_two(a : Int, b : Int) -> Int {
  if a < b { a } else { b }
}

pub fn factorial_small(n : Int) -> Int {
  if n <= 1 { 1 } else { n * factorial_small(n - 1) }
}

// 标准测试用例1: 基本算术运算验证
test "basic_arithmetic_verification" {
  // 加法验证
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  
  // 乘法验证
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(5, 0))
  assert_eq(-21, multiply(3, -7))
  
  // 复合运算
  assert_eq(16, add(multiply(2, 5), multiply(3, 2)))
}

// 标准测试用例2: 边界值测试
test "boundary_value_testing" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

// 标准测试用例3: 字符串处理测试
test "string_processing_tests" {
  // 基本字符串操作
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 特殊字符处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
}

// 标准测试用例4: 数学函数测试
test "mathematical_functions_test" {
  // 绝对值测试
  assert_eq(5, absolute_value(5))
  assert_eq(5, absolute_value(-5))
  assert_eq(0, absolute_value(0))
  
  // 偶数判断测试
  assert_true(is_even(0))
  assert_true(is_even(2))
  assert_true(is_even(-4))
  assert_false(is_even(1))
  assert_false(is_even(-3))
  
  // 奇数判断测试
  assert_true(is_odd(1))
  assert_true(is_odd(3))
  assert_true(is_odd(-1))
  assert_false(is_odd(0))
  assert_false(is_odd(2))
  
  // 最大值最小值测试
  assert_eq(10, max_of_two(5, 10))
  assert_eq(10, max_of_two(10, 5))
  assert_eq(-5, max_of_two(-10, -5))
  assert_eq(5, min_of_two(5, 10))
  assert_eq(5, min_of_two(10, 5))
  assert_eq(-10, min_of_two(-10, -5))
}

// 标准测试用例5: 递归函数测试
test "recursive_function_test" {
  // 阶乘测试（小数值）
  assert_eq(1, factorial_small(0))
  assert_eq(1, factorial_small(1))
  assert_eq(2, factorial_small(2))
  assert_eq(6, factorial_small(3))
  assert_eq(24, factorial_small(4))
  assert_eq(120, factorial_small(5))
}

// 标准测试用例6: 实际应用场景测试
test "real_world_application_test" {
  // 银行利息计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 温度转换测试
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)  // 简化公式
  
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

// 标准测试用例7: 条件逻辑测试
test "conditional_logic_test" {
  let x = 10
  let y = 20
  
  // 条件表达式测试
  let max_val = if x > y { x } else { y }
  let min_val = if x < y { x } else { y }
  
  assert_eq(20, max_val)
  assert_eq(10, min_val)
  
  // 嵌套条件测试
  let result = if x > 0 {
    if y > 0 { add(x, y) } else { add(x, -y) }
  } else {
    if y > 0 { add(-x, y) } else { add(-x, -y) }
  }
  
  assert_eq(30, result)
}

// 标准测试用例8: 数据验证测试
test "data_validation_test" {
  // 数值范围验证
  let valid_number = 100
  let invalid_number = -1
  
  assert_true(valid_number >= 0)
  assert_false(invalid_number >= 0)
  
  // 字符串长度验证（通过间接方式）
  let short_string = "Hi"
  let long_string = "ThisIsAVeryLongString"
  
  assert_true(greet(short_string).length() > greet("").length())
  assert_true(greet(long_string).length() > greet(short_string).length())
  
  // 复合条件验证
  let age = 25
  let has_license = true
  
  let can_drive = if age >= 18 && has_license { true } else { false }
  assert_true(can_drive)
  
  let age_too_young = 16
  let can_drive_young = if age_too_young >= 18 && has_license { true } else { false }
  assert_false(can_drive_young)
}

// 标准测试用例9: 性能相关测试
test "performance_related_test" {
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 连续运算测试
  let result = multiply(add(100, 200), add(50, 150))
  assert_eq(90000, result)
  
  // 嵌套运算测试
  let nested_result = add(multiply(10, add(5, 3)), multiply(20, add(2, 3)))
  assert_eq(180, nested_result)
}

// 标准测试用例10: 错误处理测试
test "error_handling_test" {
  // 除零保护（通过条件判断模拟）
  let dividend = 10
  let divisor = 0
  
  let safe_division = if divisor != 0 {
    dividend / divisor
  } else {
    0  // 默认值
  }
  
  assert_eq(0, safe_division)
  
  // 正常除法
  let normal_divisor = 2
  let normal_division = if normal_divisor != 0 {
    dividend / normal_divisor
  } else {
    0
  }
  
  assert_eq(5, normal_division)
  
  // 溢出保护验证
  let max_val = 2147483647
  let overflow_result = add(max_val, 100)
  
  assert_true(overflow_result == max_val)
}