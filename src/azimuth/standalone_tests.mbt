// 独立的 MoonBit 测试用例
// 这些测试用例包含所需的函数定义，可以独立运行

// 定义基本的数学函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn divide_with_ceil(dividend : Int, divisor : Int) -> Int {
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  if remainder > 0 { add(quotient, 1) } else { quotient }
}

// 定义断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Test failed")
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    @builtin.abort("Test failed")
  }
}

// 测试用例
test "basic_addition_patterns" {
  // 测试基本的加法模式
  let result1 = add(10, 20)
  assert_eq(30, result1)
  
  let result2 = add(-10, 20)
  assert_eq(10, result2)
  
  let result3 = add(0, 100)
  assert_eq(100, result3)
}

test "basic_multiplication_patterns" {
  // 测试基本的乘法模式
  let result1 = multiply(5, 6)
  assert_eq(30, result1)
  
  let result2 = multiply(-5, 6)
  assert_eq(-30, result2)
  
  let result3 = multiply(5, 0)
  assert_eq(0, result3)
}

test "string_greeting_patterns" {
  // 测试字符串问候模式
  let result1 = greet("Alice")
  assert_eq_string("Hello, Alice!", result1)
  
  let result2 = greet("")
  assert_eq_string("Hello, !", result2)
  
  let result3 = greet("123")
  assert_eq_string("Hello, 123!", result3)
}

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 2 + 3 + 4 + 5
  let sum1 = add(1, 2)
  let sum2 = add(sum1, 3)
  let sum3 = add(sum2, 4)
  let sum4 = add(sum3, 5)
  assert_eq(15, sum4)
}

test "compound_calculation" {
  // 复合计算：(a + b) * c
  let a = 10
  let b = 20
  let c = 3
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  
  assert_eq(90, product)
}

test "conditional_calculation" {
  // 条件计算测试
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { add(x, y) } else { multiply(x, y) }
  
  assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { add(x, y) } else { multiply(x, y) }
  
  assert_eq(300, result2)
}

test "sequential_operations" {
  // 连续运算测试
  // 计算：(a + b) * c
  let a = 5
  let b = 10
  let c = 3
  
  let sum1 = add(a, b)
  let result = multiply(sum1, c)
  
  assert_eq(45, result)
}

test "string_processing_with_numbers" {
  // 数字字符串处理测试
  let number_str = "2023"
  let result = greet(number_str)
  assert_eq_string("Hello, 2023!", result)
  
  let mixed_str = "User123"
  let result2 = greet(mixed_str)
  assert_eq_string("Hello, User123!", result2)
}

test "calculation_with_division" {
  // 包含除法的计算测试
  let dividend = 100
  let divisor = 4
  
  // 计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  assert_eq(25, quotient)
  assert_eq(0, remainder)
  
  // 使用向上取整除法
  let ceil_result = divide_with_ceil(dividend, divisor)
  assert_eq(25, ceil_result)
  
  // 测试有余数的情况
  let dividend2 = 101
  let ceil_result2 = divide_with_ceil(dividend2, divisor)
  assert_eq(26, ceil_result2)
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版）
  // F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
}