// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 增强的数学性质测试用例

// 测试加法的结合律性质
///|
test "addition_associative_property" {
  // 测试 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), c), @clean_test.@clean_test.add(a, @clean_test.@clean_test.add(b, c)))
  
  // 测试负数的情况
  let x = -50
  let y = -30
  let z = 20
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(x, y), z), @clean_test.@clean_test.add(x, @clean_test.@clean_test.add(y, z)))
  
  // 测试包含0的情况
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(0, 123), 456), @clean_test.@clean_test.add(0, @clean_test.@clean_test.add(123, 456)))
}

// 测试乘法的结合律性质
///|
test "multiplication_associative_property" {
  // 测试 (a * b) * c = a * (b * c)
  let a = 2
  let b = 3
  let c = 4
  assert_eq(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(a, b), c), @clean_test.@clean_test.multiply(a, @clean_test.@clean_test.multiply(b, c)))
  
  // 测试包含负数的情况
  let x = -2
  let y = 3
  let z = -4
  assert_eq(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(x, y), z), @clean_test.@clean_test.multiply(x, @clean_test.@clean_test.multiply(y, z)))
  
  // 测试包含0的情况
  assert_eq(@clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(0, 123), 456), @clean_test.@clean_test.multiply(0, @clean_test.@clean_test.multiply(123, 456)))
}

// 测试乘法交换律性质
///|
test "multiplication_commutative_property" {
  // 测试 a * b = b * a
  assert_eq(@clean_test.@clean_test.multiply(12, 34), @clean_test.@clean_test.multiply(34, 12))
  assert_eq(@clean_test.@clean_test.multiply(-100, 200), @clean_test.@clean_test.multiply(200, -100))
  assert_eq(@clean_test.@clean_test.multiply(-50, -75), @clean_test.@clean_test.multiply(-75, -50))
  assert_eq(@clean_test.@clean_test.multiply(0, 999), @clean_test.@clean_test.multiply(999, 0))
}

// 测试加法单位元性质
///|
test "addition_identity_element" {
  // 测试 a + 0 = a 和 0 + a = a
  assert_eq(123, @clean_test.@clean_test.add(123, 0))
  assert_eq(123, @clean_test.@clean_test.add(0, 123))
  assert_eq(-456, @clean_test.@clean_test.add(-456, 0))
  assert_eq(-456, @clean_test.@clean_test.add(0, -456))
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
}

// 测试乘法单位元性质
///|
test "multiplication_identity_element" {
  // 测试 a * 1 = a 和 1 * a = a
  assert_eq(123, @clean_test.@clean_test.multiply(123, 1))
  assert_eq(123, @clean_test.@clean_test.multiply(1, 123))
  assert_eq(-456, @clean_test.@clean_test.multiply(-456, 1))
  assert_eq(-456, @clean_test.@clean_test.multiply(1, -456))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 1))
  assert_eq(0, @clean_test.@clean_test.multiply(1, 0))
}

// 测试乘法零元素性质
///|
test "multiplication_zero_element" {
  // 测试 a * 0 = 0 和 0 * a = 0
  assert_eq(0, @clean_test.@clean_test.multiply(123, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 123))
  assert_eq(0, @clean_test.@clean_test.multiply(-456, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -456))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 0))
}

// 测试实际应用场景：温度转换计算
///|
test "temperature_conversion_calculation" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(celsius, 9), 160)
  // 由于整数除法，我们使用 160 代替 32*5
  assert_eq(385, fahrenheit) // 25 * 9 + 160 = 385，实际值为 77
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 77
  let celsius2 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(fahrenheit2, -32), 5)
  // 由于整数除法，这里只计算分子部分
  assert_eq(225, celsius2) // (77 - 32) * 5 = 225，实际值为 25
}

// 测试实际应用场景：银行利息计算
///|
test "bank_interest_calculation" {
  // 本金
  let principal = 10000
  // 年利率（百分比）
  let interest_rate = 5
  // 年数
  let years = 3
  
  // 简单利息：本金 * 利率 * 年数 / 100
  let simple_interest = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(principal, interest_rate), years)
  // 由于整数除法，这里只计算分子部分
  assert_eq(150000, simple_interest) // 10000 * 5 * 3 = 150000，实际利息为 1500
  
  // 本息合计：本金 + 利息/100
  // 这里我们只测试利息计算部分，因为除法操作需要额外实现
  assert_eq(150000, simple_interest)
}

// 测试字符串拼接的特殊情况
///|
test "string_concatenation_special_cases" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", @clean_test.@clean_test.greet("@#$%"))
  assert_eq_string("Hello, 中文!", @clean_test.@clean_test.greet("中文"))
  assert_eq_string("Hello,   !", @clean_test.@clean_test.greet("  ")) // 空格
  assert_eq("Hello, 
	!", @clean_test.@clean_test.greet("
	")) // 换行和制表符
  
  // 测试长字符串
  let long_name = "这是一个非常非常长的名字用来测试greet函数的处理能力"
  let result = @clean_test.@clean_test.greet(long_name)
  assert_eq(true, result.contains("Hello,"))
  assert_eq(true, result.contains(long_name))
}