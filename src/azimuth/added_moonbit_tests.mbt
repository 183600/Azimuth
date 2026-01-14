// 新增的 MoonBit 测试用例
// 包含10个测试场景，覆盖各种实际应用

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法
  let sum = azimuth::add(a, b)
  assert_eq(22, sum)
  
  // 乘法
  let product = azimuth::multiply(a, b)
  assert_eq(105, product)
}

test "negative_number_operations" {
  // 负数运算测试
  let negative_a = -10
  let positive_b = 5
  
  // 负数加正数
  let result1 = azimuth::add(negative_a, positive_b)
  assert_eq(-5, result1)
  
  // 负数乘正数
  let result2 = azimuth::multiply(negative_a, positive_b)
  assert_eq(-50, result2)
  
  // 负数乘负数
  let result3 = azimuth::multiply(negative_a, -2)
  assert_eq(20, result3)
}

test "zero_operations" {
  // 零值运算测试
  let number = 42
  
  // 加零
  let result1 = azimuth::add(number, 0)
  assert_eq(42, result1)
  
  // 乘零
  let result2 = azimuth::multiply(number, 0)
  assert_eq(0, result2)
  
  // 零加零
  let result3 = azimuth::add(0, 0)
  assert_eq(0, result3)
}

test "string_greeting_functions" {
  // 字符串问候函数测试
  let name1 = "Alice"
  let name2 = "Bob"
  let empty_name = ""
  
  // 正常问候
  let greeting1 = azimuth::greet(name1)
  assert_eq_string("Hello, Alice!", greeting1)
  
  // 另一个问候
  let greeting2 = azimuth::greet(name2)
  assert_eq_string("Hello, Bob!", greeting2)
  
  // 空名称问候
  let greeting3 = azimuth::greet(empty_name)
  assert_eq_string("Hello, !", greeting3)
}

test "complex_calculation_scenario" {
  // 复杂计算场景
  let length = 10
  let width = 5
  
  // 计算矩形周长：2 * (length + width)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(30, perimeter)
  
  // 计算矩形面积：length * width
  let area = azimuth::multiply(length, width)
  assert_eq(50, area)
  
  // 计算总花费：周长 + 面积
  let total = azimuth::add(perimeter, area)
  assert_eq(80, total)
}

test "financial_calculation" {
  // 金融计算场景
  let principal = 1000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 计算简单利息：principal * rate * time / 100
  let interest = azimuth::multiply(azimuth::multiply(principal, rate), time) / 100
  assert_eq(150, interest)
  
  // 计算总金额：principal + interest
  let total = azimuth::add(principal, interest)
  assert_eq(1150, total)
}

test "temperature_conversion" {
  // 温度转换场景
  let celsius = 25
  
  // 摄氏度转华氏度（简化公式）：F = C * 2 + 30
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化公式）：C = (F - 30) / 2
  let back_to_celsius = azimuth::add(fahrenheit, -30) / 2
  assert_eq(25, back_to_celsius)
}

test "sequence_calculation" {
  // 序列计算
  // 计算等差数列前5项和：1 + 3 + 5 + 7 + 9
  let term1 = 1
  let term2 = 3
  let term3 = 5
  let term4 = 7
  let term5 = 9
  
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(term1, term2), term3), term4), term5)
  assert_eq(25, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(5, average)
}

test "boundary_value_test" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加1（应该保持最大值）
  let result1 = azimuth::add(max_val, 1)
  assert_eq(max_val, result1)
  
  // 最小值减1（应该保持最小值）
  let result2 = azimuth::add(min_val, -1)
  assert_eq(min_val, result2)
  
  // 最大值乘2（应该保持最大值）
  let result3 = azimuth::multiply(max_val, 2)
  assert_eq(max_val, result3)
}

test "assert_function_test" {
  // 断言函数测试
  // 测试 assert_true
  assert_true(true)
  assert_true(5 > 3)
  assert_true(azimuth::add(2, 3) == 5)
  
  // 测试 assert_false
  assert_false(false)
  assert_false(5 < 3)
  assert_false(azimuth::add(2, 3) == 6)
  
  // 测试 assert_eq
  assert_eq(10, 10)
  assert_eq(azimuth::add(4, 6), 10)
  assert_eq(azimuth::multiply(3, 4), 12)
  
  // 测试 assert_eq_string
  assert_eq_string("hello", "hello")
  assert_eq_string(azimuth::greet("World"), "Hello, World!")
}