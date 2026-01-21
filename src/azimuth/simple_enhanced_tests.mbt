// 简化的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "simple_math_test" {
  // 简单的数学运算测试
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
}

test "boundary_test" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "string_test" {
  // 字符串测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculation" {
  // 复杂计算测试：矩形面积
  let length = 10
  let width = 5
  let area = multiply(length, width)
  assert_eq(50, area)
}

test "financial_calculation" {
  // 金融计算测试：简单利息
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(16000, total)
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}

test "game_logic" {
  // 游戏逻辑测试：得分计算
  let base_score = 100
  let multiplier = 2
  let bonus = 50
  let final_score = add(multiply(base_score, multiplier), bonus)
  assert_eq(250, final_score)
}

test "physics_calculation" {
  // 物理计算测试：速度计算
  let initial_velocity = 10
  let acceleration = 3
  let time = 5
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  assert_eq(25, final_velocity)
}

test "boolean_logic" {
  // 布尔逻辑测试
  assert_true(true)
  assert_false(false)
  assert_eq(1, add(0, 1))
}

test "error_handling" {
  // 错误处理测试：溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 1) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}