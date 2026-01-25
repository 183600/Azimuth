// 简单的 azimuth 测试用例 - 10个标准 MoonBit 测试用例

test "simple_addition" {
  // 简单加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication" {
  // 简单乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greeting" {
  // 简单问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_division_ceil" {
  // 简单向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  assert_eq(77, fahrenheit)
}

test "array_index_calculation" {
  // 数组索引计算测试
  let rows = 4
  let cols = 5
  let row_index = 2
  let col_index = 3
  let linear_index = add(multiply(row_index, cols), col_index)
  assert_eq(13, linear_index)
}

test "loan_calculation" {
  // 贷款计算测试
  let loan_amount = 10000
  let interest_rate = 5
  let total_interest = divide_with_ceil(multiply(loan_amount, interest_rate), 100)
  let total_payment = add(loan_amount, total_interest)
  assert_eq(500, total_interest)
  assert_eq(10500, total_payment)
}

test "game_score" {
  // 游戏得分测试
  let base_score = 1000
  let multiplier = 3
  let bonus = 500
  let penalty = 200
  let final_score = add(add(multiply(base_score, multiplier), bonus), -penalty)
  assert_eq(3300, final_score)
}

test "binary_conversion" {
  // 二进制转换测试
  let weight_1 = 1
  let weight_2 = 2
  let weight_4 = 4
  let weight_8 = 8
  let total = add(add(add(weight_1, weight_2), weight_4), weight_8)
  assert_eq(15, total)
}

test "prime_check" {
  // 质数检查测试
  let number = 17
  let remainder_2 = number % 2
  let remainder_3 = number % 3
  let remainder_5 = number % 5
  assert_eq(1, remainder_2)
  assert_eq(2, remainder_3)
  assert_eq(2, remainder_5)
}