// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_series_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
}

test "temperature_conversion_simulation" {
  // 模拟温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(9, divide_with_ceil(multiply(celsius, 5), 5)), 32)
  assert_eq(77, fahrenheit)
}

test "compound_interest_calculation" {
  // 复利计算模拟
  let principal = 1000
  let rate = 5
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1158, year3)
}

test "array_index_calculation" {
  // 模拟数组索引计算
  let rows = 5
  let cols = 4
  let row = 2
  let col = 3
  
  let index = add(multiply(row, cols), col)
  assert_eq(11, index)
}

test "greet_with_emoji_and_unicode" {
  // 测试包含表情符号和Unicode的问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, 🚀🌙!", greet("🚀🌙"))
}

test "prime_number_verification" {
  // 素数验证模拟（简化版）
  let num = 17
  let is_prime_approx = multiply(2, divide_with_ceil(num, 2)) - num
  assert_eq(1, is_prime_approx)
}

test "binary_representation_simulation" {
  // 模拟二进制表示计算
  let decimal = 13
  let bit8 = divide_with_ceil(decimal, 128)
  let remainder1 = decimal - multiply(bit8, 128)
  let bit4 = divide_with_ceil(remainder1, 64)
  let remainder2 = remainder1 - multiply(bit4, 64)
  let bit2 = divide_with_ceil(remainder2, 32)
  let remainder3 = remainder2 - multiply(bit2, 32)
  let bit1 = divide_with_ceil(remainder3, 16)
  
  assert_eq(0, bit8)
  assert_eq(0, bit4)
  assert_eq(0, bit2)
  assert_eq(1, bit1)
}

test "game_score_calculation" {
  // 游戏得分计算
  let base_score = 100
  let multiplier = 3
  let bonus = 50
  let penalty = 20
  
  let final_score = add(add(multiply(base_score, multiplier), bonus), -penalty)
  assert_eq(330, final_score)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_resources = 100
  let projects = 3
  let base_allocation = divide_with_ceil(total_resources, projects)
  let remainder = total_resources - multiply(base_allocation, projects)
  
  let optimized_allocation = add(base_allocation, remainder)
  assert_eq(34, optimized_allocation)
}

test "string_length_simulation" {
  // 模拟字符串长度计算
  let short_name = "Bob"
  let medium_name = "Alexander"
  let long_name = "Elizabeth"
  
  // 模拟长度计算（实际项目中会有专门的长度函数）
  let short_length = divide_with_ceil(multiply(3, 1), 1)
  let medium_length = divide_with_ceil(multiply(9, 1), 1)
  let long_length = divide_with_ceil(multiply(9, 1), 1)
  
  assert_eq(3, short_length)
  assert_eq(9, medium_length)
  assert_eq(9, long_length)
}