// MoonBit 额外测试用例
// 测试 azimuth 库的核心功能

// 导入 azimuth 库
use azimuth.*

// 测试用例 1: 数列求和
test "arithmetic_series_sum" {
  // 计算等差数列 1+2+3+...+10 的和
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2  // n*(n+1)/2
  assert_eq(55, sum)
}

// 测试用例 2: 平方数计算
test "square_calculation" {
  // 计算 15 的平方
  let num = 15
  let square = multiply(num, num)
  assert_eq(225, square)
}

// 测试用例 3: 立方数计算
test "cube_calculation" {
  // 计算 5 的立方
  let num = 5
  let cube = multiply(multiply(num, num), num)
  assert_eq(125, cube)
}

// 测试用例 4: 温度转换（摄氏度转华氏度）
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

// 测试用例 5: 字符串长度测试
test "string_length_scenarios" {
  // 测试不同长度的字符串
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试中文!", greet("测试中文"))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

// 测试用例 6: 面积计算
test "area_calculations" {
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
}

// 测试用例 7: 购物车总价计算
test "shopping_cart_total" {
  // 商品1：单价 199，数量 2
  // 商品2：单价 99，数量 3
  // 商品3：单价 49，数量 1
  let item1_total = multiply(199, 2)
  let item2_total = multiply(99, 3)
  let item3_total = multiply(49, 1)
  let cart_total = add(add(item1_total, item2_total), item3_total)
  assert_eq(643, cart_total)
}

// 测试用例 8: 复利计算
test "compound_interest" {
  // 本金 1000，年利率 5%，3年
  // 简化计算：本金 × (1 + 利率 × 年数)
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(1, multiply(rate, years)))
  assert_eq(16000, total)
}

// 测试用例 9: 平均值计算
test "average_calculation" {
  // 计算 85, 90, 95, 80, 75 的平均值
  let sum = add(add(add(add(85, 90), 95), 80), 75)
  let average = sum / 5
  assert_eq(85, average)
}

// 测试用例 10: 特殊字符串处理
test "special_string_handling" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, spaces  and\ttabs!", greet("spaces  and\ttabs"))
  assert_eq_string("Hello, emoji😊!", greet("emoji😊"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}