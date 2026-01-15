// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的各种功能

// 导入主模块中的函数
use azimuth

test "sequence_operations" {
  // 测试序列运算：计算斐波那契数列的前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "shopping_cart_total" {
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(717, final_total)
}

test "string_validation" {
  // 字符串验证测试
  assert_eq_string("Hello, John!", greet("John"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试特殊字符
  let special = "test@example.com"
  assert_eq_string("Hello, test@example.com!", greet(special))
}

test "compound_interest" {
  // 复利计算（简化版）
  let principal = 10000
  let rate = 5
  let years = 3
  
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)
}

test "geometric_calculations" {
  // 几何计算测试
  let radius = 5
  
  // 圆周长（简化）: 2 × π × r ≈ 2 × 3 × r
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 圆面积（简化）: π × r² ≈ 3 × r²
  let area = multiply(3, multiply(radius, radius))
  assert_eq(75, area)
}

test "game_score_system" {
  // 游戏计分系统
  let base_score = 1000
  let level_bonus = 500
  let combo_multiplier = 2
  let penalty = 200
  
  let level_score = add(base_score, level_bonus)
  let combo_score = multiply(level_score, combo_multiplier)
  let final_score = add(combo_score, -penalty)
  
  assert_eq(2800, final_score)
}

test "array_index_math" {
  // 数组索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 二维数组转一维数组索引：index = row × width + col
  let index = add(multiply(row, width), col)
  assert_eq(34, index)
  
  // 测试边界
  let first_index = add(multiply(0, width), 0)
  assert_eq(0, first_index)
}

test "time_conversion" {
  // 时间转换测试
  let hours = 2
  let minutes = 30
  
  // 转换为分钟
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为秒
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
}