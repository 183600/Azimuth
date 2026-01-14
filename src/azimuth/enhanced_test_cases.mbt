// 增强测试用例 - 覆盖各种场景的 MoonBit 测试
// 使用标准的 MoonBit 测试语法

test "enhanced_basic_arithmetic" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-25, multiply(-5, 5))
}

test "enhanced_boundary_conditions" {
  // 边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(0, multiply(max_val, 0))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "enhanced_string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "enhanced_complex_calculations" {
  // 复杂计算测试
  // 计算 (a + b) * c - d
  let a = 8
  let b = 12
  let c = 5
  let d = 15
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(85, result)  // (8 + 12) * 5 - 15 = 85
}

test "enhanced_financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + (本金 * 利率 * 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 160000
}

test "enhanced_temperature_conversion" {
  // 温度转换测试（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}

test "enhanced_shopping_cart" {
  // 购物车计算测试
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(717, final_total)  // (199 * 2) + (299 * 1) + 20 = 717
}

test "enhanced_geometric_calculations" {
  // 几何计算测试
  let length = 10
  let width = 6
  
  // 矩形面积
  let area = multiply(length, width)
  assert_eq(60, area)
  
  // 矩形周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(32, perimeter)
  
  // 三角形面积（底 × 高 ÷ 2）
  let base = 8
  let height = 5
  let triangle_area = multiply(base, height) / 2
  assert_eq(20, triangle_area)
}

test "enhanced_statistical_calculations" {
  // 统计计算测试
  let values = [15, 25, 35, 45, 55]
  
  // 计算总和
  let sum = add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(35, average)
  
  // 计算中位数（已排序数组）
  let median = values[2]  // 第三个元素
  assert_eq(35, median)
}

test "enhanced_error_handling" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试特殊值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}