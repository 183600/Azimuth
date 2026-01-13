// 标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // 计算 (a + b) * c
  let a = 4
  let b = 5
  let c = 6
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  
  @azimuth.assert_eq(54, result)  // (4 + 5) * 6 = 54
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 a + b + c + d
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  let step1 = @azimuth.add(a, b)
  let step2 = @azimuth.add(step1, c)
  let result = @azimuth.add(step2, d)
  
  @azimuth.assert_eq(100, result)  // 10 + 20 + 30 + 40 = 100
}

test "mathematical_formulas" {
  // 测试数学公式计算
  // 计算三角形的面积：底 × 高 ÷ 2
  let base = 12
  let height = 8
  
  let area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(48, area)  // 12 * 8 / 2 = 48
}

test "string_processing" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, Test User!", @azimuth.greet("Test User"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算购物车总价：商品1 × 数量1 + 商品2 × 数量2
  let price1 = 199
  let quantity1 = 2
  let price2 = 299
  let quantity2 = 1
  
  let total1 = @azimuth.multiply(price1, quantity1)
  let total2 = @azimuth.multiply(price2, quantity2)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
}