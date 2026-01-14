// 新增 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 1
  let last = 13
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(35, sum)
}

test "geometric_progression" {
  // 测试等比数列：2, 6, 18, 54
  // 计算前4项的和
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 3)  // 6
  let term3 = @azimuth.multiply(term2, 3)  // 18
  let term4 = @azimuth.multiply(term3, 3)  // 54
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数运算：F = C × 2 + 30 (近似值)
  let celsius = 15
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(60, fahrenheit)
  
  // 测试水的冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试水的沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}

test "simple_interest_calculation" {
  // 测试简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 5000
  let rate = 8  // 8%
  let years = 3
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(125000, total)  // 5000 + 5000×8×3 = 5000 + 120000 = 125000
}

test "rectangular_geometry" {
  // 测试矩形几何计算
  let length = 12
  let width = 7
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(38, perimeter)
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(84, area)
  
  // 计算对角线（简化）：长 + 宽
  let diagonal = @azimuth.add(length, width)
  @azimuth.assert_eq(19, diagonal)
}

test "string_processing_multilingual" {
  // 测试多语言字符串处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "number_theory_properties" {
  // 测试数论性质
  let a = 6
  let b = 8
  
  // 测试偶数性质：偶数 + 偶数 = 偶数
  let sum_even = @azimuth.add(a, b)
  @azimuth.assert_true(sum_even % 2 == 0)
  
  // 测试偶数性质：偶数 × 偶数 = 偶数
  let product_even = @azimuth.multiply(a, b)
  @azimuth.assert_true(product_even % 2 == 0)
  
  // 测试奇数性质：奇数 + 奇数 = 偶数
  let c = 7
  let d = 9
  let sum_odd = @azimuth.add(c, d)
  @azimuth.assert_true(sum_odd % 2 == 0)
}

test "boundary_value_stress_test" {
  // 测试边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_business_calculation" {
  // 测试实际业务计算：订单总价
  let unit_price = 159
  let quantity = 4
  let discount_threshold = 500
  let discount_rate = 10
  let shipping = 25
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  @azimuth.assert_eq(611, final_total)  // 159×4 = 636, 折扣63, 加运费25 = 598
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n²) 操作
  let n = 10
  
  // 模拟嵌套循环的总操作数：n × n
  let total_operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(100, total_operations)
  
  // 模拟 O(n log n) 操作（简化为 n × log₂n）
  // 对于 n=16, log₂n=4
  let n_power_of_2 = 16
  let log_n = 4
  let n_log_n = @azimuth.multiply(n_power_of_2, log_n)
  @azimuth.assert_eq(64, n_log_n)
}