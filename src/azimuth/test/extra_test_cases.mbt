// Azimuth 项目额外的 MoonBit 测试用例
// 测试各种实际应用场景和边界条件

test "array_index_calculation" {
  // 数组索引计算：二维数组转一维数组的索引
  let row = 3
  let col = 4
  let width = 5
  
  // index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)
}

test "compound_interest_precise" {
  // 精确的复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利公式：1000 × (100 + 5×3)
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(115000, total)
}

test "string_formatting_edge_cases" {
  // 字符串格式化边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, a@b.c!", @azimuth.greet("a@b.c"))
}

test "mathematical_series_sum" {
  // 等差数列求和：1 + 3 + 5 + ... + 19
  let n = 10
  let first = 1
  let last = 19
  
  // sum = n × (first + last) / 2
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(100, sum)
}

test "power_of_two_calculation" {
  // 2的幂计算：2^8 = 256
  let result = @azimuth.multiply(
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)),
    @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))
  )
  @azimuth.assert_eq(256, result)
}

test "currency_conversion" {
  // 货币转换计算
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 添加5%手续费
  let fee = @azimuth.multiply(cny_amount, 5) / 100
  let total_with_fee = @azimuth.add(cny_amount, fee)
  @azimuth.assert_eq(735, total_with_fee)
}

test "geometry_circle_calculations" {
  // 圆的几何计算（简化版）
  let radius = 5
  
  // 周长：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 面积：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
}

test "time_conversion" {
  // 时间转换计算
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9000, total_seconds)
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "order_processing_calculation" {
  // 订单处理计算
  let unit_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(674, final_total)
}