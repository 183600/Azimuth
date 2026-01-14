// Azimuth 项目额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，专注于边界值和实际应用场景

test "simple_boundary_test" {
  // 测试简单的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试基本乘法
  @azimuth.assert_eq(10, @azimuth.multiply(2, 5))
  
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "financial_calculation" {
  // 测试金融计算场景
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total)
}

test "error_handling" {
  // 测试错误处理
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_series" {
  // 测试数学级数计算
  // 计算等差数列前n项和：S = n(a1 + an) / 2
  let n = 100
  let first_term = 1
  let last_term = 100
  
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(5050, arithmetic_sum)
}

test "string_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含引号的字符串
  let quoted_name = "\"Test\""
  @azimuth.assert_eq_string("Hello, \"Test\"!", @azimuth.greet(quoted_name))
  
  // 测试长字符串
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet(long_name))
}

test "complex_calculation" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
}

test "batch_processing" {
  // 测试批量处理场景
  let batch_size = 100
  let items_per_batch = 50
  let processing_time_per_item = 2
  
  // 计算总处理时间
  let total_items = @azimuth.multiply(batch_size, items_per_batch)
  let total_processing_time = @azimuth.multiply(total_items, processing_time_per_item)
  @azimuth.assert_eq(10000, total_processing_time)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
}

test "shopping_cart" {
  // 测试购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(674, total)
}

test "comprehensive_scenario" {
  // 综合应用场景测试
  let product_price = 199
  let quantity = 3
  let discount = 10
  let shipping = 15
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount) / 100
  let discounted_subtotal = @azimuth.add(subtotal, -discount_amount)
  // 计算税费
  let tax = @azimuth.multiply(discounted_subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(discounted_subtotal, shipping), tax)
  
  @azimuth.assert_eq(643, total)
}