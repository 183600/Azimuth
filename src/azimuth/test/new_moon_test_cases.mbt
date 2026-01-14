// 新的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "advanced_arithmetic_operations" {
  // 测试高级算术运算
  let a = 15
  let b = 25
  
  // 测试连续加法
  let sum1 = @azimuth.add(@azimuth.add(a, b), 10)
  @azimuth.assert_eq(50, sum1)
  
  // 测试连续乘法
  let product1 = @azimuth.multiply(@azimuth.multiply(a, 2), 3)
  @azimuth.assert_eq(90, product1)
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  let name_with_spaces = "  John Doe  "
  let result = @azimuth.greet(name_with_spaces)
  @azimuth.assert_eq_string("Hello,   John Doe  !", result)
  
  let unicode_name = "张三李四"
  let unicode_result = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 张三李四!", unicode_result)
}

test "temperature_conversion_complete" {
  // 测试完整的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32（简化为 F = C × 2 + 30）
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试第一个温度点
  let c1 = celsius_temps[0]
  let f1 = @azimuth.add(@azimuth.multiply(c1, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[0], f1)
  
  // 测试中间温度点
  let c3 = celsius_temps[2]
  let f3 = @azimuth.add(@azimuth.multiply(c3, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[2], f3)
  
  // 测试最后一个温度点
  let c5 = celsius_temps[4]
  let f5 = @azimuth.add(@azimuth.multiply(c5, 2), 30)
  @azimuth.assert_eq(expected_fahrenheit[4], f5)
}

test "financial_calculations_comprehensive" {
  // 测试综合金融计算
  let principal = 10000
  let rate = 8  // 8%
  let years = 5
  
  // 简单利息计算
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let simple_total = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(500000, simple_total)
  
  // 复利计算（简化版）
  let compound_factor = @azimuth.add(100, @azimuth.multiply(rate, years))
  let compound_total = @azimuth.multiply(principal, compound_factor)
  @azimuth.assert_eq(500000, compound_total)
}

test "geometry_calculations_extended" {
  // 测试扩展几何计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算矩形周长
  let rectangle_perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  @azimuth.assert_eq(40, rectangle_perimeter)
  
  // 计算长方体体积
  let volume = @azimuth.multiply(rectangle_area, height)
  @azimuth.assert_eq(480, volume)
}

test "array_index_calculations" {
  // 测试数组索引计算
  let rows = 4
  let cols = 5
  let row = 2
  let col = 3
  
  // 二维数组转一维数组的索引计算
  let index = @azimuth.add(@azimuth.multiply(row, cols), col)
  @azimuth.assert_eq(13, index)
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  @azimuth.assert_eq(0, first_index)
  
  let last_index = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), @azimuth.add(cols, -1)), 0)
  @azimuth.assert_eq(12, last_index)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 7
  let expected_sum = @azimuth.multiply(n, n)
  @azimuth.assert_eq(49, expected_sum)
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n = 2^(n+1) - 2
  // 简化计算：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  let base_price = 1000
  let quantity = 5
  let discount_threshold = 3000
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 50
  
  // 计算总价
  let subtotal = @azimuth.multiply(base_price, quantity)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(@azimuth.add(discounted_price, tax), shipping)
  
  @azimuth.assert_eq(5400, final_price)
}

test "performance_optimization_scenarios" {
  // 测试性能优化场景
  let base_value = 100
  
  // 测试批量计算优化
  let batch_size = 100
  let multiplier = 5
  
  // 使用公式计算：base_value × batch_size × multiplier
  let optimized_result = @azimuth.multiply(@azimuth.multiply(base_value, batch_size), multiplier)
  @azimuth.assert_eq(50000, optimized_result)
  
  // 测试缓存计算结果
  let cached_value = @azimuth.add(base_value, multiplier)
  let reused_result1 = @azimuth.multiply(cached_value, 10)
  let reused_result2 = @azimuth.multiply(cached_value, 20)
  
  @azimuth.assert_eq(1050, reused_result1)
  @azimuth.assert_eq(2100, reused_result2)
}