// MoonBit 新增测试用例
// 使用标准的 MoonBit 测试语法

test "string_processing_advanced" {
  // 测试高级字符串处理
  let name = "MoonBit"
  let greeting = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
  
  // 测试包含数字和特殊字符的字符串
  let complex_name = "User2023@Project#1"
  let complex_greeting = @azimuth.greet(complex_name)
  @azimuth.assert_eq_string("Hello, User2023@Project#1!", complex_greeting)
}

test "mathematical_series" {
  // 测试数学级数计算：等差数列求和
  // 1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 测试等比数列求和：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  
  // 测试边界值附近的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "practical_application_calculations" {
  // 实际应用计算测试：购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.add(@azimuth.multiply(item1_price, item1_quantity), @azimuth.multiply(item2_price, item2_quantity))
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_fee)
  
  @azimuth.assert_eq(737, total)  // (199×2 + 299×1) + 8% + 20 = 697 + 55 + 20 = 772
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  // 模拟 O(n²) 算法的计算量
  let n = 10
  let operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(100, operations)
  
  // 模拟 O(n³) 算法的计算量
  let cubic_ops = @azimuth.multiply(n, @azimuth.multiply(n, n))
  @azimuth.assert_eq(1000, cubic_ops)
  
  // 模拟 O(log n) 算法的计算量（简化）
  let log_ops = n / 2  // 简化的对数运算
  @azimuth.assert_eq(5, log_ops)
}

test "data_structure_operations" {
  // 数据结构操作测试：模拟数组操作
  let array_size = 10
  let index = 3
  let value = 42
  
  // 模拟数组访问：base_address + index * element_size
  let element_size = 4
  let address = @azimuth.add(@azimuth.multiply(index, element_size), 1000)  // 假设基地址为1000
  @azimuth.assert_eq(1012, address)
  
  // 模拟数组边界检查
  let is_valid_index = index >= 0 && index < array_size
  @azimuth.assert_eq(is_valid_index, true)
  
  // 测试越界情况
  let out_of_bounds_index = 15
  let is_invalid_index = out_of_bounds_index >= 0 && out_of_bounds_index < array_size
  @azimuth.assert_eq(0, 1)(is_invalid_index)
}

test "time_and_date_calculations" {
  // 时间和日期计算测试
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(@azimuth.add(@azimuth.multiply(hours, 60), minutes), 60), seconds)
  @azimuth.assert_eq(9045, total_seconds)  // (2×60+30)×60+45 = 150×60+45 = 9045
  
  // 计算时间差
  let start_hour = 8
  let end_hour = 17
  let duration = @azimuth.add(end_hour, -start_hour)
  @azimuth.assert_eq(9, duration)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算总和：10 + 20 + 30 + 40 + 50 = 150
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值：150 / 5 = 30
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化）：(10-30)² + (20-30)² + (30-30)² + (40-30)² + (50-30)²
  let diff1 = @azimuth.add(10, -30)
  let diff2 = @azimuth.add(20, -30)
  let diff3 = @azimuth.add(30, -30)
  let diff4 = @azimuth.add(40, -30)
  let diff5 = @azimuth.add(50, -30)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), @azimuth.multiply(diff5, diff5))
  @azimuth.assert_eq(1000, variance)  // 400 + 100 + 0 + 100 + 400 = 1000
}

test "geometry_calculations_extended" {
  // 扩展几何计算测试
  let radius = 5
  
  // 圆周长（简化计算）：2 × π × r ≈ 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 圆面积（简化计算）：π × r² ≈ 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 球体体积（简化计算）：4/3 × π × r³ ≈ 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(500, volume)
  
  // 计算圆柱体表面积（简化）：2 × π × r × h + 2 × π × r²
  let height = 10
  let side_area = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(radius, height))
  let top_bottom_area = @azimuth.multiply(2, area)
  let total_surface_area = @azimuth.add(side_area, top_bottom_area)
  @azimuth.assert_eq(450, total_surface_area)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
  
  // 测试除零保护（通过避免除零操作）
  let divisor = 0
  let dividend = 100
  let safe_result = if divisor == 0 { 0 } else { dividend / divisor }
  @azimuth.assert_eq(0, safe_result)
  
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}