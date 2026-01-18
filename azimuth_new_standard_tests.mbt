// Azimuth 项目新的标准 MoonBit 测试用例
// 包含10个测试用例，使用标准 MoonBit 测试语法

test "addition_chain_calculation" {
  // 测试链式加法计算
  let base = 100
  let increment1 = 25
  let increment2 = 30
  let increment3 = 45
  
  // 链式加法
  let result1 = add(add(add(base, increment1), increment2), increment3)
  assert_eq(200, result1)
  
  // 测试负数链式加法
  let base2 = 500
  let decrement1 = 100
  let decrement2 = 150
  let decrement3 = 50
  
  let result2 = add(add(add(base2, -decrement1), -decrement2), -decrement3)
  assert_eq(200, result2)
}

test "multiplication_power_simulation" {
  // 测试通过乘法模拟幂运算
  let base = 3
  let exponent = 4
  
  // 计算 3^4 = 3 × 3 × 3 × 3 = 81
  let power_result = multiply(multiply(multiply(base, base), base), base)
  assert_eq(81, power_result)
  
  // 测试负数的幂
  let neg_base = -2
  let even_exponent = 3
  
  // 计算 (-2)^3 = -2 × -2 × -2 = -8
  let neg_power_result = multiply(multiply(neg_base, neg_base), neg_base)
  assert_eq(-8, neg_power_result)
}

test "string_unicode_handling" {
  // 测试字符串Unicode处理
  // 测试中文字符
  let chinese_name = "张三"
  let chinese_result = greet(chinese_name)
  assert_eq_string("Hello, 张三!", chinese_result)
  
  // 测试emoji字符
  let emoji_name = "🚀"
  let emoji_result = greet(emoji_name)
  assert_eq_string("Hello, 🚀!", emoji_result)
  
  // 测试混合Unicode字符
  let mixed_name = "测试123🌟"
  let mixed_result = greet(mixed_name)
  assert_eq_string("Hello, 测试123🌟!", mixed_result)
}

test "mathematical_series_fibonacci" {
  // 测试斐波那契数列计算（简化版）
  // 计算前几项：1, 1, 2, 3, 5, 8, 13, 21
  
  let f1 = 1
  let f2 = 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  let f7 = add(f5, f6)  // 13
  let f8 = add(f6, f7)  // 21
  
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  
  // 计算前8项的和
  let sum = add(add(add(add(add(add(add(f1, f2), f3), f4), f5), f6), f7), f8)
  assert_eq(54, sum)
}

test "business_profit_calculation" {
  // 测试商业利润计算
  let revenue = 50000
  let cost1 = 20000
  let cost2 = 15000
  let cost3 = 8000
  let tax_rate = 10  // 10%
  
  // 计算总成本
  let total_cost = add(add(cost1, cost2), cost3)
  assert_eq(43000, total_cost)
  
  // 计算税前利润
  let profit_before_tax = add(revenue, -total_cost)
  assert_eq(7000, profit_before_tax)
  
  // 计算税费
  let tax = multiply(profit_before_tax, tax_rate) / 100
  assert_eq(700, tax)
  
  // 计算净利润
  let net_profit = add(profit_before_tax, -tax)
  assert_eq(6300, net_profit)
}

test "geometry_volume_calculations" {
  // 测试几何体积计算
  let length = 10
  let width = 5
  let height = 3
  
  // 计算长方体体积
  let volume = multiply(multiply(length, width), height)
  assert_eq(150, volume)
  
  // 计算表面积（简化）：2 × (长×宽 + 长×高 + 宽×高)
  let area1 = multiply(length, width)   // 50
  let area2 = multiply(length, height)  // 30
  let area3 = multiply(width, height)   // 15
  let surface_area = multiply(2, add(add(area1, area2), area3))
  assert_eq(190, surface_area)
  
  // 计算立方体体积（边长为4）
  let cube_side = 4
  let cube_volume = multiply(multiply(cube_side, cube_side), cube_side)
  assert_eq(64, cube_volume)
}

test "digital_clock_conversion" {
  // 测试数字时钟转换
  let total_hours = 38
  
  // 转换为天和小时
  let days = total_hours / 24
  let remaining_hours = total_hours % 24
  
  assert_eq(1, days)
  assert_eq(14, remaining_hours)
  
  // 测试分钟转换
  let total_minutes = 150
  
  // 转换为小时和分钟
  let hours = total_minutes / 60
  let minutes = total_minutes % 60
  
  assert_eq(2, hours)
  assert_eq(30, minutes)
  
  // 计算总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
}

test "array_sum_simulation" {
  // 模拟数组求和（通过手动展开）
  // 模拟数组 [10, 20, 30, 40, 50]
  let elem1 = 10
  let elem2 = 20
  let elem3 = 30
  let elem4 = 40
  let elem5 = 50
  
  // 计算总和
  let sum = add(add(add(add(elem1, elem2), elem3), elem4), elem5)
  assert_eq(150, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  assert_eq(30, average)
  
  // 计算加权平均（权重分别为1, 2, 3, 4, 5）
  let weighted_sum = add(add(add(add(multiply(elem1, 1), multiply(elem2, 2)), 
                               multiply(elem3, 3)), multiply(elem4, 4)), multiply(elem5, 5))
  let total_weight = add(add(add(add(1, 2), 3), 4), 5)
  let weighted_average = weighted_sum / total_weight
  assert_eq(36, weighted_average)
}

test "conditional_arithmetic_operations" {
  // 测试条件算术运算
  let a = 15
  let b = 20
  let c = 25
  
  // 条件1：如果 a < b，则计算 a + b，否则计算 a * b
  let condition1 = a < b
  let result1 = if condition1 { add(a, b) } else { multiply(a, b) }
  assert_eq(35, result1)
  
  // 条件2：如果 b > c，则计算 b - c，否则计算 c - b
  let condition2 = b > c
  let result2 = if condition2 { add(b, -c) } else { add(c, -b) }
  assert_eq(5, result2)
  
  // 条件3：如果 a + b > c，则计算 (a + b) * 2，否则计算 (a + b) / 2
  let sum_ab = add(a, b)
  let condition3 = sum_ab > c
  let result3 = if condition3 { multiply(sum_ab, 2) } else { sum_ab / 2 }
  assert_eq(70, result3)
}

test "boundary_value_extreme_combinations" {
  // 测试边界值的极端组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的平方（应该溢出）
  let max_squared = multiply(max_val, max_val)
  assert_eq(max_val, max_squared)
  
  // 测试最小值的平方（应该溢出）
  let min_squared = multiply(min_val, min_val)
  assert_eq(max_val, min_squared)
  
  // 测试最大值与最小值的乘积
  let max_min_product = multiply(max_val, min_val)
  assert_eq(min_val, max_min_product)
  
  // 测试连续边界值运算
  let chain1 = add(add(max_val, min_val), max_val)
  assert_eq(max_val, chain1)
  
  let chain2 = multiply(multiply(min_val, -1), -1)
  assert_eq(min_val, chain2)
  
  // 测试边界值的算术序列
  let sequence_sum = add(add(add(add(min_val, 1), 2), 4), 8)
  assert_eq(-2147483633, sequence_sum)
}