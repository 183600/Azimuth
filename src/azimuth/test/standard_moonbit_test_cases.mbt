// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_law" {
  // 验证加法交换律：a + b = b + a
  let a = 17
  let b = 29
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "multiplication_identity_element" {
  // 验证乘法单位元：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  @azimuth.assert_eq(999999, @azimuth.multiply(999999, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  // 测试小数部分向上取整
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))    // 8/3 = 2.666... -> 3
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(21, 4))   // 21/4 = 5.25 -> 6
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(20, 3))   // 20/3 = 6.666... -> 7
  
  // 测试负数向上取整
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))  // -8/3 = -2.666... -> -2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))  // -8/-3 = 2.666... -> 3
}

test "greet_function_unicode_comprehensive" {
  // 测试问候函数的 Unicode 全面支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, Ñoño!", @azimuth.greet("Ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🚀🌙!", @azimuth.greet("🚀🌙"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
}

test "complex_business_calculation" {
  // 复杂业务计算：电商订单总价计算
  let product_price = 89
  let quantity = 6
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount = 25
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(534, subtotal)
  @azimuth.assert_eq(43, tax_amount)
  @azimuth.assert_eq(592, total_before_discount)
  @azimuth.assert_eq(567, final_total)
}

test "mathematical_distributive_law" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 测试除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 1000
  let project_a_needs = 234
  let project_b_needs = 189
  let project_c_needs = 167
  
  // 计算已分配资源
  let allocated = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  
  // 计算剩余资源
  let remaining = @azimuth.add(total_resources, -allocated)
  
  // 计算可以支持的小项目数量（每个小项目需要 45 资源）
  let small_projects_possible = @azimuth.divide_with_ceil(remaining, 45)
  
  @azimuth.assert_eq(590, allocated)
  @azimuth.assert_eq(410, remaining)
  @azimuth.assert_eq(10, small_projects_possible)  // ceil(410/45) = 10
}

test "time_series_calculation" {
  // 时间序列计算测试
  let daily_average = 125
  let weekly_target = 1000
  let monthly_target = 4500
  
  // 计算达到周目标需要的天数
  let days_for_weekly = @azimuth.divide_with_ceil(weekly_target, daily_average)
  
  // 计算达到月目标需要的天数
  let days_for_monthly = @azimuth.divide_with_ceil(monthly_target, daily_average)
  
  // 计算一周的实际产量
  let weekly_production = @azimuth.multiply(daily_average, 7)
  
  // 计算一个月（30天）的实际产量
  let monthly_production = @azimuth.multiply(daily_average, 30)
  
  @azimuth.assert_eq(8, days_for_weekly)     // ceil(1000/125) = 8
  @azimuth.assert_eq(36, days_for_monthly)   // ceil(4500/125) = 36
  @azimuth.assert_eq(875, weekly_production) // 125 * 7 = 875
  @azimuth.assert_eq(3750, monthly_production) // 125 * 30 = 3750
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let russian_name = "Анна"
  let emoji_name = "🌟🚀"
  
  // 测试不同语言的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(english_name))
  @azimuth.assert_eq_string("Hello, 小明!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, Анна!", @azimuth.greet(russian_name))
  @azimuth.assert_eq_string("Hello, 🌟🚀!", @azimuth.greet(emoji_name))
  
  // 测试复合名称
  let full_name = english_name + " " + chinese_name
  @azimuth.assert_eq_string("Hello, Alice 小明!", @azimuth.greet(full_name))
}