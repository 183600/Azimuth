// Azimuth 项目新增的10个高质量 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "boundary_values_extreme_cases" {
  // 边界值极值测试
  // 测试接近整数边界的情况
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))  // 接近 Int 最大值
  @azimuth.assert_eq(-2147483646, @azimuth.add(-2147483645, -1))  // 接近 Int 最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
  
  // 测试边界除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2147483647, 2147483647))  // 相等的大数
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(2147483647, 1073741824))  // 大数除法
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let a = 12
  let b = 8
  let c = 5
  
  // 测试嵌套表达式：(a + b) * c - a / b
  let result1 = @azimuth.subtract(@azimuth.multiply(@azimuth.add(a, b), c), @azimuth.divide_with_ceil(a, b))
  @azimuth.assert_eq(98, result1)  // (12 + 8) * 5 - ceil(12/8) = 20 * 5 - 2 = 100 - 2 = 98
  
  // 测试复杂组合：a * (b + c) + b * (c + a)
  let result2 = @azimuth.add(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.multiply(b, @azimuth.add(c, a)))
  @azimuth.assert_eq(292, result2)  // 12 * (8 + 5) + 8 * (5 + 12) = 12 * 13 + 8 * 17 = 156 + 136 = 292
}

test "real_world_business_scenario" {
  // 真实业务场景测试：电商订单计算
  let product_a_price = 89
  let product_a_qty = 2
  let product_b_price = 45
  let product_b_qty = 3
  let shipping_base = 15
  let tax_rate = 8  // 8%
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算商品总价
  let subtotal_a = @azimuth.multiply(product_a_price, product_a_qty)
  let subtotal_b = @azimuth.multiply(product_b_price, product_b_qty)
  let subtotal = @azimuth.add(subtotal_a, subtotal_b)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算运费（按商品数量计算）
  let total_items = @azimuth.add(product_a_qty, product_b_qty)
  let shipping_cost = @azimuth.multiply(shipping_base, @azimuth.divide_with_ceil(total_items, 3))
  
  // 计算总价
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  let final_total = if total_before_discount > discount_threshold {
    @azimuth.subtract(total_before_discount, discount_amount)
  } else {
    total_before_discount
  }
  
  @azimuth.assert_eq(313, subtotal)  // 89*2 + 45*3 = 178 + 135 = 313
  @azimuth.assert_eq(26, tax_amount)  // ceil(313*8/100) = ceil(25.04) = 26
  @azimuth.assert_eq(30, shipping_cost)  // 15 * ceil(5/3) = 15 * 2 = 30
  @azimuth.assert_eq(344, final_total)  // 313 + 26 + 30 - 25 = 344
}

test "performance_large_number_operations" {
  // 性能大数运算测试
  let large_num1 = 10000
  let large_num2 = 99999
  let large_num3 = 50000
  
  // 测试大数运算性能
  let large_sum = @azimuth.add(@azimuth.add(large_num1, large_num2), large_num3)
  let large_product = @azimuth.multiply(large_num1, large_num3)
  let large_division = @azimuth.divide_with_ceil(large_product, large_num2)
  
  @azimuth.assert_eq(149999, large_sum)  // 10000 + 99999 + 50000 = 149999
  @azimuth.assert_eq(500000000, large_product)  // 10000 * 50000 = 500000000
  @azimuth.assert_eq(5001, large_division)  // ceil(500000000/99999) = 5001
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零的各种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483647, 0))
  
  // 测试零元素的运算性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483647, 0))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  // 测试各种 Unicode 字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🎉🎊🎈!", @azimuth.greet("🎉🎊🎈"))
}

test "mathematical_laws_verification" {
  // 数学定律验证测试
  let x = 17
  let y = 23
  let z = 31
  
  // 加法交换律：x + y = y + x
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律：x * y = y * x
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律：(x + y) + z = x + (y + z)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律：(x * y) * z = x * (y * z)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 分配律：x * (y + z) = x * y + x * z
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

test "string_boundary_conditions" {
  // 字符串边界条件测试
  // 空字符串和特殊空白字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  
  // 单字符和长字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, abcdefghijklmnopqrstuvwxyz!", @azimuth.greet("abcdefghijklmnopqrstuvwxyz"))
  
  // 特殊字符组合
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", @azimuth.greet("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  let neg_a = -15
  let neg_b = -8
  let pos_c = 12
  
  // 负数加法
  @azimuth.assert_eq(-23, @azimuth.add(neg_a, neg_b))
  @azimuth.assert_eq(-3, @azimuth.add(neg_a, pos_c))
  @azimuth.assert_eq(3, @azimuth.add(pos_c, neg_a))
  
  // 负数乘法
  @azimuth.assert_eq(120, @azimuth.multiply(neg_a, neg_b))  // 负负得正
  @azimuth.assert_eq(-180, @azimuth.multiply(neg_a, pos_c))
  @azimuth.assert_eq(-180, @azimuth.multiply(pos_c, neg_a))
  
  // 负数除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(neg_a, pos_c))  // ceil(-15/12) = ceil(-1.25) = -1
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(neg_a, neg_b))   // ceil(-15/-8) = ceil(1.875) = 2
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(pos_c, neg_a))  // ceil(12/-15) = ceil(-0.8) = 0
}

test "practical_calculation_scenarios" {
  // 实际计算场景测试
  // 场景1：计算项目资源需求
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let total_tasks = 120
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_members, @azimuth.multiply(hours_per_day, working_days))
  
  // 计算总任务处理能力
  let total_capacity = @azimuth.multiply(total_hours, tasks_per_hour)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, @azimuth.divide_with_ceil(total_capacity, 4))
  
  @azimuth.assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  @azimuth.assert_eq(1920, total_capacity)  // 960 * 2 = 1920
  @azimuth.assert_eq(1, sprints_needed)  // ceil(120/480) = 1
  
  // 场景2：计算装运成本
  let package_weight = 125
  let weight_per_pallet = 50
  let cost_per_pallet = 25
  let handling_fee = 15
  
  let pallets_needed = @azimuth.divide_with_ceil(package_weight, weight_per_pallet)
  let shipping_cost = @azimuth.multiply(pallets_needed, cost_per_pallet)
  let total_cost = @azimuth.add(shipping_cost, handling_fee)
  
  @azimuth.assert_eq(3, pallets_needed)  // ceil(125/50) = 3
  @azimuth.assert_eq(75, shipping_cost)  // 3 * 25 = 75
  @azimuth.assert_eq(90, total_cost)  // 75 + 15 = 90
}