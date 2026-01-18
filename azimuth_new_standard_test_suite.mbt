// Azimuth 项目新的标准测试套件
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常除法（无余数）
  assert_eq(5, azimuth.divide_with_ceil(10, 2))
  assert_eq(7, azimuth.divide_with_ceil(21, 3))
  
  // 向上取整（有余数）
  assert_eq(6, azimuth.divide_with_ceil(11, 2))
  assert_eq(8, azimuth.divide_with_ceil(22, 3))
  assert_eq(4, azimuth.divide_with_ceil(7, 2))
  
  // 边界情况
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(99, 100))
  assert_eq(100, azimuth.divide_with_ceil(100, 1))
}

test "date_calculation_scenarios" {
  // 日期计算场景测试
  // 计算月份中的天数（简化版）
  let january_days = 31
  let february_days = 28  // 非闰年
  let march_days = 31
  
  // 计算第一季度的总天数
  let q1_days = azimuth.add(january_days, azimuth.add(february_days, march_days))
  assert_eq(90, q1_days)
  
  // 计算一年中的天数（非闰年）
  let q2_days = 30 + 31 + 30
  let q3_days = 31 + 31 + 30
  let q4_days = 31 + 30 + 31
  let year_days = azimuth.add(q1_days, azimuth.add(q2_days, azimuth.add(q3_days, q4_days)))
  assert_eq(365, year_days)
  
  // 计算周数
  let weeks_in_year = year_days / 7
  assert_eq(52, weeks_in_year)
}

test "physics_formula_calculations" {
  // 物理公式计算测试
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10
  let velocity = 5
  
  let velocity_squared = azimuth.multiply(velocity, velocity)
  let kinetic_energy = azimuth.multiply(mass, velocity_squared) / 2
  assert_eq(125, kinetic_energy)
  
  // 计算势能：E = m * g * h
  let height = 20
  let gravity = 10  // 简化重力加速度
  
  let potential_energy = azimuth.multiply(mass, azimuth.multiply(gravity, height))
  assert_eq(2000, potential_energy)
  
  // 计算总机械能
  let total_energy = azimuth.add(kinetic_energy, potential_energy)
  assert_eq(2125, total_energy)
}

test "array_index_calculations" {
  // 数组索引计算测试
  // 二维数组转一维数组的索引计算
  let row = 3
  let col = 4
  let width = 5
  
  // 计算二维数组在一维数组中的索引：index = row * width + col
  let index = azimuth.add(azimuth.multiply(row, width), col)
  assert_eq(19, index)
  
  // 测试边界情况
  let first_row = 0
  let first_col = 0
  let first_index = azimuth.add(azimuth.multiply(first_row, width), first_col)
  assert_eq(0, first_index)
  
  // 测试环形缓冲区索引
  let buffer_size = 10
  let current_pos = 8
  let offset = 5
  let new_pos = (azimuth.add(current_pos, offset)) % buffer_size
  assert_eq(3, new_pos)
}

test "string_template_processing" {
  // 字符串模板处理测试
  // 模拟简单的字符串模板功能
  let user_name = "Alice"
  let action = "购买"
  let item = "商品"
  let quantity = 3
  
  // 构建消息模板
  let greeting = azimuth.greet(user_name)
  let action_part = "您已" + action + "了"
  let item_part = item + "，数量：" + quantity.to_string()
  let full_message = greeting + " " + action_part + item_part + "。"
  
  assert_eq_string("Hello, Alice! 您已购买了商品，数量：3。", full_message)
  
  // 测试包含特殊字符的模板
  let special_user = "Bob@123"
  let special_greeting = azimuth.greet(special_user)
  assert_eq_string("Hello, Bob@123!", special_greeting)
}

test "mathematical_formula_validation" {
  // 数学公式验证测试
  let a = 6
  let b = 4
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 8
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  azimuth.assert_eq(left_side, right_side)
  
  // 验证平方差公式：(a + b) × (a - b) = a² - b²
  let sum = azimuth.add(a, b)
  let diff = azimuth.add(a, -b)
  let product = azimuth.multiply(sum, diff)
  let square_diff = azimuth.add(azimuth.multiply(a, a), azimuth.multiply(-b, b))
  azimuth.assert_eq(product, square_diff)
  
  // 验证勾股定理：a² + b² = c²
  let leg1 = 3
  let leg2 = 4
  let hypotenuse = 5
  let pythagorean_left = azimuth.add(azimuth.multiply(leg1, leg1), azimuth.multiply(leg2, leg2))
  let pythagorean_right = azimuth.multiply(hypotenuse, hypotenuse)
  azimuth.assert_eq(pythagorean_left, pythagorean_right)
}

test "boundary_condition_combinations" {
  // 边界条件组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试最大值与其他边界值的运算
  azimuth.assert_eq(max_val, azimuth.add(max_val, zero))
  azimuth.assert_eq(max_val, azimuth.multiply(max_val, one))
  azimuth.assert_eq(min_val, azimuth.multiply(max_val, neg_one))
  
  // 测试最小值与其他边界值的运算
  azimuth.assert_eq(min_val, azimuth.add(min_val, zero))
  azimuth.assert_eq(min_val, azimuth.multiply(min_val, one))
  azimuth.assert_eq(max_val, azimuth.multiply(min_val, neg_one))
  
  // 测试零的运算特性
  azimuth.assert_eq(zero, azimuth.multiply(zero, max_val))
  azimuth.assert_eq(zero, azimuth.multiply(zero, min_val))
  azimuth.assert_eq(max_val, azimuth.add(zero, max_val))
  azimuth.assert_eq(min_val, azimuth.add(zero, min_val))
  
  // 测试连续边界运算
  let chain_result = azimuth.add(azimuth.multiply(azimuth.add(max_val, min_val), one), max_val)
  azimuth.assert_eq(max_val, chain_result)
}

test "real_world_business_scenario" {
  // 实际业务场景测试：电商订单计算
  let product_price = 299
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let shipping_fee = 20
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = azimuth.multiply(product_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = azimuth.add(azimuth.add(discounted_total, tax_amount), shipping_fee)
  
  azimuth.assert_eq(1076, final_total)
  
  // 测试会员折扣场景
  let is_member = true
  let member_discount = if (is_member) { 5 } else { 0 }
  let member_total = azimuth.add(final_total, -azimuth.multiply(final_total, member_discount) / 100)
  azimuth.assert_eq(1022, member_total)
}

test "performance_related_calculations" {
  // 性能相关测试：大量数据的计算模拟
  let base_value = 1000
  let iterations = 100
  
  // 模拟累加计算：base_value × (1 + 2 + ... + iterations)
  // 使用公式：base_value × iterations × (iterations + 1) / 2
  let sum_formula = azimuth.multiply(base_value, azimuth.multiply(iterations, azimuth.add(iterations, 1)) / 2)
  
  // 验证前几项的计算
  let sum_first_5 = azimuth.multiply(base_value, azimuth.multiply(5, 6) / 2)
  azimuth.assert_eq(15000, sum_first_5)
  
  // 验证总和
  azimuth.assert_eq(5050000, sum_formula)
  
  // 模拟指数增长计算
  let growth_factor = 2
  let periods = 10
  let initial_amount = 100
  
  // 计算：initial_amount × growth_factor^periods（简化计算）
  let exponential_growth = azimuth.multiply(initial_amount, 1024)  // 2^10 = 1024
  azimuth.assert_eq(102400, exponential_growth)
  
  // 模拟斐波那契数列计算（前10项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth.add(fib1, fib2)  // 2
  let fib4 = azimuth.add(fib2, fib3)  // 3
  let fib5 = azimuth.add(fib3, fib4)  // 5
  let fib6 = azimuth.add(fib4, fib5)  // 8
  let fib7 = azimuth.add(fib5, fib6)  // 13
  let fib8 = azimuth.add(fib6, fib7)  // 21
  let fib9 = azimuth.add(fib7, fib8)  // 34
  let fib10 = azimuth.add(fib8, fib9) // 55
  
  azimuth.assert_eq(55, fib10)
}

test "data_type_conversion_simulation" {
  // 数据类型转换测试：模拟不同数据类型之间的转换
  // 模拟整数到字符串的转换（通过greet函数）
  let number = 2023
  let number_as_string = "2023"
  let converted_greeting = azimuth.greet(number_as_string)
  
  azimuth.assert_eq_string("Hello, 2023!", converted_greeting)
  
  // 模拟布尔值到字符串的转换
  let true_as_string = "true"
  let false_as_string = "false"
  
  azimuth.assert_eq_string("Hello, true!", azimuth.greet(true_as_string))
  azimuth.assert_eq_string("Hello, false!", azimuth.greet(false_as_string))
  
  // 模拟浮点数的简化表示（使用整数表示小数部分）
  let integer_part = 3
  let decimal_part = 14
  let pi_approx_string = "3.14"
  
  azimuth.assert_eq_string("Hello, 3.14!", azimuth.greet(pi_approx_string))
  
  // 模拟日期时间字符串
  let date_string = "2023-12-25"
  let time_string = "15:30:45"
  let datetime_string = date_string + " " + time_string
  
  azimuth.assert_eq_string("Hello, 2023-12-25 15:30:45!", azimuth.greet(datetime_string))
  
  // 模拟货币格式字符串
  let currency_amount = "¥299.99"
  azimuth.assert_eq_string("Hello, ¥299.99!", azimuth.greet(currency_amount))
}