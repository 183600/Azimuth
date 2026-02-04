// 标准 Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法

test "arithmetic_commutative_laws" {
  // 测试算术运算的交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_boundary_conditions" {
  // 测试向上取整除法的边界条件
  // 正数情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(100, divide_with_ceil(9999, 100))
  
  // 负数情况
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-4, divide_with_ceil(-21, 5))
  
  // 零除数情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_and_unicode_processing" {
  // 测试字符串处理和 Unicode 支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_workflow" {
  // 测试复合业务场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 12
  let discount = 25
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, shipping_cost)
  let total_with_discount = add(final_total, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(385, total_before_discount)
  assert_eq(397, final_total)
  assert_eq(372, total_with_discount)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 12
  let y = 18
  let z = 0
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
  assert_eq(0, multiply(0, 0))
  
  // 分配律验证：x * (y + z) = x * y + x * z
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景：项目管理
  let team_members = 8
  let tasks_per_member = 5
  let additional_tasks = 7
  let working_days = 6
  
  // 计算总任务数
  let total_tasks = add(multiply(team_members, tasks_per_member), additional_tasks)
  
  // 计算每天需要完成的任务数（向上取整）
  let tasks_per_day = divide_with_ceil(total_tasks, working_days)
  
  // 计算剩余任务
  let completed_tasks = multiply(tasks_per_day, working_days - 1)
  let remaining_tasks = add(total_tasks, -completed_tasks)
  
  assert_eq(47, total_tasks)
  assert_eq(8, tasks_per_day)
  assert_eq(15, remaining_tasks)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试极端数值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试大数值
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-3, divide_with_ceil(-10, 3))
  
  // 测试混合符号运算
  assert_eq(4, add(-3, 7))
  assert_eq(-21, multiply(3, -7))
}

test "advanced_calculation_scenarios" {
  // 测试高级计算场景：金融投资
  let principal = 10000
  let interest_rate = 6
  let years = 3
  let additional_contribution = 2000
  
  // 计算复利（简化版本，每年向上取整）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  // 添加额外贡献
  let final_amount = add(year3_amount, additional_contribution)
  
  assert_eq(10600, year1_amount)
  assert_eq(11236, year2_amount)
  assert_eq(11911, year3_amount)
  assert_eq(13911, final_amount)
}

test "comprehensive_function_combination" {
  // 测试综合函数组合
  let base_value = 25
  let multiplier = 4
  let divisor = 3
  let adjustment = 7
  
  // 复杂表达式计算
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let step3 = add(step2, adjustment)
  let final_result = multiply(step3, 2)
  
  assert_eq(100, step1)
  assert_eq(34, step2)
  assert_eq(41, step3)
  assert_eq(82, final_result)
}

test "internationalization_and_special_cases" {
  // 测试国际化和特殊情况
  // 测试各种语言和特殊字符
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, القاهرة!", greet("القاهرة"))
  assert_eq_string("Hello, 東京!", greet("東京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, München!", greet("München"))
  
  // 测试数字和符号组合
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, +1-555-0123!", greet("+1-555-0123"))
  assert_eq_string("Hello, A&B Company!", greet("A&B Company"))
  
  // 测试边界字符串长度
  let very_long_name = "This is an extremely long name that might be used in some edge case testing scenarios"
  let long_greeting = greet(very_long_name)
  assert_eq_string("Hello, " + very_long_name + "!", long_greeting)
}