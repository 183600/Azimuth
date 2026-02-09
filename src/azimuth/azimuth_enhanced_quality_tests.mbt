// Azimuth 项目增强质量测试用例
// 包含 8 个高质量标准 MoonBit 测试用例

test "arithmetics_order_of_operations" {
  // 测试算术运算的优先级和组合
  let a = 10
  let b = 3
  let c = 2
  
  // 测试 (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(26, result1)
  
  // 测试 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(32, result2)
  
  // 测试 (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(4, result3)
  
  // 测试复杂表达式: a + b * c - a / c
  let result4 = subtract(add(multiply(b, c), a), divide_with_ceil(a, c))
  assert_eq(14, result4)
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 零值情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 大数情况
  assert_eq(33334, divide_with_ceil(100000, 3))
  assert_eq(1, divide_with_ceil(100000, 99999))
}

test "string_greeting_function_comprehensive" {
  // 全面测试问候函数
  // 标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 特殊字符和数字
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  
  // 长字符串
  assert_eq_string("Hello, This is a very long string!", greet("This is a very long string"))
}

test "mathematical_commutative_associative_laws" {
  // 测试数学交换律和结合律
  let x = 15
  let y = 7
  let z = 3
  
  // 加法交换律: x + y = y + x
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律: x * y = y * x
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律: (x + y) + z = x + (y + z)
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律: (x * y) * z = x * (y * z)
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律: x * (y + z) = x * y + x * z
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "financial_calculations_practical" {
  // 实用金融计算测试
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 3
  
  // 简单利息计算
  let simple_interest = multiply(divide_with_ceil(multiply(principal, rate1), 100), years)
  let total_simple = add(principal, simple_interest)
  
  // 复利计算（简化版）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate1), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate1), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate1), 100))
  
  // 投资组合计算
  let investment1 = multiply(principal, 60) / 100  // 60% 投资
  let investment2 = multiply(principal, 40) / 100  // 40% 投资
  let return1 = add(investment1, divide_with_ceil(multiply(investment1, rate1), 100))
  let return2 = add(investment2, divide_with_ceil(multiply(investment2, rate2), 100))
  let portfolio_total = add(return1, return2)
  
  assert_eq(1500, simple_interest)
  assert_eq(11500, total_simple)
  assert_eq(11576, year3)
  assert_eq(11570, portfolio_total)
}

test "scientific_calculations_engineering" {
  // 科学工程计算测试
  // 温度转换系列
  let celsius = 25
  
  // 摄氏度转华氏度: F = C × 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转开尔文: K = (F - 32) × 5/9 + 273
  let kelvin = add(divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9), 273)
  
  // 物理运动计算
  let initial_velocity = 10
  let acceleration = 3
  let time = 5
  
  // 最终速度: v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 位移: s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  assert_eq(77, fahrenheit)
  assert_eq(298, kelvin)
  assert_eq(25, final_velocity)
  assert_eq(85, displacement)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  let max_val = 100000
  let min_val = -100000
  
  // 极值加法
  assert_eq(200000, add(max_val, max_val))
  assert_eq(-200000, add(min_val, min_val))
  assert_eq(0, add(max_val, min_val))
  
  // 极值乘法
  assert_eq(10000000000, multiply(max_val, max_val))
  assert_eq(10000000000, multiply(min_val, min_val))
  assert_eq(-10000000000, multiply(max_val, min_val))
  
  // 边界除法
  assert_eq(100000, divide_with_ceil(max_val, 1))
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(0, divide_with_ceil(1, max_val))
}

test "real_world_scenarios_comprehensive" {
  // 真实世界场景综合测试
  // 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 8
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let subtotal = add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  // 库存管理
  let initial_stock = 100
  let daily_sales = 8
  let days = 7
  let new_shipment = 50
  
  let weekly_sales = multiply(daily_sales, days)
  let remaining_stock = subtract(initial_stock, weekly_sales)
  let final_stock = add(remaining_stock, new_shipment)
  
  // 项目管理
  let total_tasks = 45
  let team_members = 5
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  let hours_per_task = 4
  let total_hours = multiply(total_tasks, hours_per_task)
  
  assert_eq(105, subtotal)
  assert_eq(9, tax)
  assert_eq(114, total)
  assert_eq(56, weekly_sales)
  assert_eq(44, remaining_stock)
  assert_eq(94, final_stock)
  assert_eq(9, tasks_per_person)
  assert_eq(180, total_hours)
}