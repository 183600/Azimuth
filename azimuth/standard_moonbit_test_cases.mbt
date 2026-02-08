// 标准 MoonBit 测试用例 - Azimuth 项目
// 测试核心数学函数和字符串处理功能

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "divide_with_ceil_positive_numbers" {
  // 向上取整除法正数测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_basic_cases" {
  // 减法函数基本测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}// 用户请求的 MoonBit 测试用例 - Azimuth 项目
// 包含10个测试用例，使用标准 MoonBit 测试语法

test "scenario_financial_tax_calculation" {
  // 财务计算场景：计算商品总价和税费
  let unit_price = 120
  let quantity = 8
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, 5), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(960, subtotal)
  assert_eq(48, tax_amount)
  assert_eq(1008, total)
}

test "scenario_project_time_estimation" {
  // 时间管理场景：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let work_days_per_week = 5
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(15, days_needed)
  assert_eq(3, weeks_needed)
}

test "scenario_server_resource_planning" {
  // 资源分配场景：计算服务器资源需求
  let users_per_server = 50
  let total_users = 237
  let servers_needed = divide_with_ceil(total_users, users_per_server)
  let backup_servers = divide_with_ceil(multiply(servers_needed, 10), 100)
  let total_servers = add(servers_needed, backup_servers)
  
  assert_eq(5, servers_needed)
  assert_eq(1, backup_servers)
  assert_eq(6, total_servers)
}

test "property_math_distributive_law" {
  // 数学分配律测试：a * (b + c) = a*b + a*c
  let a = 7
  let b = 12
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "boundary_large_number_operations" {
  // 极端边界条件测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(0, divide_with_ceil(0, 1000000))
}

test "string_special_character_handling" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  
  let long_name = "VeryLongNameThatMightCauseIssuesInSomeSystems"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "chain_complex_calculation" {
  // 复杂计算链测试
  let base_value = 10
  let step1 = add(base_value, multiply(3, 4))
  let step2 = subtract(step1, divide_with_ceil(15, 4))
  let step3 = multiply(step2, add(2, 1))
  let final_result = add(step3, divide_with_ceil(step3, 10))
  
  assert_eq(60, final_result)
}

test "scenario_inventory_optimization" {
  // 库存优化场景：计算最优订货量
  let monthly_demand = 450
  let order_cost = 50
  let holding_cost_per_unit = 2
  
  let eoq_numerator = multiply(monthly_demand, multiply(order_cost, 2))
  let eoq = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  let annual_demand = multiply(monthly_demand, 12)
  let orders_per_year = divide_with_ceil(annual_demand, eoq)
  
  assert_eq(45000, eoq_numerator)
  assert_eq(22500, eoq)
  assert_eq(1, orders_per_year)
}

test "logic_boolean_simulation" {
  // 布尔逻辑模拟：使用数值运算模拟布尔操作
  let true_val = 1
  let false_val = 0
  
  let and_true = multiply(true_val, true_val)
  let and_false1 = multiply(true_val, false_val)
  
  let or_true1 = subtract(add(true_val, false_val), multiply(true_val, false_val))
  let or_false = subtract(add(false_val, false_val), multiply(false_val, false_val))
  
  assert_eq(1, and_true)
  assert_eq(0, and_false1)
  assert_eq(1, or_true1)
  assert_eq(0, or_false)
}

test "error_division_by_zero_handling" {
  // 错误处理和恢复场景测试
  let normal_division = divide_with_ceil(10, 2)
  let zero_division = divide_with_ceil(10, 0)
  let negative_zero_division = divide_with_ceil(-10, 0)
  
  let negative_dividend = divide_with_ceil(-10, 3)
  let negative_divisor = divide_with_ceil(10, -3)
  let both_negative = divide_with_ceil(-10, -3)
  
  assert_eq(5, normal_division)
  assert_eq(0, zero_division)
  assert_eq(0, negative_zero_division)
  assert_eq(-3, negative_dividend)
  assert_eq(-4, negative_divisor)
  assert_eq(4, both_negative)
}

test "azimuth_advanced_mathematical_test" {
  // Azimuth 高级数学测试
  // 测试数学运算的结合律
  let x = 4
  let y = 6
  let z = 8
  
  // 加法结合律: (x + y) + z = x + (y + z)
  let left_assoc = add(add(x, y), z)
  let right_assoc = add(x, add(y, z))
  assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (x * y) * z = x * (y * z)
  let left_mul_assoc = multiply(multiply(x, y), z)
  let right_mul_assoc = multiply(x, multiply(y, z))
  assert_eq(left_mul_assoc, right_mul_assoc)
  
  // 分配律: x * (y + z) = x * y + x * z
  let distrib_left = multiply(x, add(y, z))
  let distrib_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distrib_left, distrib_right)
}