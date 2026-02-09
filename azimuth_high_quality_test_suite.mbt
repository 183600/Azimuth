// 高质量 MoonBit 测试用例 - 为 Azimuth 项目设计
// 包含10个标准测试用例，覆盖各种功能和场景

// 测试用例1: 数学运算边界测试
test "mathematical_boundary_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(1000000, 1000))
  
  // 测试极值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 999999))
  
  // 测试负数极值
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(-1000, -1000))
  
  // 测试大数除法
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

// 测试用例2: 除法函数综合测试
test "divide_with_ceil_comprehensive_scenarios" {
  // 正数除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况测试
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0时返回0
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  
  // 精度测试
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(666667, divide_with_ceil(2000000, 3))
}

// 测试用例3: 字符串处理增强测试
test "greet_function_enhanced_string_processing" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 空字符串和特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // Unicode和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  
  // 数字和字母混合测试
  assert_eq_string("Hello, test123!", greet("test123"))
  assert_eq_string("Hello, 123test!", greet("123test"))
}

// 测试用例4: 数学交换律和结合律测试
test "mathematical_laws_validation" {
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
  assert_eq(25, add(add(12, 8), 5))
  assert_eq(480, multiply(multiply(12, 8), 5))
}

// 测试用例5: 实际业务场景测试
test "real_world_business_scenarios" {
  // 场景1: 计算项目资源需求
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 场景2: 库存管理计算
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_products)
  
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  assert_eq(168, total_capacity)  // 7 * 24 = 168
  assert_eq(11, remaining_space)  // 168 - 157 = 11
  
  // 场景3: 预算分配计算
  let total_budget = 100000
  let departments = 3
  let base_allocation = divide_with_ceil(total_budget, departments)
  let remaining_budget = subtract(total_budget, multiply(base_allocation, departments))
  
  assert_eq(33334, base_allocation)  // ceil(100000/3) = 33334
  assert_eq(-2, remaining_budget)  // 100000 - 33334*3 = -2
}

// 测试用例6: 错误处理和边界条件测试
test "error_handling_and_edge_cases" {
  // 除零测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 溢出边界测试
  let max_val = 1000000
  assert_eq(2000000, add(max_val, max_val))
  assert_eq(0, subtract(max_val, max_val))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, AA!", greet("AA"))
}

// 测试用例7: 复杂计算流程测试
test "complex_calculation_workflow" {
  // 复杂数学表达式计算
  let step1 = add(10, 5)           // 15
  let step2 = multiply(step1, 3)   // 45
  let step3 = divide_with_ceil(step2, 8)  // 6
  let step4 = subtract(step3, 2)   // 4
  let step5 = multiply(step4, add(1, 3))  // 16
  
  assert_eq(15, step1)
  assert_eq(45, step2)
  assert_eq(6, step3)
  assert_eq(4, step4)
  assert_eq(16, step5)
  
  // 嵌套计算测试
  let complex_result = add(
    multiply(2, 3),
    divide_with_ceil(subtract(10, 2), 4)
  )
  assert_eq(8, complex_result)  // 2*3 + ceil(8/4) = 6 + 2 = 8
  
  // 条件性计算测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let final_result = divide_with_ceil(
    multiply(base_value, multiplier),
    divisor
  )
  assert_eq(7, final_result)  // ceil(15*3/7) = ceil(45/7) = 7
}

// 测试用例8: 性能和精度测试
test "performance_and_precision_validation" {
  // 大数精度测试
  let large_num1 = 1000000
  let large_num2 = 999999
  
  assert_eq(1999999, add(large_num1, large_num2))
  assert_eq(999999000000, multiply(large_num1, large_num2))
  
  // 除法精度测试
  assert_eq(1001, divide_with_ceil(large_num1, 999))
  assert_eq(2, divide_with_ceil(large_num2, 500000))
  assert_eq(1, divide_with_ceil(large_num2, 1000000))
  
  // 连续运算精度测试
  let precision_test = divide_with_ceil(
    multiply(add(100, 200), 3),
    150
  )
  assert_eq(6, precision_test)  // ceil((100+200)*3/150) = ceil(900/150) = 6
  
  // 小数处理精度测试（通过整数运算模拟）
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(67, divide_with_ceil(200, 3))
  assert_eq(1, divide_with_ceil(2, 2))
}

// 测试用例9: 国际化和特殊字符测试
test "internationalization_and_special_chars" {
  // 亚洲字符测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  
  // 欧洲字符测试
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, français!", greet("français"))
  assert_eq_string("Hello, deutsch!", greet("deutsch"))
  assert_eq_string("Hello, русский!", greet("русский"))
  
  // 特殊符号测试
  assert_eq_string("Hello, 🚀🌟!", greet("🚀🌟"))
  assert_eq_string("Hello, ♠️♥️♦️♣️!", greet("♠️♥️♦️♣️"))
  assert_eq_string("Hello, ☀️☁️⛅!", greet("☀️☁️⛅"))
  
  // 技术字符测试
  assert_eq_string("Hello, <script>!", greet("<script>"))
  assert_eq_string("Hello, &nbsp;!", greet("&nbsp;"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
}

// 测试用例10: 综合业务逻辑测试
test "comprehensive_business_logic" {
  // 复杂库存管理系统测试
  let product_types = 5
  let items_per_type = 120
  let total_items = multiply(product_types, items_per_type)
  
  let warehouse_capacity = 200
  let warehouses_needed = divide_with_ceil(total_items, warehouse_capacity)
  
  let total_warehouse_capacity = multiply(warehouses_needed, warehouse_capacity)
  let unused_capacity = subtract(total_warehouse_capacity, total_items)
  
  assert_eq(600, total_items)  // 5 * 120 = 600
  assert_eq(3, warehouses_needed)  // ceil(600/200) = 3
  assert_eq(600, total_warehouse_capacity)  // 3 * 200 = 600
  assert_eq(0, unused_capacity)  // 600 - 600 = 0
  
  // 员工排班系统测试
  let employees = 15
  let shifts_per_day = 3
  let days_per_week = 7
  let total_shifts = multiply(employees, multiply(shifts_per_day, days_per_week))
  
  let max_shifts_per_employee = 20
  let weeks_needed = divide_with_ceil(total_shifts, multiply(employees, max_shifts_per_employee))
  
  assert_eq(315, total_shifts)  // 15 * 3 * 7 = 315
  assert_eq(2, weeks_needed)  // ceil(315/(15*20)) = ceil(315/300) = 2
  
  // 财务计算测试
  let monthly_revenue = 50000
  let months = 12
  let annual_revenue = multiply(monthly_revenue, months)
  
  let expenses = 450000
  let profit = subtract(annual_revenue, expenses)
  let profit_margin = divide_with_ceil(multiply(profit, 100), annual_revenue)
  
  assert_eq(600000, annual_revenue)  // 50000 * 12 = 600000
  assert_eq(150000, profit)  // 600000 - 450000 = 150000
  assert_eq(25, profit_margin)  // ceil(150000*100/600000) = ceil(25) = 25
}