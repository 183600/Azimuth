// 额外的综合测试用例 - 针对 azimuth 库的补充测试
// 包含边界情况、特殊场景和实际应用场景的测试

test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(0, 1000))  // 零除以正数
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
}

test "financial_calculation_scenario" {
  // 金融计算场景：复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利计算）
  assert_eq(1158, amount_after_year3)  // 1000 -> 1050 -> 1103 -> 1158
}

test "inventory_management_calculation" {
  // 库存管理计算
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  assert_eq(5, boxes_small)   // ceil(100/24) = 5
  assert_eq(11, boxes_medium) // ceil(250/24) = 11
  assert_eq(42, boxes_large)  // ceil(1000/24) = 42
  assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000, multiply(1000, 1000))
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, John Doe!", greeting)
  
  // 测试特殊字符处理
  let special_name = "Alice & Bob"
  let special_greeting = greet(special_name)
  assert_eq_string("Hello, Alice & Bob!", special_greeting)
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 验证分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 验证结合律：(a + b) + c = a + (b + c)
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
}