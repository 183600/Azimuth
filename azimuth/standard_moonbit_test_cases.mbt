// 标准 MoonBit 测试用例 - 为 azimuth 库添加的10个高质量测试用例
// 这些测试用例涵盖了库的主要功能和各种使用场景

test "add_function_positive_numbers" {
  // 测试加法函数的正数运算
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(1, multiply(1, 15))
  assert_eq(0, multiply(0, 25))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
}

test "greet_function_various_names" {
  // 测试问候函数的各种名称输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob123!", greet("Bob123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(3, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合使用
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(26, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(32, result2)
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 47
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(8, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "string_and_number_integration" {
  // 字符串和数字的集成测试
  let user_name = "MoonBit"
  let user_id = 2026
  let greeting = greet(user_name)
  
  assert_eq_string("Hello, MoonBit!", greeting)
  
  // 验证用户ID的数学运算
  let next_year_id = add(user_id, 1)
  assert_eq(2027, next_year_id)
  
  let double_id = multiply(user_id, 2)
  assert_eq(4052, double_id)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let days_per_task = 2
  let total_days = multiply(total_tasks, days_per_task)
  
  let weeks_needed = divide_with_ceil(total_days, 7)
  
  // 验证计算结果
  assert_eq(40, total_tasks)  // 8 * 5 = 40
  assert_eq(80, total_days)   // 40 * 2 = 80
  assert_eq(12, weeks_needed) // ceil(80/7) = 12
}