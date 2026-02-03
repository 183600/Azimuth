// Azimuth 库的标准 MoonBit 测试用例
// 测试核心功能：add, multiply, subtract, greet, divide_with_ceil

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "subtract_function_basic_cases" {
  // 测试减法函数的基本情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_scenario" {
  // 测试业务包装场景
  let total_items = 25
  let items_per_package = 6
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "complex_workflow_calculation" {
  // 测试复杂工作流计算
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}

test "zero_value_arithmetic_operations" {
  // 测试零值在算术运算中的行为
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(0, -3))
  
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, multiply(-50, 0))
  
  assert_eq(5, subtract(5, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(0, subtract(0, 0))
  
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "extreme_value_calculations" {
  // 测试极值计算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数加法
  assert_eq(max_int, add(max_int - 1, 1))
  assert_eq(min_int, add(min_int + 1, -1))
  
  // 测试大数乘法
  assert_eq(4611686014132420609, multiply(2147483647, 2147483647))
  
  // 测试极值除法
  assert_eq(1, divide_with_ceil(max_int, max_int))
  assert_eq(1, divide_with_ceil(min_int, min_int))
}

test "string_boundary_and_special_cases" {
  // 测试字符串边界和特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, emoji😊!", greet("emoji😊"))
  assert_eq_string("Hello, a_very_long_string_that_tests_length_handling!", greet("a_very_long_string_that_tests_length_handling"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(0, subtract(y, y))
  
  // 分配律测试
  assert_eq(multiply(x, add(y, 1)), add(multiply(x, y), multiply(x, 1)))
}

test "division_edge_cases_and_sign_behavior" {
  // 测试除法边界情况和符号行为
  // 正数除法
  assert_eq(2, divide_with_ceil(5, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  
  // 负数除法
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-2, divide_with_ceil(5, -2))
  assert_eq(3, divide_with_ceil(-5, -2))
  
  // 混合符号除法
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(1, -2))
  assert_eq(1, divide_with_ceil(-1, -2))
  
  // 零除数
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 场景：库存管理和订单处理
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 计算剩余空间
  let total_space = multiply(boxes_needed, box_capacity)
  let empty_space = subtract(total_space, total_products)
  assert_eq(11, empty_space)
  
  // 场景：预算分配
  let total_budget = 10000
  let departments = 3
  let base_budget = divide_with_ceil(total_budget, departments)
  let allocated_total = multiply(base_budget, departments)
  
  assert_eq(3334, base_budget)
  assert_eq(10002, allocated_total)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试空字符串和特殊输入
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  
  // 测试数值边界
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试大数组合运算
  let big_num1 = 1000000
  let big_num2 = 2000000
  let big_result = add(multiply(big_num1, 2), big_num2)
  assert_eq(4000000, big_result)
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  // 连续运算测试
  let result1 = add(add(add(1, 2), 3), 4)
  assert_eq(10, result1)
  
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
  
  // 嵌套运算测试
  let complex_result = add(multiply(10, 5), subtract(20, 8))
  assert_eq(62, complex_result)
  
  // 字符串处理压力测试
  let long_name = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let expected_greeting = "Hello, " + long_name + "!"
  assert_eq_string(expected_greeting, greet(long_name))
}