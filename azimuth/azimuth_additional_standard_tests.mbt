// Azimuth 额外标准测试用例 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-15, add(-7, -8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(20, multiply(-4, -5))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))  // ceil(2.5) = 3
  assert_eq(2, divide_with_ceil(8, 4))   // ceil(2.0) = 2
  assert_eq(0, divide_with_ceil(5, 0))   // 除零保护
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-2, divide_with_ceil(-10, 4))  // ceil(-2.5) = -2
  assert_eq(2, divide_with_ceil(-10, -4))  // ceil(2.5) = 2
  assert_eq(-3, divide_with_ceil(10, -4))  // ceil(-2.5) = -3
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
}

test "greet_function_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算项目资源需求
  let team_size = 12
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)  // 96
  let tasks_per_sprint = 25
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)  // ceil(96/25) = 4
  
  assert_eq(96, total_tasks)
  assert_eq(4, sprints_needed)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景测试
  let total_items = 158
  let items_per_box = 24
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)  // ceil(158/24) = 7
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, items_per_box)  // 7 * 24 = 168
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)  // 168 - 158 = 10
  assert_eq(10, remaining_space)
}