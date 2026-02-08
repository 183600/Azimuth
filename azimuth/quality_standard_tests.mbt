// Azimuth 项目质量标准测试用例
// 包含10个高质量的 MoonBit 测试用例，使用标准测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(7, 13))
  assert_eq(91, multiply(7, 13))
}

test "boundary_conditions" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(2, divide_with_ceil(-8, -3))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算
  assert_eq(0, multiply(1000000, 0))
  assert_eq(1000000, add(1000000, 0))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(5, remaining_space)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1001000, 1000))
}

test "azimuth_enhanced_quality_test" {
  // Azimuth 增强质量测试 - 新增的测试用例
  // 测试加法函数的多种情况
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000, add(500, 500))
  
  // 测试乘法函数的边界情况
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-30, multiply(-6, 5))
  assert_eq(30, multiply(-6, -5))
  assert_eq(10000, multiply(100, 100))
  
  // 测试向上取整除法的综合情况
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(1, divide_with_ceil(1, 10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(8, 0))
  
  // 测试向上取整除法的负数情况
  assert_eq(-2, divide_with_ceil(-8, 4))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "azimuth_string_and_math_integration" {
  // 字符串和数学运算集成测试
  let greeting_result = greet("MoonBit")
  assert_eq_string("Hello, MoonBit!", greeting_result)
  
  // 使用数学运算计算字符串长度相关的值
  let base_length = 5  // "Hello" 的长度
  let name_length = 7  // "MoonBit" 的长度
  let total_length = add(base_length, add(name_length, 2))  // 2 for ", "
  assert_eq(14, total_length)
  
  // 使用除法计算平均长度
  let avg_length = divide_with_ceil(total_length, 2)
  assert_eq(7, avg_length)
}