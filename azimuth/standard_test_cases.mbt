// 标准 MoonBit 测试用例 - 新增10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_positive_numbers" {
  // 测试加法函数的正数情况
  assert_eq(15, add(7, 8))
  assert_eq(100, add(50, 50))
  assert_eq(1000, add(999, 1))
}

test "multiply_function_zero_handling" {
  // 测试乘法函数的零处理
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(456, 0))
  assert_eq(0, multiply(-789, 0))
}

test "greet_function_special_chars" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @user123!", greet("@user123"))
  assert_eq_string("Hello, test_case!", greet("test_case"))
  assert_eq_string("Hello, CamelCase!", greet("CamelCase"))
  assert_eq_string("Hello, snake_case!", greet("snake_case"))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(5, divide_with_ceil(21, 5))
  assert_eq(7, divide_with_ceil(43, 7))
  assert_eq(10, divide_with_ceil(91, 10))
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(100, 100))
  assert_eq(200, subtract(100, -100))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 场景：计算项目成本
  let materials_cost = 5000
  let labor_cost = 3000
  let overhead_cost = 2000
  let total_cost = add(add(materials_cost, labor_cost), overhead_cost)
  
  // 分摊到12个月
  let monthly_cost = divide_with_ceil(total_cost, 12)
  assert_eq(834, monthly_cost)  // ceil(10000/12) = 834
  
  // 验证总成本
  let annual_total = multiply(monthly_cost, 12)
  assert_eq(10008, annual_total)
}

test "mathematical_commutative_extended" {
  // 扩展的数学交换律测试
  let a = 123
  let b = 456
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(579, add(a, b))
  assert_eq(56088, multiply(a, b))
}

test "resource_planning_test" {
  // 资源规划测试
  // 场景：计算服务器需求
  let expected_users = 10000
  let users_per_server = 1200
  let servers_needed = divide_with_ceil(expected_users, users_per_server)
  
  assert_eq(9, servers_needed)  // ceil(10000/1200) = 9
  
  // 计算总容量
  let total_capacity = multiply(servers_needed, users_per_server)
  assert_eq(10800, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, expected_users)
  assert_eq(800, remaining_capacity)
}

test "error_handling_division" {
  // 除法错误处理测试
  // 测试各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极小除数
  assert_eq(1000, divide_with_ceil(999, 1))
  assert_eq(-999, divide_with_ceil(-999, 1))
}

test "string_boundary_conditions" {
  // 字符串边界条件测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space !", greet("space "))
}