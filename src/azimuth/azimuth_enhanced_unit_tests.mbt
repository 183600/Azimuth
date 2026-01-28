// Azimuth 增强单元测试 - 8个高质量测试用例
// 使用标准 MoonBit 测试语法

test "subtract_function_basic_operations" {
  // 测试减法函数的基本操作
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_zero_scenarios" {
  // 测试乘法函数的零值场景
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, -12345))
  assert_eq(0, multiply(-12345, 0))
}

test "greet_function_empty_and_special_inputs" {
  // 测试问候函数的空值和特殊输入
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(0, divide_with_ceil(5, -10))
}

test "complex_calculation_business_scenario" {
  // 测试复合运算的业务场景
  // 场景：计算项目成本分配
  let base_cost = 1000
  let tax_rate = 20
  let team_members = 8
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(base_cost, tax_rate), 100)
  
  // 计算总成本
  let total_cost = add(base_cost, tax_amount)
  
  // 计算每人分摊成本
  let cost_per_person = divide_with_ceil(total_cost, team_members)
  
  assert_eq(200, tax_amount)  // ceil(1000*20/100) = 200
  assert_eq(1200, total_cost)  // 1000 + 200 = 1200
  assert_eq(150, cost_per_person)  // ceil(1200/8) = 150
}

test "error_handling_edge_cases" {
  // 测试错误处理的边缘情况
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "resource_allocation_practical_test" {
  // 测试资源分配的实际应用
  // 场景：服务器资源分配
  let total_memory = 32768  // GB
  let servers = 12
  let min_memory_per_server = 2048  // GB
  
  // 计算每台服务器的基本内存分配
  let base_allocation = divide_with_ceil(total_memory, servers)
  
  // 计算是否满足最低要求
  let meets_minimum = base_allocation >= min_memory_per_server
  
  // 计算剩余内存
  let total_allocated = multiply(base_allocation, servers)
  let remaining_memory = subtract(total_allocated, total_memory)
  
  assert_eq(2731, base_allocation)  // ceil(32768/12) = 2731
  assert_true(meets_minimum)  // 2731 >= 2048
  assert_eq(124, remaining_memory)  // 2731*12 - 32768 = 124
}