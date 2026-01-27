// 新增的10个标准 MoonBit 测试用例
test "add_function_comprehensive_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-50, -50))
  assert_eq(1000, add(999, 1))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_zero_handling" {
  // 测试乘法函数的零处理
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(-1000, 0))
  assert_eq(12345, multiply(12345, 1))
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "divide_with_ceil_complex_cases" {
  // 测试向上取整除法的复杂情况
  assert_eq(7, divide_with_ceil(40, 6))
  assert_eq(-6, divide_with_ceil(-35, 6))
  assert_eq(6, divide_with_ceil(-35, -6))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 23
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体值
  assert_eq(40, add(23, 17))
  assert_eq(391, multiply(23, 17))
}

test "subtraction_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(0, subtract(100, 100))
  assert_eq(-50, subtract(50, 100))
  assert_eq(100, subtract(0, -100))
  assert_eq(200, subtract(-100, -300))
  assert_eq(1, subtract(1000, 999))
}

test "business_inventory_calculation" {
  // 业务场景：库存计算测试
  let products = 158
  let pallet_capacity = 25
  let pallets_needed = divide_with_ceil(products, pallet_capacity)
  
  assert_eq(7, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  assert_eq(175, total_capacity)
  
  // 验证剩余容量
  let remaining = subtract(total_capacity, products)
  assert_eq(17, remaining)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 20
  let increment = 8
  let multiplier = 3
  let divisor = 7
  
  // 计算流程
  let step1 = add(base_value, increment)        // 28
  let step2 = multiply(step1, multiplier)       // 84
  let step3 = divide_with_ceil(step2, divisor)  // 12
  let final_result = subtract(step3, 2)         // 10
  
  assert_eq(10, final_result)
}

test "error_handling_division_by_zero" {
  // 错误处理：除零情况测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  assert_eq(0, divide_with_ceil(-1000000, 0))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 500
  let teams = 6
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(84, resources_per_team)  // ceil(500/6) = 84
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(504, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(99, utilization)  // floor(50000/504) = 99%
}