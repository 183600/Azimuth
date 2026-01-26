// Azimuth 标准单元测试 - 新增10个高质量测试用例
// 覆盖核心功能、边界情况和实际应用场景

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "divide_with_ceil_negative_scenarios" {
  // 测试向上取整除法的负数场景
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-3, -3))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  assert_eq_string("Hello, space!", greet("space"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, special@#$!", greet("special@#$"))
  assert_eq_string("Hello, mixedCase!", greet("mixedCase"))
  assert_eq_string("Hello, emoji🎉!", greet("emoji🎉"))
}

test "resource_allocation_practical" {
  // 实际资源分配场景测试
  let total_items = 127
  let container_capacity = 15
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(9, containers_needed)  // ceil(127/15) = 9
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(8, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base_value = 20
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 计算流程: ((base * multiplier) / divisor) + offset
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(22, result)  // ceil(20*3/4) + 7 = 15 + 7 = 22
}

test "mathematical_identity_elements" {
  // 数学单位元素测试
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let total_minutes = 487
  let minutes_per_hour = 60
  let hours_needed = divide_with_ceil(total_minutes, minutes_per_hour)
  
  assert_eq(9, hours_needed)  // ceil(487/60) = 9
  
  // 计算剩余分钟数
  let total_hours_in_minutes = multiply(hours_needed, minutes_per_hour)
  let remaining_minutes = subtract(total_hours_in_minutes, total_minutes)
  assert_eq(53, remaining_minutes)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_quantities = add(25, multiply(3, 15))  // 25 + 3*15 = 70
  let package_size = 12
  let packages_needed = divide_with_ceil(product_quantities, package_size)
  
  assert_eq(6, packages_needed)  // ceil(70/12) = 6
  
  // 验证总包装容量
  let total_package_capacity = multiply(packages_needed, package_size)
  assert_eq(72, total_package_capacity)
  
  // 验证利用率
  let utilization_rate = divide_with_ceil(
    multiply(product_quantities, 100), 
    total_package_capacity
  )
  assert_eq(98, utilization_rate)  // ceil(70*100/72) = 98%
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 8
  
  // 逐步验证操作链
  assert_eq(13, add(initial, 5))
  assert_eq(26, multiply(13, 2))
  assert_eq(4, divide_with_ceil(26, 7))
  assert_eq(7, add(4, 3))
  assert_eq(14, multiply(7, 2))
}