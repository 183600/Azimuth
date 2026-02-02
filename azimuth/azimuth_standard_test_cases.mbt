// Azimuth 项目标准测试用例
// 包含10个高质量 MoonBit 测试用例，覆盖各种功能场景

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-5, 6))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(123, 0))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(100000000, multiply(10000, 10000))
}

test "divide_with_ceil_business_logic" {
  // 向上取整除法业务逻辑测试
  // 场景：计算装运所需的集装箱数量
  let total_packages = 47
  let packages_per_container = 10
  let containers_needed = divide_with_ceil(total_packages, packages_per_container)
  
  assert_eq(5, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, packages_per_container)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(3, remaining_space)
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "subtract_function_validation" {
  // 减法函数验证测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证最后一轮的工作量
  let full_sprints_work = multiply(sprints_needed - 1, sprint_capacity)
  let final_sprint_work = subtract(total_tasks, full_sprints_work)
  assert_eq(8, final_sprint_work)  // 48 - 2*20 = 8
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律验证
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律验证
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式验证
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式验证
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(1, divide_with_ceil(large_num, large_num))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let product_quantity = 235
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(product_quantity, box_capacity)
  
  assert_eq(10, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(240, total_capacity)
  
  // 计算利用率
  let utilization_percentage = divide_with_ceil(
    multiply(product_quantity, 100), 
    total_capacity
  )
  assert_eq(98, utilization_percentage)  // floor(23500/240) = 97%，ceil为98%
  
  // 计算剩余空间
  let unused_space = subtract(total_capacity, product_quantity)
  assert_eq(5, unused_space)
}

test "algorithm_performance_validation" {
  // 算法性能验证测试
  // 测试不同规模数据的计算一致性
  let small_input = 10
  let medium_input = 100
  let large_input = 1000
  
  // 验证不同规模下的计算逻辑一致性
  let small_result = divide_with_ceil(add(small_input, 5), 3)
  let medium_result = divide_with_ceil(add(medium_input, 50), 30)
  let large_result = divide_with_ceil(add(large_input, 500), 300)
  
  assert_eq(5, small_result)    // ceil(15/3) = 5
  assert_eq(5, medium_result)   // ceil(150/30) = 5
  assert_eq(5, large_result)    // ceil(1500/300) = 5
  
  // 验证比例关系
  let ratio_test = multiply(large_input, 2)
  assert_eq(2000, ratio_test)
  
  // 验证复杂计算链
  let complex_calculation = divide_with_ceil(
    multiply(
      add(large_input, medium_input),
      subtract(small_input, 3)
    ),
    add(medium_input, large_input)
  )
  assert_eq(7, complex_calculation)  // ceil((1100*7)/1100) = 7
}