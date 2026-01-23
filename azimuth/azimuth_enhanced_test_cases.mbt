// Azimuth 增强测试用例
// 包含8个高质量的 MoonBit 标准测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(-1, add(7, -8))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数的各种情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(-56, multiply(7, -8))
  assert_eq(56, multiply(-7, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边缘情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(7, 4))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_calculation" {
  // 复杂业务计算场景测试
  // 场景：计算项目资源需求
  let team_members = 12
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(96, total_tasks)
  
  let tasks_per_sprint = 20
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(5, sprints_needed)  // ceil(96/20) = 5
  
  let buffer_tasks = add(total_tasks, multiply(sprints_needed, 4))
  assert_eq(116, buffer_tasks)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：服务器资源分配
  let total_resources = 10000
  let departments = [3, 5, 7, 4]
  
  let dept1_resources = divide_with_ceil(multiply(total_resources, departments[0]), 20)
  let dept2_resources = divide_with_ceil(multiply(total_resources, departments[1]), 20)
  let dept3_resources = divide_with_ceil(multiply(total_resources, departments[2]), 20)
  let dept4_resources = divide_with_ceil(multiply(total_resources, departments[3]), 20)
  
  assert_eq(1500, dept1_resources)  // ceil(10000*3/20)
  assert_eq(2500, dept2_resources)  // ceil(10000*5/20)
  assert_eq(3500, dept3_resources)  // ceil(10000*7/20)
  assert_eq(2000, dept4_resources)  // ceil(10000*4/20)
  
  let total_allocated = add(add(dept1_resources, dept2_resources), add(dept3_resources, dept4_resources))
  assert_eq(9500, total_allocated)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误的多种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "real_world_logistics_scenario" {
  // 真实世界物流场景测试
  // 场景：计算运输需求
  let products = [
    (120, 25),  // (数量, 每箱容量)
    (85, 20),
    (200, 30),
    (45, 15)
  ]
  
  let boxes1 = divide_with_ceil(products[0].0, products[0].1)  // ceil(120/25) = 5
  let boxes2 = divide_with_ceil(products[1].0, products[1].1)  // ceil(85/20) = 5
  let boxes3 = divide_with_ceil(products[2].0, products[2].1)  // ceil(200/30) = 7
  let boxes4 = divide_with_ceil(products[3].0, products[3].1)  // ceil(45/15) = 3
  
  let total_boxes = add(add(boxes1, boxes2), add(boxes3, boxes4))
  assert_eq(20, total_boxes)
  
  let total_capacity = add(
    multiply(boxes1, products[0].1),
    add(
      multiply(boxes2, products[1].1),
      add(multiply(boxes3, products[2].1), multiply(boxes4, products[3].1))
    )
  )
  assert_eq(530, total_capacity)
  
  let利用率 = divide_with_ceil(multiply(450, 100), total_capacity)
  assert_eq(85, 利用率)  // ceil(450*100/530) ≈ 85%
}