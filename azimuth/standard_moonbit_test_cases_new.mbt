// 标准 MoonBit 测试用例 - 为 Azimuth 项目新增
// 包含10个高质量测试用例，覆盖各种功能和边缘情况

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-10, 14))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, émoji 🚀!", greet("émoji 🚀"))
  assert_eq_string("Hello, space !", greet("space "))
  assert_eq_string("Hello,  special!", greet(" special"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
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

test "business_packaging_scenarios" {
  // 业务包装场景测试
  // 场景1：商品装箱
  let products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(products, box_capacity)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, products)
  assert_eq(1, remaining_space)
  
  // 场景2：运输车辆安排
  let packages = 156
  let truck_capacity = 25
  let trucks_needed = divide_with_ceil(packages, truck_capacity)
  assert_eq(7, trucks_needed)
  
  let total_truck_capacity = multiply(trucks_needed, truck_capacity)
  let unused_capacity = subtract(total_truck_capacity, packages)
  assert_eq(19, unused_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 单位值测试
  assert_eq(12345, add(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(1, divide_with_ceil(12345, 12345))
  
  // 大数测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(100001, divide_with_ceil(100001, 1))
}

test "complex_calculation_workflows" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(4, sprints_needed)
  
  let total_sprint_capacity = multiply(sprints_needed, sprint_capacity)
  let buffer_capacity = subtract(total_sprint_capacity, total_tasks)
  assert_eq(12, buffer_capacity)
  
  // 场景：预算分配
  let total_budget = 50000
  let departments = 3
  let base_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(16667, base_allocation)
  
  let total_allocated = multiply(base_allocation, departments)
  let remaining_budget = subtract(total_allocated, total_budget)
  assert_eq(1, remaining_budget)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算
  let max_test = 1000000
  let min_test = -1000000
  
  assert_eq(0, multiply(max_test, 0))
  assert_eq(0, multiply(min_test, 0))
  assert_eq(max_test, add(max_test, 0))
  assert_eq(min_test, add(min_test, 0))
  assert_eq(0, subtract(max_test, max_test))
  assert_eq(0, subtract(min_test, min_test))
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, émoji 🎉!", greet("émoji 🎉"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
}

test "performance_and_efficiency_validation" {
  // 性能和效率验证测试
  // 批量计算场景
  let batch_size = 1000
  let batches = 5
  let total_items = multiply(batch_size, batches)
  assert_eq(5000, total_items)
  
  let processing_capacity = 1200
  let processing_rounds = divide_with_ceil(total_items, processing_capacity)
  assert_eq(5, processing_rounds)
  
  let total_processing_capacity = multiply(processing_rounds, processing_capacity)
  let wasted_capacity = subtract(total_processing_capacity, total_items)
  assert_eq(1000, wasted_capacity)
  
  // 效率计算
  let efficiency_percentage = divide_with_ceil(
    multiply(total_items, 100), 
    total_processing_capacity
  )
  assert_eq(83, efficiency_percentage)  // floor(500000/6000) = 83%
}