// Azimuth 额外标准测试用例
// 包含10个高质量的 MoonBit 标准测试用例

test "add_function_basic_scenarios" {
  // 测试加法函数的基本场景
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-8, add(-3, -5))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_character_support" {
  // 测试问候函数的字符支持
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 空字符串!", greet(""))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 空格 !", greet("空格 "))
}

test "divide_with_ceil_practical_scenarios" {
  // 测试向上取整除法的实际场景
  // 场景：计算需要的房间数量
  let guests = 23
  let capacity_per_room = 4
  let rooms_needed = divide_with_ceil(guests, capacity_per_room)
  assert_eq(6, rooms_needed)
  
  // 验证总容量
  let total_capacity = multiply(rooms_needed, capacity_per_room)
  assert_eq(24, total_capacity)
}

test "mathematical_commutativity_check" {
  // 检查数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目预算分配
  let total_budget = 50000
  let departments = 3
  let base_allocation = divide_with_ceil(total_budget, departments)
  
  assert_eq(16667, base_allocation)  // ceil(50000/3)
  
  // 计算剩余预算
  let allocated_total = multiply(base_allocation, departments)
  let remaining = subtract(allocated_total, total_budget)
  
  assert_eq(50001, allocated_total)
  assert_eq(1, remaining)
}

test "error_handling_division_by_zero" {
  // 除零错误处理测试
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1000, 0))
  assert_eq(0, divide_with_ceil(-1000, 0))
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  // 场景：计算运输需求
  let packages = 45
  let truck_capacity = 10
  let trucks_needed = divide_with_ceil(packages, truck_capacity)
  
  assert_eq(5, trucks_needed)
  
  // 计算实际利用的容量
  let used_capacity = multiply(trucks_needed - 1, truck_capacity)
  let final_truck_load = subtract(packages, used_capacity)
  
  assert_eq(40, used_capacity)
  assert_eq(5, final_truck_load)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试临界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(100, 100))
  
  // 测试接近边界的情况
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(99, 50))
  assert_eq(11, divide_with_ceil(101, 10))
}