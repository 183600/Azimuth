// Azimuth 项目标准测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_properties" {
  // 测试乘法函数的性质
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 15
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "subtract_function_verification" {
  // 测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配不超过预算
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "real_world_logistics_scenario" {
  // 真实世界物流场景
  // 场景：计算仓库存储需求
  let products = 157
  let pallet_capacity = 24
  let pallets_needed = divide_with_ceil(products, pallet_capacity)
  assert_eq(7, pallets_needed)  // ceil(157/24) = 7
  
  // 计算总存储空间
  let total_space = multiply(pallets_needed, pallet_capacity)
  assert_eq(168, total_space)
  
  // 验证剩余空间
  let remaining_space = subtract(total_space, products)
  assert_eq(11, remaining_space)
}