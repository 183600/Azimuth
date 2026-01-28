// 标准 MoonBit 测试用例 - azimuth 库
// 包含10个高质量测试用例，覆盖核心功能和实际应用场景

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiplication_properties" {
  // 测试乘法性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "subtraction_basic_cases" {
  // 测试基本减法情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(10, -5))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_inventory_calculation" {
  // 业务场景：库存计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线
  let base = 10
  let step1 = add(base, multiply(2, 3))  // 10 + 6 = 16
  let step2 = divide_with_ceil(step1, 5)  // ceil(16/5) = 4
  let result = multiply(step2, subtract(8, 3))  // 4 * 5 = 20
  assert_eq(20, result)
}

test "error_handling_boundaries" {
  // 错误处理和边界测试
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 5000
  let team_members = 3
  let budget_per_member = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, budget_per_member)
  
  let total_allocated = multiply(budget_per_member, team_members)
  assert_eq(5001, total_allocated)
  
  let remaining_budget = subtract(total_allocated, total_budget)
  assert_eq(1, remaining_budget)
}