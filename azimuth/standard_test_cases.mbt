// Azimuth 标准测试用例 - 10个高质量测试用例
// 覆盖核心功能：加法、乘法、减法、向上取整除法、字符串问候

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(7, 3))
  assert_eq(-2, subtract(5, 7))
  assert_eq(21, multiply(7, 3))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 15
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "division_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-8, subtract(-5, 3))
}

test "greeting_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let doubled = multiply(base, 2)
  let added = add(doubled, 5)
  let divided = divide_with_ceil(added, 3)
  assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(4, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 6*(4+3) = 6*4 + 6*3 = 42
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_budget = 10000
  let team_members = 6
  let budget_per_member = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, budget_per_member)
  
  let total_allocated = multiply(budget_per_member, team_members)
  assert_true(total_allocated >= total_budget)
}