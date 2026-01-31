// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(1000, add(500, 500))
}

test "multiplication_comprehensive" {
  // 全面乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 123))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_function_edge_cases" {
  // 减法函数边缘情况测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
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

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_items = 67
  let box_capacity = 15
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(75, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(8, remaining_space)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(2, divide_with_ceil(-10, -5))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial_value = 20
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((initial * multiplier) / divisor) + addition
  let step1 = multiply(initial_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, addition)
  
  assert_eq(22, final_result)  // ceil(20*3/4) + 7 = ceil(60/4) + 7 = 15 + 7 = 22
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 95
  let team_members = 8
  let min_resources_per_member = divide_with_ceil(total_resources, team_members)
  
  assert_eq(12, min_resources_per_member)
  
  // 验证总分配量
  let total_allocated = multiply(min_resources_per_member, team_members)
  assert_eq(96, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(99, utilization)  // floor(9500/96) = 98%
}