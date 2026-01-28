// Azimuth 高质量测试套件
// 包含8个标准MoonBit测试用例，覆盖各种测试场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(2, subtract(10, 8))
}

test "boundary_conditions_test" {
  // 边界条件测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, subtract(0, 1))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(5, 5))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 减法的逆运算性质
  assert_eq(x, add(subtract(x, y), y))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 7
  let addition = 8
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(15*3/7) + 8 = 7 + 8 = 15
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 25000
  let departments = 8
  
  // 使用向上取整确保覆盖所有部门
  let budget_per_department = divide_with_ceil(total_budget, departments)
  assert_eq(3125, budget_per_department)  // ceil(25000/8)
  
  // 验证总分配
  let total_allocated = multiply(budget_per_department, departments)
  assert_eq(25000, total_allocated)
  
  // 验证剩余预算
  let remaining_budget = subtract(total_allocated, total_budget)
  assert_eq(0, remaining_budget)
}