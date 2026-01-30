// Azimuth 项目标准测试用例
// 包含 10 个高质量的 MoonBit 测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边缘情况测试
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(5, -3))
  assert_eq(-2, subtract(-5, -3))
}

test "business_calculation_scenario" {
  // 业务计算场景测试：装盒问题
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  
  // 计算流程：((base + 5) * 3) / 8 + 2
  let step1 = add(base_value, 5)
  let step2 = multiply(step1, 3)
  let step3 = divide_with_ceil(step2, 8)
  let result = add(step3, 2)
  
  assert_eq(12, result)  // ceil((20+5)*3/8) + 2 = ceil(75/8) + 2 = 10 + 2 = 12
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let team_members = 3
  let min_allocation = divide_with_ceil(total_budget, team_members)
  
  assert_eq(1667, min_allocation)
  
  // 验证总分配
  let total_allocated = multiply(min_allocation, team_members)
  assert_eq(5001, total_allocated)
  
  // 验证剩余预算
  let remaining = subtract(total_allocated, total_budget)
  assert_eq(1, remaining)
}

test "large_number_operations" {
  // 大数运算测试
  let large_num = 100000
  
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(100001, divide_with_ceil(add(large_num, 1), 1))
  assert_eq(10000, divide_with_ceil(large_num, 10))
  
  // 验证大数减法
  assert_eq(99999, subtract(large_num, 1))
  assert_eq(0, subtract(large_num, large_num))
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 零运算测试
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, add(0, 12345))
  assert_eq(12345, add(12345, 0))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
}