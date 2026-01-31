// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 8 个高质量测试用例，覆盖所有主要功能

test "basic_arithmetic_comprehensive" {

test "basic_arithmetic_comprehensive" {
  // 综合基础算术测试
  assert_eq(10, add(7, 3))
  assert_eq(-4, add(-7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_variations" {
  // 问候函数变化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
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
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_scenario" {
  // 业务物流场景测试
  let total_packages = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_packages, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 20
  let multiplier = 3
  let divisor = 8
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  // 验证中间步骤
  assert_eq(60, step1)  // 20 * 3 = 60
  assert_eq(8, step2)   // ceil(60/8) = 8
  assert_eq(15, result) // 8 + 7 = 15
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  let emergency_reserve = 1000
  
  // 计算可分配预算
  let allocatable_budget = subtract(total_budget, emergency_reserve)
  assert_eq(9000, allocatable_budget)
  
  // 计算每人最大分配
  let max_per_person = divide_with_ceil(allocatable_budget, team_members)
  assert_eq(1500, max_per_person)
  
  // 验证总分配不超过可分配预算
  let total_allocated = multiply(max_per_person, team_members)
  assert_eq(9000, total_allocated)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}