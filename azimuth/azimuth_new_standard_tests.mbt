// 新增的 MoonBit 标准测试用例 - 为 Azimuth 项目
// 包含 10 个高质量的测试用例，测试核心功能和边缘情况

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(-10, add(-3, -7))
  assert_eq(1000, add(500, 500))
  assert_eq(0, add(0, 0))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(21, multiply(3, 7))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(5, 0))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(7, divide_with_ceil(40, 6))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(15, subtract(10, -5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_properties_validation" {
  // 验证数学性质
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
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算场景
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let addition = 3
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证资源利用率
  assert_true(allocated_total >= total_resources)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}