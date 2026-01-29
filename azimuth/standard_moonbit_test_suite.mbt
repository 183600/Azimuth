// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-5, 15))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_function_edge_cases" {
  // 测试向上取整除法函数的边缘情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtract_function_validations" {
  // 测试减法函数的各种情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 验证数学性质和恒等式
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
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
  let base_value = 10
  
  // 计算流程：((base * 3) / 4) + 5
  let step1 = multiply(base_value, 3)
  let step2 = divide_with_ceil(step1, 4)
  let result = add(step2, 5)
  
  assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数运算验证
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}