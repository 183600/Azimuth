// 标准 MoonBit 测试套件 - 8个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(5, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 4)),
    add(multiply(x, y), multiply(x, 4))
  )
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_packaging_logic" {
  // 业务包装逻辑测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = multiply(base, 3)
  let step2 = add(step1, 5)
  let step3 = divide_with_ceil(step2, 4)
  let result = add(step3, 2)
  
  assert_eq(12, result)  // ceil((10*3+5)/4) + 2 = ceil(35/4) + 2 = 9 + 2 = 11
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, resources_per_team)
  
  // 验证分配结果
  let total_allocated = multiply(resources_per_team, teams)
  assert_eq(102, total_allocated)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), total_allocated)
  assert_eq(98, utilization)
}