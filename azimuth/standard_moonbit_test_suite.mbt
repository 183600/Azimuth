// 标准 MoonBit 测试套件
// 包含8个高质量测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(1, divide_with_ceil(4, 4))
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
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, subtract(5, 0))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
}

test "string_processing_and_greeting" {
  // 字符串处理和问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
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
  let base_value = 15
  let multiplier = 4
  let divisor = 6
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(17, result)  // ceil(15*4/6) + 7 = 10 + 7 = 17
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, subtract(1000000, 1000000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}