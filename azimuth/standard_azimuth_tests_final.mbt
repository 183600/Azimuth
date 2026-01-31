// 标准 Azimuth 测试用例 - 10个高质量测试用例
// 测试覆盖基本算术运算、字符串处理和边界情况

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(21, multiply(-3, -7))
}

test "basic_greet_functionality" {
  // 基本问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(1, divide_with_ceil(1, 4))
  assert_eq(0, divide_with_ceil(0, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_functionality" {
  // 减法功能测试
  assert_eq(-4, subtract(3, 7))
  assert_eq(0, subtract(7, 7))
  assert_eq(10, subtract(17, 7))
  assert_eq(-10, subtract(-3, 7))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  assert_eq(add(5, 8), add(8, 5))
  assert_eq(multiply(5, 8), multiply(8, 5))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base = 5
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(6, result)  // ceil(5*3/4) + 2 = 4 + 2 = 6
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq_string("Hello, 世界!", greet("世界"))
}