// 标准 Azimuth 测试用例
// 包含8个高质量的 MoonBit 测试用例，覆盖核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-10, 15))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 100))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
}

test "arithmetic_subtraction" {
  // 测试减法运算
  assert_eq(5, subtract(12, 7))
  assert_eq(0, subtract(10, 10))
  assert_eq(-5, subtract(3, 8))
  assert_eq(20, subtract(-10, -30))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(7, 13), multiply(13, 7))
}

test "business_logic_packaging_calculation" {
  // 测试业务逻辑：包装计算
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, items)
  assert_eq(5, remaining_space)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base_value = 10
  let step1 = add(base_value, multiply(2, 3))
  let step2 = divide_with_ceil(step1, 4)
  let step3 = multiply(step2, add(1, 2))
  let result = divide_with_ceil(step3, 5)
  assert_eq(2, result)
}