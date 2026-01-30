// 标准 MoonBit 测试用例 - 为 azimuth 库添加
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties_test" {
  // 乘法函数性质测试
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_edge_cases_test" {
  // 向上取整除法边缘情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-1, divide_with_ceil(-3, 4))
}

test "greet_function_various_inputs_test" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "subtract_function_basic_test" {
  // 减法函数基础测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_calculation_pipeline_test" {
  // 复杂计算流水线测试
  let base = 10
  let step1 = add(base, 5)        // 15
  let step2 = multiply(step1, 2)  // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 1)  // 3
  assert_eq(3, step4)
}

test "business_logic_packaging_test" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity = 12
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(4, boxes)
  
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(1, remaining)
}

test "mathematical_commutative_laws_test" {
  // 数学交换律测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}