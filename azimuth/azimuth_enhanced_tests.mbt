// Azimuth 增强测试用例 - 10个标准 MoonBit 测试用例

test "enhanced_add_function" {
  // 测试增强的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-5, -5))
  assert_eq(2, add(-5, 7))
}

test "enhanced_multiply_function" {
  // 测试增强的乘法功能
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "enhanced_greet_function" {
  // 测试增强的问候功能
  assert_eq_string("Hello, enhanced!", greet("enhanced"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "enhanced_divide_with_ceil" {
  // 测试增强的向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "enhanced_mathematical_properties" {
  // 测试增强的数学性质
  let x = 15
  let y = 8
  
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "enhanced_business_calculation" {
  // 测试增强的业务计算
  let items = 47
  let capacity = 12
  let boxes = divide_with_ceil(items, capacity)
  
  assert_eq(4, boxes)
  assert_eq(48, multiply(boxes, capacity))
}

test "enhanced_complex_operations" {
  // 测试增强的复杂操作
  let base = 10
  let result = divide_with_ceil(
    multiply(add(base, 5), 2),
    4
  )
  assert_eq(8, result)
}

test "enhanced_negative_operations" {
  // 测试增强的负数操作
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "enhanced_large_numbers" {
  // 测试增强的大数操作
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "enhanced_edge_cases" {
  // 测试增强的边缘情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
}