// Azimuth 标准测试用例
// 包含 8 个高质量的 MoonBit 测试用例

test "add_function_basic_tests" {
  // 加法函数基本测试
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(60, 40))
}

test "multiply_function_basic_tests" {
  // 乘法函数基本测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_functionality_tests" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_tests" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_tests" {
  // 问候函数测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 2024!", greet("2024"))
}

test "subtract_function_tests" {
  // 减法函数测试
  assert_eq(3, subtract(8, 5))
  assert_eq(-3, subtract(5, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
}

test "mathematical_commutative_tests" {
  // 数学交换律测试
  let x = 9
  let y = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_packaging_calculation" {
  // 业务包装计算测试
  let products = 28
  let box_capacity = 7
  let boxes_needed = divide_with_ceil(products, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(28, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, products)
  assert_eq(0, remaining_space)
}