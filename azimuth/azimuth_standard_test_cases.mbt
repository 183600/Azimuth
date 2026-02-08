// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "azimuth_basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(15, multiply(-3, -5))
}

test "azimuth_basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(2, subtract(8, 6))
  assert_eq(-2, subtract(6, 8))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(5, -5))
}

test "azimuth_divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "azimuth_divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "azimuth_greet_function_standard" {
  // 问候函数标准测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 6
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "azimuth_business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 23
  let items_per_package = 5
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}