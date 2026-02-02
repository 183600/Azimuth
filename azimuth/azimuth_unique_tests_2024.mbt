// Azimuth 独特测试用例 2024
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_basic_operations" {
  // 基本加法运算测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_basic_operations" {
  // 基本乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 5))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界测试
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-8, 3))
}

test "greet_function_various" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_basic_operations" {
  // 基本减法运算测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
}

test "math_properties_commutative" {
  // 数学交换律性质测试
  let x = 12
  let y = 7
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "package_calculation_scenario" {
  // 包装计算场景测试
  let items = 35
  let capacity = 8
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
}