// 标准 MoonBit 测试用例 - 为 Azimuth 项目创建
// 包含10个高质量测试用例，覆盖主要功能

test "add_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "multiply_commutative_property" {
  // 测试乘法函数的交换律性质
  assert_eq(6, multiply(2, 3))
  assert_eq(6, multiply(3, 2))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_zero_divisor" {
  // 测试向上取整除法的除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = divide_with_ceil(add(10, 5), 4)
  assert_eq(4, result2)  // ceil(15/4) = 4
}

test "edge_cases_and_boundaries" {
  // 测试边界情况和极值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}