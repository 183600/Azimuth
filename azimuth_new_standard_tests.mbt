// Azimuth 项目新增标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 测试覆盖：基本运算、字符串处理、数学函数、边界条件

test "arithmetic_addition_comprehensive" {
  // 综合加法测试
  assert_eq(0, 0 + 0)
  assert_eq(10, 7 + 3)
  assert_eq(-5, -8 + 3)
  assert_eq(100, 50 + 50)
  assert_eq(0, -25 + 25)
}

test "arithmetic_multiplication_edge_cases" {
  // 乘法边界情况测试
  assert_eq(0, 0 * 42)
  assert_eq(42, 1 * 42)
  assert_eq(-42, -1 * 42)
  assert_eq(144, 12 * 12)
  assert_eq(-144, -12 * 12)
}

test "subtract_function_properties" {
  // 减法函数性质测试
  assert_eq(5, 10 - 5)
  assert_eq(-5, 5 - 10)
  assert_eq(0, 15 - 15)
  assert_eq(20, 10 - (-10))
  assert_eq(-20, (-10) - 10)
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq("Hello, World!", azimuth::greet("World"))
  assert_eq("Hello, !", azimuth::greet(""))
  assert_eq("Hello, 123!", azimuth::greet("123"))
  assert_eq("Hello, 测试!", azimuth::greet("测试"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, azimuth::divide_with_ceil(10, 3))
  assert_eq(3, azimuth::divide_with_ceil(9, 3))
  assert_eq(1, azimuth::divide_with_ceil(1, 5))
  assert_eq(0, azimuth::divide_with_ceil(0, 10))
  assert_eq(0, azimuth::divide_with_ceil(10, 0))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let a = 6
  let b = 8
  let c = 10
  
  // 验证勾股定理
  let pythagoras = a * a + b * b
  assert_eq(100, pythagoras)
  assert_eq(c * c, pythagoras)
  
  // 验证分配律
  assert_eq(48, a * (b + c))
  assert_eq(a * b + a * c, a * (b + c))
}

test "string_operations_and_validation" {
  // 字符串操作和验证测试
  let first = "Hello"
  let second = "MoonBit"
  let space = " "
  
  // 字符串连接
  let combined = first ++ space ++ second
  assert_eq("Hello MoonBit", combined)
  assert_eq(11, combined.length())
  
  // 字符串比较
  assert_eq(true, combined == "Hello MoonBit")
  assert_eq(false, combined == "hello moonbit")
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  let small = 1
  let large = 1000
  let negative = -500
  
  // 极值运算
  assert_eq(1001, large + small)
  assert_eq(999, large - small)
  assert_eq(1000, large * small)
  assert_eq(-501, negative + small)
  assert_eq(-501, negative - small)
  assert_eq(-500, negative * small)
  
  // 边界除法
  assert_eq(1000, azimuth::divide_with_ceil(large, 1))
  assert_eq(1, azimuth::divide_with_ceil(small, 1))
  assert_eq(0, azimuth::divide_with_ceil(0, large))
}