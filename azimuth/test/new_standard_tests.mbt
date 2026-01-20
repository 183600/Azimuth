test "add_basic_operations" {
  // 基本加法运算测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
}

test "multiply_basic_operations" {
  // 基本乘法运算测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "greet_basic_functionality" {
  // 基本字符串拼接测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-6, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}

test "combined_mathematical_operations" {
  // 组合运算测试
  let a = 10
  let b = 3
  
  // 加法与乘法组合
  @azimuth.assert_eq(39, @azimuth.multiply(@azimuth.add(a, b), 3))
  @azimuth.assert_eq(33, @azimuth.add(@azimuth.multiply(a, b), 3))
  
  // 复杂表达式
  @azimuth.assert_eq(16, @azimuth.add(@azimuth.multiply(2, 5), @azimuth.divide_with_ceil(10, 3)))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(2, 3), 1), 2))
}

test "mathematical_properties_verification" {
  // 数学性质测试
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-5, 3), @azimuth.add(3, -5))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(5, 3), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(@azimuth.multiply(-5, 3), @azimuth.multiply(3, -5))
  
  // 分配律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(2, @azimuth.add(3, 4)), @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(2, 4)))
}

test "boundary_value_tests" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 测试1和-1的乘法
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "unicode_and_special_characters" {
  // 特殊字符和Unicode测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
}