// 标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-3, 2))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  @azimuth.assert_eq(3, @azimuth.subtract(8, 5))
  @azimuth.assert_eq(-3, @azimuth.subtract(5, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "mathematical_properties" {
  // 测试基本数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(@azimuth.add(a, 0), a)
}

test "edge_cases_handling" {
  // 测试边界情况处理
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "combined_operations" {
  // 测试组合运算
  let x = 10
  let y = 3
  let z = 2
  
  // (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(result1, 26)
  
  // x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(result2, 32)
}