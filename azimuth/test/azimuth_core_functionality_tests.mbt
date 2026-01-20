// Azimuth 核心功能测试
// 测试基本数学运算和字符串处理

test "add_basic_operations" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_operations" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
}

test "assert_helper_functions" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "combined_math_operations" {
  let x = 10
  let y = 5
  
  // (x + y) * 2
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(30, result)
  
  // x * y + 10
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, 10)
  @azimuth.assert_eq(60, result2)
}

test "boundary_value_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_property_verification" {
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
}

test "error_handling_and_edge_cases" {
  let large_num = 1000000
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_num, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(large_num, 0))
  
  // 向上取整除法边界
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(large_num, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num, large_num))
}