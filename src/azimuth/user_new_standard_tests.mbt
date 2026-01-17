// 用户新增标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 测试加法函数的综合功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -100))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(100, product)
  
  // 计算 a * b + c
  let product2 = @azimuth.multiply(a, b)
  let result = @azimuth.add(product2, c)
  @azimuth.assert_eq(101, result)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "assert_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "real_world_calculation_scenario" {
  // 测试实际应用计算场景：计算矩形的面积和周长
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(length, 2), @azimuth.multiply(width, 2))
  @azimuth.assert_eq(46, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let long_string = "ThisIsAVeryLongStringThatContainsManyCharactersAndShouldStillWorkCorrectly"
  let result = @azimuth.greet(long_string)
  @azimuth.assert_eq_string("Hello, " + long_string + "!", result)
  
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", result2)
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "mathematical_property_validation" {
  // 测试数学性质验证
  let x = 13
  let y = 7
  let z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
}