// Azimuth 增强测试套件
// 使用标准的 MoonBit 测试语法，覆盖更多边界情况和复杂场景

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_negative_numbers" {
  // 测试负数加法的各种情况
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
}

test "multiply_by_zero_and_one" {
  // 测试乘法中的0和1的特殊处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(1, -12345))
}

test "multiply_negative_one" {
  // 测试乘法中的-1特殊处理
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(-12345, -1))
  @azimuth.assert_eq(12345, @azimuth.multiply(-1, -12345))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 100
  let y = -50
  let z = 25
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(1250, product)
  
  // 计算 x * y + z
  let product2 = @azimuth.multiply(x, y)
  let result = @azimuth.add(product2, z)
  @azimuth.assert_eq(-4975, result)
  
  // 计算 (x - z) * (y + z)
  let diff = @azimuth.add(x, -z)
  let sum2 = @azimuth.add(y, z)
  let product3 = @azimuth.multiply(diff, sum2)
  @azimuth.assert_eq(-1875, product3)
}

test "extreme_overflow_scenarios" {
  // 测试极端溢出场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法极端情况
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  
  // 乘法极端情况
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "boolean_assertions" {
  // 测试布尔断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  @azimuth.assert_true(2147483647 == 2147483647)
  @azimuth.assert_true(-2147483648 == -2147483648)
  
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  @azimuth.assert_false(2147483647 == -2147483648)
  @azimuth.assert_false(-2147483648 == 2147483647)
}