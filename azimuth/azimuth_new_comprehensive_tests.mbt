// Azimuth 项目的新增测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 25), @azimuth.add(25, -10))
  @azimuth.assert_eq(@azimuth.add(-100, -200), @azimuth.add(-200, -100))
  @azimuth.assert_eq(@azimuth.add(0, 12345), @azimuth.add(12345, 0))
}

test "multiply_function_commutativity" {
  // 测试乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-4, 9), @azimuth.multiply(9, -4))
  @azimuth.assert_eq(@azimuth.multiply(-8, -3), @azimuth.multiply(-3, -8))
  @azimuth.assert_eq(@azimuth.multiply(0, 999), @azimuth.multiply(999, 0))
}

test "add_function_associativity" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
  
  // 使用负数测试
  let x = -5
  let y = 15
  let z = -10
  
  let left2 = @azimuth.add(@azimuth.add(x, y), z)
  let right2 = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left2, right2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 8
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "greet_function_with_special_characters" {
  // 测试问候函数处理特殊字符
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, spaces   !", @azimuth.greet("spaces   "))
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(2, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
}

test "divide_with_ceil_negative_precision" {
  // 测试向上取整除法在负数情况下的精度
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-7, 2))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-9, 2))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  // 计算: (5 + 3) * 2 + 10 / 3 向上取整
  let step1 = @azimuth.add(5, 3)
  let step2 = @azimuth.multiply(step1, 2)
  let step3 = @azimuth.divide_with_ceil(10, 3)
  let result = @azimuth.add(step2, step3)
  @azimuth.assert_eq(19, result)  // (5 + 3) * 2 + ceil(10/3) = 8 * 2 + 4 = 16 + 4 = 20
  
  // 计算: 15 + 20 * 3 - 25 / 4 向上取整
  let step4 = @azimuth.multiply(20, 3)
  let step5 = @azimuth.divide_with_ceil(25, 4)
  let step6 = @azimuth.add(15, step4)
  let result2 = @azimuth.add(step6, -step5)  // 使用负数实现减法
  @azimuth.assert_eq(68, result2)  // 15 + 20 * 3 - ceil(25/4) = 15 + 60 - 7 = 68
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "assert_functions_edge_cases" {
  // 测试断言函数的边界情况
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}