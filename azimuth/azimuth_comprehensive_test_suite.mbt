// Azimuth 综合测试套件
// 使用标准的 MoonBit 测试语法，覆盖各种边界情况和复杂场景

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
}

test "add_symmetric_properties" {
  // 测试加法的对称性质
  let a = 123456
  let b = -789012
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  let x = -2147483648
  let y = 2147483647
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "multiply_zero_properties" {
  // 测试乘法的零值性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "multiply_identity_properties" {
  // 测试乘法的单位元性质
  @azimuth.assert_eq(123456, @azimuth.multiply(123456, 1))
  @azimuth.assert_eq(123456, @azimuth.multiply(1, 123456))
  @azimuth.assert_eq(-123456, @azimuth.multiply(-123456, 1))
  @azimuth.assert_eq(-123456, @azimuth.multiply(1, -123456))
}

test "multiply_negative_one_properties" {
  // 测试乘以-1的性质
  @azimuth.assert_eq(-123456, @azimuth.multiply(123456, -1))
  @azimuth.assert_eq(-123456, @azimuth.multiply(-1, 123456))
  @azimuth.assert_eq(123456, @azimuth.multiply(-123456, -1))
  @azimuth.assert_eq(123456, @azimuth.multiply(-1, -123456))
}

test "greet_empty_and_special_strings" {
  // 测试空字符串和特殊字符串的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 
	!", @azimuth.greet("
	"))
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
}

test "divide_with_ceil_precision_cases" {
  // 测试向上取整除法的精度情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(1999, 2))
  @azimuth.assert_eq(33334, @azimuth.divide_with_ceil(100000, 3))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式的求值
  // 计算: ((a + b) * c) + (d * e)
  let a = 10
  let b = 20
  let c = 3
  let d = 4
  let e = 5
  
  let sum_ab = @azimuth.add(a, b)
  let product_abc = @azimuth.multiply(sum_ab, c)
  let product_de = @azimuth.multiply(d, e)
  let result = @azimuth.add(product_abc, product_de)
  
  @azimuth.assert_eq(90, result)
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  
  // 乘法边界测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "arithmetic_consistency_checks" {
  // 测试算术一致性检查
  let x = 100
  let y = -50
  let z = 25
  
  // 检查加法结合律: (x + y) + z = x + (y + z)
  let left_assoc = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 检查乘法分配律: x * (y + z) = x * y + x * z
  let sum_yz = @azimuth.add(y, z)
  let distrib_left = @azimuth.multiply(x, sum_yz)
  let distrib_right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distrib_left, distrib_right)
}