// Azimuth 综合测试套件
// 使用标准的 MoonBit 测试语法，覆盖核心功能的各个方面

test "add_zero_handling" {
  // 测试加法中的零值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "add_symmetric_properties" {
  // 测试加法的对称性质
  let values = [0, 1, -1, 42, -42, 1000, -1000]
  
  for i = 0; i < values.length(); i = i + 1 {
    for j = i; j < values.length(); j = j + 1 {
      let a = values[i]
      let b = values[j]
      @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
    }
  }
}

test "multiply_identity_elements" {
  // 测试乘法的单位元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(1, -42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "multiply_negation_properties" {
  // 测试乘法的负数性质
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-1, -42))
  
  // 测试负数相乘
  @azimuth.assert_eq(42, @azimuth.multiply(-6, -7))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(-42, @azimuth.multiply(6, -7))
}

test "greet_empty_and_special_inputs" {
  // 测试 greet 函数的空值和特殊输入
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet("false"))
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet("true"))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  // 计算: (a * b) + (c * d) - (e * f)
  let a = 12; let b = 34
  let c = 56; let d = 78
  let e = 90; let f = 12
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let product3 = @azimuth.multiply(e, f)
  
  let sum1 = @azimuth.add(product1, product2)
  // 使用加法来实现减法：x - y = x + (-y)
  let neg_product3 = @azimuth.multiply(-1, product3)
  let result = @azimuth.add(sum1, neg_product3)
  
  @azimuth.assert_eq(408 + 4368 - 1080, result)
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 乘法溢出边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
}

test "arithmetic_property_verification" {
  // 测试算术性质的验证
  let a = 123; let b = 456; let c = 789
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul_assoc = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul_assoc = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul_assoc, right_mul_assoc)
  
  // 分配律: a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}