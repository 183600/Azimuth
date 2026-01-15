// MoonBit 标准测试用例
// 包含10个测试用例，覆盖基本功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "identity_properties" {
  // 测试恒等性质
  let value = 42
  
  // 加法恒等元
  @azimuth.assert_eq(value, @azimuth.add(value, 0))
  @azimuth.assert_eq(value, @azimuth.add(0, value))
  
  // 乘法恒等元
  @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
  @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, value))
}

test "negative_number_operations" {
  // 测试负数运算
  let pos_num = 20
  let neg_num = -15
  
  // 正负数相加
  @azimuth.assert_eq(5, @azimuth.add(pos_num, neg_num))
  
  // 负数相加
  @azimuth.assert_eq(-30, @azimuth.add(neg_num, neg_num))
  
  // 正负数相乘
  @azimuth.assert_eq(-300, @azimuth.multiply(pos_num, neg_num))
  
  // 负数相乘
  @azimuth.assert_eq(225, @azimuth.multiply(neg_num, neg_num))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  
  let sum = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum, c)  // 75
  let result = @azimuth.add(product, -d)  // 55
  
  @azimuth.assert_eq(55, result)
}

test "boundary_value_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 边界值与普通值运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet("Hello World"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "associative_properties" {
  // 测试结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
  
  // 乘法结合律
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.multiply(a, b), c),
    @azimuth.multiply(a, @azimuth.multiply(b, c))
  )
}

test "distributive_property" {
  // 测试分配律
  let a = 3
  let b = 4
  let c = 5
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}