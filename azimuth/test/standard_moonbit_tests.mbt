// 标准 MoonBit 测试用例 - 基础功能测试

test "basic_functionality_add" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "basic_functionality_multiply" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "basic_functionality_greet" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "basic_functionality_divide_with_ceil" {
  // 测试基本向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 15
  let b = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试数学单位元性质
  let value = 42
  
  // 加法单位元
  @azimuth.assert_eq(value, @azimuth.add(value, 0))
  @azimuth.assert_eq(value, @azimuth.add(0, value))
  
  // 乘法单位元
  @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
  @azimuth.assert_eq(value, @azimuth.multiply(1, value))
}

test "boundary_values_max_min" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  // 负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  
  // 负数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "string_operations_special_characters" {
  // 测试字符串特殊字符操作
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, spaced name!", @azimuth.greet("spaced name"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂算术组合
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(45, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(53, result2)
}