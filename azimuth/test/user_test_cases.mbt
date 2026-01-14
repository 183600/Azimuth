// MoonBit 测试用例
// 测试 azimuth 包中的核心功能

// 测试加法的交换律
test "add_commutative" {
  let a = 12
  let b = 34
  @azimuth.assert_eq(@azimuth.add(a)(b), @azimuth.add(b)(a))
}

// 测试加法的结合律
test "add_associative" {
  let a = 10
  let b = 20
  let c = 30
  let result1 = @azimuth.add(@azimuth.add(a)(b))(c)
  let result2 = @azimuth.add(a)(@azimuth.add(b)(c))
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法的交换律
test "multiply_commutative" {
  let a = 6
  let b = 7
  @azimuth.assert_eq(@azimuth.multiply(a)(b), @azimuth.multiply(b)(a))
}

// 测试乘法的结合律
test "multiply_associative" {
  let a = 2
  let b = 3
  let c = 4
  let result1 = @azimuth.multiply(@azimuth.multiply(a)(b))(c)
  let result2 = @azimuth.multiply(a)(@azimuth.multiply(b)(c))
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法对加法的分配律
test "multiply_distributive" {
  let a = 5
  let b = 3
  let c = 4
  let left = @azimuth.multiply(a)(@azimuth.add(b)(c))
  let right = @azimuth.add(@azimuth.multiply(a)(b))(@azimuth.multiply(a)(c))
  @azimuth.assert_eq(left, right)
}

// 测试大数运算
test "large_numbers" {
  let large1 = 1000000
  let large2 = 2000000
  
  // 大数加法
  let sum = @azimuth.add(large1)(large2)
  @azimuth.assert_eq(3000000, sum)
  
  // 大数乘法
  let product = @azimuth.multiply(large1)(2)
  @azimuth.assert_eq(2000000, product)
}

// 测试混合运算
test "mixed_operations" {
  let base = 10
  
  // (base + 5) * 2
  let step1 = @azimuth.add(base)(5)
  let result1 = @azimuth.multiply(step1)(2)
  @azimuth.assert_eq(30, result1)
}

// 测试边界值
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // max_val * 1 应该等于 max_val
  let result1 = @azimuth.multiply(max_val)(1)
  @azimuth.assert_eq(max_val, result1)
  
  // min_val * 1 应该等于 min_val
  let result2 = @azimuth.multiply(min_val)(1)
  @azimuth.assert_eq(min_val, result2)
}

// 测试字符串处理
test "string_processing" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, Hello & World!", @azimuth.greet("Hello & World"))
}