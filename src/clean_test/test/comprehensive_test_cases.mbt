test "basic_arithmetic" {
  let a = 10
  let b = 20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 2: 字符串处理
///|
test "string_processing" {
  let name1 = "世界"
  let name2 = "World"
  let name3 = "MoonBit"
  
  let greeting1 = @clean_test.greet(name1)
  let greeting2 = @clean_test.greet(name2)
  let greeting3 = @clean_test.greet(name3)
  
  // 验证问候语格式正确
  @clean_test.assert_eq_string("Hello, 世界!", greeting1)
  @clean_test.assert_eq_string("Hello, World!", greeting2)
  @clean_test.assert_eq_string("Hello, MoonBit!", greeting3)
  
  // 测试特殊字符
  let name4 = "John-Doe"
  let name5 = "O'Reilly"
  
  let greeting4 = @clean_test.greet(name4)
  let greeting5 = @clean_test.greet(name5)
  
  @clean_test.assert_eq_string("Hello, John-Doe!", greeting4)
  @clean_test.assert_eq_string("Hello, O'Reilly!", greeting5)
}

// 测试用例 3: 边界值测试
///|
test "boundary_values" {
  let zero = 0
  let one = 1
  let max_int = 2147483647
  
  let result1 = @clean_test.add(zero, one)
  let result2 = @clean_test.multiply(one, max_int)
  
  @clean_test.assert_eq(1, result1)
  @clean_test.assert_eq(2147483647, result2)
}

// 测试用例 4: 负数运算
///|
test "negative_operations" {
  let neg1 = -10
  let neg2 = -20
  let pos = 30
  
  let result1 = @clean_test.add(neg1, neg2)
  let result2 = @clean_test.add(neg1, pos)
  
  @clean_test.assert_eq(-30, result1)
  @clean_test.assert_eq(20, result2)
}

// 测试用例 5: 复合运算
///|
test "compound_operations" {
  let base = 5
  
  let result1 = @clean_test.multiply(base, 2)
  let result2 = @clean_test.add(result1, 10)
  let result3 = @clean_test.multiply(result2, 3)
  
  @clean_test.assert_eq(10, result1)
  @clean_test.assert_eq(20, result2)
  @clean_test.assert_eq(60, result3)
}
