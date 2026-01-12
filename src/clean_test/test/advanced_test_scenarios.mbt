test "complex_math_operations" {
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @clean_test.add(@clean_test.multiply(a, b), c)
  let result2 = @clean_test.multiply(@clean_test.add(a, b), c)
  
  @clean_test.assert_eq(100 * 200 + 300, result1)
  @clean_test.assert_eq((100 + 200) * 300, result2)
}

// 测试用例2: 边界值测试
///|
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = @clean_test.add(max_val, 0)
  let result2 = @clean_test.add(min_val, 0)
  
  @clean_test.assert_eq(max_val, result1)
  @clean_test.assert_eq(min_val, result2)
}

// 测试用例3: 负数运算
///|
test "negative_operations" {
  let a = -100
  let b = -200
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(-300, sum)
  @clean_test.assert_eq(20000, product)
}

// 测试用例4: 零值测试
///|
test "zero_operations" {
  let a = 100
  let b = 0
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(100, sum)
  @clean_test.assert_eq(0, product)
}

// 测试用例5: 大数运算
///|
test "large_number_operations" {
  let a = 1000000
  let b = 2000000
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, 2)
  
  @clean_test.assert_eq(3000000, sum)
  @clean_test.assert_eq(2000000, product)
}
