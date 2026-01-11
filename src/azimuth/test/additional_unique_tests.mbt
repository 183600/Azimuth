// 额外的独特测试用例

// 测试 1: 验证加法的零元素特性 - 0加任何数等于那个数本身
///|
test "add_zero_property" {
  // 正数加0
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  
  // 负数加0
  assert_eq(-42, add(-42, 0))
  assert_eq(-42, add(0, -42))
  
  // 边界值加0
  assert_eq(2147483647, add(2147483647, 0))
  assert_eq(-2147483648, add(-2147483648, 0))
}

// 测试 2: 验证乘法的零元素特性 - 任何数乘以0等于0
///|
test "multiply_zero_property" {
  // 正数乘0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  
  // 负数乘0
  assert_eq(0, multiply(-42, 0))
  assert_eq(0, multiply(0, -42))
  
  // 边界值乘0
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
}

// 测试 3: 验证乘法的单位元特性 - 任何数乘以1等于那个数本身
///|
test "multiply_one_property" {
  // 正数乘1
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  
  // 负数乘1
  assert_eq(-42, multiply(-42, 1))
  assert_eq(-42, multiply(1, -42))
  
  // 边界值乘1
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
}

// 测试 4: 验证乘法的负一特性 - 任何数乘以-1等于它的相反数
///|
test "multiply_negative_one_property" {
  // 正数乘-1
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  
  // 负数乘-1
  assert_eq(42, multiply(-42, -1))
  assert_eq(42, multiply(-1, -42))
  
  // 0乘-1
  assert_eq(0, multiply(0, -1))
  assert_eq(0, multiply(-1, 0))
}

// 测试 5: 验证加法的结合律 - (a+b)+c = a+(b+c)
///|
test "addition_associative_law" {
  let a = 100
  let b = 200
  let c = 300
  
  // (a+b)+c
  let result1 = add(add(a, b), c)
  // a+(b+c)
  let result2 = add(a, add(b, c))
  
  assert_eq(result1, result2)
  
  // 使用负数测试
  let x = -100
  let y = -200
  let z = 300
  
  let result3 = add(add(x, y), z)
  let result4 = add(x, add(y, z))
  
  assert_eq(result3, result4)
}

// 测试 6: 验证乘法的结合律 - (a*b)*c = a*(b*c)
///|
test "multiplication_associative_law" {
  let a = 2
  let b = 3
  let c = 4
  
  // (a*b)*c
  let result1 = multiply(multiply(a, b), c)
  // a*(b*c)
  let result2 = multiply(a, multiply(b, c))
  
  assert_eq(result1, result2)
  
  // 使用负数测试
  let x = -2
  let y = 3
  let z = -4
  
  let result3 = multiply(multiply(x, y), z)
  let result4 = multiply(x, multiply(y, z))
  
  assert_eq(result3, result4)
}

// 测试 7: 验证乘法对加法的分配律 - a*(b+c) = a*b + a*c
///|
test "multiplication_distributive_law" {
  let a = 5
  let b = 10
  let c = 15
  
  // a*(b+c)
  let result1 = multiply(a, add(b, c))
  // a*b + a*c
  let result2 = add(multiply(a, b), multiply(a, c))
  
  assert_eq(result1, result2)
  
  // 使用负数测试
  let x = -3
  let y = 7
  let z = -2
  
  let result3 = multiply(x, add(y, z))
  let result4 = add(multiply(x, y), multiply(x, z))
  
  assert_eq(result3, result4)
}

// 测试 8: 验证加法的交换律 - a+b = b+a
///|
test "addition_commutative_law" {
  let a = 123
  let b = 456
  
  // a+b
  let result1 = add(a, b)
  // b+a
  let result2 = add(b, a)
  
  assert_eq(result1, result2)
  
  // 使用负数测试
  let x = -123
  let y = 456
  
  let result3 = add(x, y)
  let result4 = add(y, x)
  
  assert_eq(result3, result4)
}

// 测试 9: 验证乘法的交换律 - a*b = b*a
///|
test "multiplication_commutative_law" {
  let a = 12
  let b = 34
  
  // a*b
  let result1 = multiply(a, b)
  // b*a
  let result2 = multiply(b, a)
  
  assert_eq(result1, result2)
  
  // 使用负数测试
  let x = -12
  let y = 34
  
  let result3 = multiply(x, y)
  let result4 = multiply(y, x)
  
  assert_eq(result3, result4)
}

// 测试 10: 验证字符串拼接的基本属性
///|
test "string_concatenation_properties" {
  // 测试空字符串
  assert_eq("Hello, !", greet(""))
  
  // 测试单字符
  assert_eq("Hello, A!", greet("A"))
  
  // 测试数字字符串
  assert_eq("Hello, 123!", greet("123"))
  
  // 测试特殊字符
  assert_eq("Hello, @#$!", greet("@#$"))
  
  // 测试Unicode字符
  assert_eq("Hello, 世界!", greet("世界"))
}