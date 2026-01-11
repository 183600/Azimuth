// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试1: 加法函数的零值处理
///|
test "add_zero_handling" {
  // 测试加0的情况
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(-3, add(0, -3))
}

// 测试2: 乘法函数的零值和单位元处理
///|
test "multiply_zero_and_identity" {
  // 测试乘0的情况
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-3, 0))
  assert_eq(0, multiply(0, -3))
  
  // 测试乘1的情况
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-3, multiply(-3, 1))
  assert_eq(-3, multiply(1, -3))
}

// 测试3: 负数运算
///|
test "negative_operations" {
  // 加法负数测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  
  // 乘法负数测试
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(15, multiply(-5, -3))
}

// 测试4: 边界值溢出处理
///|
test "overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试乘法溢出
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(min_val, 2))
}

// 测试5: 特殊值处理
///|
test "special_values" {
  let min_val = -2147483648
  
  // 测试最小值的特殊处理
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  
  // 测试最小值乘法
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}

// 测试6: 字符串处理
///|
test "string_operations" {
  // 基本字符串测试
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  assert_eq("Hello, 123!", greet("123"))
  assert_eq("Hello, !", greet(""))
  
  // 特殊字符测试
  assert_eq("Hello, @#$!", greet("@#$"))
  assert_eq("Hello, 中文!", greet("中文"))
}

// 测试7: 数学性质验证
///|
test "mathematical_properties" {
  // 加法结合律
  let a = 5
  let b = 3
  let c = 2
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质
  assert_eq(0, multiply(0, 123))
  assert_eq(123, add(0, 123))
}

// 测试8: 实际应用场景
///|
test "real_world_scenarios" {
  // 银行账户计算
  let balance = 1000
  let deposit = 500
  let withdrawal = -200
  let new_balance = add(add(balance, deposit), withdrawal)
  assert_eq(1300, new_balance)
  
  // 商品总价计算
  let unit_price = 25
  let quantity = 8
  let discount = -5
  let total = add(multiply(unit_price, quantity), discount)
  assert_eq(195, total)
}

// 测试9: 性能相关的大数运算
///|
test "large_number_operations" {
  // 大数运算测试
  let large_num = 1000000
  let another_large = 2000000
  
  assert_eq(3000000, add(large_num, another_large))
  assert_eq(2000000000000, multiply(large_num, another_large))
  
  // 大数与边界值混合
  assert_eq(2147483647, add(2147483647, large_num))
  assert_eq(-2147483648, add(-2147483648, -large_num))
}

// 测试10: 错误处理和边界情况
///|
test "error_handling_and_edge_cases" {
  // 极端情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  assert_eq(-1, add(max_val, min_val))
  
  // 最小值与-1相乘
  assert_eq(min_val, multiply(min_val, -1))
  
  // 最大值与-1相乘
  assert_eq(-2147483647, multiply(max_val, -1))
  
  // 连续运算测试
  let result = multiply(add(10, 5), add(3, 2))
  assert_eq(75, result)
}