// 标准MoonBit测试用例
// 使用test_only模块的函数进行测试

test "addition_identity_properties" {
  // 测试加法单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-10, add(-10, 0))
  assert_eq(-10, add(0, -10))
  
  // 测试最小值与零相加
  let min_val = -2147483648
  assert_eq(min_val, add(min_val, 0))
}

test "multiplication_identity_properties" {
  // 测试乘法单位元性质
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-15, multiply(-15, 1))
  assert_eq(-15, multiply(1, -15))
  
  // 测试零乘法性质
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(-9876, 0))
}

test "addition_commutativity" {
  // 测试加法交换律
  let a = 123
  let b = 456
  
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -100
  let d = 200
  
  assert_eq(add(c, d), add(d, c))
  
  // 测试混合符号
  let e = -50
  let f = 30
  
  assert_eq(add(e, f), add(f, e))
}

test "multiplication_commutativity" {
  // 测试乘法交换律
  let a = 12
  let b = 15
  
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试负数情况
  let c = -8
  let d = 7
  
  assert_eq(multiply(c, d), multiply(d, c))
  
  // 测试两个负数
  let e = -9
  let f = -11
  
  assert_eq(multiply(e, f), multiply(f, e))
}

test "addition_associativity" {
  // 测试加法结合律
  let a = 10
  let b = 20
  let c = 30
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -15
  let e = 25
  let f = -10
  
  let result3 = add(add(d, e), f)
  let result4 = add(d, add(e, f))
  assert_eq(result3, result4)
}

test "multiplication_associativity" {
  // 测试乘法结合律
  let a = 2
  let b = 3
  let c = 4
  
  let result1 = multiply(multiply(a, b), c)
  let result2 = multiply(a, multiply(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -2
  let e = 5
  let f = -3
  
  let result3 = multiply(multiply(d, e), f)
  let result4 = multiply(d, multiply(e, f))
  assert_eq(result3, result4)
}

test "distributive_property" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -3
  let e = 8
  let f = -2
  
  let left_side2 = multiply(d, add(e, f))
  let right_side2 = add(multiply(d, e), multiply(d, f))
  assert_eq(left_side2, right_side2)
}

test "greet_function_variations" {
  // 测试greet函数的各种变体
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   LeadingSpace!", greet("  LeadingSpace"))
  
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "boundary_value_comprehensive" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(max_val, -1))
  
  // 最小值运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 极值相加
  assert_eq(-1, add(max_val, min_val))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：计算矩形的面积和周长
  let length = 8
  let width = 6
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(48, area)
  assert_eq(28, perimeter)
  
  // 场景2：计算平均速度
  let distance = 300
  let time = 5
  let speed = distance / time
  
  assert_eq(60, speed)
  
  // 场景3：计算折扣价格
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(170, final_price)
}