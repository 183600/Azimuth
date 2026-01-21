// 额外的标准 MoonBit 测试用例
// 为 azimuth 库提供补充测试覆盖

// 测试数学运算的交换律和结合律
test "mathematical_laws_verification" {
  // 加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let c = 12
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

// 测试实际应用场景：计算折扣价格
test "discount_calculation_scenario" {
  let original_price = 120
  let discount_percentage = 15
  let discounted_amount = divide_with_ceil(multiply(original_price, discount_percentage), 100)
  let final_price = original_price - discounted_amount
  
  assert_eq(102, final_price)  // 120 - ceil(120*15/100) = 120 - 18 = 102
}

// 测试数组索引计算
test "array_index_calculation" {
  let page_size = 10
  let item_number = 47
  let page_number = divide_with_ceil(item_number, page_size)
  
  assert_eq(5, page_number)  // ceil(47/10) = 5
}

// 测试字符串长度相关计算
test "string_length_operations" {
  let base_string = "Hello"
  let appended_string = base_string + " World"
  let greeting_result = greet("World")
  
  assert_eq_string("Hello, World!", greeting_result)
  assert_eq_string("Hello World", appended_string)
}

// 测试负数运算的特殊情况
test "negative_number_special_cases" {
  // 测试负数乘法的符号规则
  assert_eq(25, multiply(-5, -5))  // 负负得正
  assert_eq(-25, multiply(5, -5))  // 正负得负
  assert_eq(-25, multiply(-5, 5))  // 负正得负
  
  // 测试负数除法的向上取整
  assert_eq(-2, divide_with_ceil(-8, 4))   // -8/4 = -2
  assert_eq(-1, divide_with_ceil(-3, 4))   // -3/4 = -0.75 -> -0 (向上取整)
}

// 测试零值的各种运算情况
test "zero_value_operations" {
  // 零的加法性质
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 零的乘法性质
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(0, 0))
  
  // 零的除法处理
  assert_eq(0, divide_with_ceil(0, 42))
  assert_eq(0, divide_with_ceil(0, 1))
}

// 测试大数运算的稳定性
test "large_number_stability" {
  let large_num = 100000
  let another_large = 200000
  
  // 大数加法
  assert_eq(300000, add(large_num, another_large))
  
  // 大数乘法
  assert_eq(200000000, multiply(large_num, 2000))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(large_num, another_large))
  assert_eq(2, divide_with_ceil(another_large, large_num))
}

// 测试分数运算的向上取整
test "fraction_ceil_operations" {
  // 测试各种分数情况的向上取整
  assert_eq(1, divide_with_ceil(1, 2))    // 0.5 -> 1
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(4, divide_with_ceil(7, 2))    // 3.5 -> 4
  assert_eq(11, divide_with_ceil(21, 2))  // 10.5 -> 11
  
  // 测试接近整数的分数
  assert_eq(10, divide_with_ceil(19, 2))   // 9.5 -> 10
  assert_eq(10, divide_with_ceil(20, 2))   // 10 -> 10
}

// 测试复合函数调用
test "composite_function_calls" {
  // 复合运算：(a + b) * (c - d) 的简化版本
  let a = 8
  let b = 12
  let c = 20
  let d = 5
  
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let result = multiply(sum_ab, diff_cd)
  
  assert_eq(300, result)  // (8 + 12) * (20 - 5) = 20 * 15 = 300
}

// 测试边界值的特殊处理
test "boundary_value_handling" {
  // 测试1的运算特性
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(42, add(42, 0))  // 加法单位元
  assert_eq(1, divide_with_ceil(42, 42))
  
  // 测试-1的运算特性
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  assert_eq(42, multiply(-42, -1))
}