// 标准 MoonBit 测试用例 v16
// 为 Azimuth 项目添加的核心功能测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(6, -3))
}

test "zero_handling_operations" {
  // 测试零值处理
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "identity_elements" {
  // 测试单位元（加法单位元0，乘法单位元1）
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(100, 1))
  @azimuth.assert_eq(-100, @azimuth.multiply(-100, 1))
  @azimuth.assert_eq(-100, @azimuth.multiply(100, -1))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 2024!", @azimuth.greet("2024"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算 (a + b) * (c - d)
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let result = @azimuth.multiply(sum_ab, diff_cd)
  
  @azimuth.assert_eq(300, result)
}

test "practical_business_calculation" {
  // 测试实际业务计算
  // 场景：计算订单总价（商品价格 × 数量 + 运费）
  let unit_price = 159
  let quantity = 4
  let shipping_fee = 25
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(661, total)
}

test "assert_function_verification" {
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(123, 123)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试条件断言
  let test_value = @azimuth.add(10, 20)
  @azimuth.assert_eq(test_value == 30, true)
  @azimuth.assert_eq(0, 1)(test_value == 25)
}

test "sequential_operations_chain" {
  // 测试连续操作链
  // 计算 (((a + b) * c) - d) / e
  let a = 5
  let b = 3
  let c = 4
  let d = 6
  let e = 2
  
  let step1 = @azimuth.add(a, b)        // 8
  let step2 = @azimuth.multiply(step1, c)  // 32
  let step3 = @azimuth.add(step2, -d)   // 26
  let final_result = step3 / e          // 13
  
  @azimuth.assert_eq(13, final_result)
}