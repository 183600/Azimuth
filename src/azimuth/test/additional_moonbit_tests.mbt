// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "addition_with_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(3, @azimuth.add(-7, 10))
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-10, @azimuth.multiply(2, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_function_with_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试最大值和最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 0))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + a * c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(25, result2)
  
  // (a + b) * (a - b)
  let result3 = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(a, -b))
  @azimuth.assert_eq(16, result3)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, user2023!", @azimuth.greet("user2023"))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  // 验证结果：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, final_total)
}

test "mathematical_series_calculation" {
  // 测试数学数列计算
  // 计算等差数列前5项的和：2 + 4 + 6 + 8 + 10
  let sum1 = @azimuth.add(2, @azimuth.add(4, @azimuth.add(6, @azimuth.add(8, 10))))
  @azimuth.assert_eq(30, sum1)
  
  // 计算等比数列前4项的和：1 + 2 + 4 + 8
  let sum2 = @azimuth.add(1, @azimuth.add(2, @azimuth.add(4, 8)))
  @azimuth.assert_eq(15, sum2)
  
  // 使用公式验证等差数列求和：n * (首项 + 末项) / 2
  let n = 5
  let first = 2
  let last = 10
  let formula_sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(30, formula_sum)
}