// 用户请求的 MoonBit 测试用例集
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-50, @azimuth.add(0, -50))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "overflow_handling_addition" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "overflow_handling_multiplication" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律简化测试
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), 
                     @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_shopping_cart" {
  // 实际应用：购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 4
  let item3_price = 40
  let item3_qty = 2
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let total3 = @azimuth.multiply(item3_price, item3_qty)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 验证结果: 25×3 + 15×4 + 40×2 = 75 + 60 + 80 = 215
  @azimuth.assert_eq(215, cart_total)
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let base = 100
  let increment = 25
  let multiplier = 3
  let deduction = 50
  
  // 计算: ((base + increment) × multiplier) - deduction
  let sum = @azimuth.add(base, increment)
  let product = @azimuth.multiply(sum, multiplier)
  let result = @azimuth.add(product, -deduction)
  
  // 验证: ((100 + 25) × 3) - 50 = (125 × 3) - 50 = 375 - 50 = 325
  @azimuth.assert_eq(325, result)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
  
  // 与实际功能结合测试
  let add_result = @azimuth.add(17, 25)
  @azimuth.assert_eq(42, add_result)
  @azimuth.assert_true(add_result > 40)
  @azimuth.assert_false(add_result < 40)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
}

test "edge_cases_and_special_values" {
  // 边界情况和特殊值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试连续运算
  let chain_result = @azimuth.add(@azimuth.multiply(5, 10), @azimuth.multiply(3, 4))
  @azimuth.assert_eq(62, chain_result)  // 5×10 + 3×4 = 50 + 12 = 62
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", @azimuth.greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
}