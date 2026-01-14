// Azimuth 库的标准测试用例
// 这些测试用例验证了库的核心功能和边界情况处理

// 测试 add 函数的基本功能
test "add_basic_functionality" {
  // 测试正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  
  // 测试负数加法
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  
  // 测试正负数混合加法
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  
  // 测试零值加法
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

// 测试 add 函数的边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试边界值的正常加法
  @azimuth.assert_eq(-1, @azimuth.add(min_val, max_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

// 测试 multiply 函数的基本功能
test "multiply_basic_functionality" {
  // 测试正数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  
  // 测试负数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  
  // 测试零值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试单位元乘法
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
}

// 测试 multiply 函数的边界值处理
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, max_val))
  
  // 测试负数溢出保护
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 3))
  
  // 测试边界值的正常乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
}

// 测试 greet 函数
test "greet_functionality" {
  // 测试基本字符串拼接
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Moon Bit!", @azimuth.greet("Moon Bit"))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试数学恒等式
test "mathematical_identities" {
  let a = 12
  let b = 8
  
  // 验证加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律：a × b = b × a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律：a × (b + c) = a × b + a × c
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
  
  // 验证加法单位元：a + 0 = a
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 验证乘法单位元：a × 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

// 测试函数组合
test "function_composition" {
  // 计算：(a + b) × (c + d)
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = @azimuth.add(a, b)
  let sum_cd = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum_ab, sum_cd)
  
  @azimuth.assert_eq(240, result)
  
  // 测试嵌套函数调用
  let nested = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, nested)
}

// 测试错误处理机制
test "error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护不会导致程序崩溃
  let overflow_add = @azimuth.add(max_val, max_val)
  let overflow_mul = @azimuth.multiply(max_val, max_val)
  let underflow_add = @azimuth.add(min_val, min_val)
  let underflow_mul = @azimuth.multiply(min_val, -1)
  
  // 验证返回值是合理的
  @azimuth.assert_eq(max_val, overflow_add)
  @azimuth.assert_eq(max_val, overflow_mul)
  @azimuth.assert_eq(min_val, underflow_add)
  @azimuth.assert_eq(min_val, underflow_mul)
}

// 测试实际应用场景
test "practical_application" {
  // 场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价（包含运费）
  let total = @azimuth.add(subtotal, shipping)
  
  // 验证计算结果
  @azimuth.assert_eq(398, item1_total)  // 199 * 2 = 398
  @azimuth.assert_eq(299, item2_total)  // 299 * 1 = 299
  @azimuth.assert_eq(697, subtotal)     // 398 + 299 = 697
  @azimuth.assert_eq(717, total)        // 697 + 20 = 717
}