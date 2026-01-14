// MoonBit 测试套件
// 标准的 MoonBit 测试语法示例

// 测试 1: 数字运算边界测试
test "numeric_boundary_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试 2: 字符串处理多语言测试
test "multilingual_string_processing" {
  // 测试中文
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  
  // 测试日文
  @azimuth.assert_eq_string("Hello, こんにちは!", @azimuth.greet("こんにちは"))
  
  // 测试韩文
  @azimuth.assert_eq_string("Hello, 안녕하세요!", @azimuth.greet("안녕하세요"))
  
  // 测试阿拉伯文
  @azimuth.assert_eq_string("Hello, مرحبا!", @azimuth.greet("مرحبا"))
  
  // 测试表情符号
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

// 测试 3: 复杂数学表达式
test "complex_mathematical_expressions" {
  // 测试 (a + b) * (c - d) + e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  let result = @azimuth.add(product, e)
  
  @azimuth.assert_eq(93, result)
}

// 测试 4: 条件分支测试
test "conditional_branching_test" {
  let x = 15
  let y = 10
  
  // 测试三元条件
  let max_val = if x > y { x } else { y }
  @azimuth.assert_eq(15, max_val)
  
  // 测试嵌套条件
  let result = if x > 10 {
    if y > 5 {
      @azimuth.add(x, y)
    } else {
      @azimuth.multiply(x, y)
    }
  } else {
    @azimuth.multiply(x, y)
  }
  
  @azimuth.assert_eq(25, result)
}

// 测试 5: 数组索引计算模拟
test "array_index_calculation" {
  // 二维数组到一维数组的索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算一维索引: index = row * width + col
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(34, index)
  
  // 测试边界情况
  let first_index = @azimuth.add(@azimuth.multiply(0, width), 0)
  let last_index = @azimuth.add(@azimuth.multiply(9, width), 9)
  
  @azimuth.assert_eq(0, first_index)
  @azimuth.assert_eq(99, last_index)
}

// 测试 6: 字符串模板模拟
test "string_template_simulation" {
  // 模拟简单的字符串模板功能
  let name = "Alice"
  let age = "25"
  let city = "Beijing"
  
  // 构建个人信息字符串
  let greeting = @azimuth.greet(name)
  let age_info = " Age: " + age
  let city_info = " City: " + city
  let full_info = greeting + age_info + city_info
  
  @azimuth.assert_eq_string("Hello, Alice! Age: 25 City: Beijing", full_info)
}

// 测试 7: 数学恒等式验证
test "mathematical_identities" {
  let a = 12
  let b = 5
  
  // 验证交换律: a + b = b + a
  let sum_ab = @azimuth.add(a, b)
  let sum_ba = @azimuth.add(b, a)
  @azimuth.assert_eq(sum_ab, sum_ba)
  
  // 验证结合律: (a + b) + c = a + (b + c)
  let c = 3
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

// 测试 8: 实际应用场景 - 订单计算
test "order_calculation_scenario" {
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10
  let shipping_fee = 15
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣
  let discount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(discounted_price, tax), shipping_fee)
  
  @azimuth.assert_eq(626, total)
}

// 测试 9: 性能相关计算
test "performance_related_calculations" {
  // 计算等差数列的和: 1 + 2 + 3 + ... + n
  let n = 100
  
  // 使用公式: n * (n + 1) / 2
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列的和: 1 + 2 + 4 + 8 + 16
  let power_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, power_sum)
}

// 测试 10: 错误处理和边界情况
test "error_handling_and_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_result = @azimuth.add(max_val, max_val)
  let underflow_result = @azimuth.add(min_val, min_val)
  
  @azimuth.assert_eq(max_val, overflow_result)
  @azimuth.assert_eq(min_val, underflow_result)
  
  // 测试特殊情况
  let special_case1 = @azimuth.multiply(min_val, -1)
  let special_case2 = @azimuth.multiply(max_val, -1)
  
  @azimuth.assert_eq(min_val, special_case1)
  @azimuth.assert_eq(min_val, special_case2)
  
  // 测试断言函数
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
}