// 用户添加的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "addition_with_various_ranges" {
  // 测试不同范围的加法运算
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiplication_comprehensive" {
  // 测试全面的乘法运算
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, aBcDeF!", @azimuth.greet("aBcDeF"))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(100, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(101, result2)
  
  // a * (b + c)
  let result3 = @azimuth.multiply(a, @azimuth.add(b, c))
  @azimuth.assert_eq(156, result3)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 圆的周长和面积（简化计算：π ≈ 3）
  let radius = 10
  
  // 周长：2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(60, circumference)
  
  // 面积：π × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(300, area)
}

test "string_processing_scenarios" {
  // 测试字符串处理场景
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
  @azimuth.assert_eq_string("Hello, #hashtag!", @azimuth.greet("#hashtag"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, mix3dStr1ng!", @azimuth.greet("mix3dStr1ng"))
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, sum1)
  
  // 计算 2 * 3 * 4
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, product1)
  
  // 计算 (1 + 2) * (3 + 4)
  let result1 = @azimuth.multiply(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(21, result1)
}

test "error_handling_validation" {
  // 测试错误处理验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 验证溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "comprehensive_application_scenario" {
  // 测试综合应用场景：计算订单总价
  let product_price = 199
  let quantity = 3
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税费
  let shipping = 15       // 运费
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算折扣（仅当总价超过500时）
  let discount = if (subtotal > 500) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax), shipping)
  
  // 验证结果：199*3 = 597，折扣：597*10/100 = 59，折后：597-59 = 538
  // 税费：538*8/100 = 43，总价：538+43+15 = 596
  @azimuth.assert_eq(596, final_total)
}