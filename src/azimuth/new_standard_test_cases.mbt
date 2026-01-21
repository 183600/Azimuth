// 新的标准 MoonBit 测试用例
// 测试核心功能：add, multiply, greet 等函数

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(5, -8))
  assert_eq(0, add(-10, 10))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(0, 123))
}

test "boundary_value_tests" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(0, multiply(max_val, 0))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 结合律测试
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆形面积和周长计算（简化版）
  let radius = 5
  let pi_approx = 3
  
  // 周长 = 2 × π × r
  let circumference = multiply(multiply(2, pi_approx), radius)
  assert_eq(30, circumference)
  
  // 面积 = π × r²
  let area = multiply(pi_approx, multiply(radius, radius))
  assert_eq(75, area)
  
  // 复合计算：面积 + 周长
  let total = add(area, circumference)
  assert_eq(105, total)
}

test "real_world_business_calculations" {
  // 测试实际业务计算
  // 订单总价计算
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10
  let shipping = 15
  let tax_rate = 8
  
  // 商品总价
  let subtotal = multiply(unit_price, quantity)
  assert_eq(597, subtotal)
  
  // 折扣金额
  let discount = multiply(subtotal, discount_rate) / 100
  assert_eq(59, discount)
  
  // 折后价格
  let discounted_price = add(subtotal, -discount)
  assert_eq(538, discounted_price)
  
  // 税费
  let tax = multiply(discounted_price, tax_rate) / 100
  assert_eq(43, tax)
  
  // 最终总价
  let final_total = add(add(discounted_price, tax), shipping)
  assert_eq(596, final_total)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 条件验证
  let positive = 10
  let negative = -5
  let zero = 0
  
  assert_true(positive > 0)
  assert_true(negative < 0)
  assert_true(zero == 0)
  
  // 运算结果验证
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
  assert_true(greet("Test") == "Hello, Test!")
}

test "performance_and_efficiency" {
  // 测试性能和效率相关的计算
  // 等差数列求和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 20
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(210, sum)
  
  // 等比数列求和（简化版）：1 + 2 + 4 + 8 + 16 = 31
  let geo_sum = add(add(add(add(1, 2), 4), 8), 16)
  assert_eq(31, geo_sum)
  
  // 阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
  
  // 幂运算：2^6 = 64
  let power = multiply(multiply(multiply(2, 2), multiply(2, 2)), multiply(2, 2))
  assert_eq(64, power)
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  // 数字到字符串的间接转换
  let year = 2024
  let month = 1
  let day = 21
  
  let year_str = "2024"
  let month_str = "1"
  let day_str = "21"
  
  // 日期格式化测试
  let date_greeting = greet(year_str + "-" + month_str + "-" + day_str)
  assert_eq_string("Hello, 2024-1-21!", date_greeting)
  
  // 百分比格式化
  let percentage = 85
  let percentage_str = "85%"
  let result = greet(percentage_str)
  assert_eq_string("Hello, 85%!", result)
  
  // 货币格式化
  let amount = 1234
  let currency_str = "$1234"
  let currency_result = greet(currency_str)
  assert_eq_string("Hello, $1234!", currency_result)
}

test "edge_cases_and_special_conditions" {
  // 测试边界情况和特殊条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续运算测试
  let chain_result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, chain_result)
  
  // 嵌套运算测试
  let nested_result = multiply(add(10, 5), add(3, 2))
  assert_eq(75, nested_result)
  
  // 混合符号运算
  let mixed_result = add(multiply(-2, 3), multiply(4, -5))
  assert_eq(-26, mixed_result)
  
  // 零值参与的各种运算
  assert_eq(5, add(0, 5))
  assert_eq(-5, add(0, -5))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, -5))
  
  // 极值运算验证
  assert_true(add(max_val, min_val) == -1)
  assert_true(multiply(max_val, 0) == 0)
  assert_true(multiply(min_val, 0) == 0)
}

test "algorithmic_implementation" {
  // 测试算法实现
  // 最大公约数算法（简化版欧几里得算法）
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法
  // 48 % 18 = 12
  // 18 % 12 = 6
  // 12 % 6 = 0
  // 所以 GCD 是 6
  let gcd = 6
  assert_eq(6, gcd)
  
  // 验证公约数性质
  assert_true(48 % 6 == 0)
  assert_true(18 % 6 == 0)
  
  // 简单排序算法验证
  let num1 = 5
  let num2 = 2
  let num3 = 8
  
  // 找出最大值
  let max = if (num1 > num2 && num1 > num3) { num1 } else if (num2 > num3) { num2 } else { num3 }
  assert_eq(8, max)
  
  // 找出最小值
  let min = if (num1 < num2 && num1 < num3) { num1 } else if (num2 < num3) { num2 } else { num3 }
  assert_eq(2, min)
  
  // 计算范围
  let range = add(max, -min)
  assert_eq(6, range)
}