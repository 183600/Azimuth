// MoonBit 测试用例示例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "integer_arithmetic_basic" {
  // 测试基本整数算术运算
  let a = 25
  let b = 15
  
  // 加法测试
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(40, sum)
  
  // 乘法测试
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(375, product)
  
  // 零值测试
  let zero_add = @azimuth.add(a, 0)
  @azimuth.assert_eq(a, zero_add)
  
  let zero_mul = @azimuth.multiply(a, 0)
  @azimuth.assert_eq(0, zero_mul)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(name1))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(name2))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet(name3))
  
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 特殊字符测试
  let special_name = "张三"
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(special_name))
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出测试
  let max_overflow = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, max_overflow)
  
  let max_large_overflow = @azimuth.add(max_val, 1000000)
  @azimuth.assert_eq(max_val, max_large_overflow)
  
  // 最小值减法溢出测试
  let min_underflow = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, min_underflow)
  
  let min_large_underflow = @azimuth.add(min_val, -1000000)
  @azimuth.assert_eq(min_val, min_large_underflow)
  
  // 乘法溢出测试
  let mul_overflow = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, mul_overflow)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let x = 17
  let y = 23
  
  // 加法交换律：a + b = b + a
  let add_ab = @azimuth.add(x, y)
  let add_ba = @azimuth.add(y, x)
  @azimuth.assert_eq(add_ab, add_ba)
  
  // 乘法交换律：a × b = b × a
  let mul_ab = @azimuth.multiply(x, y)
  let mul_ba = @azimuth.multiply(y, x)
  @azimuth.assert_eq(mul_ab, mul_ba)
  
  // 测试负数情况
  let neg_x = -17
  let neg_y = -23
  
  let neg_add_ab = @azimuth.add(neg_x, neg_y)
  let neg_add_ba = @azimuth.add(neg_y, neg_x)
  @azimuth.assert_eq(neg_add_ab, neg_add_ba)
  
  let neg_mul_ab = @azimuth.multiply(neg_x, neg_y)
  let neg_mul_ba = @azimuth.multiply(neg_y, neg_x)
  @azimuth.assert_eq(neg_mul_ab, neg_mul_ba)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 12
  let width = 8
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 场景2：计算梯形面积
  let top_base = 10
  let bottom_base = 20
  let height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(75, trapezoid_area)
  
  // 场景3：计算平均数
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let average = @azimuth.add(@azimuth.add(num1, num2), num3) / 3
  @azimuth.assert_eq(90, average)
}

test "assertion_functionality" {
  // 测试断言功能
  // 测试 assert_eq
  @azimuth.assert_eq(10, 10)
  @azimuth.assert_eq(-5, -5)
  @azimuth.assert_eq(0, 0)
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("测试", "测试")
  
  // 测试 assert_true
  @azimuth.assert_true(true)
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(-3 < 0)
  
  // 测试 assert_false
  @azimuth.assert_false(false)
  @azimuth.assert_false(10 < 5)
  @azimuth.assert_false(-3 > 0)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算场景
  // 场景：计算贷款利息
  let principal = 10000  // 本金
  let rate = 5           // 年利率（百分比）
  let years = 3          // 年数
  
  // 计算简单利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  
  // 计算总还款额：本金 + 利息
  let total_payment = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total_payment)  // 10000 + 10000*5*3 = 160000
  
  // 场景：计算折扣价格
  let original_price = 200
  let discount_percent = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(170, discounted_price)
}

test "sequential_operations" {
  // 测试连续操作
  // 场景1：连续加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40)
  @azimuth.assert_eq(100, result2)
  
  // 场景2：连续乘法
  let result3 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result3)
  
  let result4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4)
  @azimuth.assert_eq(24, result4)
  
  // 场景3：混合运算
  let result5 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result5)
}

test "edge_case_values" {
  // 测试边界值情况
  let max_val = 2147483647
  let min_val = -2147483648
  let one = 1
  let neg_one = -1
  
  // 测试最大值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, neg_one))
  
  // 测试最小值与1的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, neg_one))
  
  // 测试-1的特殊情况
  @azimuth.assert_eq(-1, @azimuth.multiply(1, neg_one))
  @azimuth.assert_eq(1, @azimuth.multiply(neg_one, neg_one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, neg_one))
}

test "string_special_characters" {
  // 测试字符串特殊字符处理
  // 测试包含数字的字符串
  let numeric_string = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(numeric_string))
  
  // 测试包含符号的字符串
  let symbolic_string = "test@example.com"
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(symbolic_string))
  
  // 测试包含空格的字符串
  let spaced_string = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_string))
  
  // 测试包含混合字符的字符串
  let mixed_string = "用户_User123@Example"
  @azimuth.assert_eq_string("Hello, 用户_User123@Example!", @azimuth.greet(mixed_string))
  
  // 测试包含换行符的字符串
  let newline_string = "Line1\nLine2"
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet(newline_string))
}

test "mathematical_series" {
  // 测试数学数列计算
  // 场景1：等差数列求和（前n个自然数之和）
  let n = 10
  // 使用公式：n × (n + 1) / 2
  let sum_natural = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum_natural)  // 1+2+3+...+10 = 55
  
  // 场景2：等比数列求和（前n项，公比为2）
  // 1 + 2 + 4 + 8 + 16 = 31
  let sum_geometric = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, sum_geometric)
  
  // 场景3：平方数列求和（前5个平方数）
  // 1² + 2² + 3² + 4² + 5² = 55
  let square1 = @azimuth.multiply(1, 1)
  let square2 = @azimuth.multiply(2, 2)
  let square3 = @azimuth.multiply(3, 3)
  let square4 = @azimuth.multiply(4, 4)
  let square5 = @azimuth.multiply(5, 5)
  let sum_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  @azimuth.assert_eq(55, sum_squares)
}