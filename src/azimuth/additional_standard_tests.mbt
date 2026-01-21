// 额外的标准 MoonBit 测试用例
// 包含 10 个测试用例，使用标准的 MoonBit 测试语法

test "divide_with_ceil_basic" {
  // 测试 divide_with_ceil 函数的基本功能
  // 10 / 3 = 3.333... 向上取整为 4
  let result1 = divide_with_ceil(10, 3)
  assert_eq(4, result1)
  
  // 15 / 5 = 3.0 向上取整为 3
  let result2 = divide_with_ceil(15, 5)
  assert_eq(3, result2)
  
  // 7 / 2 = 3.5 向上取整为 4
  let result3 = divide_with_ceil(7, 2)
  assert_eq(4, result3)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  // -10 / 3 = -3.333... 向上取整为 -3
  let result1 = divide_with_ceil(-10, 3)
  assert_eq(-3, result1)
  
  // 10 / -3 = -3.333... 向上取整为 -3
  let result2 = divide_with_ceil(10, -3)
  assert_eq(-3, result2)
  
  // -10 / -3 = 3.333... 向上取整为 4
  let result3 = divide_with_ceil(-10, -3)
  assert_eq(4, result3)
}

test "large_number_operations" {
  // 测试大数运算的稳定性
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 测试大数相加
  let sum_result = add(large_num1, large_num2)
  assert_eq(3000000, sum_result)
  
  // 测试大数相乘
  let product_result = multiply(1000, 2000)
  assert_eq(2000000, product_result)
  
  // 测试大数除法向上取整
  let divide_result = divide_with_ceil(1000000, 3)
  assert_eq(333334, divide_result)
}

test "string_special_characters" {
  // 测试字符串处理的特殊情况
  // 测试包含空格的字符串
  let spaced_name = " John Doe "
  let result1 = greet(spaced_name)
  assert_eq_string("Hello,  John Doe !", result1)
  
  // 测试包含转义字符的字符串
  let escaped_name = "Line1\nLine2\tTabbed"
  let result2 = greet(escaped_name)
  assert_eq_string("Hello, Line1\nLine2\tTabbed!", result2)
  
  // 测试包含引号的字符串
  let quoted_name = "\"Alice\""
  let result3 = greet(quoted_name)
  assert_eq_string("Hello, \"Alice\"!", result3)
}

test "mathematical_associativity" {
  // 测试数学运算的结合律
  let a = 15
  let b = 25
  let c = 35
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let result3 = multiply(multiply(a, b), c)
  let result4 = multiply(a, multiply(b, c))
  assert_eq(result3, result4)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试最大值与-1的运算
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值与-1的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试除法向上取整的边界情况
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(0, divide_with_ceil(0, max_val))
}

test "discount_price_calculation" {
  // 测试实际应用场景：计算折扣价格
  let original_price = 1200
  let discount_percent = 15  // 15% 折扣
  let tax_percent = 8        // 8% 税费
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percent) / 100
  assert_eq(180, discount_amount)
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(1020, discounted_price)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_percent) / 100
  assert_eq(81, tax_amount)
  
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  assert_eq(1101, final_price)
}

test "average_score_calculation" {
  // 测试实际应用场景：计算平均分
  let math_score = 85
  let english_score = 92
  let science_score = 78
  let history_score = 88
  let art_score = 95
  
  // 计算总分
  let total_score = add(add(add(add(math_score, english_score), science_score), history_score), art_score)
  assert_eq(438, total_score)
  
  // 计算平均分
  let average_score = total_score / 5
  assert_eq(87, average_score)
  
  // 计算向上取整的平均分（如果有小数）
  let rounded_average = divide_with_ceil(total_score, 5)
  assert_eq(88, rounded_average)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形周长和面积
  let length = 12
  let width = 8
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算矩形周长：2 × (length + width)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
  
  // 计算对角线长度的平方（勾股定理）：length² + width²
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(208, diagonal_squared)
  
  // 测试字符串拼接
  let description = "Rectangle(" + length.to_string() + "x" + width.to_string() + ")"
  let result = greet(description)
  assert_eq_string("Hello, Rectangle(12x8)!", result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = add(max_val, max_val)
  assert_eq(max_val, overflow_add)
  
  let underflow_add = add(min_val, min_val)
  assert_eq(min_val, underflow_add)
  
  let overflow_mul = multiply(max_val, 2)
  assert_eq(max_val, overflow_mul)
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试除数为0的情况（这里我们避免触发除零错误）
  // 在实际应用中，应该有适当的错误处理
  let safe_division = divide_with_ceil(10, 1)
  assert_eq(10, safe_division)
  
  // 测试空字符串处理
  let empty_result = greet("")
  assert_eq_string("Hello, !", empty_result)
}