// Azimuth 增强质量测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "enhanced_addition_operations" {
  // 增强加法运算测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "enhanced_multiplication_operations" {
  // 增强乘法运算测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "enhanced_subtraction_operations" {
  // 增强减法运算测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "enhanced_division_with_ceil" {
  // 增强向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "enhanced_string_greetings" {
  // 增强字符串问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_business_calculation" {
  // 复杂商业计算测试
  let price = 25
  let quantity = 4
  let tax_rate = 10
  let discount = 15
  
  let subtotal = multiply(price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = subtract(total_before_discount, discount)
  
  assert_eq(100, subtotal)
  assert_eq(10, tax_amount)
  assert_eq(110, total_before_discount)
  assert_eq(95, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 等差数列：2, 5, 8, 11, 14
  let term1 = 2
  let difference = 3
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  let term5 = add(term4, difference)
  
  let sequence_sum = add(add(add(add(term1, term2), term3), term4), term5)
  let average = divide_with_ceil(sequence_sum, 5)
  
  assert_eq(5, term2)
  assert_eq(8, term3)
  assert_eq(11, term4)
  assert_eq(14, term5)
  assert_eq(40, sequence_sum)
  assert_eq(8, average)
}

test "boundary_value_comprehensive" {
  // 边界值综合测试
  let max_val = 100000
  let min_val = -100000
  let zero = 0
  
  // 边界加法测试
  assert_eq(add(max_val, 1), 100001)
  assert_eq(add(min_val, -1), -100001)
  assert_eq(add(zero, max_val), max_val)
  
  // 边界乘法测试
  assert_eq(multiply(max_val, 1), max_val)
  assert_eq(multiply(min_val, 1), min_val)
  assert_eq(multiply(max_val, zero), zero)
  
  // 边界除法测试
  assert_eq(divide_with_ceil(max_val, 1), max_val)
  assert_eq(divide_with_ceil(min_val, 1), min_val)
  assert_eq(divide_with_ceil(zero, 1), zero)
  
  // 复合边界计算
  let complex_result = add(multiply(subtract(max_val, min_val), 2), 1)
  assert_eq(complex_result, 400001)
}