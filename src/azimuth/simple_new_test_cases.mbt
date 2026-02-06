// 简单的新测试用例 - 验证基本功能
test "simple_financial_calculation" {
  // 简单贷款计算测试
  let loan_amount = 100000
  let interest_rate = 5
  let years = 10
  
  let total_interest = multiply(loan_amount, interest_rate)
  let total_repayment = add(loan_amount, divide_with_ceil(total_interest, 100))
  
  assert_eq(500000, total_interest)  // 100000 * 5 = 500000
  assert_eq(105000, total_repayment)  // 100000 + 5000 = 105000
}

test "simple_inventory_management" {
  // 简单库存管理测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, items)
  
  assert_eq(48, total_capacity)  // 4 * 12 = 48
  assert_eq(1, remaining_space)  // 48 - 47 = 1
}

test "simple_string_processing" {
  // 简单字符串处理测试
  let name = "MoonBit"
  let greeting = greet(name)
  
  assert_eq_string("Hello, MoonBit!", greeting)
  
  let empty_greeting = greet("")
  assert_eq_string("Hello, !", empty_greeting)
}

test "simple_mathematical_operations" {
  // 简单数学运算测试
  let a = 15
  let b = 7
  
  // 加法和乘法
  let sum = add(a, b)
  let product = multiply(a, b)
  
  assert_eq(22, sum)  // 15 + 7 = 22
  assert_eq(105, product)  // 15 * 7 = 105
  
  // 减法和除法
  let difference = subtract(a, b)
  let quotient = divide_with_ceil(a, b)
  
  assert_eq(8, difference)  // 15 - 7 = 8
  assert_eq(3, quotient)  // ceil(15/7) = 3
}

test "simple_boundary_conditions" {
  // 简单边界条件测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-10, 5))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
}