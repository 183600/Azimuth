// 标准 MoonBit 测试用例 - 10个核心功能测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(3, subtract(8, 5))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(8, subtract(3, -5))
}

test "zero_operations" {
  // 测试零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(0, subtract(1000000, 1000000))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), subtract(10, 2))
  assert_eq(64, result2)  // (5+3) * (10-2) = 8 * 8 = 64
  
  let result3 = divide_with_ceil(add(multiply(3, 4), 5), 7)
  assert_eq(3, result3)  // ceil((3*4+5)/7) = ceil(17/7) = 3
}

test "practical_business_calculations" {
  // 测试实际业务计算
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
  
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
}

test "boundary_conditions" {
  // 测试边界条件
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "complex_workflow_simulation" {
  // 测试复杂工作流程模拟
  let initial_amount = 1000
  let monthly_income = 200
  let monthly_expense = 150
  let months = 6
  
  // 计算总收入
  let total_income = multiply(monthly_income, months)
  
  // 计算总支出
  let total_expense = multiply(monthly_expense, months)
  
  // 计算净收益
  let net_profit = subtract(total_income, total_expense)
  
  // 计算最终余额
  let final_balance = add(initial_amount, net_profit)
  
  // 计算需要的投资周期（假设目标为2000）
  let investment_cycles = divide_with_ceil(2000, final_balance)
  
  assert_eq(1200, total_income)
  assert_eq(900, total_expense)
  assert_eq(300, net_profit)
  assert_eq(1300, final_balance)
  assert_eq(2, investment_cycles)
}