// 简化的综合单元测试 - 5个测试用例

// 测试用例1: 批量操作性能测试
///|
test "batch_operations_performance" {
  // 批量加法操作
  let numbers = [1, 2, 3, 4, 5]
  let mut sum = 0
  for i in 0..<numbers.length() {
    sum = add(sum, numbers[i])
  }
  assert_eq(15, sum)  // 1+2+3+4+5 = 15
  
  // 批量乘法操作（阶乘计算）
  let mut factorial = 1
  for i in 2..=5 {  // 2*3*4*5 = 120
    factorial = multiply(factorial, i)
  }
  assert_eq(120, factorial)
}

// 测试用例2: 数学恒等式验证
///|
test "mathematical_identities" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 3
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
  
  // 测试零元素性质: a + 0 = a, a * 0 = 0
  assert_eq(42, add(42, 0))
  assert_eq(0, multiply(42, 0))
  
  // 测试单位元素性质: a * 1 = a
  assert_eq(42, multiply(42, 1))
}

// 测试用例3: 特殊边界组合测试
///|
test "special_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与-1的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483648, multiply(max_val, -1))
  
  // 测试最小值与1的运算
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(-2147483648, multiply(min_val, 1))
  
  // 测试最大值与最小值的乘法
  assert_eq(-2147483648, multiply(max_val, min_val))
}

// 测试用例4: 字符串处理高级测试
///|
test "advanced_string_processing" {
  // 测试包含空格的字符串
  assert_eq("Hello, Moon Bit!", greet("Moon Bit"))
  
  // 测试包含数字和字母的混合字符串
  assert_eq("Hello, Test123!", greet("Test123"))
  
  // 测试包含Unicode字符的字符串
  assert_eq("Hello, 测试!", greet("测试"))
  
  // 测试只包含空格的字符串
  assert_eq("Hello,   !", greet("   "))
}

// 测试用例5: 实际业务场景测试
///|
test "real_business_scenarios" {
  // 场景1: 银行账户余额计算
  let initial_balance = 1000
  let deposit1 = 500
  let deposit2 = 300
  let withdrawal1 = 200
  let withdrawal2 = 150
  
  let final_balance = add(
    add(
      add(initial_balance, deposit1), 
      deposit2
    ),
    add(-withdrawal1, -withdrawal2)
  )
  assert_eq(1450, final_balance)  // 1000+500+300-200-150 = 1450
  
  // 场景2: 库存管理
  let initial_stock = 100
  let shipments_received = 3
  let items_per_shipment = 25
  let items_sold = 57
  
  let current_stock = add(
    initial_stock,
    multiply(shipments_received, items_per_shipment)
  )
  current_stock = add(current_stock, -items_sold)
  assert_eq(118, current_stock)  // 100 + 3*25 - 57 = 118
}