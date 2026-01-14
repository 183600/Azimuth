// 简化的综合单元测试 - 5个测试用例

// 导入必要的包
// 注意：由于moonbitlang/core/array不可用，我们将使用简化的方法

// 使用从 lib.mbt 导入的函数

// 测试用例1: 批量操作性能测试
test "batch_operations_performance" {
  // 批量加法操作（简化版本，不使用数组）
  let mut sum = 0
  sum = @test_only.add(sum, 1)
  sum = @test_only.add(sum, 2)
  sum = @test_only.add(sum, 3)
  sum = @test_only.add(sum, 4)
  sum = @test_only.add(sum, 5)
  @test_only.assert_eq(15, sum)
  
  // 批量乘法操作（简化版本，不使用数组）
  let mut product = 1
  product = @test_only.multiply(product, 1)
  product = @test_only.multiply(product, 2)
  product = @test_only.multiply(product, 3)
  @test_only.assert_eq(6, product)
}

// 测试用例2: 数学恒等式验证
test "mathematical_identities" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 3
  let c = 5
  let left = @test_only.multiply(a, @test_only.add(b, c))
  let right = @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c))
  @test_only.assert_eq(left, right)
  
  // 测试零元素性质: a + 0 = a, a * 0 = 0
  @test_only.assert_eq(42, @test_only.add(42, 0))
  @test_only.assert_eq(0, @test_only.multiply(42, 0))
  
  // 测试单位元素性质: a * 1 = a
  @test_only.assert_eq(42, @test_only.multiply(42, 1))
}

// 测试用例3: 特殊边界组合测试
test "special_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与-1的运算
  @test_only.assert_eq(2147483646, @test_only.add(max_val, -1))
  @test_only.assert_eq(-2147483648, @test_only.multiply(max_val, -1))
  
  // 测试最小值与1的运算
  @test_only.assert_eq(-2147483647, @test_only.add(min_val, 1))
  @test_only.assert_eq(-2147483648, @test_only.multiply(min_val, 1))
  
  // 测试最大值与最小值的乘法
  @test_only.assert_eq(-2147483648, @test_only.multiply(max_val, min_val))
}

// 测试用例4: 数学性质验证测试
test "mathematical_properties_verification" {
  // 验证交换律（在非溢出情况下）
  @test_only.assert_eq(@test_only.add(5, 3), @test_only.add(3, 5))
  @test_only.assert_eq(@test_only.multiply(4, 7), @test_only.multiply(7, 4))
  @test_only.assert_eq(@test_only.add(-2, 8), @test_only.add(8, -2))
  @test_only.assert_eq(@test_only.multiply(-3, 6), @test_only.multiply(6, -3))
  
  // 验证结合律（在非溢出情况下）
  @test_only.assert_eq(@test_only.add(@test_only.add(1, 2), 3), @test_only.add(1, @test_only.add(2, 3)))
  @test_only.assert_eq(@test_only.multiply(@test_only.multiply(2, 3), 4), @test_only.multiply(2, @test_only.multiply(3, 4)))
  
  // 验证分配律（在非溢出情况下）
  let a = 5
  let b = 3
  let c = 2
  @test_only.assert_eq(@test_only.multiply(a, @test_only.add(b, c)), @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c)))
}

// 测试用例5: 实际业务场景测试
test "real_business_scenarios" {
  // 场景1: 银行账户余额计算
  let initial_balance = 1000
  let deposit1 = 500
  let deposit2 = 300
  let withdrawal1 = 200
  let withdrawal2 = 150
  
  let final_balance = @test_only.add(
    @test_only.add(
      @test_only.add(initial_balance, deposit1), 
      deposit2
    ),
    @test_only.add(-withdrawal1, -withdrawal2)
  )
  @test_only.assert_eq(1450, final_balance)  // 1000+500+300-200-150 = 1450
  
  // 场景2: 库存管理
  let initial_stock = 100
  let shipments_received = 3
  let items_per_shipment = 25
  let items_sold = 57
  
  let mut current_stock = @test_only.add(
    initial_stock,
    @test_only.multiply(shipments_received, items_per_shipment)
  )
  current_stock = @test_only.add(current_stock, -items_sold)
  @test_only.assert_eq(118, current_stock)  // 100 + 3*25 - 57 = 118
}

// 新增的测试用例
test "arithmetics_add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  @test_only.assert_eq(@test_only.add(max_val, 0), max_val)
  @test_only.assert_eq(@test_only.add(min_val, 0), min_val)
  @test_only.assert_eq(@test_only.add(max_val, -max_val), 0)
}

test "arithmetics_multiply_edge_cases" {
  // 测试乘法边界情况
  @test_only.assert_eq(@test_only.multiply(-2147483648, 0), 0)
  @test_only.assert_eq(@test_only.multiply(2147483647, 1), 2147483647)
  @test_only.assert_eq(@test_only.multiply(-2147483648, 1), -2147483648)
}

test "string_greet_unicode_characters" {
  // 测试包含Unicode字符的字符串处理
  @test_only.assert_eq_string(@test_only.greet("世界"), "Hello, 世界!")
  @test_only.assert_eq_string(@test_only.greet("🌟"), "Hello, 🌟!")
  @test_only.assert_eq_string(@test_only.greet("Café"), "Hello, Café!")
}