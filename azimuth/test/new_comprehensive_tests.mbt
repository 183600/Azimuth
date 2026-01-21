// 新的综合测试用例
// 测试 azimuth 包的核心功能，专注于边界情况和实际应用场景

test "add_sequence_operations" {
  // 测试连续加法运算
  let result1 = @azimuth.add(1, 2)
  let result2 = @azimuth.add(result1, 3)
  let result3 = @azimuth.add(result2, 4)
  let result4 = @azimuth.add(result3, 5)
  @azimuth.assert_eq(15, result4)
  
  // 测试负数连续加法
  let neg_result1 = @azimuth.add(-1, -2)
  let neg_result2 = @azimuth.add(neg_result1, -3)
  let neg_result3 = @azimuth.add(neg_result2, 6)
  @azimuth.assert_eq(0, neg_result3)
}

test "multiply_geometric_progression" {
  // 测试几何级数乘法
  let result1 = @azimuth.multiply(2, 2)
  let result2 = @azimuth.multiply(result1, 2)
  let result3 = @azimuth.multiply(result2, 2)
  let result4 = @azimuth.multiply(result3, 2)
  @azimuth.assert_eq(32, result4)
  
  // 测试混合符号的几何级数
  let mixed_result1 = @azimuth.multiply(-2, 3)
  let mixed_result2 = @azimuth.multiply(mixed_result1, -2)
  let mixed_result3 = @azimuth.multiply(mixed_result2, 3)
  @azimuth.assert_eq(72, mixed_result3)
}

test "divide_with_ceil_real_world_scenarios" {
  // 测试实际应用场景中的向上取整除法
  
  // 场景1：计算需要的容器数量
  let items = 17
  let container_capacity = 5
  let containers_needed = @azimuth.divide_with_ceil(items, container_capacity)
  @azimuth.assert_eq(4, containers_needed)
  
  // 场景2：计算需要的页数
  let lines = 23
  let lines_per_page = 10
  let pages_needed = @azimuth.divide_with_ceil(lines, lines_per_page)
  @azimuth.assert_eq(3, pages_needed)
  
  // 场景3：负数场景（债务分摊）
  let debt = -25
  let people = 4
  let debt_per_person = @azimuth.divide_with_ceil(debt, people)
  @azimuth.assert_eq(-6, debt_per_person)
}

test "complex_financial_calculation" {
  // 测试复杂的金融计算场景
  
  // 计算商品总价（含税）
  let item_price = 199
  let quantity = 3
  let tax_multiplier = 12  // 表示120%（含20%税）
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total_with_tax = @azimuth.multiply(subtotal, tax_multiplier)
  
  // 由于我们的乘法函数返回整数，这里我们假设tax_multiplier已经是百分比形式
  // 实际结果应该是 199 * 3 * 1.2 = 716.4，但整数运算会得到 7164
  @azimuth.assert_eq(7164, total_with_tax)
  
  // 计算折扣后的价格
  let discount_amount = 50
  let final_price = @azimuth.add(total_with_tax, -discount_amount)
  @azimuth.assert_eq(7114, final_price)
}

test "greet_multilingual_support" {
  // 测试多语言支持的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Мира!", @azimuth.greet("Мира"))
  @azimuth.assert_eq_string("Hello, 世界🌍!", @azimuth.greet("世界🌍"))
  @azimuth.assert_eq_string("Hello, Mr. Smith!", @azimuth.greet("Mr. Smith"))
  @azimuth.assert_eq_string("Hello, José!", @azimuth.greet("José"))
  @azimuth.assert_eq_string("Hello, 名字!", @azimuth.greet("名字"))
}

test "boundary_value_comprehensive_test" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况
  
  // 测试接近边界的值
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  
  // 加法交换律
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  let c = 11111
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  
  // 测试极值运算后的函数仍然可用
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 执行溢出运算
  let _ = @azimuth.add(max_val, max_val)
  let _ = @azimuth.multiply(max_val, max_val)
  
  // 验证函数仍然正常工作
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  
  // 测试断言函数的健壮性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "performance_and_stress_test" {
  // 性能和压力测试
  
  // 连续运算测试
  let mut result = 1
  for i = 1; i <= 10; i = i + 1 {
    result = @azimuth.multiply(result, 2)
  }
  @azimuth.assert_eq(1024, result)
  
  // 累加测试
  let mut sum = 0
  for i = 1; i <= 100; i = i + 1 {
    sum = @azimuth.add(sum, i)
  }
  @azimuth.assert_eq(5050, sum)
  
  // 混合运算压力测试
  let mut mixed_result = 10
  mixed_result = @azimuth.add(mixed_result, 5)  // 15
  mixed_result = @azimuth.multiply(mixed_result, 2)  // 30
  mixed_result = @azimuth.add(mixed_result, -10)  // 20
  mixed_result = @azimuth.multiply(mixed_result, 3)  // 60
  mixed_result = @azimuth.divide_with_ceil(mixed_result, 4)  // 15
  @azimuth.assert_eq(15, mixed_result)
}