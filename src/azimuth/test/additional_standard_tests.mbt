// 额外的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 测试1: 基本算术运算组合
test "arithmetic_combination_operations" {
  // 测试 (a + b) * c - d 的复合运算
  let a = 15
  let b = 25
  let c = 3
  let d = 40
  
  let sum_ab = @azimuth.add(a, b)  // 40
  let product = @azimuth.multiply(sum_ab, c)  // 120
  let result = @azimuth.add(product, -d)  // 80
  
  @azimuth.assert_eq(80, result)
}

// 测试2: 字符串边界情况处理
test "string_boundary_handling" {
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_name = "VeryLongNameThatTestsStringHandlingCapabilities"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

// 测试3: 数学运算律验证
test "mathematical_laws_verification" {
  let x = 18
  let y = 23
  let z = 7
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(x, @azimuth.add(y, z))
  let distributive_right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

// 测试4: 边界值特殊运算
test "boundary_value_special_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 特殊乘法情况
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试5: 实际应用场景 - 库存管理
test "inventory_management_scenario" {
  // 商品A: 单价120，库存5件
  let item_a_price = 120
  let item_a_quantity = 5
  
  // 商品B: 单价85，库存3件
  let item_b_price = 85
  let item_b_quantity = 3
  
  // 商品C: 单价200，库存2件
  let item_c_price = 200
  let item_c_quantity = 2
  
  // 计算各商品总价值
  let total_a = @azimuth.multiply(item_a_price, item_a_quantity)
  let total_b = @azimuth.multiply(item_b_price, item_b_quantity)
  let total_c = @azimuth.multiply(item_c_price, item_c_quantity)
  
  // 计算库存总价值
  let inventory_value = @azimuth.add(@azimuth.add(total_a, total_b), total_c)
  
  @azimuth.assert_eq(1255, inventory_value)  // 120*5 + 85*3 + 200*2 = 600 + 255 + 400 = 1255
}

// 测试6: 数字序列计算
test "numeric_sequence_calculations" {
  // 计算等差数列前5项和: 3, 7, 11, 15, 19
  let term1 = 3
  let term2 = 7
  let term3 = 11
  let term4 = 15
  let term5 = 19
  
  let sequence_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(55, sequence_sum)
  
  // 计算等比数列前4项积: 2, 4, 8, 16
  let geo_term1 = 2
  let geo_term2 = 4
  let geo_term3 = 8
  let geo_term4 = 16
  
  let sequence_product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(1024, sequence_product)
}

// 测试7: 条件计算场景
test "conditional_calculation_scenarios" {
  let base_value = 100
  let multiplier = 3
  let adjustment = 25
  
  // 模拟条件：如果基础值大于50，则应用乘数，否则加调整值
  let condition = base_value > 50
  let result = if condition {
    @azimuth.multiply(base_value, multiplier)
  } else {
    @azimuth.add(base_value, adjustment)
  }
  
  @azimuth.assert_eq(300, result)
  
  // 测试相反条件
  let small_base = 30
  let condition2 = small_base > 50
  let result2 = if condition2 {
    @azimuth.multiply(small_base, multiplier)
  } else {
    @azimuth.add(small_base, adjustment)
  }
  
  @azimuth.assert_eq(55, result2)
}

// 测试8: 字符串格式化应用
test "string_formatting_applications" {
  // 测试数字字符串
  let year = "2023"
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet(year))
  
  // 测试包含符号的字符串
  let email = "user@example.com"
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  
  // 测试混合内容字符串
  let mixed = "Test123@Project"
  @azimuth.assert_eq_string("Hello, Test123@Project!", @azimuth.greet(mixed))
}

// 测试9: 函数组合应用
test "function_composition_applications" {
  // 计算圆柱体表面积（简化）: 2πr² + 2πrh
  // 使用简化值: π ≈ 3
  let radius = 5
  let height = 10
  
  // 底面积: πr²
  let base_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  // 侧面积: 2πrh
  let side_area = @azimuth.multiply(2, @azimuth.multiply(3, @azimuth.multiply(radius, height)))
  // 总表面积: 2 * 底面积 + 侧面积
  let surface_area = @azimuth.add(@azimuth.multiply(2, base_area), side_area)
  
  @azimuth.assert_eq(450, surface_area)  // 2*75 + 300 = 150 + 300 = 450
}

// 测试10: 错误处理和异常情况
test "error_handling_and_exception_cases" {
  // 测试断言函数的正确性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试边界值的安全处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试正常运算
  @azimuth.assert_true(@azimuth.add(25, 17) == 42)
  @azimuth.assert_true(@azimuth.multiply(6, 9) == 54)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}