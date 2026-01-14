// 新的标准 MoonBit 测试用例
// 这些测试用例专注于验证 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本的算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "overflow_protection_mechanisms" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 减法溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), 
                     @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 零值运算
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  
  // 单位值运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形的周长和面积
  let length = 15
  let width = 8
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(46, perimeter)  // 2*(15+8) = 46
  @azimuth.assert_eq(120, area)      // 15*8 = 120
  
  // 场景2：计算平均数
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4)
  let average = sum / 4
  
  @azimuth.assert_eq(350, sum)      // 85+90+95+80 = 350
  @azimuth.assert_eq(87, average)   // 350/4 = 87 (整数除法)
}

test "conditional_logic_integration" {
  // 测试条件逻辑集成
  let base_value = 100
  let multiplier = 3
  let threshold = 200
  
  // 如果 base_value * multiplier > threshold，则计算总和，否则计算差值
  let product = @azimuth.multiply(base_value, multiplier)
  let condition = product > threshold
  let result = if condition {
    @azimuth.add(base_value, product)
  } else {
    @azimuth.add(base_value, -product)
  }
  
  @azimuth.assert_eq(400, result)  // 100*3=300>200, so 100+300=400
  
  // 反向测试
  let base_value2 = 50
  let product2 = @azimuth.multiply(base_value2, multiplier)
  let condition2 = product2 > threshold
  let result2 = if condition2 {
    @azimuth.add(base_value2, product2)
  } else {
    @azimuth.add(base_value2, -product2)
  }
  
  @azimuth.assert_eq(-100, result2)  // 50*3=150<200, so 50-150=-100
}

test "real_world_application_simulation" {
  // 测试实际应用模拟
  // 场景：计算购物车的总价（含折扣和税费）
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算税费和最终总价
  let discounted_total = @azimuth.add(subtotal, -discount)
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  let total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(797, total)  // (299*2 + 199*1) = 797, 折扣79, 税费57, 总计775
}

test "function_composition_patterns" {
  // 测试函数组合模式
  // 计算：((a + b) * c) - (d * e)
  let a = 10
  let b = 5
  let c = 3
  let d = 4
  let e = 6
  
  let sum_ab = @azimuth.add(a, b)
  let product1 = @azimuth.multiply(sum_ab, c)
  let product2 = @azimuth.multiply(d, e)
  let result = @azimuth.add(product1, -product2)
  
  @azimuth.assert_eq(21, result)  // ((10+5)*3) - (4*6) = 45 - 24 = 21
  
  // 嵌套函数调用测试
  let nested_result = @azimuth.add(
    @azimuth.multiply(
      @azimuth.add(2, 3),
      @azimuth.add(4, 5)
    ),
    @azimuth.multiply(6, 7)
  )
  
  @azimuth.assert_eq(67, nested_result)  // (2+3)*(4+5) + 6*7 = 5*9 + 42 = 45 + 42 = 87
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 100) == min_val)
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}