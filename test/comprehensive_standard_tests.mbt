// 综合标准测试用例
// 测试 azimuth 核心功能的各个方面

test "arithmetics_operations_combination" {
  // 测试算术运算的组合
  let a = 10
  let b = 20
  let c = 30
  
  // 复合运算：(a + b) * c
  let sum = add(a, b)
  let product = multiply(sum, c)
  assert_eq(900, product)
  
  // 另一种组合：a * b + c
  let product2 = multiply(a, b)
  let sum2 = add(product2, c)
  assert_eq(230, sum2)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  assert_eq_string("Hello, Hello World!", greet(spaced_string))
  
  // 测试数字字符串
  let numeric_string = "2023"
  assert_eq_string("Hello, 2023!", greet(numeric_string))
}

test "overflow_boundary_comprehensive" {
  // 综合测试溢出边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的各种运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  
  // 最小值的各种运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 极值相乘
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  let z = 35
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
}

test "real_world_calculation_scenarios" {
  // 实际计算场景
  // 场景1：计算矩形周长
  let length = 15
  let width = 10
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(50, perimeter)
  
  // 场景2：计算折扣价格
  let original_price = 200
  let discount_percent = 20
  let discount_amount = multiply(original_price, discount_percent) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(160, final_price)
}

test "negative_number_operations" {
  // 负数运算测试
  let a = -10
  let b = -20
  let c = 30
  
  // 负数相加
  assert_eq(-30, add(a, b))
  
  // 正负数相加
  assert_eq(20, add(b, c))
  
  // 负数相乘
  assert_eq(200, multiply(a, b))
  
  // 正负数相乘
  assert_eq(-300, multiply(a, c))
}

test "zero_value_operations" {
  // 零值运算测试
  let positive = 100
  let negative = -50
  let zero = 0
  
  // 与零相加
  assert_eq(positive, add(positive, zero))
  assert_eq(negative, add(negative, zero))
  assert_eq(zero, add(zero, zero))
  
  // 与零相乘
  assert_eq(zero, multiply(positive, zero))
  assert_eq(zero, multiply(negative, zero))
  assert_eq(zero, multiply(zero, zero))
}

test "large_number_operations" {
  // 大数运算测试
  let large1 = 1000000
  let large2 = 2000000
  let large3 = -3000000
  
  // 大数相加
  assert_eq(3000000, add(large1, large2))
  assert_eq(-2000000, add(large1, large3))
  
  // 大数相乘
  assert_eq(2000000000000, multiply(large1, large2))
  assert_eq(-3000000000000, multiply(large1, large3))
}

test "sequential_operations_test" {
  // 连续运算测试
  let initial = 100
  
  // 连续加法
  let result1 = add(add(add(initial, 10), 20), 30)
  assert_eq(160, result1)
  
  // 连续乘法
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
  
  // 混合连续运算
  let result3 = add(multiply(initial, 2), multiply(50, 3))
  assert_eq(350, result3)
}

test "conditional_calculation_logic" {
  // 条件计算逻辑
  let base_value = 100
  let multiplier = 5
  let addend = 50
  
  // 模拟条件：如果base_value > 50，则进行乘法运算，否则进行加法运算
  let condition = base_value > 50
  let result = if condition {
    multiply(base_value, multiplier)
  } else {
    add(base_value, addend)
  }
  
  assert_eq(500, result)
  
  // 反向条件测试
  let small_value = 30
  let condition2 = small_value > 50
  let result2 = if condition2 {
    multiply(small_value, multiplier)
  } else {
    add(small_value, addend)
  }
  
  assert_eq(80, result2)
}