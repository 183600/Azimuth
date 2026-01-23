// Azimuth 综合标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "add_function_negative_and_positive" {
  // 测试加法函数的正负数组合
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-15, 5))
  assert_eq(10, add(15, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(1500, -500))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, multiply(-456, 0))
  assert_eq(0, multiply(0, -789))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Bonjour!", greet("Bonjour"))
  assert_eq_string("Hello, Hola!", greet("Hola"))
  assert_eq_string("Hello, Guten Tag!", greet("Guten Tag"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(5, divide_with_ceil(24, 5))
  assert_eq(7, divide_with_ceil(50, 8))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let x = 12
  let y = 34
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(46, add(12, 34))
  assert_eq(46, add(34, 12))
  assert_eq(408, multiply(12, 34))
  assert_eq(408, multiply(34, 12))
}

test "inventory_packing_calculation" {
  // 库存打包计算测试
  let products = [23, 45, 67, 89, 12]
  let box_capacity = 15
  
  let boxes_1 = divide_with_ceil(products[0], box_capacity)  // ceil(23/15) = 2
  let boxes_2 = divide_with_ceil(products[1], box_capacity)  // ceil(45/15) = 3
  let boxes_3 = divide_with_ceil(products[2], box_capacity)  // ceil(67/15) = 5
  let boxes_4 = divide_with_ceil(products[3], box_capacity)  // ceil(89/15) = 6
  let boxes_5 = divide_with_ceil(products[4], box_capacity)  // ceil(12/15) = 1
  
  let total_boxes = add(add(boxes_1, boxes_2), add(add(boxes_3, boxes_4), boxes_5))
  assert_eq(17, total_boxes)  // 2+3+5+6+1 = 17
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let base = 10
  
  // 嵌套计算：((base + 5) * 2) / 3 + (base * 4) / 5
  let part1 = divide_with_ceil(multiply(add(base, 5), 2), 3)  // ceil((10+5)*2/3) = ceil(30/3) = 10
  let part2 = divide_with_ceil(multiply(base, 4), 5)          // ceil(10*4/5) = ceil(40/5) = 8
  let result = add(part1, part2)
  
  assert_eq(18, result)
}

test "string_input_boundary_conditions" {
  // 字符串输入边界条件测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, AB!", greet("AB"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  assert_eq_string("Hello, special_chars_@#$!", greet("special_chars_@#$"))
}

test "financial_rounding_calculations" {
  // 金融舍入计算测试
  // 场景：计算分期付款
  let total_amount = 1000
  let payment_periods = 3
  
  let regular_payment = total_amount / payment_periods  // 333
  let final_payment = add(regular_payment, total_amount - multiply(regular_payment, payment_periods))
  
  // 使用向上取整确保覆盖全部金额
  let adjusted_payment = divide_with_ceil(total_amount, payment_periods)
  let total_adjusted = multiply(adjusted_payment, payment_periods)
  
  assert_eq(334, adjusted_payment)  // ceil(1000/3) = 334
  assert_eq(1002, total_adjusted)  // 334 * 3 = 1002
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let data_size = 1000
  
  // 线性算法模拟
  let linear_operations = multiply(data_size, 5)
  
  // 对数算法模拟（简化版）
  let logarithmic_operations = divide_with_ceil(data_size, 50)
  
  // 验证效率差异
  let efficiency_ratio = divide_with_ceil(linear_operations, logarithmic_operations)
  
  assert_eq(5000, linear_operations)    // 1000 * 5 = 5000
  assert_eq(20, logarithmic_operations) // ceil(1000/50) = 20
  assert_eq(250, efficiency_ratio)      // ceil(5000/20) = 250
}