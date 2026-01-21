// 高质量增强测试用例 - 为 azimuth 库添加的额外测试
// 使用标准 MoonBit 测试语法

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  assert_eq(x, add(x, 0))  // 加法恒等元
  assert_eq(x, multiply(x, 1))  // 乘法恒等元
  assert_eq(0, multiply(x, 0))  // 零乘积性质
}

test "boundary_value_analysis" {
  // 边界值分析测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "divide_with_ceil_fractional_behavior" {
  // 测试向上取整除法的小数行为
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数
  let prefix = "Dear"
  let name = "MoonBit"
  let full_greeting = greet(prefix + " " + name)
  assert_eq_string("Hello, Dear MoonBit!", full_greeting)
}

test "complex_arithmetic_sequence" {
  // 复杂算术序列测试
  let a1 = 5
  let d = 3
  let n = 6
  
  // 计算等差数列第n项：an = a1 + (n-1)*d
  let an = add(a1, multiply(n - 1, d))
  assert_eq(20, an)  // 5 + 5*3 = 20
  
  // 计算等差数列前n项和：Sn = n/2 * (a1 + an)
  let sn = divide_with_ceil(multiply(n, add(a1, an)), 2)
  assert_eq(75, sn)  // 6/2 * (5 + 20) = 3 * 25 = 75
}

test "error_resilience_edge_cases" {
  // 错误恢复边界情况测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
}

test "real_world_inventory_calculation" {
  // 真实世界库存计算
  let product_a_qty = 23
  let product_b_qty = 17
  let product_c_qty = 31
  let box_capacity = 12
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)  // ceil(23/12) = 2
  assert_eq(2, boxes_b)  // ceil(17/12) = 2
  assert_eq(3, boxes_c)  // ceil(31/12) = 3
  assert_eq(7, total_boxes)  // 2 + 2 + 3 = 7
}

test "associative_and_commutative_laws" {
  // 结合律和交换律测试
  let a = 8
  let b = 13
  let c = 21
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "calculation_precision_validation" {
  // 计算精度验证
  let dividend = 100
  let divisor = 3
  let expected_quotient = 34  // ceil(100/3) = 34
  
  assert_eq(expected_quotient, divide_with_ceil(dividend, divisor))
  
  // 验证结果确实大于或等于实际除法结果
  let actual_division = dividend / divisor
  assert_eq(true, expected_quotient >= actual_division)
}