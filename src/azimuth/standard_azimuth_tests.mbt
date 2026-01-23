// 标准 Azimuth 测试用例
// 使用标准 MoonBit 测试语法编写的10个高质量测试用例

test "add_function_identity_elements" {
  // 测试加法的单位元性质
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, add(0, 0))
  assert_eq(2147483647, add(2147483647, 0))
}

test "multiply_function_zero_property" {
  // 测试乘法的零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(-10, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(2147483647, 0))
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(1000, divide_with_ceil(99999, 100))  // 999.99 -> 1000
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "mathematical_commutative_laws" {
  // 测试加法和乘法的交换律
  let a = 12
  let b = 18
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试不同的数值组合
  let x = -5
  let y = 7
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax)
  assert_eq(609, total_before_discount)
  assert_eq(559, final_total)
}

test "negative_number_operations_comprehensive" {
  // 全面的负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试接近Int最大值的运算
  let near_max = 2147483646
  assert_eq(2147483647, add(near_max, 1))
  
  // 测试接近Int最小值的运算
  let near_min = -2147483647
  assert_eq(-2147483648, add(near_min, -1))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_packaging_optimization" {
  // 真实世界包装优化场景
  let product_weights = [15, 23, 8, 12, 7]  // 产品重量（kg）
  let max_weight_per_box = 20  // 每箱最大重量（kg）
  
  // 计算每个产品需要的包装箱数量
  let boxes_1 = divide_with_ceil(product_weights[0], max_weight_per_box)
  let boxes_2 = divide_with_ceil(product_weights[1], max_weight_per_box)
  let boxes_3 = divide_with_ceil(product_weights[2], max_weight_per_box)
  let boxes_4 = divide_with_ceil(product_weights[3], max_weight_per_box)
  let boxes_5 = divide_with_ceil(product_weights[4], max_weight_per_box)
  
  // 计算总包装箱数量
  let total_boxes = add(add(add(add(boxes_1, boxes_2), boxes_3), boxes_4), boxes_5)
  
  assert_eq(1, boxes_1)  // ceil(15/20) = 1
  assert_eq(2, boxes_2)  // ceil(23/20) = 2
  assert_eq(1, boxes_3)  // ceil(8/20) = 1
  assert_eq(1, boxes_4)  // ceil(12/20) = 1
  assert_eq(1, boxes_5)  // ceil(7/20) = 1
  assert_eq(6, total_boxes)  // 1 + 2 + 1 + 1 + 1 = 6
}

test "error_handling_and_safety" {
  // 错误处理和安全性测试
  // 测试除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值运算
  let extreme_large = 1000000
  let extreme_small = 1
  assert_eq(1000001, add(extreme_large, extreme_small))
  assert_eq(999999, add(extreme_large, -extreme_small))
  assert_eq(1000000, multiply(extreme_large, extreme_small))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, This is a very long string that tests the greet function with extensive input!", greet("This is a very long string that tests the greet function with extensive input!"))
}